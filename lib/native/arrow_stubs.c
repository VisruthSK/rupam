#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "nanoarrow/nanoarrow.h"
#include "nanoarrow/nanoarrow_ipc.h"

typedef struct {
  uint8_t *data;
  int64_t size;
#ifdef _WIN32
  HANDLE file;
  HANDLE mapping;
#endif
  struct ArrowIpcDecoder decoder;
  struct ArrowSchema schema;
  int64_t batches;
  struct ArrowIpcFileBlock *blocks;
  int64_t *starts;
  int64_t rows;
  int open;
} result;

#define Result_val(v) (*((result **)Data_custom_val(v)))

static void release(result *r) {
  if (!r->open) return;
  r->open = 0;
  ArrowIpcDecoderReset(&r->decoder);
  if (r->schema.release) r->schema.release(&r->schema);
  free(r->blocks);
  free(r->starts);
#ifdef _WIN32
  UnmapViewOfFile(r->data);
  CloseHandle(r->mapping);
  CloseHandle(r->file);
#else
  munmap(r->data, r->size);
#endif
}

static void finalize(value v) {
  result *r = Result_val(v);
  if (r) {
    release(r);
    free(r);
  }
}

static struct custom_operations operations = {
    "rupam.arrow_result", finalize, custom_compare_default, custom_hash_default,
    custom_serialize_default, custom_deserialize_default, custom_compare_ext_default,
    custom_fixed_length_default};

static void fail(result *r, const char *message) {
  release(r);
  free(r);
  caml_failwith(message);
}

static int map(result *r, const char *path) {
#ifdef _WIN32
  r->file = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_DELETE, NULL,
                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (r->file == INVALID_HANDLE_VALUE) return 0;
  LARGE_INTEGER size;
  if (!GetFileSizeEx(r->file, &size)) return 0;
  r->size = size.QuadPart;
  r->mapping = CreateFileMappingA(r->file, NULL, PAGE_READONLY, 0, 0, NULL);
  if (!r->mapping) return 0;
  r->data = MapViewOfFile(r->mapping, FILE_MAP_READ, 0, 0, 0);
  return r->data != NULL;
#else
  int fd = open(path, O_RDONLY);
  if (fd < 0) return 0;
  struct stat st;
  if (fstat(fd, &st) != 0) {
    close(fd);
    return 0;
  }
  r->size = st.st_size;
  r->data = mmap(NULL, r->size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  return r->data != MAP_FAILED;
#endif
}

static struct ArrowBufferView view(const uint8_t *data, int64_t size) {
  struct ArrowBufferView v;
  v.data.as_uint8 = data;
  v.size_bytes = size;
  return v;
}

static struct ArrowArrayView *batch(result *r, int64_t i, struct ArrowError *error) {
  struct ArrowIpcFileBlock *b = &r->blocks[i];
  struct ArrowBufferView header = view(r->data + b->offset, b->metadata_length);
  struct ArrowBufferView body =
      view(r->data + b->offset + b->metadata_length, b->body_length);
  struct ArrowArrayView *out = NULL;
  if (ArrowIpcDecoderDecodeHeader(&r->decoder, header, error) != NANOARROW_OK) return NULL;
  if (ArrowIpcDecoderDecodeArrayView(&r->decoder, body, -1, &out, error) != NANOARROW_OK)
    return NULL;
  return out;
}

CAMLprim value rupam_arrow_open(value path) {
  CAMLparam1(path);
  CAMLlocal1(handle);
  struct ArrowError error;
  result *r = calloc(1, sizeof(result));
  if (!r || !map(r, String_val(path))) {
    free(r);
    caml_failwith("Cannot map the result file");
  }
  r->open = 1;
  ArrowIpcDecoderInit(&r->decoder);
  struct ArrowBufferView whole = view(r->data, r->size);
  if (ArrowIpcDecoderVerifyFooter(&r->decoder, whole, &error) != NANOARROW_OK ||
      ArrowIpcDecoderDecodeFooter(&r->decoder, whole, &error) != NANOARROW_OK)
    fail(r, "The result file has no valid Arrow footer");
  struct ArrowIpcFooter *footer = r->decoder.footer;
  if (ArrowSchemaDeepCopy(&footer->schema, &r->schema) != NANOARROW_OK)
    fail(r, "Cannot copy the result schema");
  struct ArrowSchema decoding;
  if (ArrowSchemaDeepCopy(&footer->schema, &decoding) != NANOARROW_OK ||
      ArrowIpcDecoderSetSchema(&r->decoder, &decoding, &error) != NANOARROW_OK)
    fail(r, "Unsupported result schema");
  r->batches = footer->record_batch_blocks.size_bytes / sizeof(struct ArrowIpcFileBlock);
  r->blocks = malloc(sizeof(struct ArrowIpcFileBlock) * (r->batches + 1));
  r->starts = malloc(sizeof(int64_t) * (r->batches + 1));
  memcpy(r->blocks, footer->record_batch_blocks.data, footer->record_batch_blocks.size_bytes);
  r->rows = 0;
  for (int64_t i = 0; i < r->batches; i++) {
    r->starts[i] = r->rows;
    struct ArrowArrayView *v = batch(r, i, &error);
    if (!v) fail(r, "The result file has an unreadable batch");
    r->rows += v->length;
  }
  r->starts[r->batches] = r->rows;
  handle = caml_alloc_custom(&operations, sizeof(result *), 0, 1);
  Result_val(handle) = r;
  CAMLreturn(handle);
}

static result *live(value handle) {
  result *r = Result_val(handle);
  if (!r || !r->open) caml_failwith("The result was released");
  return r;
}

CAMLprim value rupam_arrow_rows(value handle) {
  return Val_long(live(handle)->rows);
}

CAMLprim value rupam_arrow_names(value handle) {
  CAMLparam1(handle);
  CAMLlocal1(names);
  result *r = live(handle);
  names = caml_alloc(r->schema.n_children, 0);
  for (int64_t i = 0; i < r->schema.n_children; i++)
    Store_field(names, i, caml_copy_string(r->schema.children[i]->name));
  CAMLreturn(names);
}

static value cell(struct ArrowArrayView *column, int64_t i) {
  char text[64];
  if (ArrowArrayViewIsNull(column, i)) return Val_none;
  if (column->dictionary) {
    struct ArrowStringView s =
        ArrowArrayViewGetStringUnsafe(column->dictionary, ArrowArrayViewGetIntUnsafe(column, i));
    return caml_alloc_some(caml_alloc_initialized_string(s.size_bytes, s.data));
  }
  switch (column->storage_type) {
    case NANOARROW_TYPE_BOOL:
      return caml_alloc_some(
          caml_copy_string(ArrowArrayViewGetIntUnsafe(column, i) ? "true" : "false"));
    case NANOARROW_TYPE_INT8:
    case NANOARROW_TYPE_INT16:
    case NANOARROW_TYPE_INT32:
    case NANOARROW_TYPE_INT64:
    case NANOARROW_TYPE_UINT8:
    case NANOARROW_TYPE_UINT16:
    case NANOARROW_TYPE_UINT32:
      snprintf(text, sizeof text, "%" PRId64, ArrowArrayViewGetIntUnsafe(column, i));
      return caml_alloc_some(caml_copy_string(text));
    case NANOARROW_TYPE_HALF_FLOAT:
    case NANOARROW_TYPE_FLOAT:
    case NANOARROW_TYPE_DOUBLE:
      snprintf(text, sizeof text, "%.17g", ArrowArrayViewGetDoubleUnsafe(column, i));
      return caml_alloc_some(caml_copy_string(text));
    case NANOARROW_TYPE_STRING:
    case NANOARROW_TYPE_LARGE_STRING:
    case NANOARROW_TYPE_STRING_VIEW: {
      struct ArrowStringView s = ArrowArrayViewGetStringUnsafe(column, i);
      return caml_alloc_some(caml_alloc_initialized_string(s.size_bytes, s.data));
    }
    default:
      return caml_alloc_some(caml_copy_string("?"));
  }
}

CAMLprim value rupam_arrow_cells(value handle, value v_offset, value v_limit, value v_columns) {
  CAMLparam4(handle, v_offset, v_limit, v_columns);
  CAMLlocal3(columns, column, c);
  struct ArrowError error;
  result *r = live(handle);
  int64_t offset = Long_val(v_offset);
  int64_t limit = Long_val(v_limit);
  if (offset < 0) offset = 0;
  if (offset > r->rows) offset = r->rows;
  if (limit > r->rows - offset) limit = r->rows - offset;
  if (limit < 0) limit = 0;
  int64_t width = Wosize_val(v_columns);
  for (int64_t j = 0; j < width; j++)
    if (Long_val(Field(v_columns, j)) < 0 || Long_val(Field(v_columns, j)) >= r->schema.n_children)
      caml_invalid_argument("column index");
  columns = caml_alloc(width, 0);
  for (int64_t j = 0; j < width; j++) Store_field(columns, j, caml_alloc(limit, 0));
  int64_t row = offset;
  int64_t low = 0, high = r->batches;
  while (low < high) {
    int64_t mid = low + (high - low) / 2;
    if (r->starts[mid + 1] <= row) low = mid + 1; else high = mid;
  }
  int64_t b = low;
  while (row < offset + limit && b < r->batches) {
    struct ArrowArrayView *v = batch(r, b, &error);
    if (!v) caml_failwith("The result file has an unreadable batch");
    int64_t end = r->starts[b + 1];
    for (; row < end && row < offset + limit; row++) {
      for (int64_t j = 0; j < width; j++) {
        c = cell(v->children[Long_val(Field(v_columns, j))], row - r->starts[b]);
        column = Field(columns, j);
        Store_field(column, row - offset, c);
      }
    }
    b++;
  }
  CAMLreturn(columns);
}

CAMLprim value rupam_arrow_value(value handle, value v_row, value v_col) {
  CAMLparam3(handle, v_row, v_col);
  CAMLlocal2(payload, answer);
  result *r = live(handle);
  int64_t row = Long_val(v_row), col = Long_val(v_col);
  if (row < 0 || row >= r->rows || col < 0 || col >= r->schema.n_children)
    caml_invalid_argument("cell index");
  int64_t low = 0, high = r->batches;
  while (low < high) {
    int64_t mid = low + (high - low) / 2;
    if (r->starts[mid + 1] <= row) low = mid + 1; else high = mid;
  }
  struct ArrowError error;
  struct ArrowArrayView *v = batch(r, low, &error);
  if (!v) caml_failwith("The result file has an unreadable batch");
  struct ArrowArrayView *column = v->children[col];
  row -= r->starts[low];
  if (ArrowArrayViewIsNull(column, row)) CAMLreturn(Val_int(0));
  if (column->dictionary) {
    row = ArrowArrayViewGetIntUnsafe(column, row);
    column = column->dictionary;
    if (ArrowArrayViewIsNull(column, row)) CAMLreturn(Val_int(0));
  }
  int tag;
  switch (column->storage_type) {
    case NANOARROW_TYPE_BOOL:
      tag = 0;
      payload = Val_bool(ArrowArrayViewGetIntUnsafe(column, row));
      break;
    case NANOARROW_TYPE_INT8: case NANOARROW_TYPE_INT16:
    case NANOARROW_TYPE_INT32: case NANOARROW_TYPE_INT64:
    case NANOARROW_TYPE_UINT8: case NANOARROW_TYPE_UINT16: case NANOARROW_TYPE_UINT32:
      tag = 1;
      payload = caml_copy_int64(ArrowArrayViewGetIntUnsafe(column, row));
      break;
    case NANOARROW_TYPE_HALF_FLOAT: case NANOARROW_TYPE_FLOAT: case NANOARROW_TYPE_DOUBLE:
      tag = 2;
      payload = caml_copy_double(ArrowArrayViewGetDoubleUnsafe(column, row));
      break;
    case NANOARROW_TYPE_STRING: case NANOARROW_TYPE_LARGE_STRING:
    case NANOARROW_TYPE_STRING_VIEW: {
      struct ArrowStringView s = ArrowArrayViewGetStringUnsafe(column, row);
      tag = 3;
      payload = caml_alloc_initialized_string(s.size_bytes, s.data);
      break;
    }
    default: CAMLreturn(Val_int(1));
  }
  answer = caml_alloc(1, tag);
  Store_field(answer, 0, payload);
  CAMLreturn(answer);
}

CAMLprim value rupam_arrow_close(value handle) {
  result *r = Result_val(handle);
  if (r) release(r);
  return Val_unit;
}

static int numeric(struct ArrowArrayView *c) {
  switch (c->storage_type) {
    case NANOARROW_TYPE_INT8:
    case NANOARROW_TYPE_INT16:
    case NANOARROW_TYPE_INT32:
    case NANOARROW_TYPE_INT64:
    case NANOARROW_TYPE_UINT8:
    case NANOARROW_TYPE_UINT16:
    case NANOARROW_TYPE_UINT32:
    case NANOARROW_TYPE_FLOAT:
    case NANOARROW_TYPE_DOUBLE:
      return 1;
    default:
      return 0;
  }
}

CAMLprim value rupam_arrow_numbers(value handle, value v_column, value v_size) {
  CAMLparam3(handle, v_column, v_size);
  CAMLlocal1(numbers);
  struct ArrowError error;
  result *r = live(handle);
  int64_t j = Long_val(v_column);
  int64_t count = 0;
  numbers = caml_alloc_float_array(Long_val(v_size));
  for (int64_t b = 0; b < r->batches; b++) {
    struct ArrowArrayView *v = batch(r, b, &error);
    if (!v) caml_failwith("The result file has an unreadable batch");
    struct ArrowArrayView *c = v->children[j];
    if (!numeric(c)) caml_failwith("Expected a numeric column");
    for (int64_t i = 0; i < c->length; i++) {
      if (ArrowArrayViewIsNull(c, i)) continue;
      Store_double_field(numbers, count++, ArrowArrayViewGetDoubleUnsafe(c, i));
    }
  }
  CAMLreturn(numbers);
}

CAMLprim value rupam_arrow_histogram(value handle, value v_column, value v_bins) {
  CAMLparam3(handle, v_column, v_bins);
  CAMLlocal2(out, counts);
  struct ArrowError error;
  result *r = live(handle);
  int64_t j = Long_val(v_column);
  int64_t bins = Long_val(v_bins);
  double low = 0, high = 0;
  int64_t present = 0, missing = 0;
  for (int pass = 0; pass < 2; pass++) {
    int64_t *tally = pass ? calloc(bins, sizeof(int64_t)) : NULL;
    for (int64_t b = 0; b < r->batches; b++) {
      struct ArrowArrayView *v = batch(r, b, &error);
      if (!v) caml_failwith("The result file has an unreadable batch");
      struct ArrowArrayView *c = v->children[j];
      if (!numeric(c)) caml_failwith("!hist needs a numeric column");
      for (int64_t i = 0; i < c->length; i++) {
        if (ArrowArrayViewIsNull(c, i)) {
          if (!pass) missing++;
          continue;
        }
        double x = ArrowArrayViewGetDoubleUnsafe(c, i);
        if (!pass) {
          if (present == 0 || x < low) low = x;
          if (present == 0 || x > high) high = x;
          present++;
        } else {
          double span = high / 2 - low / 2;
          double position = span > 0 ? (x / 2 - low / 2) / span * bins : 0;
          int64_t k = position >= bins ? bins - 1 : position > 0 ? (int64_t)position : 0;
          tally[k]++;
        }
      }
    }
    if (pass) {
      counts = caml_alloc(bins, 0);
      for (int64_t k = 0; k < bins; k++) Store_field(counts, k, Val_long(tally[k]));
      free(tally);
    }
  }
  out = caml_alloc_tuple(4);
  Store_field(out, 0, caml_copy_double(low));
  Store_field(out, 1, caml_copy_double(high));
  Store_field(out, 2, counts);
  Store_field(out, 3, Val_long(missing));
  CAMLreturn(out);
}
