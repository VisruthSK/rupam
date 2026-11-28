#if defined(__linux__)
#define _GNU_SOURCE
#endif

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <fcntl.h>
#include <unistd.h>
#endif

#include "blake3.c"

#define ALIGNMENT 4096

#ifdef _WIN32
static int read_handle(HANDLE file, uint64_t offset, uint8_t *out, size_t length) {
  size_t done = 0;
  while (done < length) {
    OVERLAPPED at = {0};
    uint64_t position = offset + done;
    at.Offset = (DWORD)(position & 0xFFFFFFFFu);
    at.OffsetHigh = (DWORD)(position >> 32);
    DWORD want = (DWORD)(length - done > 0x40000000u ? 0x40000000u : length - done);
    DWORD got = 0;
    if (!ReadFile(file, out + done, want, &got, &at) || got == 0) break;
    done += got;
  }
  return done == length;
}

static int read_at(const char *path, uint64_t offset, uint8_t *out, size_t length, int direct) {
  int wide = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
  if (wide <= 0) return 0;
  wchar_t *name = malloc(sizeof(wchar_t) * wide);
  if (!name) return 0;
  MultiByteToWideChar(CP_UTF8, 0, path, -1, name, wide);
  DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
  int ok = 0;
  if (direct) {
    HANDLE file = CreateFileW(name, GENERIC_READ, share, NULL, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, NULL);
    if (file != INVALID_HANDLE_VALUE) {
      ok = read_handle(file, offset, out, length);
      CloseHandle(file);
    }
  }
  if (!ok) {
    HANDLE file = CreateFileW(name, GENERIC_READ, share, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
    if (file != INVALID_HANDLE_VALUE) {
      ok = read_handle(file, offset, out, length);
      CloseHandle(file);
    }
  }
  free(name);
  return ok;
}
#else
static int read_fd(int fd, uint64_t offset, uint8_t *out, size_t length) {
  size_t done = 0;
  while (done < length) {
    ssize_t got = pread(fd, out + done, length - done, (off_t)(offset + done));
    if (got <= 0) break;
    done += (size_t)got;
  }
  return done == length;
}

static int read_at(const char *path, uint64_t offset, uint8_t *out, size_t length, int direct) {
  int ok = 0;
#if defined(O_DIRECT)
  if (direct) {
    int fd = open(path, O_RDONLY | O_DIRECT);
    if (fd >= 0) {
      ok = read_fd(fd, offset, out, length);
      close(fd);
    }
  }
#endif
  if (!ok) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) return 0;
#if defined(F_NOCACHE)
    if (direct) fcntl(fd, F_NOCACHE, 1);
#endif
    ok = read_fd(fd, offset, out, length);
    close(fd);
  }
  return ok;
}
#endif

CAMLprim value rupam_blake3_misalignment(value buffer) {
  uintptr_t address = (uintptr_t)Caml_ba_data_val(buffer);
  return Val_long((ALIGNMENT - (long)(address % ALIGNMENT)) % ALIGNMENT);
}

CAMLprim value rupam_blake3_read(value path, value offset, value buffer, value length, value direct) {
  CAMLparam5(path, offset, buffer, length, direct);
  char *name = caml_stat_strdup(String_val(path));
  uint8_t *out = Caml_ba_data_val(buffer);
  int uncached = Bool_val(direct);
  caml_release_runtime_system();
  int ok = read_at(name, (uint64_t)Long_val(offset), out, Long_val(length), uncached);
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (!ok) caml_failwith("the file could not be read at the expected size");
  CAMLreturn(Val_unit);
}

CAMLprim value rupam_blake3_state_size(value unit) {
  (void)unit;
  return Val_long(sizeof(blake3_hasher));
}

CAMLprim value rupam_blake3_init(value state) {
  blake3_hasher_init(Caml_ba_data_val(state));
  return Val_unit;
}

CAMLprim value rupam_blake3_update(value state, value data, value length) {
  CAMLparam3(state, data, length);
  blake3_hasher *hasher = Caml_ba_data_val(state);
  const uint8_t *bytes = Caml_ba_data_val(data);
  size_t n = Long_val(length);
  caml_release_runtime_system();
  blake3_hasher_update(hasher, bytes, n);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLprim value rupam_blake3_subtree(value data, value length, value counter, value pairs,
                                    value slot) {
  CAMLparam5(data, length, counter, pairs, slot);
  const uint8_t *bytes = Caml_ba_data_val(data);
  size_t n = Long_val(length);
  uint64_t chunk = (uint64_t)Long_val(counter);
  uint8_t *pair = (uint8_t *)Caml_ba_data_val(pairs) + 2 * BLAKE3_OUT_LEN * Long_val(slot);
  caml_release_runtime_system();
  compress_subtree_to_parent_node(bytes, n, IV, chunk, 0, pair, false);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLprim value rupam_blake3_push(value state, value pairs, value slot, value chunks) {
  blake3_hasher *hasher = Caml_ba_data_val(state);
  uint8_t *pair = (uint8_t *)Caml_ba_data_val(pairs) + 2 * BLAKE3_OUT_LEN * Long_val(slot);
  uint64_t count = (uint64_t)Long_val(chunks);
  hasher_push_cv(hasher, pair, hasher->chunk.chunk_counter);
  hasher_push_cv(hasher, pair + BLAKE3_OUT_LEN, hasher->chunk.chunk_counter + count / 2);
  hasher->chunk.chunk_counter += count;
  return Val_unit;
}

CAMLprim value rupam_blake3_finish(value state) {
  CAMLparam1(state);
  CAMLlocal1(out);
  out = caml_alloc_string(BLAKE3_OUT_LEN);
  blake3_hasher_finalize(Caml_ba_data_val(state), (uint8_t *)Bytes_val(out), BLAKE3_OUT_LEN);
  CAMLreturn(out);
}
