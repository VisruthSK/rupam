#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "blake3.c"

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
