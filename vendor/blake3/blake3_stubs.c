#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "blake3.h"

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

CAMLprim value rupam_blake3_finish(value state) {
  CAMLparam1(state);
  CAMLlocal1(out);
  out = caml_alloc_string(BLAKE3_OUT_LEN);
  blake3_hasher_finalize(Caml_ba_data_val(state), (uint8_t *)Bytes_val(out), BLAKE3_OUT_LEN);
  CAMLreturn(out);
}
