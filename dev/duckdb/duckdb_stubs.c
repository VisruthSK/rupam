#include <stdio.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#define DUCKDB_STATIC_BUILD
#include <duckdb.h>
#include <duckdb_static_extension.h>

int32_t duckdb_extension_parquet_describe(duckdb_extension_descriptor *);
int32_t duckdb_extension_json_describe(duckdb_extension_descriptor *);
int32_t duckdb_extension_core_functions_describe(duckdb_extension_descriptor *);

static duckdb_database database;
static duckdb_connection connection;
static int connected = 0;

static void connect_once(void) {
  if (connected) return;
  if (duckdb_register_static_extension(duckdb_extension_parquet_describe) ||
      duckdb_register_static_extension(duckdb_extension_json_describe) ||
      duckdb_register_static_extension(duckdb_extension_core_functions_describe))
    caml_failwith("Cannot register DuckDB extensions");
  if (duckdb_open(NULL, &database) == DuckDBError) caml_failwith("Cannot open DuckDB");
  if (duckdb_connect(database, &connection) == DuckDBError) {
    duckdb_close(&database);
    caml_failwith("Cannot connect to DuckDB");
  }
  connected = 1;
}

value rupam_duckdb_query(value sql) {
  CAMLparam1(sql);
  CAMLlocal5(names, rows, row, cell, reply);
  duckdb_result result;
  connect_once();
  if (duckdb_query(connection, String_val(sql), &result) == DuckDBError) {
    char message[2048];
    const char *reason = duckdb_result_error(&result);
    snprintf(message, sizeof message, "%s", reason ? reason : "DuckDB query failed");
    duckdb_destroy_result(&result);
    caml_failwith(message);
  }
  idx_t columns = duckdb_column_count(&result);
  idx_t count = duckdb_row_count(&result);
  names = caml_alloc(columns, 0);
  for (idx_t c = 0; c < columns; c++)
    Store_field(names, c, caml_copy_string(duckdb_column_name(&result, c)));
  rows = caml_alloc(count, 0);
  for (idx_t r = 0; r < count; r++) {
    row = caml_alloc(columns, 0);
    for (idx_t c = 0; c < columns; c++) {
      if (duckdb_value_is_null(&result, c, r)) {
        cell = Val_none;
      } else {
        char *text = duckdb_value_varchar(&result, c, r);
        if (!text) {
          duckdb_destroy_result(&result);
          caml_failwith("DuckDB cannot render a value of this column as text");
        }
        cell = caml_alloc_some(caml_copy_string(text));
        duckdb_free(text);
      }
      Store_field(row, c, cell);
    }
    Store_field(rows, r, row);
  }
  duckdb_destroy_result(&result);
  reply = caml_alloc_tuple(2);
  Store_field(reply, 0, names);
  Store_field(reply, 1, rows);
  CAMLreturn(reply);
}
