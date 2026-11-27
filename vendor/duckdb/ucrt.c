#ifdef _WIN32
#define MINGW_HAS_SECURE_API 1
#include <stdarg.h>
#include <stdio.h>

/* DuckDB's Windows archive is built against UCRT; MinGW OCaml links msvcrt, which lacks this entry point. */
int __cdecl __stdio_common_vsnprintf_s(unsigned __int64 options, char *buffer, size_t size,
                                       size_t count, const char *format, _locale_t locale,
                                       va_list args)
{
  (void)options;
  (void)locale;
  return _vsnprintf_s(buffer, size, count, format, args);
}
#endif
