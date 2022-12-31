#include "scc.h"

void
vdiagnostic_at(token* tok, const char* prefix, const char* fmt, va_list args)
{
  // Two things we want to accomplish here, for the sake of the error message
  // we're about to emit:
  //  1. Calculate the line and column of the error, based on the given
  //  token's position
  //  2. Save the line upon which the error occurs so that we can print a
  //  carat pinpointing the error
  //
  // Both are done in this loop.
  int line = 1, col = 1;
  const char* line_start = source;
  const char* c = source;
  while (*c != '\0') {
    if (c == tok->pos) {
      break;
    }

    col++;
    if (*c == '\n') {
      line++;
      col = 1;
      c++;
      line_start = c;
    } else {
      c++;
    }
  }

  // We've stopped midway through a line - advance the cursor to the end of
  // the line.
  while (*c++ != '\n' && *c != '\0')
    ;

  // Don't print the last newline.
  c--;
  size_t len = c - line_start;
  char* line_text = strndup(line_start, len);

  fprintf(stderr, "%s:%d:%d: %s: ", source_name, line, col, prefix);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  fprintf(stderr, "%5d | %s\n", line, line_text);
  fprintf(stderr, "      |%*s^\n", col, " ");
  fflush(stderr);
}

void
error_at(token* tok, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  vdiagnostic_at(tok, "error", fmt, args);
  va_end(args);
  exit(1);
}

void
warn_at(token* tok, const char* fmt, ...)
{
  if (!enable_warnings) {
    return;
  }

  va_list args;
  va_start(args, fmt);
  vdiagnostic_at(tok, "warning", fmt, args);
  va_end(args);
}

void
ice_at(token* tok, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  if (!tok) {
    fprintf(stderr, "internal compiler error: ");
    vfprintf(stderr, fmt, args);
    fputs("\n", stderr);
    fflush(stderr);
    abort();
  }

  vdiagnostic_at(tok, "internal compiler error", fmt, args);
  va_end(args);
  abort();
}
