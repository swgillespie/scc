#include "scc.h"

void
verror_at(token* tok, const char* fmt, va_list args)
{
  (void)tok;
  fprintf(stderr, "error: ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  fflush(stderr);
  exit(1);
}

void
error_at(token* tok, const char* fmt, ...)
{
  (void)tok;
  va_list args;
  va_start(args, fmt);
  verror_at(tok, fmt, args);
  va_end(args);
}

/**
 * Code generation
 */

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  load_file(argv[1]);
  token* tok = tokenize();
  node* n = parse(&tok);
  codegen(n);
}