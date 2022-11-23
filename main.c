#include "scc.h"

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  load_file(argv[1]);
  token* tok = tokenize();
  symbol* sym = parse(&tok);
  codegen(sym);
}