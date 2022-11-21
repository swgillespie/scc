#include <stdio.h>
#include <string.h>

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  FILE* in;
  if (strcmp(argv[1], "-") == 0) {
    in = stdin;
  } else {
    in = fopen(argv[1], "r");
    if (!in) {
      perror("failed to open source file");
      return 1;
    }
  }

  FILE* out = stdout;

  fprintf(out, ".globl main\n");
  fprintf(out, "main:\n");
  fprintf(out, "  mov $0, %%rax\n");
  fprintf(out, "  ret\n");
  return 0;
}