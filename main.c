#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char* source;

void
load_file(const char* filename)
{
  FILE* f = fopen(filename, "r");
  if (!f) {
    perror("failed to open source file");
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  size_t length = (size_t)ftell(f);
  fseek(f, 0, SEEK_SET);
  source = malloc(length);
  if (fread(source, sizeof(char), length, f) != length) {
    perror("fread");
    exit(1);
  }
}

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  load_file(argv[1]);
  FILE* out = stdout;

  fprintf(out, ".globl main\n");
  fprintf(out, "main:\n");
  fprintf(out, "  mov $0, %%rax\n");
  fprintf(out, "  ret\n");
  return 0;
}