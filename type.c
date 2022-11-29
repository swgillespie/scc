#include "scc.h"

/* this lies and says that sizeof(int) is 8, we will fix this later. */
type* ty_int = &(type){ .kind = TYPE_INT, .base = NULL, .size = 4 };
type* ty_void = &(type){ .kind = TYPE_VOID, .base = NULL, .size = 0 };
type* ty_char = &(type){ .kind = TYPE_CHAR, .base = NULL, .size = 1 };

type*
make_pointer_type(type* base)
{
  type* t = malloc(sizeof(type));
  t->kind = TYPE_POINTER;
  t->base = base;
  t->size = 8 /* pointers are always 8 for this 64-bit compiler */;
  return t;
}

type*
make_array_type(type* base, int len)
{
  type* t = malloc(sizeof(type));
  t->kind = TYPE_ARRAY;
  t->base = base;
  t->array_length = len;
  t->size = base->size * len;
  return t;
}

static void
type_name_to_stream(type* ty, FILE* stream)
{
  switch (ty->kind) {
    case TYPE_VOID:
      fputs("void", stream);
      return;
    case TYPE_INT:
      fputs("int", stream);
      return;
    case TYPE_CHAR:
      fputs("char", stream);
      return;
    case TYPE_ARRAY:
      type_name_to_stream(ty->base, stream);
      fprintf(stream, "[%d]", ty->array_length);
      return;
    case TYPE_POINTER:
      type_name_to_stream(ty->base, stream);
      fputc('*', stream);
      return;
  }
}

char*
type_name(type* ty)
{
  char* name;
  size_t len;
  FILE* stream = open_memstream(&name, &len);
  type_name_to_stream(ty, stream);
  fclose(stream);
  fflush(stream);
  return name;
}