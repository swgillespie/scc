#include "scc.h"

type* ty_int = &(type){ .kind = TYPE_INT, .base = NULL };
type* ty_void = &(type){ .kind = TYPE_VOID, .base = NULL };

type*
make_pointer_type(type* base)
{
  type* t = malloc(sizeof(type));
  t->kind = TYPE_POINTER;
  t->base = base;
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