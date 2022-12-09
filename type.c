#include "scc.h"

/* this lies and says that sizeof(int) is 8, we will fix this later. */
type* ty_int = &(type){ .kind = TYPE_INT, .base = NULL, .size = 4 };
type* ty_void = &(type){ .kind = TYPE_VOID, .base = NULL, .size = 0 };
type* ty_char = &(type){ .kind = TYPE_CHAR, .base = NULL, .size = 1 };
type* ty_bool = &(type){ .kind = TYPE_BOOL, .base = NULL, .size = 1 };

type*
make_pointer_type(type* base)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  t->kind = TYPE_POINTER;
  t->base = base;
  t->size = 8 /* pointers are always 8 for this 64-bit compiler */;
  return t;
}

type*
make_array_type(type* base, int len)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  t->kind = TYPE_ARRAY;
  t->base = base;
  t->u.array_length = len;
  t->size = base->size * len;
  return t;
}

type*
make_function_type(type* ret, parameter* params)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  t->kind = TYPE_FUNCTION;
  t->u.function.ret = ret;
  t->u.function.params = params;
  return t;
}

type*
make_typedef(token* name, type* ty)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  memcpy(t, ty, sizeof(type));
  t->aka = ty;
  t->aka_name = name;
  return t;
}

field*
make_field(token* name, type* ty, int offset)
{
  field* f = malloc(sizeof(field));
  memset(f, 0, sizeof(field));
  f->name = name;
  f->ty = ty;
  f->offset = offset;
  return f;
}

type*
make_struct(token* name, field* fields, int size)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  t->kind = TYPE_STRUCT;
  t->size = size;
  t->u.aggregate.name = name;
  t->u.aggregate.fields = fields;
  return t;
}

type*
make_union(token* name, field* fields, int size)
{
  type* t = malloc(sizeof(type));
  memset(t, 0, sizeof(type));
  t->kind = TYPE_UNION;
  t->size = size;
  t->u.aggregate.name = name;
  t->u.aggregate.fields = fields;
  return t;
}

static void
type_name_to_stream(type* ty, FILE* stream)
{
  if (ty->aka) {
    fprintf(stream, "%s (aka `", strndup(ty->aka_name->pos, ty->aka_name->len));
    type_name_to_stream(ty->aka, stream);
    fprintf(stream, "`)");
    return;
  }

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
    case TYPE_BOOL:
      fputs("_Bool", stream);
      return;
    case TYPE_FUNCTION:
      type_name_to_stream(ty->u.function.ret, stream);
      fprintf(stream, "(");
      for (parameter* p = ty->u.function.params; p; p = p->next) {
        type_name_to_stream(p->ty, stream);
        if (p->next) {
          fprintf(stream, ", ");
        }
      }
      fprintf(stream, ")");
      return;
    case TYPE_ARRAY:
      type_name_to_stream(ty->base, stream);
      fprintf(stream, "[%d]", ty->u.array_length);
      return;
    case TYPE_POINTER:
      type_name_to_stream(ty->base, stream);
      fputc('*', stream);
      return;
    case TYPE_STRUCT:
      fprintf(stream,
              "struct %s",
              strndup(ty->u.aggregate.name->pos, ty->u.aggregate.name->len));
      return;
    case TYPE_UNION:
      fprintf(stream,
              "union %s",
              strndup(ty->u.aggregate.name->pos, ty->u.aggregate.name->len));
      return;
  }
}

field*
field_lookup(token* name, type* ty)
{
  SCC_ASSERT(name,
             ty->kind == TYPE_STRUCT || ty->kind == TYPE_UNION,
             "field_lookup on non-aggregate type");
  for (field* f = ty->u.aggregate.fields; f; f = f->next) {
    if (name->len == f->name->len &&
        strncmp(name->pos, f->name->pos, name->len) == 0) {
      return f;
    }
  }

  return NULL;
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

int
integer_conversion_rank_compare(type* left, type* right)
{
  if (left == right) {
    return 0;
  }

  // The rank of _Bool shall be less than the rank of all other standard integer
  // types.
  if (left == ty_bool) {
    return -1;
  }

  if (right == ty_bool) {
    return 1;
  }

  // This implementation only supports char and int as standard integer types.
  if (left->size < right->size) {
    return -1;
  } else {
    return 1;
  }
}

int
is_arithmetic_type(type* left)
{
  switch (left->kind) {
    case TYPE_INT:
    case TYPE_CHAR:
    case TYPE_BOOL:
      return 1;
    default:
      return 0;
  }
}