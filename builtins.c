#include "scc.h"

#ifndef SCC_SELFHOST

/**
 * __builtin_debugtrap() - raise a SIGTRAP, which will break a debugger.
 *
 * Supported by clang, not supported by gcc for some reason.
 */
static void
codegen_debugtrap(node* args)
{
  (void)args;
  printf("  int3\n");
  printf("  mov $0, %%rax\n");
}

static type*
type_debugtrap(void)
{
  return make_function_type(ty_void, NULL);
}

/**
 * __builtin_trap() - executes an invalid instruction.
 *
 * Supported by clang and gcc.
 */
static void
codegen_trap(node* args)
{
  (void)args;
  printf("  ud2\n");
  printf("  mov $0, %%rax\n");
}

static type*
type_trap(void)
{
  return make_function_type(ty_void, NULL);
}

static builtin_function builtins[] = { { .name = "__builtin_debugtrap",
                                         .codegen = codegen_debugtrap,
                                         .ty = type_debugtrap },
                                       {
                                         .name = "__builtin_trap",
                                         .codegen = codegen_trap,
                                         .ty = type_trap,
                                       } };

builtin_function*
builtin_lookup(char* name)
{
  if (strncmp("__builtin_", name, sizeof("__builtin_")) != 0) {
    // early out - anything with out the __builtin_ prefix is not a builtin.
    return NULL;
  }

  for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); i++) {
    if (strcmp(builtins[i].name, name) == 0) {
      return &builtins[i];
    }
  }

  return NULL;
}

#endif /* SCC_SELFHOST */
