#include "scc.h"

static const char* argument_regs[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

/** for debugging purposes, the current depth of the value stack */
static int depth;

static int
gen_label()
{
  static int count = 0;
  return count++;
}

static void
push(const char* reg)
{
  depth++;
  printf("  push %%%s\n", reg);
}

static void
pop(const char* reg)
{
  if (depth == 0) {
    ice_at(NULL, "pop from empty value stack");
  }
  depth--;
  printf("  pop %%%s\n", reg);
}

static void
pop_void(void)
{
  if (depth == 0) {
    ice_at(NULL, "pop from empty value stack");
  }
  depth--;
  printf("  add $8, %%rsp\n");
}

static void
assert_stack_empty(void)
{
  if (depth != 0) {
    ice_at(NULL, "value stack is not empty (depth %d)", depth);
  }
}

/**
 * Loads a value of type ty from an address currently on the stack into rax.
 */
static void
load(type* ty)
{
  pop("rax");
  /**
   * In C, arrays "decay" to pointers. The identifier reference of an array
   * evaluates to a pointer to the array's first element.
   *
   * There's no actual codegen to "load" a value of type array, since the
   * pointer to the first element of the array is already on the stack.
   */
  if (ty->kind == TYPE_ARRAY) {
    return;
  }

  switch (ty->size) {
    case 1:
      printf("  movzbl (%%rax), %%eax\n");
      printf("  cltq\n");
      break;
    case 4:
      printf("  mov (%%rax), %%eax\n");
      printf("  cltq\n");
      break;
    case 8:
      printf("  mov (%%rax), %%rax\n");
      break;
    default:
      ice_at(NULL, "unknown type size %d", ty->size);
  }
}

/**
 * Pops a value and an lvalue address off of the stack and stores the value into
 * the lvalue address.
 */
static void
store(type* ty)
{
  pop("rdi"); // lvalue
              // rvalue
  switch (ty->size) {
    case 1:
      pop("rax");
      printf("  movb %%al, (%%rdi)\n");
      break;
    case 4:
      pop("rax");
      printf("  mov %%eax, (%%rdi)\n");
      break;
    default:
      // Values of size 8 and arrays (pointers or pointer-like integers)
      // use all of rax.
      pop("rax");
      printf("  mov %%rax, (%%rdi)\n");
      break;
  }
}

void
codegen_expr(node*);

/**
 * Given an expression that produces an lvalue, push the address of that lvalue
 * onto the value stack.
 */
void
codegen_lvalue_addr(node* n)
{
  switch (n->kind) {
    case NODE_SYMBOL_REF:
      if (!n->u.symbol_ref) {
        error_at(n->tok, "unbound identifier");
      }

      if (n->u.symbol_ref->kind == SYMBOL_GLOBAL_VAR) {
        printf("  lea %s(%%rip), %%rax\n", n->u.symbol_ref->name);
      } else {
        printf("  lea %d(%%rbp), %%rax # symbol ref lvalue `%s`\n",
               n->u.symbol_ref->u.frame_offset,
               n->u.symbol_ref->name);
      }
      push("rax");
      break;
    case NODE_DEREF:
      codegen_expr(n->u.deref_value);
      break;
    default:
      error_at(n->tok, "not a lvalue");
      break;
  }
}

/**
 * Generates code to evaluate an expression node and push its value onto the top
 * of the value stack.
 *
 * scc's codegen strategy uses the x86 stack as a stack machine. When an
 * expression term is evaluated, it pushes its value onto the stack;
 * higher-level expression terms pop their arguments off of the stack and push
 * results back onto the stack.
 *
 * This generates very inefficient code (particularly if a value is calculated
 * and immediately used, since rax is then pushed and immediately popped)
 */
void
codegen_expr(node* n)
{
  if (n->kind == NODE_CONST) {
    printf("  mov $%d, %%rax\n", n->u.const_value);
    push("rax");
    return;
  }

  if (n->kind == NODE_BINOP) {
    codegen_expr(n->u.binop.left);
    codegen_expr(n->u.binop.right);
    pop("rdi"); // right
    pop("rax"); // left
    switch (n->u.binop.op) {
      case BINOP_ADD:
        printf("  add %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_SUB:
        printf("  sub %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_MUL:
        printf("  imul %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_DIV:
        printf("  xor %%rdx, %%rdx\n");
        printf("  idiv %%rdi\n");
        push("rax");
        break;
      case BINOP_MOD:
        printf("  xor %%rdx, %%rdx\n");
        printf("  idiv %%rdi\n");
        push("rdx");
        break;
      case BINOP_EQUAL:
      case BINOP_NOT_EQUAL:
      case BINOP_LT:
      case BINOP_LT_EQUAL:
        printf("  cmp %%rdi, %%rax\n");
        if (n->u.binop.op == BINOP_EQUAL) {
          printf("  sete %%al\n");
        } else if (n->u.binop.op == BINOP_NOT_EQUAL) {
          printf("  setne %%al\n");
        } else if (n->u.binop.op == BINOP_LT) {
          printf("  setl %%al\n");
        } else if (n->u.binop.op == BINOP_LT_EQUAL) {
          printf("  setle %%al\n");
        }
        printf("  movzb %%al, %%rax\n");
        push("rax");
    }
  }

  if (n->kind == NODE_SYMBOL_REF) {
    if (!n->u.symbol_ref) {
      error_at(n->tok, "unbound identifier");
    }

    codegen_lvalue_addr(n);
    load(n->ty);
    push("rax");
  }

  if (n->kind == NODE_ASSIGN) {
    codegen_expr(n->u.assign.rvalue);
    codegen_lvalue_addr(n->u.assign.lvalue);
    store(n->ty);
    push("rax");
  }

  if (n->kind == NODE_ADDROF) {
    codegen_lvalue_addr(n->u.addrof_value);
  }

  if (n->kind == NODE_DEREF) {
    codegen_expr(n->u.deref_value);
    load(n->ty);
    push("rax");
  }

  if (n->kind == NODE_CALL) {
    int num_args = 0;
    for (node* arg = n->u.call.args; arg; arg = arg->next) {
      codegen_expr(arg);
      num_args++;
    }

    if (num_args > 6) {
      error_at(n->tok, "functions with arity > 6 are not supported");
    }

    // The first use of the SystemV ABI in scc; we pass integer and
    // pointer-sized arguments through the first six ABI regs.
    while (num_args > 0) {
      pop(argument_regs[num_args-- - 1]);
    }

    // When calling variadic functions, the System V ABI requires that the
    // number of vector arguments be written into %al. We don't support that, so
    // we just write zero.
    printf("  mov $0, %%al\n");
    printf("  call %s\n", n->u.call.name);
    push("rax");
  }
}

void
codegen_stmt(node* base)
{
  for (node* n = base; n; n = n->next) {
    switch (n->kind) {
      case NODE_RETURN:
        codegen_expr(n->u.return_value);
        pop("rax");
        printf("  leave\n");
        printf("  ret\n");
        break;
      case NODE_EXPR_STMT:
        codegen_expr(n->u.expr_stmt_value);
        pop_void();
        break;
      case NODE_COMPOUND_STMT:
        for (node* stmt = n->u.compound_stmts; stmt; stmt = stmt->next) {
          codegen_stmt(stmt);
        }
        break;
      case NODE_IF: {
        int label_count = gen_label();
        codegen_expr(n->u.if_.cond);
        pop("rax");
        printf("  cmp $0, %%rax\n");
        printf("  je .L.else.%d\n", label_count);
        codegen_stmt(n->u.if_.then);
        printf("  jmp .L.end.%d\n", label_count);
        printf(".L.else.%d:\n", label_count);
        if (n->u.if_.else_) {
          codegen_stmt(n->u.if_.else_);
        }
        printf(".L.end.%d:\n", label_count);
        break;
      }
      case NODE_FOR: {
        int label_count = gen_label();
        if (n->u.for_.initializer) {
          codegen_stmt(n->u.for_.initializer);
        }
        printf(".L.for.header.%d:\n", label_count);
        if (n->u.for_.cond) {
          codegen_expr(n->u.for_.cond);
          pop("rax");
          printf("  cmp $0, %%rax\n");
          printf("  je .L.for.end.%d\n", label_count);
        }
        codegen_stmt(n->u.for_.body);
        if (n->u.for_.next) {
          codegen_expr(n->u.for_.next);
          pop_void();
        }
        printf("  jmp .L.for.header.%d\n", label_count);
        printf(".L.for.end.%d:\n", label_count);
        break;
      }
      case NODE_DO: {
        int label_count = gen_label();
        printf(".L.do.body.%d:\n", label_count);
        codegen_stmt(n->u.do_.body);
        codegen_expr(n->u.do_.cond);
        pop("rax");
        printf("  cmp $0, %%rax\n");
        printf("  jne .L.do.body.%d\n", label_count);
        break;
      }

      default:
        break;
    }

    assert_stack_empty();
  }
}

int
calculate_frame_layout(symbol* func)
{
  int offset = 0;
  for (symbol* sym = func->u.function.locals; sym; sym = sym->next) {
    offset -= sym->ty->size;
    sym->u.frame_offset = offset;
  }

  return -offset;
}

static void
codegen_function(symbol* sym)
{
  int offset = calculate_frame_layout(sym);
  printf(".globl %s\n", sym->name);
  printf("%s:\n", sym->name);
  printf("  push %%rbp\n");
  printf("  movq %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", offset);
  codegen_stmt(sym->u.function.body);
  printf("  leave\n");
  printf("  ret\n");
}

static void
codegen_global(symbol* sym)
{
  // technically this only really works for string literals, but we'll get there
  // when we get there
  char* data = sym->u.global_data;
  printf("%s:\n", sym->name);
  for (size_t i = 0; i < strlen(data); i++) {
    printf("  .byte 0x%x\n", data[i]);
  }

  printf("  .byte 0x00\n");
}

void
codegen(symbol* symbols)
{
  for (symbol* sym = symbols; sym; sym = sym->next) {
    switch (sym->kind) {
      case SYMBOL_FUNCTION:
        codegen_function(sym);
        break;
      case SYMBOL_GLOBAL_VAR:
        codegen_global(sym);
        break;
      case SYMBOL_LOCAL_VAR:
        ice_at(sym->tok, "local variable in global symbols list?");
        break;
      case SYMBOL_EMPTY:
        break;
    }
  }
}