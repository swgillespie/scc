#include "scc.h"

static const char* argument_regs[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

static int
gen_label()
{
  static int count = 0;
  return count++;
}

static void
push(const char* reg)
{
  printf("  push %%%s\n", reg);
}

static void
pop(const char* reg)
{
  printf("  pop %%%s\n", reg);
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

      printf("  lea %d(%%rbp), %%rax # symbol ref lvalue `%s`\n",
             n->u.symbol_ref->u.frame_offset,
             n->u.symbol_ref->name);
      push("rax");
      break;
    case NODE_DEREF:
      codegen_expr(n->u.deref_value);
      break;
    default:
      error_at(n->tok, "not an lvalue");
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

    if (n->ty->kind == TYPE_ARRAY) {
      /**
       * In C, arrays "decay" to pointers. The identifier reference of an array
       * evaluates to a pointer to the array's first element.
       */
      printf("  lea %d(%%rbp), %%rax # array symbol ref `%s`\n",
             n->u.symbol_ref->u.frame_offset,
             n->u.symbol_ref->name);
    }

    printf("  movq %d(%%rbp), %%rax # symbol ref `%s`\n",
           n->u.symbol_ref->u.frame_offset,
           n->u.symbol_ref->name);
    push("rax");
  }

  if (n->kind == NODE_ASSIGN) {
    codegen_expr(n->u.assign.rvalue);
    codegen_lvalue_addr(n->u.assign.lvalue);
    pop("rdi"); // lvalue
    pop("rax"); // rvalue
    printf("  movq %%rax, (%%rdi)\n");
    push("rax");
  }

  if (n->kind == NODE_ADDROF) {
    codegen_lvalue_addr(n->u.addrof_value);
  }

  if (n->kind == NODE_DEREF) {
    codegen_expr(n->u.deref_value);
    pop("rax");
    printf("  movq (%%rax), %%rax\n");
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

    printf("  call %s\n", n->u.call.name);
    push("rax");
  }
}

void
codegen_stmt(node* n)
{
  switch (n->kind) {
    case NODE_RETURN:
      codegen_expr(n->u.return_value);
      pop("rax");
      printf("  leave\n");
      printf("  ret\n");
      break;
    case NODE_EXPR_STMT:
      codegen_expr(n->u.expr_stmt_value);
      printf("  add $8, %%rsp\n");
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
        printf("  add $8, %%rsp\n");
      }
      printf("  jmp .L.for.header.%d\n", label_count);
      printf(".L.for.end.%d:\n", label_count);
      break;
    }

    default:
      break;
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

void
codegen(symbol* sym)
{
  int offset = calculate_frame_layout(sym);
  printf(".globl main\n");
  printf("main:\n");
  push("rbp");
  printf("  movq %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", offset);
  for (node* cursor = sym->u.function.body; cursor != NULL;
       cursor = cursor->next) {
    codegen_stmt(cursor);
  }
  printf("  leave\n");
  printf("  ret\n");
}
