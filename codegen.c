#include "scc.h"

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
      printf("  lea %d(%%rbp), %%rax # symbol ref lvalue `%s`\n",
             n->u.symbol_ref->frame_offset,
             symbol_name(n->u.symbol_ref));
      push("rax");
      break;
    case NODE_DEREF:
      codegen_expr(n->u.deref_value);
      break;
    default:
      error_at(NULL, "not an lvalue");
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
    printf("  movq %d(%%rbp), %%rax # symbol ref `%s`\n",
           n->u.symbol_ref->frame_offset,
           symbol_name(n->u.symbol_ref));
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
calculate_frame_layout()
{
  int offset = 0;
  for (symbol* sym = scopes->symbols; sym; sym = sym->next) {
    offset -= 8;
    sym->frame_offset = offset;
  }

  return -offset;
}

void
codegen(node* n)
{
  int offset = calculate_frame_layout();
  printf(".globl main\n");
  printf("main:\n");
  push("rbp");
  printf("  movq %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", offset);
  for (node* cursor = n; cursor != NULL; cursor = cursor->next) {
    codegen_stmt(cursor);
  }
  printf("  leave\n");
  printf("  ret\n");
}
