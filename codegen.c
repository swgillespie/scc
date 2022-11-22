#include "scc.h"

int lvalue_context = 0;

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
    if (lvalue_context) {
      printf("  lea %d(%%rbp), %%rax # symbol ref lvalue `%s`\n",
             n->u.symbol_ref->frame_offset,
             symbol_name(n->u.symbol_ref));
      push("rax");
    } else {
      printf("  movq %d(%%rbp), %%rax # symbol ref `%s`\n",
             n->u.symbol_ref->frame_offset,
             symbol_name(n->u.symbol_ref));
      push("rax");
    }
  }

  if (n->kind == NODE_ASSIGN) {
    codegen_expr(n->u.assign.rvalue);
    lvalue_context = 1;
    codegen_expr(n->u.assign.lvalue);
    lvalue_context = 0;
    pop("rdi"); // lvalue
    pop("rax"); // rvalue
    printf("  movq %%rax, (%%rdi)\n");
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
}
