#include "scc.h"

#define MAX_LOOP_DEPTH 25

static char* argument_regs[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

/** for debugging purposes, the current depth of the value stack */
static int depth;

/**
 * Stack of labels that break statements jump to, when codegen-ing loops.
 */
static char* break_stack[MAX_LOOP_DEPTH];
static int break_stack_len;

/**
 * Stack of labels that continue statements jump to, when codegen-ing loops.
 */
static char* continue_stack[MAX_LOOP_DEPTH];
static int continue_stack_len;

static void
push_break(char* break_label)
{
  SCC_ASSERT(NULL, break_stack_len < MAX_LOOP_DEPTH, "break stack overflow");
  break_stack[break_stack_len++] = break_label;
}

static void
pop_break(void)
{
  SCC_ASSERT(NULL, break_stack_len > 0, "pop from empty break stack");
  break_stack_len--;
}

static void
push_loop_labels(char* break_label, char* continue_label)
{
  SCC_ASSERT(
    NULL, continue_stack_len < MAX_LOOP_DEPTH, "continue stack overflow");
  push_break(break_label);
  continue_stack[continue_stack_len++] = continue_label;
}

static void
pop_loop_labels(void)
{
  SCC_ASSERT(NULL, continue_stack_len > 0, "pop from empty break stack");
  pop_break();
  continue_stack_len--;
}

static char*
break_target(void)
{
  SCC_ASSERT(NULL, break_stack_len > 0, "not in loop for break");
  return break_stack[break_stack_len - 1];
}

static char*
continue_target(void)
{
  SCC_ASSERT(NULL, continue_stack_len > 0, "not in loop for continue");
  return continue_stack[continue_stack_len - 1];
}

int
gen_label()
{
  static int count = 0;
  return count++;
}

char*
gen_label_name(char* prefix, int count)
{
  char* name;
  size_t len;
  FILE* stream = open_memstream(&name, &len);
  fprintf(stream, "%s.%d", prefix, count);
  fflush(stream);
  fclose(stream);
  return name;
}

static void
push(const char* reg)
{
  depth++;
  emit("  push %%%s\n", reg);
}

static void
pop(const char* reg)
{
  if (depth == 0) {
    ice_at(NULL, "pop from empty value stack");
  }
  depth--;
  emit("  pop %%%s\n", reg);
}

static void
pop_void(void)
{
  if (depth == 0) {
    ice_at(NULL, "pop from empty value stack");
  }
  depth--;
  emit("  add $8, %%rsp\n");
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
  if (ty->kind == TYPE_STRUCT) {
    ice_at(NULL, "nyi: load of non-scalar type");
  }

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
      emit("  movzbl (%%rax), %%eax\n");
      emit("  cltq\n");
      break;
    case 4:
      emit("  mov (%%rax), %%eax\n");
      emit("  cltq\n");
      break;
    case 8:
      emit("  mov (%%rax), %%rax\n");
      break;
    default:
      ice_at(NULL, "unknown type size %d", ty->size);
  }
}

/**
 * Pops a value and an lvalue address off of the stack and stores the value into
 * the lvalue address.
 *
 * Trashes rdi and rax.
 */
static void
store(type* ty)
{
  pop("rdi"); // lvalue
              // rvalue
  switch (ty->size) {
    case 1:
      pop("rax");
      emit("  movb %%al, (%%rdi)\n");
      break;
    case 4:
      pop("rax");
      emit("  mov %%eax, (%%rdi)\n");
      break;
    default:
      // Values of size 8 and arrays (pointers or pointer-like integers)
      // use all of rax.
      pop("rax");
      emit("  mov %%rax, (%%rdi)\n");
      break;
  }
}

/**
 * stack_duplicates the element at the top of the stack and pushes it onto the
 * stack.
 */
static void
stack_dup(void)
{
  emit("  mov (%%rsp), %%rax\n");
  push("rax");
}

/**
 * Swaps the order of the two elements on the top of the stack.
 */
static void
stack_swap(void)
{
  // [a, b]
  pop("rax");  // [a], rax = b
  pop("rdi");  // [], rax = b, rdi = a
  push("rax"); // [b], rax = b, rdi = a
  push("rdi"); // [b, a], rax = b, rdi = a
}

/**
 * Calls the given symbol and pushes its result onto the stack. This function
 * ensures that the stack is aligned prior to calling any function.
 *
 * Assumes that the argument registers have already been set up.
 */
static void
emit_call(char* function)
{
  // When calling variadic functions, the System V ABI requires that the
  // number of vector arguments be written into %al. We don't support that, so
  // we just write zero.
  emit("  mov $0, %%al\n");
  // Furthermore, if we're calling C functions, C generally expects the stack
  // to be aligned to a 16-byte boundary. Since we are dynamically pushing and
  // popping things from the stack as part of our codegen strategy, we have no
  // idea what the stack pointer looks like when we get here. We must align it
  // ourselves. SCC generally does not care about stack alignment because it
  // doesn't generally emit instructions that require it, but other C
  // compilers do, and we will call them (such as gcc-compiled functions in
  // libc).
  //
  // First, we save the current stack pointer, for when we return;
  push("rbp");
  emit("  mov %%rsp, %%rbp\n");
  // Then, we align our stack pointer downwards.
  emit("  and $0xFFFFFFFFFFFFFFF0, %%rsp\n");
  // With a properly aligned stack, we can now call...
  emit("  call %s\n", function);
  // ...restore our potentially-unaligned stack pointer...
  emit("  mov %%rbp, %%rsp\n");
  pop("rbp");
  // ...and continue.
  push("rax");
}

/**
 * Evaluates the constant expression `n` and returns its value.
 */
static int consteval(node* n)
{
  if (n->kind != NODE_CONST) {
    error_at(n->tok,
             "not a constant expression as currently recognized by "
             "this compiler");
  }

  return n->u.const_value;
}

void
codegen_expr(node*);

static void
codegen_local_initialization(node* n)
{
  SCC_ASSERT(n->tok,
             n->kind == NODE_INITIALIZE,
             "codegen_local_initialization on non-init node");
  SCC_ASSERT(n->tok,
             n->u.init.sym->kind == SYMBOL_LOCAL_VAR,
             "NYI: non-local variable initialization");

  symbol* initialized_symbol = n->u.init.sym;
  type* ty = initialized_symbol->ty;
  if (is_scalar_type(ty) && ty->kind != TYPE_ARRAY) {
    // Scalar initialization only uses the first initializer, if there are
    // multiple.
    //
    // If there are no initializers, then we're initializing it to a zero value.
    if (!n->u.init.initializer) {
      emit("  mov $0, %%rax\n");
      push("rax");
    } else {
      SCC_ASSERT(n->tok,
                 n->u.init.initializer->kind == INITIALIZER_SCALAR,
                 "non-scalar initializer for scalar");
      // Otherwise, eval the initializer. This silently drops any other
      // initializers that were present, but that's fine, because this is C and
      // we issued a warning earlier about it.
      codegen_expr(n->u.init.initializer->u.value);
    }

    emit("  lea %d(%%rbp), %%rax\n", initialized_symbol->u.frame_offset);
    push("rax");
    store(n->u.init.sym->ty);
    return;
  }

  if (ty->kind == TYPE_ARRAY) {
    // Array initializers proceed in order and assign consecutive indices to the
    // array.
    //
    // First, though, we need to memset the entire array to zero - the spec
    // requires it.
    emit("  lea %d(%%rbp), %%rax\n", initialized_symbol->u.frame_offset);
    emit("  mov %%rax, %%%s\n", argument_regs[0]);
    emit("  mov $0, %%%s\n", argument_regs[1]);
    emit("  mov $%d, %%%s\n", ty->size, argument_regs[2]);
    emit_call("memset"); // memset returns a pointer to the memory it just set,
                         // emit_call pushes it onto the stack
    int offset = 0;
    initializer* agg_init = n->u.init.initializer;
    SCC_ASSERT(n->tok,
               agg_init->kind == INITIALIZER_AGGREGATE,
               "non-aggregate initializer of aggregate");
    for (initializer* i = agg_init->u.initializers; i; i = i->next) {
      // stack state at the start of the loop:
      // [arr_ptr]
      stack_dup();              // [arr_ptr, arr_ptr]
      codegen_expr(i->u.value); // [arr_ptr, arr_ptr, expr]
      stack_swap();             // [arr_ptr, expr, arr_ptr]

      if (offset != 0) {
        pop("rax"); // [arr_ptr, expr], rax = arr_ptr
        emit("  add $%d, %%rax\n",
             offset); // [arr_ptr, expr], rax = arr_ptr + offset
        push("rax");  // [arr_ptr, expr, arr_ptr + offset]
      }

      // [arr_ptr, expr, addr_ptr + offset]
      store(ty->base);                  // [arr_ptr]
      offset = offset + ty->base->size; // TODO - alignment
    }

    pop("rax"); // []
    return;
  }

  if (ty->kind == TYPE_STRUCT) {
    // Struct initialization begins the same as array initialization, with a
    // memset.
    //
    // In fact, this is all quite similar and could maybe be combined...
    emit("  lea %d(%%rbp), %%rax\n", initialized_symbol->u.frame_offset);
    emit("  mov %%rax, %%%s\n", argument_regs[0]);
    emit("  mov $0, %%%s\n", argument_regs[1]);
    emit("  mov $%d, %%%s\n", ty->size, argument_regs[2]);
    emit_call("memset"); // memset returns a pointer to the memory it just set,
                         // emit_call pushes it onto the stack
    field* field_cursor = ty->u.aggregate.fields;
    initializer* agg_init = n->u.init.initializer;
    SCC_ASSERT(n->tok,
               agg_init->kind == INITIALIZER_AGGREGATE,
               "non-aggregate initializer of aggregate");
    for (initializer* i = agg_init->u.initializers; i; i = i->next) {
      // stack state at the start of the loop:
      // [struct_ptr]

      if (!field_cursor) {
        continue;
      }

      int offset = field_cursor->offset;
      stack_dup();              // [struct_ptr, struct_ptr]
      codegen_expr(i->u.value); // [struct_ptr, struct_ptr, expr]
      stack_swap();             // [struct_ptr, expr, struct_ptr]

      if (offset != 0) {
        pop("rax"); // [struct_ptr, expr], rax = struct_ptr
        emit("  add $%d, %%rax\n",
             offset); // [struct_ptr, expr], rax = struct_ptr + offset
        push("rax");  // [struct_ptr, expr, struct_ptr + offset]
      }

      // [struct_ptr, expr, addr_ptr + offset]
      store(field_cursor->ty); // [struct_ptr]
      field_cursor = field_cursor->next;
    }

    pop("rax"); // []
    return;
  }

  ice_at(n->tok, "nyi: some weird kind of initialization");
}

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
        emit("  lea %s(%%rip), %%rax\n", n->u.symbol_ref->name);
      } else {
        emit("  lea %d(%%rbp), %%rax # symbol ref lvalue `%s`\n",
             n->u.symbol_ref->u.frame_offset,
             n->u.symbol_ref->name);
      }
      push("rax");
      break;
    case NODE_DEREF:
      codegen_expr(n->u.deref_value);
      break;
    case NODE_MEMBER:
      codegen_lvalue_addr(n->u.member.base);
      if (n->u.member.field->offset != 0) {
        pop("rax");
        emit("  add $%d, %%rax\n", n->u.member.field->offset);
        push("rax");
      }
      break;
    case NODE_MEMBER_DEREF:
      codegen_expr(n->u.member.base);
      if (n->u.member.field->offset != 0) {
        pop("rax");
        emit("  add $%d, %%rax\n", n->u.member.field->offset);
        push("rax");
      }
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
    emit("  mov $%d, %%rax\n", n->u.const_value);
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
        emit("  add %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_SUB:
        emit("  sub %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_MUL:
        emit("  imul %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_DIV:
        emit("  xor %%rdx, %%rdx\n");
        emit("  idiv %%rdi\n");
        push("rax");
        break;
      case BINOP_MOD:
        emit("  xor %%rdx, %%rdx\n");
        emit("  idiv %%rdi\n");
        push("rdx");
        break;
      case BINOP_INCLUSIVE_OR:
        emit("  or %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_AND:
        emit("  and %%rdi, %%rax\n");
        push("rax");
        break;
      case BINOP_EQUAL:
      case BINOP_NOT_EQUAL:
      case BINOP_LT:
      case BINOP_LT_EQUAL:
      case BINOP_GT:
      case BINOP_GT_EQUAL:
        emit("  cmp %%rdi, %%rax\n");
        if (n->u.binop.op == BINOP_EQUAL) {
          emit("  sete %%al\n");
        } else if (n->u.binop.op == BINOP_NOT_EQUAL) {
          emit("  setne %%al\n");
        } else if (n->u.binop.op == BINOP_LT) {
          emit("  setl %%al\n");
        } else if (n->u.binop.op == BINOP_LT_EQUAL) {
          emit("  setle %%al\n");
        } else if (n->u.binop.op == BINOP_GT) {
          emit("  setg %%al\n");
        } else if (n->u.binop.op == BINOP_GT_EQUAL) {
          emit("  setge %%al\n");
        }
        emit("  movzb %%al, %%rax\n");
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
    store(n->u.assign.lvalue->ty);
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

    emit_call(n->u.call.name);
  }

  if (n->kind == NODE_AND) {
    int label_idx = gen_label();
    char* label = gen_label_name(".L.and", label_idx);
    codegen_expr(n->u.logic_binop.left);
    pop("rax");
    emit("  cmp $0, %%rax\n");
    emit("  je %s\n", label);
    codegen_expr(n->u.logic_binop.right);
    pop("rax");
    emit("  cmp $0, %%rax\n");
    emit("  setne %%al\n");
    emit("  movzb %%al, %%eax\n");
    emit("%s:\n", label);
    push("rax");
  }

  if (n->kind == NODE_OR) {
    int label_idx = gen_label();
    char* label = gen_label_name(".L.or", label_idx);
    codegen_expr(n->u.logic_binop.left);
    pop("rax");
    emit("  cmp $0, %%rax\n");
    emit("  jne %s\n", label);
    codegen_expr(n->u.logic_binop.right);
    pop("rax");
    emit("  cmp $0, %%rax\n");
    emit("%s:\n", label);
    emit("  setne %%al\n");
    emit("  movzb %%al, %%eax\n");
    push("rax");
  }

  if (n->kind == NODE_CONV) {
    codegen_expr(n->u.conv.value);
    pop("rax");
    // TODO: conv to a bool is special
    switch (n->ty->size) {
      case 1:
        emit("  movsx %%al, %%eax\n");
        break;
      case 4:
        emit("  cltq\n");
        break;
      default:
        break;
    }

    push("rax");
  }

  if (n->kind == NODE_ARG) {
    if (n->u.arg.count > 5) {
      error_at(n->tok, "greater than 6 parameters not currently supported");
    }

    char* arg = argument_regs[n->u.arg.count];
    push(arg);
  }

  if (n->kind == NODE_POSTINCREMENT || n->kind == NODE_POSTDECREMENT) {
    codegen_lvalue_addr(n->u.postincrement.arg); // [addr]
    stack_dup();                                 // [addr, addr]
    load(n->ty);                                 //
    push("rax");                                 // [addr, value], rax = value
    if (n->kind == NODE_POSTINCREMENT) {
      emit("  add $1, %%rax\n"); // [addr, value], rax = value + 1
    } else {
      emit("  sub $1, %%rax\n"); // [addr, value], rax = value + 1
    }
    pop("rdx");   // [addr], rax = value + 1, rdi = value
    pop("rsi");   // [], rax = value + 1, rdi = value, rsi = addr
    push("rax");  // [value + 1], rdi = value, rsi = addr
    push("rsi");  // [value + 1, addr], rdi = value
    store(n->ty); // [], rdi = value
    push("rdx");  // [value]
  }

  if (n->kind == NODE_MEMBER || n->kind == NODE_MEMBER_DEREF) {
    // The standard does not require the lhs of a member expression to be an
    // lvalue.
    //
    // However, we do require that the base of a member expression be an lvalue
    // in the code generator. In cases such as e.g. a function returning a
    // structure by value, we must allocate a local in the calling function,
    // ensure the called function stores its return value into the pointer, and
    // use that local as an lvalue in the calling function.
    //
    // For example, in the expression `def().x`, where `def` is a function
    // returning a struct by value, `def()` is not an lvalue; however, we
    // codegen def as a function that looks like `void def(struct foo*)`, and
    // transform the call site to look like:
    //
    // ```
    // struct foo;
    // def(&foo);
    // foo.x
    // ```
    //
    // Which preserves our ability to always codegen member base expressions as
    // lvalues.
    //
    // Note that we don't do any of this stuff yet, we'll have to do it later.
    codegen_lvalue_addr(n);
    load(n->u.member.field->ty);
    push("rax");
  }

  if (n->kind == NODE_COND) {
    // Similar codegen as a if statement, except we leave the true and false
    // expr values on the stack.
    char* false_label = gen_label_name(".L.cond.false", gen_label());
    char* end_label = gen_label_name(".L.cond.end", gen_label());
    codegen_expr(n->u.cond.cond);
    pop("rax");
    emit("  cmp $0, %%rax\n");
    emit("  je %s\n", false_label);
    codegen_expr(n->u.cond.true_expr);
    emit("  jmp %s\n", end_label);
    emit("%s:\n", false_label);
    codegen_expr(n->u.cond.false_expr);
    emit("%s:\n", end_label);

    // Bit of a hack, here - we have some assertions that check that the stack
    // push/pop count equals zero at a statement boundary; however, we're
    // actually "pushing" two branches of code here, which confuses the logic of
    // ensuring the count is zero.
    //
    // Just decrement the depth. shh, it's fine, scc.
    depth--;
  }

  if (n->kind == NODE_COMMA) {
    // Evaluate left for side-effects, pop its value off of the stack, and then
    // evaluate right.
    codegen_expr(n->u.comma.left);
    pop_void();
    codegen_expr(n->u.comma.right);
  }
}

void
codegen_stmt(node* base)
{
  for (node* n = base; n; n = n->next) {
    switch (n->kind) {
      case NODE_RETURN:
        if (n->u.return_value) {
          codegen_expr(n->u.return_value);
          pop("rax");
        }
        emit("  leave\n");
        emit("  ret\n");
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
        emit("  cmp $0, %%rax\n");
        emit("  je .L.else.%d\n", label_count);
        codegen_stmt(n->u.if_.then);
        emit("  jmp .L.end.%d\n", label_count);
        emit(".L.else.%d:\n", label_count);
        if (n->u.if_.else_) {
          codegen_stmt(n->u.if_.else_);
        }
        emit(".L.end.%d:\n", label_count);
        break;
      }
      case NODE_FOR: {
        int label_count = gen_label();
        char* header = gen_label_name(".L.for.header", label_count);
        char* next = gen_label_name(".L.for.next", label_count);
        char* end = gen_label_name(".L.for.end", label_count);
        if (n->u.for_.initializer) {
          codegen_stmt(n->u.for_.initializer);
        }
        emit("%s:\n", header);
        if (n->u.for_.cond) {
          codegen_expr(n->u.for_.cond);
          pop("rax");
          emit("  cmp $0, %%rax\n");
          emit("  je %s\n", end);
        }
        push_loop_labels(end, next);
        codegen_stmt(n->u.for_.body);
        pop_loop_labels();
        emit("%s:\n", next);
        if (n->u.for_.next) {
          codegen_expr(n->u.for_.next);
          pop_void();
        }
        emit("  jmp %s\n", header);
        emit("%s:\n", end);
        break;
      }
      case NODE_DO: {
        int label_count = gen_label();
        char* header = gen_label_name(".L.do.body", label_count);
        char* end = gen_label_name(".L.do.end", label_count);
        emit("%s:\n", header);
        push_loop_labels(end, header);
        codegen_stmt(n->u.do_.body);
        pop_loop_labels();
        codegen_expr(n->u.do_.cond);
        pop("rax");
        emit("  cmp $0, %%rax\n");
        emit("  jne .L.do.body.%d\n", label_count);
        emit("%s:\n", end);
        break;
      }
      case NODE_BREAK:
        emit("  jmp %s\n", break_target());
        break;
      case NODE_CONTINUE:
        emit("  jmp %s\n", continue_target());
        break;
      case NODE_SWITCH: {
        codegen_expr(n->u.switch_.cond);
        char* end = gen_label_name(".L.switch.end", gen_label());
        push_break(end);
        pop("rax");
        switch_case* default_case = NULL;
        // TODO(check) switch case evaluated constants are unique
        for (switch_case* c = n->u.switch_.cases; c; c = c->next) {
          SCC_ASSERT(
            c->tok, c->label->kind == NODE_LABEL, "case label is not a label?");
          if (!c->cond) {
            if (default_case) {
              error_at(default_case->tok, "multiple default cases in switch");
            }
            default_case = c;
            continue;
          }
          int cond_val = consteval(c->cond);
          emit("  cmp $%d, %%rax\n", cond_val);
          emit("  je %s\n", c->label->u.label_name);
        }

        if (default_case) {
          emit("  jmp %s\n", default_case->label->u.label_name);
        } else {
          emit("  jmp %s\n", end);
        }

        codegen_stmt(n->u.switch_.body);
        pop_break();
        emit("%s:\n", end);
        break;
      }
      case NODE_LABEL:
        emit("%s:\n", n->u.label_name);
        break;
      case NODE_INITIALIZE:
        codegen_local_initialization(n);
        break;
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
    if (sym->ty->size == 0) {
      error_at(sym->tok, "storage size is not known");
    }

    offset = offset - sym->ty->size;
    sym->u.frame_offset = offset;
  }

  return -offset;
}

static void
codegen_function(symbol* sym)
{
  if (sym->linkage == LINKAGE_EXTERNAL) {
    return;
  }

  int offset = calculate_frame_layout(sym);
  emit(".text\n");
  emit(".globl %s\n", sym->name);
  emit(".type  %s, @function\n", sym->name);
  emit("%s:\n", sym->name);
  emit("  push %%rbp\n");
  emit("  movq %%rsp, %%rbp\n");
  emit("  sub $%d, %%rsp\n", offset);
  codegen_stmt(sym->u.function.body);
  emit("  leave\n");
  emit("  ret\n");
}

static void
codegen_scalar_initializer(node* value, type* ty)
{
  if (ty->base == ty_char) {
    // These can only be initialized by string literals.
    SCC_ASSERT(value->tok,
               value->kind == NODE_SYMBOL_REF,
               "non-symbol string literal ref");
    char* strsym = value->u.symbol_ref->name;
    emit("  .quad %s\n", strsym);
    return;
  }

  // In a real compiler, we'd have a pretty sophisticated constant
  // evaluation system here where we'd evaluate each expression.
  //
  // We're not a real compiler, and we're not going to do that.
  if (value->kind == NODE_ADDROF &&
      value->u.addrof_value->kind == NODE_SYMBOL_REF) {
    // One of the dumb hacks we do here: the addrof operator of a symbol ref
    // can be replaced with the address of the symbol ref.
    symbol* sym = value->u.addrof_value->u.symbol_ref;
    emit("  .quad %s\n", sym->name);
    return;
  }

  int scalar_value = consteval(value);
  switch (ty->size) {
    case 4:
      emit("  .long %d\n", scalar_value);
      break;
    case 8:
      emit("  .quad %d\n", scalar_value);
      break;
    default:
      ice_at(NULL, "unknown type size for global initializer");
  }
}

static void
codegen_initializer(initializer*, type*);

static void
codegen_aggregate_initializer(initializer* inits, type* ty)
{
  if (ty->kind == TYPE_ARRAY) {
    for (initializer* i = inits; i; i = i->next) {
      codegen_initializer(i, ty->base);
    }

    return;
  }

  if (ty->kind == TYPE_STRUCT) {
    // Initializing a struct is a little special, since we need to insert zeroed
    // padding sections where appropriate.
    int offset = 0;
    field* cursor = ty->u.aggregate.fields;
    for (initializer* i = inits; i; i = i->next) {
      if (!cursor) {
        break;
      }

      int padding = ALIGN_UP(offset, cursor->ty->align) - offset;
      if (padding != 0) {
        emit("  .zero %d\n", padding);
      }
      offset = ALIGN_UP(offset, cursor->ty->align);
      offset = offset + cursor->ty->size;
      codegen_initializer(i, cursor->ty);
      cursor = cursor->next;
    }

    if (offset < ty->size) {
      emit("  .zero %d\n", ty->size - offset);
    }

    return;
  }
}

static void
codegen_initializer(initializer* init, type* ty)
{
  if (init->kind == INITIALIZER_AGGREGATE) {
    codegen_aggregate_initializer(init->u.initializers, ty);
  } else {
    codegen_scalar_initializer(init->u.value, ty);
  }
}

static void
codegen_global_initializer(symbol* sym)
{
  if (!sym->u.global_initializer) {
    // No initializer => zero initialization.
    emit(".bss\n");
    emit(".globl %s\n", sym->name);
    emit("%s:\n", sym->name);
    emit("  .zero %d\n", sym->ty->size);
    return;
  }

  emit(".data\n");
  emit(".globl %s\n", sym->name);
  emit("%s:\n", sym->name);
  codegen_initializer(sym->u.global_initializer, sym->ty);
}

static void
codegen_global(symbol* sym)
{
  if (sym->linkage == LINKAGE_EXTERNAL) {
    return;
  }

  if (sym->ty->size == 0) {
    error_at(sym->tok, "storage size is not known");
  }

  if (!sym->string_literal_data) {
    codegen_global_initializer(sym);
    return;
  }

  char* data = sym->string_literal_data;
  if (!data) {
    emit(".bss\n");
    emit(".globl %s\n", sym->name);
    emit("%s:\n", sym->name);
    emit("  .zero %d\n", sym->ty->size);
    return;
  }

  emit(".section .rodata\n");
  emit("%s:\n", sym->name);
  for (size_t i = 0; i < strlen(data); i++) {
    emit("  .byte 0x%x\n", data[i]);
  }

  emit("  .byte 0x00\n");
}

#ifndef SCC_SELFHOST
void
emit(char* format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(output_file, format, args);
  va_end(args);
}
#endif

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
      case SYMBOL_CONSTANT:
      case SYMBOL_EMPTY:
        break;
    }
  }
}