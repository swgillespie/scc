#include "scc.h"

#define MAX_SWITCH_DEPTH 25

/**
 * The current function being parsed, if any.
 */
static symbol* current_function;

/**
 * All of the symbols that will need to be emitted later.
 */
static symbol* symbols;

/**
 * A type symbol is a symbol representing a type (introduced by e.g. typedef,
 * struct, or union).
 */
typedef struct type_symbol
{
  struct type_symbol* next;
  char* name;
  type* ty;
  // Whether this type is a `struct`, and must be prefixed with the `struct`
  // keyword
  int is_struct;
  int is_union;
  int is_enum;
} type_symbol;

/**
 * Lexical scope management
 */
typedef struct scope
{
  struct scope* next;
  struct symbol* symbols;
  struct type_symbol* type_symbols;
} scope;

static scope* current_scope;
static scope* global_scope;

static void
push_scope(void)
{
  scope* s = malloc(sizeof(scope));
  memset(s, 0, sizeof(scope));
  s->next = current_scope;
  current_scope = s;
}

static void
pop_scope(void)
{
  current_scope = current_scope->next;
}

static void
define(symbol* sym)
{
  sym->next_in_scope = current_scope->symbols;
  current_scope->symbols = sym;
}

static void
define_type(type_symbol* sym)
{
  sym->next = current_scope->type_symbols;
  current_scope->type_symbols = sym;
}

typedef enum type_lookup_scope
{
  TYPE_SCOPE_TYPEDEF,
  TYPE_SCOPE_STRUCT,
  TYPE_SCOPE_UNION,
  TYPE_SCOPE_ENUM,
} type_lookup_scope;

static type_symbol*
scope_lookup_type(token* name, type_lookup_scope tls)
{
  for (scope* s = current_scope; s; s = s->next) {
    for (type_symbol* sym = s->type_symbols; sym; sym = sym->next) {
      if (tls == TYPE_SCOPE_STRUCT && !sym->is_struct) {
        continue;
      }

      if (tls == TYPE_SCOPE_UNION && !sym->is_union) {
        continue;
      }

      if (tls == TYPE_SCOPE_ENUM && !sym->is_enum) {
        continue;
      }

      if (tls == TYPE_SCOPE_TYPEDEF &&
          (sym->is_struct || sym->is_union || sym->is_enum)) {
        continue;
      }

      // This symbol can't be referenced by name.
      if (!sym->name) {
        continue;
      }

      if (strncmp(name->pos, sym->name, name->len) == 0 &&
          name->len == strlen(sym->name)) {
        return sym;
      }
    }
  }

  return NULL;
}

/**
 * State for switch statements.
 */

static int switch_depth;
static switch_case* switch_stack[MAX_SWITCH_DEPTH];

static void
push_switch(switch_case* head)
{
  switch_stack[switch_depth++] = head;
}

static void
pop_switch(void)
{
  SCC_ASSERT(NULL, switch_depth > 0, "pop_switch outside of switch");
  switch_depth--;
}

static void
switch_record_case(switch_case* case_)
{
  switch_stack[switch_depth - 1] = switch_stack[switch_depth - 1]->next = case_;
}

static int
in_switch()
{
  return switch_depth > 0;
}

/**
 * 0 if we are not in a loop or breakable expr (switch), >1 if we are. Used for
 * detecting if a "break" or "continue" is valid.
 */
static int loop_depth;

static void
push_loop(void)
{
  loop_depth++;
}

static void
pop_loop(void)
{
  SCC_ASSERT(NULL, loop_depth > 0, "pop_loop outside of loop");
  loop_depth--;
}

static int
in_loop(void)
{
  return loop_depth > 0;
}

static type_symbol*
make_type_symbol(token* tok, type* ty)
{
  type_symbol* sym = malloc(sizeof(type_symbol));
  memset(sym, 0, sizeof(type_symbol));
  if (tok) {
    sym->name = strndup(tok->pos, tok->len);
  }
  sym->ty = ty;
  return sym;
}

static symbol*
make_symbol_local(token* tok, type* ty)
{
  symbol* s = malloc(sizeof(symbol));
  memset(s, 0, sizeof(symbol));
  s->tok = tok;
  s->linkage = LINKAGE_NONE;
  s->name = strndup(tok->pos, tok->len);
  s->kind = SYMBOL_LOCAL_VAR;
  s->ty = ty;
  s->next = current_function->u.function.locals;
  current_function->u.function.locals = s;
  return s;
}

static symbol*
make_symbol_function(token* tok, type* ty)
{
  symbol* s = malloc(sizeof(symbol));
  memset(s, 0, sizeof(symbol));
  s->tok = tok;
  s->linkage = LINKAGE_EXTERNAL;
  s->name = strndup(tok->pos, tok->len);
  s->kind = SYMBOL_FUNCTION;
  s->ty = ty;
  return s;
}

static symbol*
make_symbol_global(token* tok, type* ty, char* name)
{
  symbol* s = malloc(sizeof(symbol));
  memset(s, 0, sizeof(symbol));
  s->tok = tok;
  s->linkage = LINKAGE_INTERNAL;
  s->name = name;
  s->kind = SYMBOL_GLOBAL_VAR;
  s->ty = ty;
  return s;
}

static symbol*
make_symbol_constant(token* tok, type* ty, token* name, int value)
{
  symbol* s = malloc(sizeof(symbol));
  memset(s, 0, sizeof(symbol));
  s->tok = tok;
  s->linkage = LINKAGE_NONE;
  s->name = strndup(name->pos, name->len);
  s->kind = SYMBOL_CONSTANT;
  s->ty = ty;
  s->u.constant_value = value;
  return s;
}

static node*
make_node_const(token* tok, type* ty, int value)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_CONST;
  n->ty = ty;
  n->tok = tok;
  n->u.const_value = value;
  return n;
}

static node*
make_node_arg(token* tok, type* ty, int count)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_ARG;
  n->tok = tok;
  n->ty = ty;
  n->u.arg.count = count;
  return n;
}

static node*
make_symbol_ref(token* tok, symbol* sym);

static node*
make_string_literal(token* tok)
{
  static int counter = 0;

  char symbol_name_buf[4096];
  memset(symbol_name_buf, 0, 4096);

  char* contents = tok->string_value;
  size_t len = strlen(contents);
  type* ty = make_array_type(ty_char, len);
  snprintf(symbol_name_buf, 4096, ".L.str.%d", counter++);
  symbol* sym = make_symbol_global(tok, ty, strdup(symbol_name_buf));
  sym->u.global_data = contents;
  sym->next = symbols;
  symbols = sym;
  // TODO - deduplicate string literals
  return make_symbol_ref(tok, sym);
}

static node*
make_node_binary(token* tok, binop op, node* left, node* right)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_BINOP;
  n->tok = tok;
  SCC_ASSERT(
    n->tok, op != BINOP_ADD, "call make_add_or_sub() instead for add ops");
  SCC_ASSERT(
    n->tok, op != BINOP_SUB, "call make_add_or_sub() instead for sub ops");
  // TODO - there are complex promotion rules that inform what the type of
  // this expression will be
  n->ty = left->ty;
  n->u.binop.op = op;
  n->u.binop.left = left;
  n->u.binop.right = right;
  return n;
}

static node*
make_conv(token* tok, node* value, type* ty)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->tok = tok;
  n->kind = NODE_CONV;
  n->ty = ty;
  n->u.conv.value = value;
  return n;
}

static void
usual_arithmetic_conversions(node** left, node** right)
{
  // 6.3.1.8 Usual arithmetic conversions
  type* left_ty = (*left)->ty;
  type* right_ty = (*right)->ty;

  // TODO type equality?
  if (left_ty == right_ty) {
    return;
  }

  // The spec defines the priority of conversions using "integer conversion
  // rank", defined in 6.3.1.1.
  //
  // The type with the lesser conversion rank is promoted to the type with the
  // greater conversion rank.
  int cmp = integer_conversion_rank_compare(left_ty, right_ty);
  if (cmp < 0) {
    *left = make_conv((*left)->tok, *left, right_ty);
  } else if (cmp > 0) {
    *right = make_conv((*right)->tok, *right, left_ty);
  }
}

static node*
make_add_or_sub(binop op, token* tok, node* left, node* right)
{
  SCC_ASSERT(tok, op == BINOP_ADD || op == BINOP_SUB, "not an add or sub");
  char* tok_str = op == BINOP_ADD ? "+" : "-";
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->tok = tok;
  n->u.binop.op = op;
  // 6.5.6 Additive Operators (C11: Page 92)
  if (is_arithmetic_type(left->ty) && is_arithmetic_type(right->ty)) {
    usual_arithmetic_conversions(&left, &right);
  }

  if ((left->ty->base && right->ty->kind == TYPE_INT) ||
      (left->ty->kind == TYPE_INT && right->ty->base)) {
    // Only "arithmetic type" is required for the non-pointer argument here,
    // but we only have ints right now.
    n->ty = left->ty->base ? left->ty : right->ty;

    // This is where "pointer arithmetic" for the + operator is implemented.
    n->u.binop.left = left;
    n->u.binop.right = make_node_binary(
      tok, BINOP_MUL, right, make_node_const(tok, ty_int, n->ty->base->size));
    return n;
  }

  if (right->ty->base && left->ty->base && op == BINOP_SUB) {
    // 6.5.6.9 Subtraction of two pointers
    // TODO type equality?
    if (right->ty->base != left->ty->base) {
      error_at(right->tok,
               "invalid types for `%s` operator (have `%s` and `%s`)",
               tok_str,
               type_name(right->ty),
               type_name(left->ty));
    }
    n->ty = left->ty;
  } else if (left->ty->kind != TYPE_INT) {
    error_at(left->tok,
             "invalid type for `%s` operator (have `%s`)",
             tok_str,
             type_name(left->ty));
  } else if (right->ty->kind != TYPE_INT) {
    error_at(right->tok,
             "invalid type for `%s` operator (have `%s`)",
             tok_str,
             type_name(right->ty));
  }

  n->ty = left->ty;
  n->u.binop.left = left;
  n->u.binop.right = right;
  return n;
}

static node*
make_logical_and(token* tok, node* left, node* right)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_AND;
  n->ty = ty_int;
  n->tok = tok;
  n->u.logic_binop.left = left;
  n->u.logic_binop.right = right;
  return n;
}

static node*
make_logical_or(token* tok, node* left, node* right)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_OR;
  n->ty = ty_int;
  n->tok = tok;
  n->u.logic_binop.left = left;
  n->u.logic_binop.right = right;
  return n;
}

static node*
make_return(token* tok, node* val)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_RETURN;
  n->ty = ty_void;
  n->tok = tok;
  n->u.return_value = val;
  return n;
}

// this sucks, we probably don't need it
static node*
make_nop()
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_NOP;
  return n;
}

static node*
convert(node* value, type* ty);

static node*
make_node_assign(token* tok, node* lvalue, node* rvalue)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_ASSIGN;
  n->tok = tok;
  n->ty = rvalue->ty;
  n->u.assign.lvalue = lvalue;
  n->u.assign.rvalue = convert(rvalue, lvalue->ty);
  return n;
}

static node*
make_symbol_ref(token* tok, symbol* sym)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_SYMBOL_REF;
  n->tok = tok;
  if (sym) {
    n->ty = sym->ty;
  } else {
    n->ty = ty_int;
  }
  n->u.symbol_ref = sym;
  return n;
}

static node*
make_expr_stmt(token* tok, node* value)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_EXPR_STMT;
  n->tok = tok;
  n->ty = ty_void;
  n->u.expr_stmt_value = value;
  return n;
}

static node*
make_if_stmt(token* tok, node* cond, node* then, node* else_)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_IF;
  n->tok = tok;
  n->ty = ty_void;
  n->u.if_.cond = cond;
  n->u.if_.then = then;
  n->u.if_.else_ = else_;
  return n;
}

static node*
make_for_stmt(token* tok, node* initializer, node* cond, node* next, node* body)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_FOR;
  n->tok = tok;
  n->ty = ty_void;
  n->u.for_.initializer = initializer;
  n->u.for_.cond = cond;
  n->u.for_.next = next;
  n->u.for_.body = body;
  return n;
}

static node*
make_do_stmt(token* tok, node* body, node* cond)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_DO;
  n->tok = tok;
  n->ty = ty_void;
  // TODO(check): cond->ty is scalar
  n->u.do_.body = body;
  n->u.do_.cond = cond;
  return n;
}

static node*
make_break_stmt(token* tok)
{
  if (!in_loop() && !in_switch()) {
    error_at(tok, "break outside of loop or switch");
  }

  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_BREAK;
  n->tok = tok;
  n->ty = ty_void;
  return n;
}

static node*
make_continue_stmt(token* tok)
{
  if (!in_loop()) {
    error_at(tok, "continue outside of loop");
  }

  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_CONTINUE;
  n->tok = tok;
  n->ty = ty_void;
  return n;
}

static node*
make_deref(token* tok, node* base)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_DEREF;
  n->tok = tok;
  n->ty = base->ty->base;
  n->u.deref_value = base;
  return n;
}

static node*
make_addrof(token* tok, node* base)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_ADDROF;
  n->tok = tok;
  n->ty = make_pointer_type(base->ty);
  n->u.addrof_value = base;
  return n;
}

static node*
make_call(token* tok, char* name, node* args, type* ret_ty)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_CALL;
  n->tok = tok;
  n->ty = ret_ty;
  n->u.call.name = name;
  n->u.call.args = args;
  return n;
}

static node*
make_postincrement(token* tok, node* base)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_POSTINCREMENT;
  n->tok = tok;
  n->ty = base->ty;
  n->u.postincrement.arg = base;
  return n;
}

static node*
make_postdecrement(token* tok, node* base)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_POSTDECREMENT;
  n->tok = tok;
  n->ty = base->ty;
  n->u.postdecrement.arg = base;
  return n;
}

static node*
make_member(token* tok, node* base, field* field)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_MEMBER;
  n->tok = tok;
  n->ty = field->ty;
  n->u.member.base = base;
  n->u.member.field = field;
  return n;
}

static node*
make_member_deref(token* tok, node* base, field* field)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_MEMBER_DEREF;
  n->tok = tok;
  n->ty = field->ty;
  n->u.member.base = base;
  n->u.member.field = field;
  return n;
}

static node*
make_switch(token* tok, node* cond, node* body, switch_case* cases)
{
  if (!is_arithmetic_type(cond->ty)) {
    error_at(cond->tok, "switch condition must have integer type");
  }

  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_SWITCH;
  n->tok = tok;
  n->ty = ty_void;
  n->u.switch_.cond = cond;
  n->u.switch_.body = body;
  n->u.switch_.cases = cases;
  return n;
}

static switch_case*
make_switch_case(token* tok, node* cond, node* label)
{
  switch_case* sc = malloc(sizeof(switch_case));
  memset(sc, 0, sizeof(switch_case));
  sc->tok = tok;
  sc->cond = cond;
  sc->label = label;
  return sc;
}

static node*
make_label(token* tok, char* name)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_LABEL;
  n->tok = tok;
  n->ty = ty_void;
  n->u.label_name = name;
  return n;
}

static int
is_valid_cond(type* tru, type* fls)
{
  // 6.5.15 Conditional operator
  //
  // The ternary conditional operator has a bunch of constraints on the second
  // and third operands, which are enforced here.
  //
  // One of the following must hold:
  // 1. Both operands have arithmetic type.
  if (is_arithmetic_type(tru) && is_arithmetic_type(fls)) {
    return 1;
  }

  // 2. Both operands have the same structure or union type
  if (tru->kind == TYPE_STRUCT && fls->kind == TYPE_STRUCT && tru == fls) {
    return 1;
  }

  if (tru->kind == TYPE_UNION && fls->kind == TYPE_UNION && tru == fls) {
    return 1;
  }

  // 3. Both operands have void type.
  // (weird, but ok.)
  if (tru == ty_void && fls == ty_void) {
    return 1;
  }

  // TODO
  // 4. Both operands are pointers to qualified or unqualified versions of
  // compatible types;
  if (tru->base && fls->base) {
    return is_valid_cond(tru->base, fls->base);
  }

  // 5. One operand is a pointer and the other is a null pointer constant,
  // 6. One operand is a pointer to an object type and the other is a pointer
  // to a qualified or unqualified version of void
  return 0;
}

static node*
make_cond(token* tok, node* cond, node* tru, node* fls)
{
  if (!is_scalar_type(cond->ty)) {
    error_at(cond->tok,
             "condition of ternary operator must have scalar type (has `%s`)",
             type_name(cond->ty));
  }

  if (!is_valid_cond(tru->ty, fls->ty)) {
    error_at(tok,
             "invalid types for true and false branches of conditional "
             "expression (have `%s` and `%s`)",
             type_name(tru->ty),
             type_name(fls->ty));
  }
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_COND;
  n->tok = tok;
  n->ty = ty_void;
  n->u.cond.cond = cond;
  n->u.cond.true_expr = tru;
  n->u.cond.false_expr = fls;
  return n;
}

static token*
eat(token** cursor, token_kind kind)
{
  token* tok = *cursor;
  if (tok->kind != kind) {
    error_at(tok, "unexpected token");
  }

  *cursor = tok->next;
  return tok;
}

static int
peek(token** cursor, token_kind kind)
{
  token* tok = *cursor;
  return tok->kind == kind;
}

static int
equal(token** cursor, token_kind kind)
{
  token* tok = *cursor;
  if (tok->kind != kind) {
    return 0;
  }

  eat(cursor, kind);
  return 1;
}

/**
 * Converts a value generated by the expression "e" to the target type.
 */
static node*
convert(node* e, type* target_ty)
{
  // 6.3: Conversions
  if (target_ty->base) {
    // 6.3.2.3 Pointers
    if (e->kind == NODE_CONST && e->u.const_value == 0) {
      // 6.3.2.3.3 An integer constant with the value 0 is a null pointer
      // constant and is convertible to a pointer.
      return make_node_const(e->tok, make_pointer_type(ty_void), 0);
    }

    if (is_arithmetic_type(e->ty)) {
      // 6.3.2.3.5 An integer may be converted into any pointer type.
      //
      // Every compiler warns on this because this is nonsense. Clang upgraded
      // the warning into an error in their most recent release.
      warn_at(
        e->tok,
        "conversion of `%s` to `%s` makes pointer from integer without a cast",
        type_name(e->ty),
        type_name(target_ty));
    }
  }

  return e;
}

static node*
return_stmt(token** cursor);

static node*
primary(token**);

static node*
mul_expr(token**);

static node*
relational_expr(token**);

static node*
add_expr(token**);

static node*
postfix_expr(token**);

static node*
expr(token**);

static node*
assignment_expr(token**);

static node*
declaration(token**);

static node*
compound_stmt(token**);

static node*
if_stmt(token**);

static node*
for_stmt(token**);

static node*
while_stmt(token**);

static node*
do_stmt(token**);

static node*
unary_expr(token**);

static node*
logical_or_expr(token**);

static node*
logical_and_expr(token**);

static type*
declaration_specifiers(token**, storage_class*);

static token*
declarator(token**, type**);

static node*
switch_stmt(token**);

static node*
conditional_expr(token**);

static node*
external_declaration(token**, int);

/**
 * 6.7.7 - Type names
 *
 * type_name = specifier_qualifier_list abstract_declarator?
 *
 * specifier_qualifier_list = (type_specifier | type_qualifier)*
 * type_specifier = (void | char | int)
 * type_qualifier = epsilon
 *
 * abstract_declarator = "*"+
 *
 * We're not getting into abstract declarators yet, since those get wild.
 */
static type*
decl_type_name(token** cursor)
{
  type* decltype = declaration_specifiers(cursor, NULL);
  while (equal(cursor, TOKEN_STAR)) {
    decltype = make_pointer_type(decltype);
  }

  return decltype;
}

static int
can_start_type_name(token** cursor)
{
  // C is quite ambiguous; an identifier can begin a decl, but only if that
  // identifier refers to a typedef.
  if (peek(cursor, TOKEN_IDENT)) {
    if (scope_lookup_type(*cursor, TYPE_SCOPE_TYPEDEF)) {
      return 1;
    }

    return 0;
  }

  return peek(cursor, TOKEN_CHAR) || peek(cursor, TOKEN_INT) ||
         peek(cursor, TOKEN_BOOL) || peek(cursor, TOKEN_VOID) ||
         peek(cursor, TOKEN_STRUCT) || peek(cursor, TOKEN_UNION) ||
         peek(cursor, TOKEN_EXTERN) || peek(cursor, TOKEN_TYPEDEF) ||
         peek(cursor, TOKEN_STATIC);
}

/**
 * stmt = return_stmt | declaration | expr SEMI | compound_statement |
 * if_statement | for_statement | while_statement | do_statement
 */
static node*
stmt(token** cursor)
{
  if (peek(cursor, TOKEN_RETURN)) {
    return return_stmt(cursor);
  }

  if (can_start_type_name(cursor)) {
    return declaration(cursor);
  }

  if (peek(cursor, TOKEN_LBRACE)) {
    return compound_stmt(cursor);
  }

  if (peek(cursor, TOKEN_IF)) {
    return if_stmt(cursor);
  }

  if (peek(cursor, TOKEN_FOR)) {
    return for_stmt(cursor);
  }

  if (peek(cursor, TOKEN_WHILE)) {
    return while_stmt(cursor);
  }

  if (peek(cursor, TOKEN_DO)) {
    return do_stmt(cursor);
  }

  if (peek(cursor, TOKEN_SWITCH)) {
    return switch_stmt(cursor);
  }

  if (peek(cursor, TOKEN_CASE)) {
    token* case_tok = eat(cursor, TOKEN_CASE);
    if (!in_switch()) {
      error_at(case_tok, "case outside of switch statement");
    }

    node* case_cond = logical_or_expr(cursor);
    eat(cursor, TOKEN_COLON);

    // Labeled statements require a statement to follow it.
    if (peek(cursor, TOKEN_RBRACE)) {
      error_at(*cursor, "expected statement");
    }

    node* label = make_label(case_tok, gen_label_name(".L.case", gen_label()));
    switch_record_case(make_switch_case(case_tok, case_cond, label));
    return label;
  }

  if (peek(cursor, TOKEN_DEFAULT)) {
    token* default_tok = eat(cursor, TOKEN_DEFAULT);
    if (!in_switch()) {
      error_at(default_tok, "default outside of switch statement");
    }

    eat(cursor, TOKEN_COLON);
    // Labeled statements require a statement to follow it.
    if (peek(cursor, TOKEN_RBRACE)) {
      error_at(*cursor, "expected statement");
    }

    node* label =
      make_label(default_tok, gen_label_name(".L.default", gen_label()));
    switch_record_case(make_switch_case(default_tok, NULL, label));
    return label;
  }

  if (peek(cursor, TOKEN_BREAK)) {
    token* break_tok = eat(cursor, TOKEN_BREAK);
    eat(cursor, TOKEN_SEMICOLON);
    return make_break_stmt(break_tok);
  }

  if (peek(cursor, TOKEN_CONTINUE)) {
    token* continue_tok = eat(cursor, TOKEN_CONTINUE);
    eat(cursor, TOKEN_SEMICOLON);
    return make_continue_stmt(continue_tok);
  }

  node* e = expr(cursor);
  token* semi_tok = eat(cursor, TOKEN_SEMICOLON);
  return make_expr_stmt(semi_tok, e);
}

/**
 * return_stmt = "return" expr ";"
 */
static node*
return_stmt(token** cursor)
{
  token* ret_tok = eat(cursor, TOKEN_RETURN);
  if (equal(cursor, TOKEN_SEMICOLON)) {
    return make_return(ret_tok, NULL);
  }

  node* val = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  return make_return(ret_tok, val);
}

/**
 * expr_stmt = expression SEMICOLON | SEMICOLON
 */
static node*
expr_stmt(token** cursor)
{
  if (equal(cursor, TOKEN_SEMICOLON)) {
    return NULL;
  }

  node* e = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  return e;
}

/**
 * The declaration production is tangled in with the external_declaration
 * production. This production is for decls that have block scope, which do not
 * permit things like function definitions.
 */
static node*
declaration(token** cursor)
{
  return external_declaration(cursor, 1);
}

/**
 * compound_statement
 *  : "{" statement* "}"
 */
static node*
compound_stmt(token** cursor)
{
  eat(cursor, TOKEN_LBRACE);
  node head = { 0 };
  node* stmts = &head;
  push_scope();
  while (!peek(cursor, TOKEN_RBRACE)) {
    node* s = stmt(cursor);
    for (node* c = s; c; c = c->next) {
      stmts = stmts->next = c;
    }
  }
  pop_scope();

  eat(cursor, TOKEN_RBRACE);
  return head.next;
}

static node*
if_stmt(token** cursor)
{
  token* if_tok = eat(cursor, TOKEN_IF);
  eat(cursor, TOKEN_LPAREN);
  node* cond = expr(cursor);
  eat(cursor, TOKEN_RPAREN);
  node* then = stmt(cursor);
  node* else_ = NULL;
  if (equal(cursor, TOKEN_ELSE)) {
    else_ = stmt(cursor);
  }

  return make_if_stmt(if_tok, cond, then, else_);
}

static node*
switch_stmt(token** cursor)
{
  token* switch_tok = eat(cursor, TOKEN_SWITCH);
  eat(cursor, TOKEN_LPAREN);
  node* cond = expr(cursor);
  eat(cursor, TOKEN_RPAREN);
  switch_case head = { 0 };
  push_switch(&head);
  node* body = stmt(cursor);
  pop_switch();
  return make_switch(switch_tok, cond, body, head.next);
}

/**
 * for_statement
 *   : "for" "(" expr_stmt expr_stmt ")" stmt
 *   | "for" "(" expr_stmt expr_stmt expr ")" stmt
 *   | "for" "(" declaration expr_stmt ")" stmt
 *   | "for" "(" declaration expr_stmt expr ")" stmt
 *   ;
 */
static node*
for_stmt(token** cursor)
{
  token* for_tok = eat(cursor, TOKEN_FOR);
  eat(cursor, TOKEN_LPAREN);
  node* initializer;
  // Declarations are technically not statements, but C11 makes an exception
  // and allows for statements to contain one decl in the initializer stanza.
  // TODO: this should be any token in the first set of a decl
  push_scope();
  if (can_start_type_name(cursor)) {
    initializer = declaration(cursor);
  } else {
    initializer = expr_stmt(cursor);
  }

  node* cond = expr_stmt(cursor);
  node* next = NULL;
  if (!peek(cursor, TOKEN_LPAREN)) {
    next = expr(cursor);
  }

  eat(cursor, TOKEN_RPAREN);
  push_loop();
  node* body = stmt(cursor);
  pop_loop();
  pop_scope();
  return make_for_stmt(for_tok, initializer, cond, next, body);
}

/**
 * while_stmt
 *  : "while" "(" expr ")" statement
 */
static node*
while_stmt(token** cursor)
{
  token* while_tok = eat(cursor, TOKEN_WHILE);
  eat(cursor, TOKEN_LPAREN);
  node* cond = expr(cursor);
  eat(cursor, TOKEN_RPAREN);
  push_loop();
  node* body = stmt(cursor);
  pop_loop();
  return make_for_stmt(while_tok, NULL, cond, NULL, body);
}

/**
 * do_stmt
 *  : "do" stmt "while" "(" expr ")" ";"
 */
static node*
do_stmt(token** cursor)
{
  token* do_tok = eat(cursor, TOKEN_DO);
  push_loop();
  node* body = stmt(cursor);
  pop_loop();
  eat(cursor, TOKEN_WHILE);
  eat(cursor, TOKEN_LPAREN);
  node* cond = expr(cursor);
  eat(cursor, TOKEN_RPAREN);
  eat(cursor, TOKEN_SEMICOLON);
  return make_do_stmt(do_tok, body, cond);
}

/**
 *  expression
 *    : assignment_expression
 */
static node*
expr(token** cursor)
{
  return assignment_expr(cursor);
}

/**
 * assignment_expression
 *   : relational_expression
 *   | primary_expression ("=" assignment_expression)*
 */
static node*
assignment_expr(token** cursor)
{
  // TODO - enforce that this expression produces an lvalue
  node* base = conditional_expr(cursor);
  for (;;) {
    token* eq_tok = *cursor;
    if (equal(cursor, TOKEN_EQUAL)) {
      base = make_node_assign(eq_tok, base, assignment_expr(cursor));
      continue;
    }

    return base;
  }
}

static node*
conditional_expr(token** cursor)
{
  node* base = logical_or_expr(cursor);
  for (;;) {
    token* question_tok = *cursor;
    if (equal(cursor, TOKEN_QUESTION)) {
      node* true_expr = expr(cursor);
      eat(cursor, TOKEN_COLON);
      node* false_expr = conditional_expr(cursor);
      base = make_cond(question_tok, base, true_expr, false_expr);
      continue;
    }

    break;
  }

  return base;
}

/**
 * logical-OR-expression ::=
 *  logical-AND-expression
 *  logical-OR-expression "||" logical-AND-expression
 */
static node*
logical_or_expr(token** cursor)
{
  node* base = logical_and_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_DOUBLE_PIPE)) {
      base = make_logical_or(op_tok, base, logical_or_expr(cursor));
      continue;
    }

    break;
  }

  return base;
}

/**
 * logical-AND-expression ::=
 *  inclusive-OR-expression
 *  logical-AND-expression "&&" inclusive-OR-expression
 */
static node*
logical_and_expr(token** cursor)
{
  node* base = relational_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_DOUBLE_AMPERSAND)) {
      base = make_logical_and(op_tok, base, logical_and_expr(cursor));
      continue;
    }

    break;
  }

  return base;
}

/**
 * relational_expr = add_expr ("==" add_expr | "!=" add_expr | ">" add_expr |
 * "<" add_expr | ">=" add_expr | "<=" add_expr)*
 */
static node*
relational_expr(token** cursor)
{
  node* base = add_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_DOUBLE_EQ)) {
      base = make_node_binary(op_tok, BINOP_EQUAL, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_NOT_EQ)) {
      base = make_node_binary(op_tok, BINOP_NOT_EQUAL, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_LT)) {
      base = make_node_binary(op_tok, BINOP_LT, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_LT_EQ)) {
      base = make_node_binary(op_tok, BINOP_LT_EQUAL, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_GT)) {
      base = make_node_binary(op_tok, BINOP_GT, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_GT_EQ)) {
      base = make_node_binary(op_tok, BINOP_GT_EQUAL, base, add_expr(cursor));
      continue;
    }

    return base;
  }
}

/**
 * add_expr = mul_expr ("+" mul_expr | "-" mul_expr)*
 */
static node*
add_expr(token** cursor)
{
  node* base = mul_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_PLUS)) {
      base = make_add_or_sub(BINOP_ADD, op_tok, base, mul_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_MINUS)) {
      base = make_add_or_sub(BINOP_SUB, op_tok, base, mul_expr(cursor));
      continue;
    }

    return base;
  }
}

/*
 * multiplicative-expression ::=
 * 	cast-expression
 * 	multiplicative-expression "*" cast-expression
 * 	multiplicative-expression "/" cast-expression
 * 	multiplicative-expression "%" cast-expression
 */
static node*
mul_expr(token** cursor)
{
  node* base = unary_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_STAR)) {
      base = make_node_binary(op_tok, BINOP_MUL, base, unary_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_PERCENT)) {
      base = make_node_binary(op_tok, BINOP_MOD, base, unary_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_SLASH)) {
      base = make_node_binary(op_tok, BINOP_DIV, base, unary_expr(cursor));
      continue;
    }

    return base;
  }
}

/**
 * A simplistic definition of an lvalue.
 */
static int
is_lvalue(node* n)
{
  if (n->kind == NODE_MEMBER || n->kind == NODE_MEMBER_DEREF) {
    return is_lvalue(n->u.member.base);
  }

  return n->kind == NODE_SYMBOL_REF || n->kind == NODE_DEREF;
}

/*
 * unary_expression
 *   : postfix_expression
 *   | ("*" | "&") unary_expression
 *   | "." IDENTIFIER
 *   ;
 */
static node*
unary_expr(token** cursor)
{
  token* unop_tok = *cursor;
  if (equal(cursor, TOKEN_STAR)) {
    node* base = unary_expr(cursor);
    if (base->ty->kind != TYPE_POINTER) {
      error_at(base->tok,
               "cannot dereference non-pointer type (type is `%s`)",
               type_name(base->ty));
    }
    if (base->ty->base->kind == TYPE_VOID) {
      error_at(base->tok, "cannot dereference void pointer");
    }
    return make_deref(unop_tok, base);
  }

  if (equal(cursor, TOKEN_AMPERSAND)) {
    node* base = unary_expr(cursor);
    if (!is_lvalue(base)) {
      error_at(unop_tok, "not a lvalue");
    }
    return make_addrof(unop_tok, base);
  }

  if (equal(cursor, TOKEN_SIZEOF)) {
    /**
     * One of the many ambiguities in the C grammar lurks here; the meaning of
     * the argument to sizeof is dependent on the nature of an identifier given
     * to it.
     *
     * For example: sizeof(x) can mean two things:
     *   1. The size in bytes required to store the type of x, where x is a
     * value
     *   2. The size in bytes required to store an instance of x, where x is a
     * type
     *
     * Form 1 requires parens, which we can use to disambiguate part of this
     * parse.
     */
    node* sizeof_arg;
    if (equal(cursor, TOKEN_LPAREN)) {
      // The next token might be an identifier and, if it is, it might be a
      // type. If it is, we'll parse form 2.
      if (can_start_type_name(cursor)) {
        type* ty = decl_type_name(cursor);
        eat(cursor, TOKEN_RPAREN);
        return make_node_const(unop_tok, ty_int, ty->size);
      } else {
        sizeof_arg = expr(cursor);
        eat(cursor, TOKEN_RPAREN);
      }
    } else {
      sizeof_arg = unary_expr(cursor);
    }
    return make_node_const(unop_tok, ty_int, sizeof_arg->ty->size);
  }

  if (equal(cursor, TOKEN_EXCLEM)) {
    // Logical not.
    node* base = unary_expr(cursor);
    if (!is_scalar_type(base->ty)) {
      error_at(base->tok,
               "logical not requires a scalar type (have `%s`)",
               type_name(base->ty));
    }

    // 6.5.3.3.5 Unary arithmetic operators
    //
    // The expression !E is equivalent to (0==E).
    return make_node_binary(
      base->tok, BINOP_EQUAL, make_node_const(base->tok, ty_int, 0), base);
  }

  if (equal(cursor, TOKEN_MINUS)) {
    // Negation.
    node* base = unary_expr(cursor);
    if (!is_arithmetic_type(base->ty)) {
      error_at(base->tok,
               "arithmetic negation requires an arithmetic type (have `%s`)",
               type_name(base->ty));
    }

    // TODO(efficiency) probably worth encoding this directly as a node for
    // better codegen
    return make_add_or_sub(
      BINOP_SUB, base->tok, make_node_const(base->tok, ty_int, 0), base);
  }

  return postfix_expr(cursor);
}

/**
 * postfix_expr = primary ("++" | "(" argument_list ")" | "[" expression "]")*
 */
static node*
postfix_expr(token** cursor)
{
  node* base = primary(cursor);
  for (;;) {
    token* candidate = *cursor;
    if (equal(cursor, TOKEN_PLUS_PLUS)) {
      // Post-increment is only legal on lvalues
      if (!is_lvalue(base)) {
        error_at(*cursor, "lvalue required as increment operand");
      }

      return make_postincrement(candidate, base);
    }

    if (equal(cursor, TOKEN_MINUS_MINUS)) {
      // Post-decrement is only legal on lvalues
      if (!is_lvalue(base)) {
        error_at(*cursor, "lvalue required as decrement operand");
      }

      return make_postdecrement(candidate, base);
    }

    if (equal(cursor, TOKEN_LPAREN)) {
      node arg_head = { 0 };
      node* args = &arg_head;

      if (!equal(cursor, TOKEN_RPAREN)) {
        args = args->next = assignment_expr(cursor);
        while (!equal(cursor, TOKEN_RPAREN)) {
          eat(cursor, TOKEN_COMMA);
          args = args->next = assignment_expr(cursor);
        }
      }

      if (base->kind != NODE_SYMBOL_REF) {
        error_at(base->tok, "only calls of bare identifiers are supported");
      }

      char* name;
      type* ret_ty = ty_int;
      if (!base->u.symbol_ref) {
        // This can be one of two things: a compiler builtin, or an unbound
        // identifier.
        //
        // C permits calling unbound identifiers, so we'll defer to the code
        // generator to sort that out. All unbound identifiers are assumed to be
        // functions that take no parameters and return an int.
        //
        // Compiler builtins have their own signature that can be checked here.
        name = strndup(base->tok->pos, base->tok->len);
#ifndef SCC_SELFHOST
        builtin_function* builtin = builtin_lookup(name);
        if (!builtin) {
          warn_at(base->tok, "implicit declaration of function `%s`", name);
        } else {
          // TODO(check) parameter assignability checking
        }
#endif /* !SCC_SELFHOST */
      } else {
        name = base->u.symbol_ref->name;
        if (base->u.symbol_ref->ty->kind != TYPE_FUNCTION) {
          error_at(base->tok,
                   "called object is not a function (has type `%s`)",
                   type_name(base->u.symbol_ref->ty));
        }

        ret_ty = base->u.symbol_ref->ty->u.function.ret;
      }

      return make_call(candidate, name, arg_head.next, ret_ty);
    }

    if (equal(cursor, TOKEN_LBRACKET)) {
      // 6.5.2.1 Array subscripting
      //
      // Array subscripting in C is just sugar for pointer arithmetic.
      // e1[e2] is exactly equivalent to *(e1 + e2).
      //
      // These types are checked in make_add_or_sub, but we check them again
      // here for a better error message prior to desugaring down to `+` and
      // `*`.
      node* subscript = expr(cursor);
      eat(cursor, TOKEN_RBRACKET);
      if ((base->ty->base && subscript->ty->kind == TYPE_INT) ||
          (subscript->ty->kind == TYPE_INT && base->ty->base)) {
        // Quick constant-fold here to avoid generating really dumb code when
        // the subscript expr is trivially zero.
        if (subscript->kind == NODE_CONST && subscript->u.const_value == 0) {
          base = make_deref(candidate, base);
        } else {
          // Otherwise, *(e1 + e2)
          base = make_deref(
            candidate, make_add_or_sub(BINOP_ADD, candidate, base, subscript));
        }
        continue;
      }

      error_at(
        candidate,
        "invalid types for array subscript operator (have `%s` and `%s`)",
        type_name(base->ty),
        type_name(subscript->ty));
    }

    if (equal(cursor, TOKEN_DOT)) {
      if (base->ty->kind != TYPE_STRUCT && base->ty->kind != TYPE_UNION) {
        error_at(
          base->tok,
          "left-hand-side of member expression is not a struct or union");
      }

      token* field_name = eat(cursor, TOKEN_IDENT);
      field* member = field_lookup(field_name, base->ty);
      if (!member) {
        error_at(field_name,
                 "no such field `%s` in struct type `%s`",
                 strndup(field_name->pos, field_name->len),
                 type_name(base->ty));
      }

      base = make_member(candidate, base, member);
      continue;
    }

    if (equal(cursor, TOKEN_ARROW)) {
      if (!base->ty->base) {
        error_at(base->tok,
                 "left-hand-side of member deref expression is not a pointer");
      }

      token* field_name = eat(cursor, TOKEN_IDENT);
      field* member = field_lookup(field_name, base->ty->base);
      if (!member) {
        error_at(field_name,
                 "no such field `%s` in struct type `%s`",
                 strndup(field_name->pos, field_name->len),
                 type_name(base->ty->base));
      }

      base = make_member_deref(candidate, base, member);
      continue;
    }

    return base;
  }
}

/**
 * primary = integer | char_literal | "(" expression ")" | identifier
 */
static node*
primary(token** cursor)
{
  if (equal(cursor, TOKEN_LPAREN)) {
    node* nested = expr(cursor);
    eat(cursor, TOKEN_RPAREN);
    return nested;
  }

  if (peek(cursor, TOKEN_IDENT)) {
    token* name = eat(cursor, TOKEN_IDENT);
    for (scope* s = current_scope; s; s = s->next) {
      for (symbol* sym = s->symbols; sym; sym = sym->next_in_scope) {
        if (strncmp(name->pos, sym->name, name->len) == 0 &&
            name->len == strlen(sym->name)) {
          if (sym->kind == SYMBOL_CONSTANT) {
            return make_node_const(name, ty_int, sym->u.constant_value);
          }

          return make_symbol_ref(name, sym);
        }
      }
    }

    /* you would expect that we'd emit an error here if an identifier isn't
     * found; however, calls to unbound identifiers are completely legal in C.
     *
     * we rely on the code generator to emit an error if it requires this
     * identifier to actually be bound.
     */
    return make_symbol_ref(name, NULL);
  }

  if (peek(cursor, TOKEN_CHAR_LITERAL)) {
    token* char_lit = eat(cursor, TOKEN_CHAR_LITERAL);
    return make_node_const(char_lit, ty_int, char_lit->value);
  }

  if (peek(cursor, TOKEN_STRING_LITERAL)) {
    token* str_lit = eat(cursor, TOKEN_STRING_LITERAL);
    return make_string_literal(str_lit);
  }

  token* integer = eat(cursor, TOKEN_INTEGER);
  return make_node_const(integer, ty_int, integer->value);
}

static parameter*
make_parameter(token* name, type* ty)
{
  parameter* p = malloc(sizeof(parameter));
  memset(p, 0, sizeof(parameter));
  p->name = name;
  p->ty = ty;
  return p;
}

/**
 * parameter-type-list ::=
 *	parameter-list
 *	parameter-list "," "..."
 *
 * parameter-list ::=
 *  parameter-declaration
 *  parameter-list "," parameter-declaration
 *
 * parameter-declaration ::=
 *	declaration-specifiers declarator
 *  declaration-specifiers abstract-declarator?
 */
static parameter*
parameter_list(token** cursor, type* func_ty)
{
  parameter head = { 0 };
  parameter* params = &head;
  int seen_one_parameter = 0;
  int seen_void = 0;
  while (!equal(cursor, TOKEN_RPAREN)) {
    if (seen_void) {
      error_at(*cursor,
               "`void` must be the first and only parameter if specified");
    }

    if (peek(cursor, TOKEN_ELLIPSIS)) {
      token* ellipsis = eat(cursor, TOKEN_ELLIPSIS);
      if (!seen_one_parameter) {
        error_at(ellipsis, "ISO C requires a named argument before `...`");
      }

      func_ty->u.function.is_vararg = 1;
      eat(cursor, TOKEN_RPAREN);
      return head.next;
    }

    type* declspec = declaration_specifiers(cursor, NULL);
    token* param_name = declarator(cursor, &declspec);
    // Prototypes can declare a `void` parameter list, but it can be the only
    // parameter and it must not be named
    if (declspec == ty_void) {
      if (param_name) {
        error_at(param_name, "argument may not have `void` type");
      }

      seen_void = 1;
    } else {
      params = params->next = make_parameter(param_name, declspec);
    }
    if (!peek(cursor, TOKEN_RPAREN)) {
      eat(cursor, TOKEN_COMMA);
    }

    seen_one_parameter = 1;
  }

  return head.next;
}

/**
 * declarator ::=
 *  pointer? direct-declarator
 *
 * direct-declarator ::=
 *  identifier
 *  direct-declarator "(" ")"
 */
static token*
declarator(token** cursor, type** base)
{
  while (equal(cursor, TOKEN_STAR)) {
    *base = make_pointer_type(*base);
  }

  // Abstract declarators can be omitted, which (in the grammar) allows for
  // declspecs without decls.
  if (!peek(cursor, TOKEN_IDENT)) {
    return NULL;
  }

  token* ident = eat(cursor, TOKEN_IDENT);
  for (;;) {
    if (equal(cursor, TOKEN_LPAREN)) {
      if ((*base)->kind == TYPE_FUNCTION) {
        error_at(*cursor,
                 "declaration declares a function that returns a function");
      }
      parameter* params = parameter_list(cursor, *base);
      *base = make_function_type(*base, params);
      continue;
    }

    if (equal(cursor, TOKEN_LBRACKET)) {
      token* array_len = eat(cursor, TOKEN_INTEGER);
      eat(cursor, TOKEN_RBRACKET);
      *base = make_array_type(*base, array_len->value);
      continue;
    }

    break;
  }

  return ident;
}

/*
 * struct-or-union-specifier ::=
 *	struct-or-union identifier? "{" struct-declaration-list "}"
 *	struct-or-union identifier
 *
 * struct-declaration-list ::=
 *	struct-declaration
 *	struct-declaration-list struct-declaration
 *
 * struct-declaration ::=
 *  specifier-qualifier-list struct-declarator-list ";"
 */
static type*
struct_or_union_specifier(token** cursor)
{
  // 6.7.2.1 Structure and union specifiers
  token* struct_or_union_tok;
  if (peek(cursor, TOKEN_STRUCT)) {
    struct_or_union_tok = eat(cursor, TOKEN_STRUCT);
  } else {
    struct_or_union_tok = eat(cursor, TOKEN_UNION);
  }
  int is_union = struct_or_union_tok->kind == TOKEN_UNION;
  token* name = NULL;
  if (peek(cursor, TOKEN_IDENT)) {
    name = eat(cursor, TOKEN_IDENT);
  }

  if (equal(cursor, TOKEN_LBRACE)) {
    field head = { 0 };
    field* fields = &head;
    // Stays at zero for unions, incremented for structs
    int offset = 0;
    // Ignored for structs, used as the union's size for unions
    int max_member_size = 0;
    while (!equal(cursor, TOKEN_RBRACE)) {
      // Struct offsets begin at zero and are allocated at 4-byte boundaries.
      type* declspec = declaration_specifiers(cursor, NULL);
      token* decl = declarator(cursor, &declspec);
      eat(cursor, TOKEN_SEMICOLON);
      if (!declspec->size) {
        error_at(decl, "field has incomplete type `%s`", type_name(declspec));
      }

      // This is ugly and quadratic, but who cares. We'll fix it later if it's
      // an issue.
      for (field* f = head.next; f; f = f->next) {
        if (decl->len == f->name->len &&
            strncmp(decl->pos, f->name->pos, decl->len) == 0) {
          error_at(decl,
                   "struct or union declares duplicate field `%s`",
                   strndup(decl->pos, decl->len));
        }
      }

      int field_offset = offset;
      if (!is_union) {
        offset = (offset + declspec->size + 3) & -4;
      }
      if (declspec->size > max_member_size) {
        max_member_size = declspec->size;
      }
      fields = fields->next = make_field(decl, declspec, field_offset);
    }

    type* ty;
    type_symbol* ty_sym;
    if (is_union) {
      ty = make_union(name, head.next, (max_member_size + 3) & -4);
      ty_sym = make_type_symbol(name, ty);
      ty_sym->is_union = 1;
    } else {
      ty = make_struct(name, head.next, offset);
      ty_sym = make_type_symbol(name, ty);
      ty_sym->is_struct = 1;
    }

    define_type(ty_sym);
    return ty;
  } else if (!name) {
    // 6.7.2.1.2 - Anonymous structs must have a definition
    error_at(struct_or_union_tok,
             "declaration of anonymous struct or union must be a definition");
  } else {
    // This can be a forward declaration of a struct or a reference to a
    // previously-defined struct.
    type_lookup_scope lookup_scope =
      is_union ? TYPE_SCOPE_UNION : TYPE_SCOPE_STRUCT;
    type_symbol* ty_sym = scope_lookup_type(name, lookup_scope);
    if (ty_sym) {
      return ty_sym->ty;
    }

    type* ty;
    type_symbol* new_sym;
    if (is_union) {
      ty = make_union(name, NULL, 0);
      new_sym = make_type_symbol(name, ty);
      new_sym->is_union = 1;
    } else {
      ty = make_struct(name, NULL, 0);
      new_sym = make_type_symbol(name, ty);
      new_sym->is_struct = 1;
    }

    define_type(new_sym);
    return ty;
  }
}

/**
 * enum-specifier ::=
 *	"enum" identifier? "{" enumerator-list "}"
 *	"enum" identifier? "{" enumerator-list "," "}"
 *	"enum" identifier
 *
 * enumerator-list ::=
 *	enumerator
 *	enumerator-list "," enumerator
 *
 * enumerator ::=
 *	enumeration-constant
 *	enumeration-constant "=" constant-expression
 */
static type*
enum_specifier(token** cursor)
{
  token* enum_tok = eat(cursor, TOKEN_ENUM);
  token* name = NULL;
  if (peek(cursor, TOKEN_IDENT)) {
    name = eat(cursor, TOKEN_IDENT);
  }

  if (equal(cursor, TOKEN_LBRACE)) {
    int iota = 0;
    while (!equal(cursor, TOKEN_RBRACE)) {
      token* enumerator = eat(cursor, TOKEN_IDENT);
      token* value = NULL;
      if (equal(cursor, TOKEN_EQUAL)) {
        // Enumerator constants can be any constant expression; for now, we only
        // allow integer literals.
        value = eat(cursor, TOKEN_INTEGER);
      }

      if (!peek(cursor, TOKEN_RBRACE)) {
        eat(cursor, TOKEN_COMMA);
      }
      if (value) {
        // Enumeration constants are assigned one at a time, starting at 0 and
        // incrementing upwards. Explicitly setting a value jumps ahead the
        // incrementing counter.
        //
        // Note that this does get you into weird situations such as:
        // ```
        // enum foo {
        //   a = 5,
        //   b = 4,
        //   c
        // }
        // ```
        // where both c and a have the value 5, but that's just C for you; clang
        // and gcc roll with this with no warnings.
        iota = value->value;
      }

      int enumerator_value = iota++;
      symbol* sym =
        make_symbol_constant(enumerator, ty_int, enumerator, enumerator_value);
      define(sym);
    }

    if (iota == 0) {
      error_at(enum_tok, "empty enums are invalid");
    }

    type* ty = make_enum(name);
    if (name) {
      type_symbol* sym = make_type_symbol(name, ty);
      sym->is_enum = 1;
      define_type(sym);
    }

    return ty;
  } else if (!name) {
    error_at(enum_tok, "declaration of anonymous enum must be a definition");
  } else {
    type_symbol* ty_sym = scope_lookup_type(name, TYPE_SCOPE_ENUM);
    if (ty_sym) {
      return ty_sym->ty;
    }

    type* ty = make_enum(name);
    type_symbol* new_sym = make_type_symbol(name, ty);
    new_sym->is_enum = 1;
    define_type(new_sym);
    return ty;
  }
}

/*
 * declaration-specifiers ::=
 *	storage-class-specifier declaration-specifiers?
 *	type-specifier declaration-specifiers?
 *	type-qualifier declaration-specifiers?
 *	function-specifier declaration-specifiers?
 *
 * storage-class-specifier ::= "extern"
 * type-specifier ::=
 *   "int"
 *   "char"
 *   "_Bool"
 *   identifier
 *   struct-or-union-specifier
 * type-qualifier ::= epsilon
 * function-specifier ::= epsilon
 */
static type*
declaration_specifiers(token** cursor, storage_class* storage)
{
  type* type_spec = NULL;
  for (;;) {
    if (equal(cursor, TOKEN_INT)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_int;
      continue;
    }

    if (equal(cursor, TOKEN_CHAR)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_char;
      continue;
    }

    if (equal(cursor, TOKEN_BOOL)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_bool;
      continue;
    }

    if (equal(cursor, TOKEN_LONG)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_long;
      continue;
    }

    if (equal(cursor, TOKEN_VOID)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_void;
      continue;
    }

    if (equal(cursor, TOKEN_UNSIGNED)) {
      // TODO(selfhost)
      continue;
    }

    if (equal(cursor, TOKEN_CONST)) {
      // TODO(selfhost)
      continue;
    }

    if (equal(cursor, TOKEN_STATIC)) {
      if (!storage) {
        error_at(*cursor, "storage class not permitted here");
      }

      if (*storage != STORAGE_CLASS_NONE) {
        error_at(*cursor, "more than one storage class not permitted");
      }

      *storage = STORAGE_CLASS_STATIC;
      continue;
    }

    if (equal(cursor, TOKEN_EXTERN)) {
      if (!storage) {
        error_at(*cursor, "storage class not permitted here");
      }

      if (*storage != STORAGE_CLASS_NONE) {
        error_at(*cursor, "more than one storage class not permitted");
      }

      *storage = STORAGE_CLASS_EXTERN;
      continue;
    }

    if (equal(cursor, TOKEN_TYPEDEF)) {
      if (!storage) {
        error_at(*cursor, "storage class not permitted here");
      }

      if (*storage != STORAGE_CLASS_NONE) {
        error_at(*cursor, "more than one storage class not permitted");
      }

      *storage = STORAGE_CLASS_TYPEDEF;
      continue;
    }

    if (peek(cursor, TOKEN_STRUCT) || peek(cursor, TOKEN_UNION)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");

      type_spec = struct_or_union_specifier(cursor);
    }

    if (peek(cursor, TOKEN_ENUM)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");

      type_spec = enum_specifier(cursor);
    }

    if (peek(cursor, TOKEN_IDENT)) {
      // C11 has an ambiguity here about whether or not this identifier is
      // the name of a decl to follow or a typedef that describes the decl.
      // We partially resolve the ambiguity here by assuming that this
      // identifier is the decl name if a typespec has already been parsed.
      if (!type_spec) {
        // A reference to a typedef.
        token* name = eat(cursor, TOKEN_IDENT);
        type_symbol* type_sym = scope_lookup_type(name, TYPE_SCOPE_TYPEDEF);
        if (type_sym) {
          type_spec = type_sym->ty;
          return type_spec;
        }

        error_at(name, "unknown type name `%s`", strndup(name->pos, name->len));
      }
    }

    break;
  }

  SCC_ASSERT(
    *cursor, type_spec != NULL, "type spec was null after parsing declspec");
  return type_spec;
}

/**
 *
 * external-declaration ::=
 *	function-definition
 *  declaration
 *
 * function-definition ::=
 *  declaration-specifiers declarator declaration-list? compound-statement
 *
 * declaration ::=
 *  declaration-specifiers init-declarator-list? ";"
 *
 * init-declarator-list ::= init-declarator
 * init-declarator ::= declarator ("=" initializer)?
 * initializer ::= assignment-expression
 *
 * For now, we only support a single decl in the init-declarator-list, for
 * simplicity.
 */
static node*
external_declaration(token** cursor, int in_compound_statement)
{
  // Step 1 - Parsing the decl into something we can make sense of.
  token* start_tok = *cursor;
  storage_class storage = STORAGE_CLASS_NONE;
  type* declspec = declaration_specifiers(cursor, &storage);
  token* decl = NULL;
  // Only used in block scope (in_compount_statement)
  node initializer_head = { 0 };
  node* initializers = &initializer_head;
  initializers = initializers->next = make_nop();
  if (!peek(cursor, TOKEN_SEMICOLON)) {
    decl = declarator(cursor, &declspec);
  }

  // Step 2 - Figuring out what kind of symbol we're going to define for this
  // decl.
  symbol* sym = NULL;
  if (decl) {
    if (declspec->kind == TYPE_FUNCTION) {
      sym = make_symbol_function(decl, declspec);
    } else if (in_compound_statement) {
      // A "extern" storage class declares a global with external linkage, even
      // in a compound statement.
      if (storage == STORAGE_CLASS_EXTERN) {
        sym = make_symbol_global(decl, declspec, strndup(decl->pos, decl->len));
      } else if (storage != STORAGE_CLASS_TYPEDEF) {
        sym = make_symbol_local(decl, declspec);
      }
    } else {
      sym = make_symbol_global(decl, declspec, strndup(decl->pos, decl->len));
    }
  }

  if (storage == STORAGE_CLASS_EXTERN) {
    sym->linkage = LINKAGE_EXTERNAL;
  }

  if (equal(cursor, TOKEN_EQUAL)) {
    if (storage == STORAGE_CLASS_TYPEDEF) {
      error_at(decl, "illegal initializer for typedef");
    }

    if (in_compound_statement) {
      // A decl in a compound statement with an initializer is evaluated at the
      // point of the decl and assigned to the decl symbol.
      if (sym->linkage != LINKAGE_NONE) {
        error_at(decl,
                 "declaration of block scope identifier with linkage can't "
                 "have an initializer");
      }

      node* initializer = assignment_expr(cursor);
      node* init_stmt = make_expr_stmt(
        decl, make_node_assign(decl, make_symbol_ref(decl, sym), initializer));
      initializers = initializers->next = init_stmt;
    } else {
      // Global decls can only be initialized by constants, which will be
      // handled by the code generator.
      error_at(*cursor, "nyi: global decl initializers");
    }
  }

  if (storage == STORAGE_CLASS_TYPEDEF) {
    type_symbol* sym = make_type_symbol(decl, make_typedef(decl, declspec));
    define_type(sym);
  }

  if (peek(cursor, TOKEN_LBRACE)) {
    if (in_compound_statement) {
      error_at(decl, "function definition not allowed here");
    }

    // This is a function definition.
    if (storage == STORAGE_CLASS_TYPEDEF) {
      error_at(decl, "function definition declared `typedef`");
    }

    current_function = sym;
    define(sym);
    current_function->linkage = LINKAGE_INTERNAL;

    // Declare locals for every parameter of this function and initialize
    // them with their corresponding function argument.
    int arg_count = 0;
    node parameter_inits = { 0 };
    node* parameter_cursor = &parameter_inits;
    for (parameter* p = current_function->ty->u.function.params; p;
         p = p->next) {
      symbol* arg = make_symbol_local(p->name, p->ty);
      define(arg);
      parameter_cursor = parameter_cursor->next = make_expr_stmt(
        p->name,
        make_node_assign(p->name,
                         make_symbol_ref(p->name, arg),
                         make_node_arg(p->name, p->ty, arg_count++)));
    }

    node* body = compound_stmt(cursor);
    parameter_cursor->next = body;
    current_function->u.function.body = parameter_inits.next;
    current_function->next = symbols;
    symbols = current_function;
    return initializer_head.next;
  } else if (equal(cursor, TOKEN_SEMICOLON)) {
    // just a decl.
    if (!decl) {
      // Anonymous (forward) declarations don't declare anything unless their
      // decls themselves declare something in the type namespace (e.g. structs)
      if (declspec->kind != TYPE_STRUCT && declspec->kind != TYPE_UNION &&
          declspec->kind != TYPE_ENUM) {
        warn_at(start_tok, "declaration does not declare anything");
      }

      return NULL;
    }

    if (storage == STORAGE_CLASS_TYPEDEF) {
      // Typedefs don't define any actual symbols.
      return NULL;
    }

    define(sym);
    if (sym->kind != SYMBOL_LOCAL_VAR) {
      sym->next = symbols;
      symbols = sym;
    }
    return initializer_head.next;
  }

  return NULL;
}

symbol*
parse(token** cursor)
{
  global_scope = malloc(sizeof(scope));
  memset(global_scope, 0, sizeof(scope));
  current_scope = global_scope;
  while (!equal(cursor, TOKEN_EOF)) {
    external_declaration(cursor, 0);
  }

  return symbols;
}
