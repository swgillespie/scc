#include "scc.h"

/**
 * The current function being parsed, if any.
 */
static symbol* current_function;

/**
 * All of the symbols that will need to be emitted later.
 */
static symbol* symbols;

/**
 * Lexical scope management
 */
typedef struct scope
{
  struct scope* next;
  struct symbol* symbols;
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
  n->u.and_.left = left;
  n->u.and_.right = right;
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
make_node_assign(token* tok, node* lvalue, node* rvalue)
{
  node* n = malloc(sizeof(node));
  memset(n, 0, sizeof(node));
  n->kind = NODE_ASSIGN;
  n->tok = tok;
  n->ty = rvalue->ty;
  n->u.assign.lvalue = lvalue;
  n->u.assign.rvalue = rvalue;
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
  if (!in_loop()) {
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
logical_and_expr(token**);

static type*
declaration_specifiers(token**);

static token*
declarator(token**, type**);

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
  type* decltype = declaration_specifiers(cursor);
  while (equal(cursor, TOKEN_STAR)) {
    decltype = make_pointer_type(decltype);
  }

  return decltype;
}

static int
can_start_type_name(token** cursor)
{
  return peek(cursor, TOKEN_CHAR) || peek(cursor, TOKEN_INT) ||
         peek(cursor, TOKEN_BOOL) || peek(cursor, TOKEN_VOID);
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

  if (peek(cursor, TOKEN_BREAK)) {
    token* break_tok = eat(cursor, TOKEN_BREAK);
    eat(cursor, TOKEN_SEMICOLON);
    return make_break_stmt(break_tok);
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
 *  declaration
 *    : declaration_specifiers declarator ("=" initializer) ";"
 *
 *  declaration_specifiers
 *    : int
 *
 *  direct_declarator
 *    : IDENTIFIER ( "[" assignment_expr? "]")*
 */
static node*
declaration(token** cursor)
{
  type* decltype = decl_type_name(cursor);
  token* ident = eat(cursor, TOKEN_IDENT);
  while (equal(cursor, TOKEN_LBRACKET)) {
    // C is ridiculously permissive with what it accepts as an array length
    // initializer. We'll start with known constants.
    node* array_length = assignment_expr(cursor);
    if (array_length->kind != NODE_CONST) {
      error_at(array_length->tok,
               "non-literal nonsense not supported in arrays yet");
    }

    decltype = make_array_type(decltype, array_length->u.const_value);
    eat(cursor, TOKEN_RBRACKET);
  }

  token* eq_tok = *cursor;
  if (equal(cursor, TOKEN_EQUAL)) {
    node* initializer = expr(cursor);
    token* semi_tok = eat(cursor, TOKEN_SEMICOLON);
    symbol* s = make_symbol_local(ident, decltype);
    define(s);
    return make_expr_stmt(
      semi_tok,
      make_node_assign(eq_tok, make_symbol_ref(ident, s), initializer));
  }

  /**
   * Declarations are not really statements; without an initializer, they don't
   * result in any codegen.
   */
  eat(cursor, TOKEN_SEMICOLON);
  define(make_symbol_local(ident, decltype));
  return make_nop();
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
    stmts = stmts->next = stmt(cursor);
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
  node* base = logical_and_expr(cursor);
  for (;;) {
    token* eq_tok = *cursor;
    if (equal(cursor, TOKEN_EQUAL)) {
      base = make_node_assign(eq_tok, base, assignment_expr(cursor));
      continue;
    }

    return base;
  }
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

/**
 * mul_expr = primary ("*" primary | "%" primary | "/" primary)*
 */
static node*
mul_expr(token** cursor)
{
  node* base = unary_expr(cursor);
  for (;;) {
    token* op_tok = *cursor;
    if (equal(cursor, TOKEN_STAR)) {
      base = make_node_binary(op_tok, BINOP_MUL, base, postfix_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_PERCENT)) {
      base = make_node_binary(op_tok, BINOP_MOD, base, postfix_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_SLASH)) {
      base = make_node_binary(op_tok, BINOP_DIV, base, postfix_expr(cursor));
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
  return n->kind == NODE_SYMBOL_REF || n->kind == NODE_DEREF;
}

/*
 * unary_expression
 *   : postfix_expression
 *   | ("*" | "&") unary_expression
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
     * Form 1 doesn't require parens, form 2 does. We only handle form 1 for
     * now.
     */
    node* expr = unary_expr(cursor);
    return make_node_const(unop_tok, ty_int, expr->ty->size);
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

      // rough desugar into "base = base + 1", where lhs base is evaluated
      // in an lvalue context and rhs base in a rvalue context
      return make_node_assign(
        candidate,
        base,
        make_add_or_sub(
          BINOP_ADD, candidate, base, make_node_const(candidate, ty_int, 1)));
    }

    if (equal(cursor, TOKEN_MINUS_MINUS)) {
      // Post-decrement is only legal on lvalues
      if (!is_lvalue(base)) {
        error_at(*cursor, "lvalue required as decrement operand");
      }

      // rough desugar into "base = base - 1", where lhs base is evaluated
      // in an lvalue context and rhs base in a rvalue context
      return make_node_assign(
        candidate,
        base,
        make_add_or_sub(
          BINOP_SUB, candidate, base, make_node_const(candidate, ty_int, 1)));
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
        /* it's clowny as hell that this is only a warning, that's C for you */
        name = strndup(base->tok->pos, base->tok->len);
        warn_at(base->tok, "implicit declaration of function `%s`", name);
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
parameter_list(token** cursor)
{
  parameter head = { 0 };
  parameter* params = &head;
  while (!equal(cursor, TOKEN_RPAREN)) {
    type* declspec = declaration_specifiers(cursor);
    token* param_name = declarator(cursor, &declspec);
    params = params->next = make_parameter(param_name, declspec);
    if (!peek(cursor, TOKEN_RPAREN)) {
      eat(cursor, TOKEN_COMMA);
    }
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

  token* ident = eat(cursor, TOKEN_IDENT);
  for (;;) {
    if (equal(cursor, TOKEN_LPAREN)) {
      if ((*base)->kind == TYPE_FUNCTION) {
        error_at(*cursor,
                 "declaration declares a function that returns a function");
      }
      parameter* params = parameter_list(cursor);
      *base = make_function_type(*base, params);
      continue;
    }

    break;
  }

  return ident;
}

/*
 * declaration-specifiers ::=
 *	storage-class-specifier declaration-specifiers?
 *	type-specifier declaration-specifiers?
 *	type-qualifier declaration-specifiers?
 *	function-specifier declaration-specifiers?
 *
 * storage-class-specifier ::= epsilon
 * type-specifier ::=
 *   "int"
 *   "char"
 *   "_Bool"
 * type-qualifier ::= epsilon
 * function-specifier ::= epsilon
 */
static type*
declaration_specifiers(token** cursor)
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

    if (equal(cursor, TOKEN_VOID)) {
      if (type_spec)
        error_at(*cursor, "two or more data types in declaration specifier");
      type_spec = ty_void;
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
static void
external_declaration(token** cursor)
{
  type* declspec = declaration_specifiers(cursor);
  token* decl = declarator(cursor, &declspec);
  if (equal(cursor, TOKEN_EQUAL)) {
    error_at(*cursor, "nyi: global decl initializers");
  }

  if (peek(cursor, TOKEN_LBRACE)) {
    // This is a function definition.
    current_function = make_symbol_function(decl, declspec);
    define(current_function);
    current_function->linkage = LINKAGE_INTERNAL;

    // Declare locals for every parameter of this function and initialize them
    // with their corresponding function argument.
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
  } else if (equal(cursor, TOKEN_SEMICOLON)) {
    // just a decl.
    symbol* sym;
    if (declspec->kind == TYPE_FUNCTION) {
      sym = make_symbol_function(decl, declspec);
    } else {
      sym = make_symbol_global(decl, declspec, strndup(decl->pos, decl->len));
    }
    define(sym);
    sym->next = symbols;
    symbols = sym;
  }
}

symbol*
parse(token** cursor)
{
  global_scope = malloc(sizeof(scope));
  memset(global_scope, 0, sizeof(scope));
  current_scope = global_scope;
  while (!equal(cursor, TOKEN_EOF)) {
    external_declaration(cursor);
  }

  return symbols;
}
