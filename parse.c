#include "scc.h"

scope* scopes;

static scope*
make_scope()
{
  scope* s = malloc(sizeof(scope));
  return s;
}

static symbol*
make_symbol(token* name, type* ty)
{
  symbol* s = malloc(sizeof(symbol));
  s->name = name;
  s->next = scopes->symbols;
  s->ty = ty;
  scopes->symbols = s;
  return s;
}

static node*
make_node_const(token* tok, type* ty, int value)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_CONST;
  n->ty = ty;
  n->tok = tok;
  n->u.const_value = value;
  return n;
}

static node*
make_node_binary(token* tok, binop op, node* left, node* right)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_BINOP;
  n->tok = tok;
  // TODO - there are complex promotion rules that inform what the type of
  // this expression will be
  n->ty = left->ty;
  n->u.binop.op = op;
  n->u.binop.left = left;
  n->u.binop.right = right;
  return n;
}

static node*
make_return(token* tok, node* val)
{
  node* n = malloc(sizeof(node));
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
  n->kind = NODE_NOP;
  return n;
}

static node*
make_node_assign(token* tok, node* lvalue, node* rvalue)
{
  node* n = malloc(sizeof(node));
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
  n->kind = NODE_SYMBOL_REF;
  n->tok = tok;
  n->ty = sym->ty;
  n->u.symbol_ref = sym;
  return n;
}

static node*
make_expr_stmt(token* tok, node* value)
{
  node* n = malloc(sizeof(node));
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
make_deref(token* tok, node* base)
{
  node* n = malloc(sizeof(node));
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
  n->kind = NODE_ADDROF;
  n->tok = tok;
  n->ty = make_pointer_type(base->ty);
  n->u.addrof_value = base;
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
unary_expr(token**);

/**
 * stmt = return_stmt | declaration | expr SEMI | compound_statement |
 * if_statement | for_statement | while_statement
 */
static node*
stmt(token** cursor)
{
  if (peek(cursor, TOKEN_RETURN)) {
    return return_stmt(cursor);
  }

  if (peek(cursor, TOKEN_INT)) {
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
 *  declarator
 *    : IDENTIFIER
 */
static node*
declaration(token** cursor)
{
  eat(cursor, TOKEN_INT);
  type* decltype = ty_int;
  while (equal(cursor, TOKEN_STAR)) {
    decltype = make_pointer_type(decltype);
  }
  token* ident = eat(cursor, TOKEN_IDENT);
  token* eq_tok = *cursor;
  if (equal(cursor, TOKEN_EQUAL)) {
    node* initializer = expr(cursor);
    token* semi_tok = eat(cursor, TOKEN_SEMICOLON);
    symbol* s = make_symbol(ident, decltype);
    return make_expr_stmt(
      semi_tok,
      make_node_assign(eq_tok, make_symbol_ref(ident, s), initializer));
  }

  /**
   * Declarations are not really statements; without an initializer, they don't
   * result in any codegen.
   */
  eat(cursor, TOKEN_SEMICOLON);
  make_symbol(ident, decltype);
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
  while (!peek(cursor, TOKEN_RBRACE)) {
    stmts = stmts->next = stmt(cursor);
  }

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
  if (peek(cursor, TOKEN_INT)) {
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
  node* body = stmt(cursor);
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
  node* body = stmt(cursor);
  return make_for_stmt(while_tok, NULL, cond, NULL, body);
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
  node* base = relational_expr(cursor);
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
      base = make_node_binary(op_tok, BINOP_ADD, base, mul_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_MINUS)) {
      base = make_node_binary(op_tok, BINOP_SUB, base, mul_expr(cursor));
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
      error_at(unop_tok, "not an lvalue");
    }
    return make_addrof(unop_tok, base);
  }

  return postfix_expr(cursor);
}

/**
 * postfix_expr = primary ("++")*
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
      return base = make_node_assign(
               candidate,
               base,
               make_node_binary(candidate,
                                BINOP_ADD,
                                base,
                                make_node_const(candidate, ty_int, 1)));
    }

    return base;
  }
}

/**
 * primary = integer | "(" expression ")" | identifier
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
    for (symbol* sym = scopes->symbols; sym; sym = sym->next) {
      if (strncmp(name->pos, sym->name->pos, MIN(name->len, sym->name->len)) ==
          0) {
        return make_symbol_ref(name, sym);
      }
    }

    error_at(name, "unknown identifier");
  }

  token* integer = eat(cursor, TOKEN_INTEGER);
  return make_node_const(integer, ty_int, integer->value);
}

node*
parse(token** cursor)
{
  scopes = make_scope();
  eat(cursor, TOKEN_INT);
  eat(cursor, TOKEN_MAIN);
  eat(cursor, TOKEN_LPAREN);
  eat(cursor, TOKEN_RPAREN);
  eat(cursor, TOKEN_LBRACE);
  node head = { 0 };
  node* n = &head;
  while ((*cursor)->kind != TOKEN_RBRACE) {
    n = n->next = stmt(cursor);
  }
  eat(cursor, TOKEN_RBRACE);
  eat(cursor, TOKEN_EOF);
  return head.next;
}

char*
symbol_name(symbol* s)
{
  char* buf = calloc(s->name->len + 1, 1);
  strncpy(buf, s->name->pos, s->name->len);
  buf[s->name->len] = '\0';
  return buf;
}