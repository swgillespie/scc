#include "scc.h"

scope* scopes;

static scope*
make_scope()
{
  scope* s = malloc(sizeof(scope));
  return s;
}

static symbol*
make_symbol(token* name)
{
  symbol* s = malloc(sizeof(symbol));
  s->name = name;
  s->next = scopes->symbols;
  scopes->symbols = s;
  return s;
}

static node*
make_node_const(int value)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_CONST;
  n->u.const_value = value;
  return n;
}

static node*
make_node_binary(binop op, node* left, node* right)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_BINOP;
  n->u.binop.op = op;
  n->u.binop.left = left;
  n->u.binop.right = right;
  return n;
}

static node*
make_return(node* val)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_RETURN;
  n->u.return_value = val;
  return n;
}

/*
static node*
make_nop()
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_NOP;
  return n;
}
*/
static node*
make_node_assign(node* lvalue, node* rvalue)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_ASSIGN;
  n->u.assign.lvalue = lvalue;
  n->u.assign.rvalue = rvalue;
  return n;
}

static node*
make_symbol_ref(symbol* sym)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_SYMBOL_REF;
  n->u.symbol_ref = sym;
  return n;
}

static node*
make_expr_stmt(node* value)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_EXPR_STMT;
  n->u.expr_stmt_value = value;
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
expr(token**);

static node*
assignment_expr(token**);

static node*
declaration(token**);

/**
 * stmt = return_stmt | declaration | expr SEMI
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

  node* e = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  return make_expr_stmt(e);
}

/**
 * return_stmt = "return" expr ";"
 */
static node*
return_stmt(token** cursor)
{
  eat(cursor, TOKEN_RETURN);
  node* val = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  return make_return(val);
}

/**
 *  declaration
 *    : declaration_specifiers declarator ("=" initializer)? ";"
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
  token* ident = eat(cursor, TOKEN_IDENT);
  eat(cursor, TOKEN_EQUAL);
  node* initializer = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  symbol* s = make_symbol(ident);
  return make_expr_stmt(make_node_assign(make_symbol_ref(s), initializer));
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
    if (equal(cursor, TOKEN_EQUAL)) {
      base = make_node_assign(base, assignment_expr(cursor));
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
    if (equal(cursor, TOKEN_DOUBLE_EQ)) {
      base = make_node_binary(BINOP_EQUAL, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_NOT_EQ)) {
      base = make_node_binary(BINOP_NOT_EQUAL, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_LT)) {
      base = make_node_binary(BINOP_LT, base, add_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_LT_EQ)) {
      base = make_node_binary(BINOP_LT_EQUAL, base, add_expr(cursor));
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
    if (equal(cursor, TOKEN_PLUS)) {
      base = make_node_binary(BINOP_ADD, base, mul_expr(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_MINUS)) {
      base = make_node_binary(BINOP_SUB, base, mul_expr(cursor));
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
  node* base = primary(cursor);
  for (;;) {
    if (equal(cursor, TOKEN_STAR)) {
      base = make_node_binary(BINOP_MUL, base, primary(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_PERCENT)) {
      base = make_node_binary(BINOP_MOD, base, primary(cursor));
      continue;
    }

    if (equal(cursor, TOKEN_SLASH)) {
      base = make_node_binary(BINOP_DIV, base, primary(cursor));
      continue;
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
        return make_symbol_ref(sym);
      }
    }

    error_at(name, "unknown identifier");
  }

  token* integer = eat(cursor, TOKEN_INTEGER);
  return make_node_const(integer->value);
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