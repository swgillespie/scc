#ifndef __SCC_H__
#define __SCC_H__

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * tokenize.c - C source tokenizer
 */

typedef enum
{
  // Probably not real tokens, these are identifiers?
  TOKEN_INT,
  TOKEN_MAIN,
  // These are real tokens.
  TOKEN_IDENT,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_RETURN,
  TOKEN_INTEGER,
  TOKEN_SEMICOLON,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_STAR,
  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_EQUAL,
  TOKEN_DOUBLE_EQ,
  TOKEN_NOT_EQ,
  TOKEN_LT,
  TOKEN_LT_EQ,
  TOKEN_EOF,
} token_kind;

typedef struct token
{
  struct token* next;
  token_kind kind;
  char* pos;
  size_t len;
  int value;
} token;

void
load_file(const char* filename);

token*
tokenize(void);

/**
 * parse.c - Parsing C source into node trees
 */

typedef enum node_kind
{
  NODE_BINOP,
  NODE_CONST,
  NODE_RETURN,
  NODE_NOP,
} node_kind;

typedef enum binop
{
  BINOP_ADD,
  BINOP_SUB,
  BINOP_MUL,
  BINOP_DIV,
  BINOP_MOD,
  BINOP_EQUAL,
  BINOP_NOT_EQUAL,
  BINOP_LT,
  BINOP_LT_EQUAL,
} binop;

typedef struct node
{
  struct node* next;
  node_kind kind;
  union
  {
    struct
    {
      binop op;
      struct node* left;
      struct node* right;
    } binop;
    int const_value;
    struct node* return_value;
  } u;
} node;

typedef struct symbol
{
  struct symbol* next;
  token* name;
} symbol;

typedef struct scope
{
  struct scope* next;
  symbol* symbols;
} scope;

node*
parse(token** cursor);

/**
 * codegen.c - Turn node trees into code
 */

void
codegen(node* n);

/**
 * Miscellaneous utility routines
 */
void
error_at(token* tok, const char* fmt, ...);

#endif /* __SCC_H__ */
