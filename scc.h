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
  TOKEN_ERROR,
  // Probably not real tokens, these are identifiers?
  TOKEN_INT,
  TOKEN_CHAR,
  TOKEN_MAIN,
  // Reserved words
  TOKEN_DO,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_FOR,
  TOKEN_WHILE,
  TOKEN_SIZEOF,
  TOKEN_BOOL,
  TOKEN_BREAK,
  TOKEN_CONTINUE,
  TOKEN_VOID,
  TOKEN_EXTERN,
  TOKEN_TYPEDEF,
  // These are real tokens.
  TOKEN_IDENT,
  TOKEN_CHAR_LITERAL,
  TOKEN_STRING_LITERAL,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
  TOKEN_RETURN,
  TOKEN_INTEGER,
  TOKEN_SEMICOLON,
  TOKEN_PLUS,
  TOKEN_PLUS_PLUS,
  TOKEN_MINUS,
  TOKEN_MINUS_MINUS,
  TOKEN_STAR,
  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_EQUAL,
  TOKEN_DOUBLE_EQ,
  TOKEN_NOT_EQ,
  TOKEN_LT,
  TOKEN_LT_EQ,
  TOKEN_GT,
  TOKEN_GT_EQ,
  TOKEN_AMPERSAND,
  TOKEN_DOUBLE_AMPERSAND,
  TOKEN_COMMA,
  TOKEN_ELLIPSIS,
  TOKEN_EOF,
} token_kind;

typedef struct token
{
  struct token* next;
  token_kind kind;
  char* pos;
  size_t len;
  int value;
  char* string_value;
} token;

void
load_file(const char* filename);

token*
tokenize(void);

/*
 * type.c - The representation of C data types
 */

typedef enum type_kind
{
  TYPE_INT,
  TYPE_CHAR,
  TYPE_BOOL,
  TYPE_POINTER,
  TYPE_VOID,
  TYPE_FUNCTION,
  TYPE_ARRAY,
} type_kind;

typedef struct type
{
  type_kind kind;
  /**
   * The "base type" of this type - for arrays and pointers, it's the content
   * type of the array and dereferenced pointer type respectively.
   */
  struct type* base;

  /**
   * The size of this type, e.g. what sizeof returns.
   */
  int size;

  /**
   * If this type is a typedef, the aliased type.
   */
  struct type* aka;

  /**
   * If this type is a typedef, the name of the typedef identifier.
   */
  struct token* aka_name;

  union
  {
    /**
     * If TYPE_ARRAY, the length of the array.
     */
    int array_length;
    struct
    {
      /**
       * If TYPE_FUNCTION, the return type of the function.
       */
      struct type* ret;
      /**
       * If TYPE_FUNCTION, the parameter types of this function.
       */
      struct parameter* params;
      /**
       * If TYPE_FUNCTION, whether this is a variadic function.
       */
      int is_vararg;
    } function;
  } u;
} type;

extern type* ty_int;
extern type* ty_void;
extern type* ty_char;
extern type* ty_bool;

typedef struct parameter
{
  struct parameter* next;
  token* name;
  type* ty;
} parameter;

type*
make_pointer_type(type* base);

type*
make_array_type(type* base, int len);

type*
make_function_type(type* ret, parameter* params);

type*
make_typedef(token* aka_name, type* aka);

char*
type_name(type* ty);

/**
 * Compares two types and returns a number based on the comparison of the
 * "integer conversion rank" of the two types. Returns -1 if left < right,
 * 0 if left == 0, and 1 if left > right.
 *
 * Implements the algorithm in 6.3.1.1 of the spec for ranking integer
 * conversions.
 */
int
integer_conversion_rank_compare(type* left, type* right);

int
is_arithmetic_type(type* ty);

/**
 * parse.c - Parsing C source into node trees
 */

typedef enum node_kind
{
  NODE_BINOP,
  NODE_CONST,
  NODE_ASSIGN,
  NODE_NOP,
  NODE_SYMBOL_REF,
  NODE_ADDROF,
  NODE_DEREF,
  NODE_CALL,
  NODE_AND,
  NODE_CONV,
  NODE_BREAK,
  NODE_CONTINUE,
  NODE_ARG,
  NODE_POSTINCREMENT,
  NODE_POSTDECREMENT,
  /* Control flow */
  NODE_IF,
  /* TODO: it's probably possible to unify the two loop nodes */
  NODE_FOR,
  NODE_DO,
  /* Statements */
  NODE_RETURN,
  NODE_EXPR_STMT,
  NODE_COMPOUND_STMT,
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
  BINOP_GT,
  BINOP_GT_EQUAL,
} binop;

typedef struct node
{
  struct node* next;
  node_kind kind;
  type* ty;
  /* A representative token of this node, for error messages. */
  token* tok;
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
    struct
    {
      struct node* lvalue;
      struct node* rvalue;
    } assign;
    struct symbol* symbol_ref;
    struct node* expr_stmt_value;
    struct node* compound_stmts;
    struct
    {
      struct node* cond;
      struct node* then;
      struct node* else_;
    } if_;
    struct
    {
      /* e.g. for (i = 0; i < 5; i++) */
      struct node* initializer; /* i = 0 */
      struct node* cond;        /* i < 5 */
      struct node* next;        /* i++ */
      struct node* body;
    } for_;
    struct node* addrof_value;
    struct node* deref_value;
    struct
    {
      char* name;
      struct node* args;
    } call;
    struct
    {
      struct node* body;
      struct node* cond;
    } do_;
    struct
    {
      struct node* left;
      struct node* right;
    } and_;
    struct
    {
      struct node* value;
    } conv;
    struct
    {
      int count;
    } arg;
    struct
    {
      struct node* arg;
    } postincrement;
    struct
    {
      struct node* arg;
    } postdecrement;
  } u;
} node;

typedef enum linkage
{
  LINKAGE_NONE,
  LINKAGE_EXTERNAL,
  LINKAGE_INTERNAL,
} linkage;

typedef enum storage_class
{
  STORAGE_CLASS_NONE,
  STORAGE_CLASS_EXTERN,
  STORAGE_CLASS_TYPEDEF,
} storage_class;

typedef enum symbol_kind
{
  SYMBOL_EMPTY,
  SYMBOL_LOCAL_VAR,
  SYMBOL_GLOBAL_VAR,
  SYMBOL_FUNCTION,
} symbol_kind;

/**
 * A symbol, representing an object that has a location in memory.
 */
typedef struct symbol
{
  struct symbol* next;
  /**
   * The next symbol in this declaration scope, if this symbol can be referred
   * to by name.
   */
  struct symbol* next_in_scope;
  symbol_kind kind;
  char* name;
  token* tok;
  type* ty;
  linkage linkage;
  union
  {
    /**
     * For SYMBOL_LOCAL_VAR, the offset from the frame pointer allocated to
     * this variable.
     */
    int frame_offset;
    /**
     * For SYMBOL_FUNCTION, the list of locals for this function.
     */
    struct
    {
      struct symbol* locals;
      struct node* body;
    } function;

    /**
     * For SYMBOL_GLOBAL_VAR, the data of the global.
     */
    char* global_data;
  } u;
} symbol;

symbol*
parse(token** cursor);

/**
 * codegen.c - Turn node trees into code
 */

void
codegen(symbol* sym);

/*
 * builtins.c - Management for builtin functions
 */

typedef struct builtin_function
{
  /**
   * The name of this builtin.
   */
  char* name;

  /**
   * The type of this builtin.
   */
  type* (*ty)(void);

  /**
   * Generates code for this builtin given a list of arguments.
   */
  void (*codegen)(node*);
} builtin_function;

builtin_function*
builtin_lookup(char* name);

/**
 * Miscellaneous utility routines
 */
__attribute__((noreturn)) void
error_at(token* tok, const char* fmt, ...);

void
warn_at(token* tok, const char* fmt, ...);

__attribute__((noreturn)) void
ice_at(token* tok, const char* fmt, ...);

#define SCC_ASSERT(tok, cond, fmt, ...)                                        \
  do {                                                                         \
    if (!(cond)) {                                                             \
      ice_at((tok), fmt, ##__VA_ARGS__);                                       \
    }                                                                          \
  } while (0)

#endif /* __SCC_H__ */
