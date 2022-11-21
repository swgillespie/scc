#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char* source;
static size_t source_len;

void
load_file(const char* filename)
{
  FILE* f = fopen(filename, "r");
  if (!f) {
    perror("failed to open source file");
    exit(1);
  }

  FILE* stream = open_memstream(&source, &source_len);
  for (;;) {
    char read_buf[4096];
    size_t nbytes = fread(read_buf, 1, sizeof(read_buf), f);
    if (nbytes == 0) {
      break;
    }

    fwrite(read_buf, 1, nbytes, stream);
  }

  fclose(f);
  fflush(stream);

  // Ensure the last line is terminated with a newline, even if it is not.
  if (source_len == 0 || source[source_len - 1] != '\n') {
    fputc('\n', stream);
  }
  fputc('\0', stream);
  fclose(stream);
}

typedef enum
{
  // Probably not real tokens, these are identifiers?
  TOKEN_INT,
  TOKEN_MAIN,
  // These are real tokens.
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

token*
make_token(token_kind kind, char* pos, size_t len)
{
  token* t = malloc(sizeof(token));
  memset(t, 0, sizeof(token));
  t->kind = kind;
  t->pos = pos;
  t->len = len;
  return t;
}

token*
tokenize(void)
{
  token head = { 0 };
  token* cursor = &head;
  char* c = source;
  while (*c != '\0') {
    while (isspace(*c)) {
      c++;
    }

    if (*c == '\0') {
      break;
    }

    switch (*c) {
      case '(':
        cursor->next = make_token(TOKEN_LPAREN, c, 1);
        c++;
        break;
      case ')':
        cursor->next = make_token(TOKEN_RPAREN, c, 1);
        c++;
        break;
      case '{':
        cursor->next = make_token(TOKEN_LBRACE, c, 1);
        c++;
        break;
      case '}':
        cursor->next = make_token(TOKEN_RBRACE, c, 1);
        c++;
        break;
      case ';':
        cursor->next = make_token(TOKEN_SEMICOLON, c, 1);
        c++;
        break;
      case '+':
        cursor->next = make_token(TOKEN_PLUS, c, 1);
        c++;
        break;
      case '-':
        cursor->next = make_token(TOKEN_MINUS, c, 1);
        c++;
        break;
      case '*':
        cursor->next = make_token(TOKEN_STAR, c, 1);
        c++;
        break;
      default:
        if (isalpha(*c)) {
          size_t len = 0;
          while (isalpha(*c)) {
            c++;
            len++;
          }

          if (strncmp(c - len, "int", 3) == 0) {
            cursor->next = make_token(TOKEN_INT, c - len, len);
          } else if (strncmp(c - len, "main", 4) == 0) {
            cursor->next = make_token(TOKEN_MAIN, c - len, len);
          } else if (strncmp(c - len, "return", 6) == 0) {
            cursor->next = make_token(TOKEN_RETURN, c - len, len);
          }
        } else if (isdigit(*c)) {
          size_t len = 0;
          int value = 0;
          while (isdigit(*c)) {
            value = value * 10 + *c - '0';
            c++;
            len++;
          }

          cursor->next = make_token(TOKEN_INTEGER, c - len, len);
          cursor->next->value = value;
        }

        break;
    }

    cursor = cursor->next;
  }

  cursor->next = make_token(TOKEN_EOF, c, 0);
  return head.next;
}

token*
eat(token** cursor, token_kind kind)
{
  token* tok = *cursor;
  if (tok->kind != kind) {
    abort();
  }

  *cursor = tok->next;
  return tok;
}

int
equal(token** cursor, token_kind kind)
{
  token* tok = *cursor;
  if (tok->kind != kind) {
    return 0;
  }

  eat(cursor, kind);
  return 1;
}

typedef enum node_kind
{
  NODE_BINOP,
  NODE_CONST,
} node_kind;

typedef enum binop
{
  BINOP_ADD,
  BINOP_SUB,
  BINOP_MUL,
} binop;

typedef struct node
{
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
  } u;
} node;

node*
make_node_const(int value)
{
  node* n = malloc(sizeof(node));
  n->kind = NODE_CONST;
  n->u.const_value = value;
  return n;
}

node*
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
primary(token**);

static node*
mul_expr(token**);

/**
 * expression = mul_expr ("+" mul_expr | "-" mul_expr)*
 */
node*
expr(token** cursor)
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
 * mul_expr = primary ("*" primary)*
 */
node*
mul_expr(token** cursor)
{
  node* base = primary(cursor);
  for (;;) {
    if (equal(cursor, TOKEN_STAR)) {
      base = make_node_binary(BINOP_MUL, base, primary(cursor));
      continue;
    }

    return base;
  }
}

/**
 * primary = integer
 */
node*
primary(token** cursor)
{
  token* integer = eat(cursor, TOKEN_INTEGER);
  return make_node_const(integer->value);
}

/**
 * Code generation
 */

void
push(void)
{
  printf("  push %%rax\n");
}

void
pop(const char* reg)
{
  printf("  pop %%%s\n", reg);
}

void
codegen_expr(node* n)
{
  if (n->kind == NODE_CONST) {
    printf("  mov $%d, %%rax\n", n->u.const_value);
    push();
    return;
  }

  if (n->kind == NODE_BINOP) {
    codegen_expr(n->u.binop.left);
    codegen_expr(n->u.binop.right);
    pop("rdi");
    pop("rax");
    switch (n->u.binop.op) {
      case BINOP_ADD:
        printf("  add %%rdi, %%rax\n");
        break;
      case BINOP_SUB:
        printf("  sub %%rdi, %%rax\n");
        break;
      case BINOP_MUL:
        printf("  imul %%rdi, %%rax\n");
        break;
    }
    push();
  }
}

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  load_file(argv[1]);
  token* tok = tokenize();
  token** cursor = &tok;
  eat(cursor, TOKEN_INT);
  eat(cursor, TOKEN_MAIN);
  eat(cursor, TOKEN_LPAREN);
  eat(cursor, TOKEN_RPAREN);
  eat(cursor, TOKEN_LBRACE);
  eat(cursor, TOKEN_RETURN);
  node* n = expr(cursor);
  eat(cursor, TOKEN_SEMICOLON);
  eat(cursor, TOKEN_RBRACE);
  eat(cursor, TOKEN_EOF);

  FILE* out = stdout;

  fprintf(out, ".globl main\n");
  fprintf(out, "main:\n");
  codegen_expr(n);
  pop("rax");
  fprintf(out, "  ret\n");
  return 0;
}