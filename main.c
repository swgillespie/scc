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

  fseek(f, 0, SEEK_END);
  source_len = (size_t)ftell(f);
  fseek(f, 0, SEEK_SET);
  source = malloc(source_len);
  if (fread(source, sizeof(char), source_len, f) != source_len) {
    perror("fread");
    exit(1);
  }
}

// the minimum set of tokens to recognize "int main() { return <number>; }"

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
          int value;
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
    // error?
  }

  *cursor = tok->next;
  return tok;
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
  token* intval = eat(cursor, TOKEN_INTEGER);
  eat(cursor, TOKEN_SEMICOLON);
  eat(cursor, TOKEN_RBRACE);
  eat(cursor, TOKEN_EOF);

  FILE* out = stdout;

  fprintf(out, ".globl main\n");
  fprintf(out, "main:\n");
  fprintf(out, "  mov $%d, %%rax\n", intval->value);
  fprintf(out, "  ret\n");
  return 0;
}