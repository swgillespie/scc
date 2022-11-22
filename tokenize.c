#include "scc.h"

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
      case '/':
        cursor->next = make_token(TOKEN_SLASH, c, 1);
        c++;
        break;
      case '%':
        cursor->next = make_token(TOKEN_PERCENT, c, 1);
        c++;
        break;
      case '=':
        if (*++c == '=') {
          cursor->next = make_token(TOKEN_DOUBLE_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_EQUAL, c, 1);
        }

        break;
      case '!':
        if (*++c == '=') {
          cursor->next = make_token(TOKEN_NOT_EQ, c - 1, 2);
          c++;
        } else {
          error_at(cursor, "expected `=`");
        }

        break;
      case '<':
        if (*++c == '=') {
          cursor->next = make_token(TOKEN_LT_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_LT, c, 1);
        }

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
          } else {
            cursor->next = make_token(TOKEN_IDENT, c - len, len);
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
