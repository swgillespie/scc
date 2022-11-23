#include "scc.h"

static char* source_name;
static char* source;
static size_t source_len;

typedef struct keyword
{
  const char* str;
  token_kind kind;
} keyword;

static keyword keywords[] = {
  { "int", TOKEN_INT },     { "main", TOKEN_MAIN }, { "return", TOKEN_RETURN },
  { "if", TOKEN_IF },       { "else", TOKEN_ELSE }, { "for", TOKEN_FOR },
  { "while", TOKEN_WHILE },
};

void
load_file(const char* filename)
{
  FILE* f = fopen(filename, "r");
  if (!f) {
    perror("failed to open source file");
    exit(1);
  }

  source_name = strdup(filename);
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
        if (*++c == '+') {
          cursor->next = make_token(TOKEN_PLUS_PLUS, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_PLUS, c, 1);
        }

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
      case '&':
        cursor->next = make_token(TOKEN_AMPERSAND, c, 1);
        c++;
        break;
      default:
        if (isalpha(*c)) {
          size_t len = 0;
          while (isalnum(*c) || *c == '_') {
            c++;
            len++;
          }

          int was_keyword = 0;
          for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
            if (strncmp(c - len, keywords[i].str, strlen(keywords[i].str)) ==
                0) {
              cursor->next = make_token(keywords[i].kind, c - len, len);
              was_keyword = 1;
            }
          }

          if (!was_keyword) {
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
        } else {
          error_at(make_token(TOKEN_ERROR, c, 1), "unrecognized character");
        }

        break;
    }

    cursor = cursor->next;
  }

  cursor->next = make_token(TOKEN_EOF, c, 0);
  return head.next;
}

void
verror_at(token* tok, const char* fmt, va_list args)
{
  // Two things we want to accomplish here, for the sake of the error message
  // we're about to emit:
  //  1. Calculate the line and column of the error, based on the given token's
  //  position
  //  2. Save the line upon which the error occurs so that we can print a carat
  //  pinpointing the error
  //
  // Both are done in this loop.
  int line = 1, col = 1;
  const char* line_start = source;
  const char* c = source;
  while (*c != '\0') {
    if (c == tok->pos) {
      break;
    }

    col++;
    if (*c == '\n') {
      line++;
      col = 1;
      c++;
      line_start = c;
    } else {
      c++;
    }
  }

  // We've stopped midway through a line - advance the cursor to the end of
  // the line.
  while (*c++ != '\n')
    ;

  // Don't print the last newline.
  c--;
  size_t len = c - line_start;
  char* line_text = strndup(line_start, len);

  fprintf(stderr, "%s:%d:%d: error: ", source_name, line, col);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  fprintf(stderr, "%5d | %s\n", line, line_text);
  fprintf(stderr, "      |%*s^\n", col, " ");
  fflush(stderr);
  exit(1);
}

void
error_at(token* tok, const char* fmt, ...)
{
  (void)tok;
  va_list args;
  va_start(args, fmt);
  verror_at(tok, fmt, args);
  va_end(args);
}
