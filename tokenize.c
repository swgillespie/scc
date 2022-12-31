#include "scc.h"

char* source_name;
char* source;
static size_t source_len;

typedef struct keyword
{
  const char* str;
  token_kind kind;
  size_t len;
} keyword;

static keyword keywords[] = {
  { "int", TOKEN_INT, 3 },
  { "return", TOKEN_RETURN, 6 },
  { "if", TOKEN_IF, 2 },
  { "else", TOKEN_ELSE, 4 },
  { "for", TOKEN_FOR, 3 },
  { "while", TOKEN_WHILE, 5 },
  { "sizeof", TOKEN_SIZEOF, 6 },
  { "char", TOKEN_CHAR, 4 },
  { "do", TOKEN_DO, 2 },
  { "_Bool", TOKEN_BOOL, 5 },
  { "void", TOKEN_VOID, 4 },
  { "break", TOKEN_BREAK, 5 },
  { "extern", TOKEN_EXTERN, 6 },
  { "continue", TOKEN_CONTINUE, 8 },
  { "typedef", TOKEN_TYPEDEF, 7 },
  { "struct", TOKEN_STRUCT, 6 },
  { "union", TOKEN_UNION, 5 },
  { "switch", TOKEN_SWITCH, 6 },
  { "case", TOKEN_CASE, 4 },
  { "default", TOKEN_DEFAULT, 7 },
  { "enum", TOKEN_ENUM, 4 },
  { "unsigned", TOKEN_UNSIGNED, 8 },
  { "long", TOKEN_LONG, 4 },
  { "const", TOKEN_CONST, 5 },
  { "static", TOKEN_STATIC, 6 },
  { "_Alignof", TOKEN_ALIGNOF, 8 },
  { "__alignof__", TOKEN_ALIGNOF, 11 } // GCC extension
};

void
load_file(const char* filename, FILE* f)
{
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
      case '[':
        cursor->next = make_token(TOKEN_LBRACKET, c, 1);
        c++;
        break;
      case ']':
        cursor->next = make_token(TOKEN_RBRACKET, c, 1);
        c++;
        break;
      case ';':
        cursor->next = make_token(TOKEN_SEMICOLON, c, 1);
        c++;
        break;
      case ':':
        cursor->next = make_token(TOKEN_COLON, c, 1);
        c++;
        break;
      case '+':
        c++;
        if (*c == '+') {
          cursor->next = make_token(TOKEN_PLUS_PLUS, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_PLUS, c, 1);
        }

        break;
      case '-':
        c++;
        if (*c == '-') {
          cursor->next = make_token(TOKEN_MINUS_MINUS, c - 1, 2);
          c++;
        } else if (*c == '>') {
          cursor->next = make_token(TOKEN_ARROW, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_MINUS, c, 1);
        }

        break;
      case '*':
        cursor->next = make_token(TOKEN_STAR, c, 1);
        c++;
        break;
      case '/':
        c++;
        if (*c == '*') {
          c++;
          while (1) {
            if (*c == '*') {
              c++;
              if (*c == '/') {
                c++;
                break;
              }
            } else {
              c++;
            }
          }

          continue;
        }

        if (*c == '/') {
          c++;
          while (*c != '\n')
            c++;

          continue;
        }
        cursor->next = make_token(TOKEN_SLASH, c, 1);
        c++;
        break;
      case '%':
        cursor->next = make_token(TOKEN_PERCENT, c, 1);
        c++;
        break;
      case '=':
        c++;
        if (*c == '=') {
          cursor->next = make_token(TOKEN_DOUBLE_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_EQUAL, c, 1);
        }

        break;
      case '!':
        c++;
        if (*c == '=') {
          cursor->next = make_token(TOKEN_NOT_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_EXCLEM, c, 1);
        }

        break;
      case '<':
        c++;
        if (*c == '=') {
          cursor->next = make_token(TOKEN_LT_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_LT, c, 1);
        }

        break;
      case '>':
        c++;
        if (*c == '=') {
          cursor->next = make_token(TOKEN_GT_EQ, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_GT, c, 1);
        }

        break;
      case '&':
        c++;
        if (*c == '&') {
          cursor->next = make_token(TOKEN_DOUBLE_AMPERSAND, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_AMPERSAND, c, 1);
        }

        break;
      case '|':
        c++;
        if (*c == '|') {
          cursor->next = make_token(TOKEN_DOUBLE_PIPE, c - 1, 2);
          c++;
        } else {
          cursor->next = make_token(TOKEN_PIPE, c, 1);
        }

        break;
      case ',':
        cursor->next = make_token(TOKEN_COMMA, c, 1);
        c++;
        break;
      case '.':
        c++;
        if (*c == '.') {
          c++;
          if (*c == '.') {
            cursor->next = make_token(TOKEN_ELLIPSIS, c - 2, 3);
            c++;
          } else {
            c--;
            cursor->next = make_token(TOKEN_DOT, c, 1);
          }
        } else {
          cursor->next = make_token(TOKEN_DOT, c, 1);
        }

        break;
      case '?':
        cursor->next = make_token(TOKEN_QUESTION, c, 1);
        c++;
        break;
      default:
        if (isalpha(*c) || *c == '_') {
          size_t len = 0;
          while (isalnum(*c) || *c == '_') {
            c++;
            len++;
          }

          int was_keyword = 0;
          for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
            if (len == keywords[i].len &&
                strncmp(c - len, keywords[i].str, len) == 0) {
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
        } else if (*c == '\'') {
          char* pos = c;
          c++;
          int seen_char_already = 0;
          int warned = 0;

          // C has this mind-boggling feature in which you can have multiple
          // characters live in a single character literal squence. The value of
          // this literal is equal to the source character values (in this
          // case, 8-bit chars) smashed together into an int.
          //
          // This is ludicrous, but luckily not hard to implement, so we do so
          // here.
          int value = 0;
          while (*c != '\'') {
            if (seen_char_already) {
              if (!warned) {
                warn_at(cursor, "multi-character character constant");
                warned = 1;
              }

              value = value * 256;
              // value <<= 8;
            }

            seen_char_already = 1;
            switch (*c) {
              case '\\': {
                c++;
                switch (*c) {
                  case '\'':
                    value = value | '\'';
                    break;
                  case '\"':
                    value = value | '"';
                    break;
                  case '\\':
                    value = value | '\\';
                    break;
                  case '\?':
                    value = value | '\?';
                    break;
                  case 'a':
                    value = value | '\a';
                    break;
                  case 'b':
                    value = value | '\b';
                    break;
                  case 'f':
                    value = value | '\f';
                    break;
                  case 'r':
                    value = value | '\r';
                    break;
                  case 't':
                    value = value | '\t';
                    break;
                  case 'v':
                    value = value | '\v';
                    break;
                  case 'n':
                    value = value | '\n';
                    break;
                  case '0':
                    value = value | '\0';
                    break;
                  default:
                    error_at(cursor, "unrecognized escape sequence");
                    break;
                }

                break;
              }
              default:
                value = value | *c;
                break;
            }

            c++;
          }

          c++;
          cursor->next = make_token(TOKEN_CHAR_LITERAL, pos, c - pos);
          cursor->next->value = value;
        } else if (*c == '"') {
          char* pos = c;
          c++;
          char* contents;
          size_t len;
          FILE* stream = open_memstream(&contents, &len);
          while (*c != '"') {
            switch (*c) {
              case '\\': {
                c++;
                switch (*c) {
                  case '\'':
                    fputc('\'', stream);
                    break;
                  case '\"':
                    fputc('\"', stream);
                    break;
                  case '\?':
                    fputc('\?', stream);
                    break;
                  case '\\':
                    fputc('\\', stream);
                    break;
                  case 'a':
                    fputc('\a', stream);
                    break;
                  case 'b':
                    fputc('\b', stream);
                    break;
                  case 'f':
                    fputc('\f', stream);
                    break;
                  case 'n':
                    fputc('\n', stream);
                    break;
                  case 'r':
                    fputc('\r', stream);
                    break;
                  case 't':
                    fputc('\t', stream);
                    break;
                  case 'v':
                    fputc('\v', stream);
                    break;
                  default:
                    error_at(cursor, "unrecognized escape sequence");
                }
              }

              break;
              default:
                fputc(*c, stream);
                break;
            }

            c++;
          }

          c++;
          fputc('\0', stream);
          fflush(stream);
          fclose(stream);
          cursor->next = make_token(TOKEN_STRING_LITERAL, pos, c - pos);
          cursor->next->string_value = contents;
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
