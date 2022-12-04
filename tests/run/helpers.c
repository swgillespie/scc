#include <stdarg.h>

int
adds_six_arguments(int a, ...)
{
  va_list args;
  va_start(args, a);
  int sum = a;
  for (int i = 0; i < 5; i++) {
    sum += va_arg(args, int);
  }

  return sum;
}