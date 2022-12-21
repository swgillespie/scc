/**
RUN: %scc %s 2>&1 | filecheck %s
*/

typedef struct initializer
{
  struct initializer* next;
} initializer;

// CHECK: main:
int
main()
{
  int* initializer;
  initializer = 0;
}
