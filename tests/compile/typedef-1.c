/**
RUN: %scc %s 2>&1 | filecheck %s
*/

typedef int foo;

int
main()
{
  foo x = 5;
  // CHECK: (type is `foo (aka `int`)`)
  *x = 10;
  return 0;
}
