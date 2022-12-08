/**
RUN: %scc %s 2>&1 | filecheck %s
*/

typedef int foo;
typedef foo bar;

int
main()
{
  bar x = 5;
  // CHECK: (type is `bar (aka `foo (aka `int`)`)`)
  *x = 10;
  return 0;
}
