/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// more silly things C allows
char typedef* foo;

int
main()
{
  foo x;
  foo y;
  // CHECK: (have `foo (aka `char*`)`)
  foo z = x + y;
  return 0;
}
