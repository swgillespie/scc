/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: warning: braces around scalar initializer
  int x = { { { { { { 5 } } } } } };
}