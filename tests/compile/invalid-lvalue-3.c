/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: not a lvalue
  int* x = &5;
  return 0;
}