/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  int x = 42;
  // CHECK: error: cannot dereference non-pointer type
  *x = 5;
  return 0;
}