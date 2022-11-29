/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: invalid types for array subscript operator (have `int` and `int`)
  int x = 5 [5];
  return 0;
}