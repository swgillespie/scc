/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  int x = 0;
  int xprefix[5];
  // CHECK-NOT: invalid types for array subscript operator
  xprefix[0] = x;
  return 0;
}