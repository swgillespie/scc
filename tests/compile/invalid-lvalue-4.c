/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: lvalue required as increment operand
  5 ++;
  return 0;
}