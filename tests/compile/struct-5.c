/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  int x;
  // CHECK: error: left-hand-side of member expression is not a struct
  x.y = 5;
}