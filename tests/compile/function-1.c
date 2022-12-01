/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  int x;
  // CHECK: error: called object is not a function (has type `int`)
  return x();
}