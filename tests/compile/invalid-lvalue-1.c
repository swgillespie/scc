/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: cannot dereference non-pointer type
  *0 = 5;
  return 0;
}