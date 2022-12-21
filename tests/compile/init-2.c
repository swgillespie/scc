/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: warning: excess elements in scalar initializer
  int x = { 5, 6 };
}