/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: ud2
  __builtin_trap();
  return 0;
}