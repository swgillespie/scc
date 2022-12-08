/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: int3
  __builtin_debugtrap();
  return 0;
}