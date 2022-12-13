/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: illegal initializer for typedef
  typedef int x = 42;
}