/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: function definition declared `typedef`
typedef int
foo()
{
  return 0;
}