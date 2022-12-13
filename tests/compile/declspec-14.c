/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  typedef int x;
  // x should not be allocated a stack slot
  // CHECK: sub $0, %rsp
}