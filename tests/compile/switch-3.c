/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: switch condition must have integer type
  switch ("hello") {
  }
}