/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: two or more data types in declaration specifier
  int _Bool x;
  return 0;
}