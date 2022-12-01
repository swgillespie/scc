/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: two or more data types in declaration specifier
  int char x;
  return 0;
}