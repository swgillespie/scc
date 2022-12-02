/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  char* x = 0;
  int* y = 0;
  // CHECK: error: invalid types for `-` operator (have `int*` and `char*`)
  int* z = x - y;
  return 0;
}