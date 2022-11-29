/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  int x = 42;
  int y = 99;
  int* z = &x;
  int* q = &x;
  // CHECK: error: invalid type for `+` operator (have `int*`)
  int* m = z + q;
  return 0;
}