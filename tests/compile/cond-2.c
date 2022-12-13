/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  int x;
};

int
main()
{
  int* ptr;
  // CHECK: error: invalid types for true and false branches of conditional expression (have `int*` and `int`)
  int q = 0 ? ptr : 1;
  return q;
}
