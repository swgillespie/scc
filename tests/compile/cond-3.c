/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  int x;
};

struct bar
{
  int x;
};

int
main()
{
  struct foo x;
  struct bar y;
  // CHECK: error: invalid types for true and false branches of conditional expression (have `struct foo` and `struct bar`)
  struct foo x = 0 ? x : y;
  return q;
}
