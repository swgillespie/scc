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
  struct foo x;
  // CHECK: error: logical not requires a scalar type (have `struct foo`)
  int q = !x;
}
