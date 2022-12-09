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
  struct foo p;
  // CHECK: error: no such field `y` in struct type `struct foo`
  p.y = 5;
  return 0;
}
