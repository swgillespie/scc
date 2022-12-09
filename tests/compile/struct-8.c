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
  // CHECK: error: left-hand-side of member deref expression is not a pointer
  p->x = 5;
  return 0;
}