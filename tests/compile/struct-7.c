/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{};

int
main()
{
  struct foo* ptr;
  // CHECK: no such field `x` in struct type `struct foo`
  ptr->x = 5;
  return 0;
}