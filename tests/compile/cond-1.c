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
  // CHECK: error: condition of ternary operator must have scalar type
  int q = p ? 0 : 1;
  return q;
}
