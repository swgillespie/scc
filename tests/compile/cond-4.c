/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  int x;
};

void
returns_void()
{
  return 0;
}

int
main()
{
  // CHECK: error: invalid types for true and false branches of conditional expression (have `int` and `void`)
  int q = 0 ? 1 : returns_void();
  return q;
}
