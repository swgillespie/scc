/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  int x;
  // CHECK: error: struct or union declares duplicate field `x`
  char x;
};
