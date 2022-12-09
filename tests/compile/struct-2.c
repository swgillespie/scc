/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  // CHECK: field has incomplete type `struct bar`
  struct bar x;
};
