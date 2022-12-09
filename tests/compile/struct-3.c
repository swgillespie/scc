/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  // CHECK: field has incomplete type `struct foo`
  struct foo recursive;
};
