/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct node
{
  struct node* next;
  int value;
};

// CHECK: error: redefinition of `struct node`
struct node
{
  int value;
};
