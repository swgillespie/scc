/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct node
{
  struct node* next;
  int value;
};

int
main()
{
  struct node head;
  // The `value` field uses an incomplete definition of `struct node`, but the
  // decl should be made complete by the time the rbrace on line 9 is read.
  //
  // CHECK-NOT: error: no such field `value` in struct type `struct node`
  return head.next->value;
}