/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: storage class not permitted here
int
func(extern int x)
{
  return 0;
}