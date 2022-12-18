/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
orf(void* x, int y)
{
  // CHECK: error: expected integer type for bitwise inclusive or (have `void*`)
  return x | y;
}