/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
orf(void* x, int y)
{
  // CHECK: error: expected integer type for bitwise operation (have `void*`)
  return x | y;
}