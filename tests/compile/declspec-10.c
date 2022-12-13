/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: argument may not have `void` type
int
test(void x)
{
}
