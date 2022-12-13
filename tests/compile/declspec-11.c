/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: `void` must be the first and only parameter if specified
int
test(void, int x)
{
}
