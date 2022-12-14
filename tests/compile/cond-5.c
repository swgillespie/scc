/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK-NOT: error: invalid types for true and false branches
  char* tok_str = 0 ? "+" : "-";
}