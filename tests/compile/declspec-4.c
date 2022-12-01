/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: declaration declares a function that returns a function
int
function()();

int
main()
{
  return 0;
}