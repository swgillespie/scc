/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: function definition not allowed here
  int func()
  {
    return 0;
  }

  return 0;
}