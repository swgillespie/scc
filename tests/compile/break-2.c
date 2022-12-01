/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  for (int i = 0; i < 5; i++) {
  }

  // CHECK: error: break outside of loop or switch
  break;
}