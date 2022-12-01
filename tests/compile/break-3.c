/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: .L.for.header.0:
  for (int i = 0; i < 5; i++) {
    // CHECK-NOT: error: break outside of loop or switch
    // CHECK: jmp .L.for.end.0
    break;
  }
  // CHECK: .L.for.end.0:

  // CHECK: .L.for.header.1:
  while (1) {
    // CHECK-NOT: error: break outside of loop or switch
    // CHECK: jmp .L.for.end.1
    break;
  }
  // CHECK: .L.for.end.1:
}