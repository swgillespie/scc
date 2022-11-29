/**
RUN: clang %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: error: indirection requires pointer operand
  *0 = 5;
  return 0;
}