/**
RUN: %scc %s 2>&1 | filecheck %s
*/

typedef int a;

// CHECK: func
int
func(int a, int b)
{
  return a + b;
}

// CHECK: main
int
main()
{
  // scc had a bug in which it put parameters as locals in the global namespace.
  //
  // This only manifested when parsing the below line:
  a* b = (a*)0;
  // This line parses if `a` is a type identifier and fails if `a` is a value
  // identifier.
  return 0;
}

// CHECK-NOT: warning:
// CHECK-NOT: error: