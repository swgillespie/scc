/**
RUN: %scc -w %s 2>&1 | filecheck %s
*/

// CHECK-NOT: warning: declaration does not declare anything
int;

int
main()
{
}