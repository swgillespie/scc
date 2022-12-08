/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: illegal initializer for typedef
typedef int foo = 5;