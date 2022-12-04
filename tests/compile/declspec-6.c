/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: ISO C requires a named argument before `...`
int
vararg(...);