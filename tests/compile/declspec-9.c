/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: warning: declaration does not declare anything
int;
