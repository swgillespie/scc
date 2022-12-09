/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: declaration of anonymous struct or union must be a definition
struct;
