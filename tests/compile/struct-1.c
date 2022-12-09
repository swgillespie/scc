/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: declaration of anonymous struct must be a definition
struct;
