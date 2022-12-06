/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: more than one storage class not permitted
extern extern int c;