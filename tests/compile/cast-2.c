/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK-NOT: error: conversion to non-scalar type
  (void)5;
}
