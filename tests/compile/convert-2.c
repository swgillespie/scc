/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int
main()
{
  // CHECK: warning: conversion of `int` to `int*` makes pointer from integer
  // without a cast
  int* x = 1;
}
