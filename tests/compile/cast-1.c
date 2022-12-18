/**
RUN: %scc %s 2>&1 | filecheck %s
*/

struct foo
{
  int x;
};

int
main()
{
  int q;
  // CHECK: error: conversion to non-scalar type
  struct foo x = (struct foo)q;
}
