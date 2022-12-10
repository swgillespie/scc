/**
RUN: %scc %s 2>&1 | filecheck %s
*/

// CHECK: error: empty enums are invalid
enum
{
};
