/**
RUN: %scc %s 2>&1 | filecheck %s
*/

char*
constant_string()
{
  return "hello!";
}

int
main()
{
  // CHECK: invalid type for `+` operator (have `char*`)
  return constant_string() + constant_string();
}
