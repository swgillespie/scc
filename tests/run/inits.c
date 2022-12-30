#include "test.h"

void
scalars()
{
  int x = 4;
  ASSERT_EQ(x, 4);
  int y = { 5 };
  ASSERT_EQ(y, 5);
  int z = {};
  ASSERT_EQ(z, 0);
}

void
arrays()
{
  int arr[] = { 1, 2, 3 };
  ASSERT_EQ(sizeof(arr), 3 * sizeof(int));
  ASSERT_EQ(arr[0], 1);
  ASSERT_EQ(arr[1], 2);
  ASSERT_EQ(arr[2], 3);

  int abc[2] = { 5 };
  ASSERT_EQ(abc[0], 5);
  ASSERT_EQ(abc[1], 0);
}

struct abc
{
  int a;
  int b;
  int c;
};

void
structs()
{
  struct abc x = { 1, 2, 3 };
  ASSERT_EQ(x.a, 1);
  ASSERT_EQ(x.b, 2);
  ASSERT_EQ(x.c, 3);

  struct abc q = {};
  ASSERT_EQ(q.a, 0);
  ASSERT_EQ(q.b, 0);
  ASSERT_EQ(q.c, 0);

  struct abc r = { 4 };
  ASSERT_EQ(r.a, 4);
  ASSERT_EQ(r.b, 0);
  ASSERT_EQ(r.c, 0);
}

int some_numbers[] = { 1, 2, 3, 4, 5 };

char* some_strings[] = { "foo", "bars", "bazes" };

struct init
{
  int a;
  char* b;
  int c;
};

struct init inits[] = {
  { 4, "hello", 5 },
  { 9, "world", 10 },
};

void
global_arrays()
{
  ASSERT_EQ(sizeof(some_numbers), 5 * sizeof(int));
  ASSERT_EQ(some_numbers[0], 1);
  ASSERT_EQ(some_numbers[1], 2);
  ASSERT_EQ(some_numbers[2], 3);
  ASSERT_EQ(some_numbers[3], 4);
  ASSERT_EQ(some_numbers[4], 5);
  some_numbers[4] = 9;
  ASSERT_EQ(some_numbers[4], 9);
  ASSERT_EQ(sizeof(some_strings), 3 * sizeof(char*));
  ASSERT_EQ(strlen(some_strings[0]), 3);

  ASSERT_EQ(sizeof(inits), 2 * sizeof(struct init));
  struct init* zero = &inits[0];
  ASSERT_EQ(zero->a, 4);
  ASSERT_EQ(strlen(zero->b), 5);
  ASSERT_EQ(zero->c, 5);

  struct init* one = &inits[1];
  ASSERT_EQ(one->a, 9);
  ASSERT_EQ(strlen(one->b), 5);
  ASSERT_EQ(one->c, 10);
}

int
main()
{
  scalars();
  arrays();
  global_arrays();
  puts("inits ok");
  return 0;
}