#include "test.h"

typedef int an_int;

struct foo
{
  int x;
};

int
main()
{
  int x = 0;
  int arr[5];
  ASSERT_EQ(sizeof(int), 4);
  ASSERT_EQ(sizeof(an_int), 4);
  ASSERT_EQ(sizeof(x), 4);
  ASSERT_EQ(sizeof x, 4);
  ASSERT_EQ(sizeof(int) * 5, 20);
  ASSERT_EQ(sizeof(arr), 20);
  ASSERT_EQ(sizeof(struct { int x; }), 4);
  ASSERT_EQ(sizeof(struct foo), 4);
  puts("types ok");
  return 0;
}