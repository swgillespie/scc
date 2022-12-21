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

  int arr[] = { 1, 2, 3 };
  ASSERT_EQ(sizeof(arr), 3 * sizeof(int));
  ASSERT_EQ(arr[0], 1);
  ASSERT_EQ(arr[1], 2);
  ASSERT_EQ(arr[2], 3);
}

int
main()
{
  scalars();
  puts("inits ok");
  return 0;
}