#include "test.h"

typedef int an_int;

int
main()
{
  int x = 0;
  int arr[5];
  ASSERT_EQ(4, sizeof(int));
  ASSERT_EQ(4, sizeof(an_int));
  ASSERT_EQ(4, sizeof(x));
  ASSERT_EQ(4, sizeof x);
  ASSERT_EQ(20, sizeof(int) * 5);
  ASSERT_EQ(20, sizeof(arr));
  puts("types ok");
  return 0;
}