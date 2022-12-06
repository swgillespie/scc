#include "test.h"

extern int one;

int
returns_zero()
{
  return 0;
}

int
adds_arguments(int x, int y)
{
  return x + y;
}

int
returns_argument(int x)
{
  return x;
}

int
calls()
{
  ASSERT_EQ(returns_zero(), 0);
  ASSERT_EQ(returns_argument(1), 1);
  ASSERT_EQ(adds_arguments(1, 2), 3);
  ASSERT_EQ(adds_six_arguments(1, 2, 3, 4, 5, 6), 21);
  ASSERT_EQ(strlen("four"), 4);
  ASSERT_EQ(one, 1);
  return 0;
}

int
main()
{
  calls();
  puts("calls ok");
  return 0;
}