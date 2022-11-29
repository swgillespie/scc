#include "test.h"

int
returns_zero()
{
  return 0;
}

int
calls()
{
  ASSERT_EQ(returns_zero(), 0);
  ASSERT_EQ(returns_argument(1), 1);
  ASSERT_EQ(adds_arguments(1, 2), 3);
  ASSERT_EQ(adds_six_arguments(1, 2, 3, 4, 5, 6), 21);
  ASSERT_EQ(strlen("four"), 4);
  return 0;
}

int
main()
{
  calls();
  puts("calls ok");
  return 0;
}