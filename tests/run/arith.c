#include "test.h"

int
main()
{
  ASSERT_EQ(1 + 1, 2);
  ASSERT_EQ(1 + 1 + 2, 4);
  ASSERT_EQ(20 - 3, 17);
  ASSERT_EQ(2 * 3, 6);
  ASSERT_EQ(1 + 1 * 3, 4);
  ASSERT_EQ(6 / 2, 3);
  ASSERT_EQ(6 % 4, 2);
  ASSERT_EQ(1 == 0, 0);
  ASSERT_EQ(1 == 1, 1);
  ASSERT_EQ(3 < 4, 1);
  ASSERT_EQ(3 < 3, 0);
  ASSERT_EQ(3 <= 3, 1);
  ASSERT_EQ(0 != 0, 0);
  ASSERT_EQ(0 != 1, 1);
  ASSERT_EQ((1 + 1) * 2, 4);
  ASSERT_EQ('x', 120);
  ASSERT_EQ('\n', 10);

  _Bool t = 1;
  _Bool f = 0;
  ASSERT_EQ(t, 1);
  ASSERT_EQ(f, 0);
  f = 1;
  ASSERT_EQ(f, 1);
  puts("arith ok");
  return 0;
}