#include "test.h"

int
arrays()
{
  int s;
  int x[5];
  ASSERT_EQ(sizeof(x), sizeof(s) * 5); /* wrong, but temporary */
  ASSERT_EQ(sizeof x, sizeof(s) * 5);
  x[0] = 1;
  ASSERT_EQ(x[0], 1);
  x[1] = 2;
  ASSERT_EQ(x[1], 2);
  return 0;

  int y[2];
  int* z = y;
  z[0] = 1;
  ASSERT_EQ(y[0], 1);

  int* q = y + 1;
  q[0] = 2;
  ASSERT_EQ(y[1], 2);
}

int
pointers()
{
  int x = 0;
  int* y = &x;
  *y = 42;
  ASSERT_EQ(x, 42);
  return 0;
}

int
main()
{
  arrays();
  pointers();
  puts("pointers ok");
  return 0;
}