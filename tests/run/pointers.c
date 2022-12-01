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
multidim_arrays()
{
  int x[3][3];
  x[0][1] = 1;
  x[2][2] = 4;
  ASSERT_EQ(x[0][1], 1);
  ASSERT_EQ(x[2][2], 4);

  int* p = x[1];
  p[1] = 9;
  ASSERT_EQ(x[1][1], 9);

  return 0;
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

int global;

int
globals()
{
  global = 0;
  ASSERT_EQ(global, 0);
  global++;
  ASSERT_EQ(global, 1);
}

int
main()
{
  arrays();
  pointers();
  globals();
  multidim_arrays();
  puts("pointers ok");
  return 0;
}