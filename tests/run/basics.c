#include "test.h"

int
decls()
{
  int x = 1;
  int y = 2;
  ASSERT_EQ(x + y, 3);
  int z;
  z = 0;
  ASSERT_EQ(z, 0);
  return 0;
}

int
branches()
{
  int x = 0;
  if (1) {
    x = 1;
  }
  ASSERT_EQ(x, 1);

  int z = 0;
  if (0) {
    z = 1;
  }
  ASSERT_EQ(z, 0);

  int q = 0;
  if (0) {
    q = 1;
  } else {
    q = 2;
  }
  ASSERT_EQ(q, 2);

  int r = 0;
  if (0) {
    r = 1;
  } else {
    r = 2;
  }
  ASSERT_EQ(r, 2);
  return 0;
}

int
loops()
{
  int x = 0;
  for (int i = 0; i < 4; i++)
    x++;
  ASSERT_EQ(x, 4);

  int y = 0;
  for (int i = 0; i < 4; i++) {
    y++;
  }
  ASSERT_EQ(y, 4);

  int z = 0;
  while (z < 4)
    z++;
  ASSERT_EQ(z, 4);

  int q = 0;
  do {
    q++;
  } while (0);
  ASSERT_EQ(q, 1);

  int v = 0;
  do {
    q++;
  } while (q < 5);
  ASSERT_EQ(q, 5);
}

int
main()
{
  decls();
  branches();
  loops();
  puts("basics ok");
  return 0;
}