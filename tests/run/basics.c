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

  int m = 0;
  for (int i = 0; i < 10; i++) {
    break;
    m++;
  }

  ASSERT_EQ(m, 0);

  int t = 0;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < i; j++) {
      break;
      t++;
    }

    t++;
  }

  ASSERT_EQ(t, 5);

  int u = 5;
  u--;
  ASSERT_EQ(u, 4);
  return 0;

  int _a = 0;
  for (int i = 0; i < 5; i++) {
    continue;
    _a++;
  }
  ASSERT_EQ(_a, 0);
}

int
scopes()
{
  int x = 0;
  {
    int x = 1;
    ASSERT_EQ(x, 1);
    x++;
    ASSERT_EQ(x, 2);
  }
  ASSERT_EQ(x, 0);
  return 0;

  int q = 99;
  for (int q = 0; q < 4; q++) {
  }
  ASSERT_EQ(q, 99);
}

int
main()
{
  decls();
  branches();
  loops();
  scopes();
  puts("basics ok");
  return 0;
}