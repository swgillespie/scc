#include "test.h"

int
aborts()
{
  abort();
  return 0;
}

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

  ASSERT_EQ(0 ? 5 : 6, 6);
  ASSERT_EQ(1 ? 5 : 6, 5);
  ASSERT_EQ(1 ? 5 : aborts(), 5);
  ASSERT_EQ(0 ? aborts() : 6, 6);
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

  int _a = 0;
  for (int i = 0; i < 5; i++) {
    continue;
    _a++;
  }
  ASSERT_EQ(_a, 0);

  int _b = 0;
  int _c = _b++;
  ASSERT_EQ(_c, 0);
  ASSERT_EQ(_b, 1);
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

void
switches()
{
  int x = 4;
  int res = 0;
  switch (x) {
    case 3:
      res = 1;
      break;
    case 4:
      res = 2;
      break;
    default:
      res = 3;
      break;
  }

  ASSERT_EQ(res, 2);
  x = 0;
  res = 0;
  switch (x) {
    case 0:
      res++;
    case 1:
      res++;
    case 2:
      res++;
  }

  ASSERT_EQ(res, 3);
  x = 0;
  res = 0;
  switch (x) {
    case 1:
      res = 1;
      break;
    default:
      res = 2;
      break;
  }
  ASSERT_EQ(res, 2);
}

int
main()
{
  decls();
  branches();
  loops();
  scopes();
  switches();
  puts("basics ok");
  return 0;
}