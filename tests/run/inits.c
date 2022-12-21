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
}

int
main()
{
  puts("inits ok");
  return 0;
}