#include "test.h"

void
scalars()
{
  int x = 4;
  ASSERT_EQ(x, 4);
  int y = { 5 };
}

int
main()
{
  puts("inits ok");
  return 0;
}