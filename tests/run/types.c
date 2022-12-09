#include "test.h"

typedef int an_int;

struct foo
{
  int x;
};

struct membertest
{
  int x;
  int y;
  char z;
};

void
size_of()
{
  int x = 0;
  int arr[5];
  ASSERT_EQ(sizeof(int), 4);
  ASSERT_EQ(sizeof(an_int), 4);
  ASSERT_EQ(sizeof(x), 4);
  ASSERT_EQ(sizeof x, 4);
  ASSERT_EQ(sizeof(int) * 5, 20);
  ASSERT_EQ(sizeof(arr), 20);
  ASSERT_EQ(sizeof(struct { int x; }), 4);
  ASSERT_EQ(sizeof(struct foo), 4);
  ASSERT_EQ(sizeof(struct {
              int y;
              char z;
            }),
            8);
}

void
members()
{
  struct membertest t;
  t.x = 1;
  t.y = 2;
  t.z = 'c';
  ASSERT_EQ(t.x, 1);
  ASSERT_EQ(t.y, 2);
  ASSERT_EQ(t.z, 'c');
  t.x++;
  ASSERT_EQ(t.x, 2);
  t.x--;
  ASSERT_EQ(t.x, 1);
  int* ptr = &t.x;
  *ptr = 8;
  ASSERT_EQ(t.x, 8);

  struct membertest p;
  struct membertest* pptr = &p;

  pptr->x = 1;
  pptr->y = 2;
  pptr->z = 'c';
  ASSERT_EQ(pptr->x, 1);
  ASSERT_EQ(p.x, 1);
  ASSERT_EQ(pptr->y, 2);
  ASSERT_EQ(p.y, 2);
  ASSERT_EQ(pptr->z, 'c');
  ASSERT_EQ(p.z, 'c');
  int* pptr_x = &pptr->x;
  *pptr_x = 9;
  ASSERT_EQ(p.x, 9);
}

int
main()
{

  members();
  size_of();
  puts("types ok");
  return 0;
}