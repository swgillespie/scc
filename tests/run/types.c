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

union foo_union
{
  int x;
  int y;
  int z;
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
  ASSERT_EQ(sizeof(union foo_union), 4);
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

  union foo_union u;
  u.y = 0;
  u.x = 1;
  ASSERT_EQ(u.y, 1);
  ASSERT_EQ(&u.x == &u.y, 1);
}

enum inc
{
  INC_A,
  INC_B,
  INC_C = 8,
  INC_D
};

void
enums()
{
  ASSERT_EQ(INC_A, 0);
  ASSERT_EQ(INC_B, 1);
  ASSERT_EQ(INC_C, 8);
  ASSERT_EQ(INC_D, 9);
}

void
casts()
{
  ASSERT_EQ((int)'a', 97);
  ASSERT_EQ((char)97, 'a');
  ASSERT_EQ(&(((struct membertest*)0)->y), 4);
}

struct aligned
{
  int x;
  void* y;
  char z;
  char p;
  void* q;
};

void
alignment()
{
  ASSERT_EQ(_Alignof(int), 4);
  ASSERT_EQ(_Alignof(long), 8);
  ASSERT_EQ(_Alignof(char), 1);
  ASSERT_EQ(_Alignof(void), 1);
  ASSERT_EQ(_Alignof(_Bool), 1);
  ASSERT_EQ(_Alignof(void*), 8);

  ASSERT_EQ(_Alignof(struct aligned), 8);
  ASSERT_EQ(sizeof(struct aligned), 32);
  ASSERT_EQ(&(((struct aligned*)0)->x), 0);
  ASSERT_EQ(&(((struct aligned*)0)->y), 8); // 4 bytes of padding
  ASSERT_EQ(&(((struct aligned*)0)->z), 16);
  ASSERT_EQ(&(((struct aligned*)0)->p), 17); // 6 bytes of padding
  ASSERT_EQ(&(((struct aligned*)0)->q), 24);
}

int
main()
{

  members();
  size_of();
  enums();
  casts();
  alignment();
  puts("types ok");
  return 0;
}