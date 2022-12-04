#ifndef __TEST_H__
#define __TEST_H__

int
puts(char* str);
int
strlen(char* str);
int
printf(char* format, ...);
void
abort();

int
adds_six_arguments(int a, ...);

#define ASSERT_EQ(actual, expected)                                            \
  do {                                                                         \
    if ((expected) != (actual)) {                                              \
      printf("%s:%d ", __FILE__, __LINE__);                                    \
      printf("assertion failure: %s = %d\n", #actual, actual);                 \
      abort();                                                                 \
    }                                                                          \
  } while (0)

#endif /* __TEST_H__ */
