#ifndef __TEST_H__
#define __TEST_H__

#define ASSERT_EQ(actual, expected)                                            \
  do {                                                                         \
    if ((expected) != (actual)) {                                              \
      printf("%s:%d ", __FILE__, __LINE__);                                    \
      printf("assertion failure: %s = %d\n", #actual, actual);                 \
      abort();                                                                 \
    }                                                                          \
  } while (0)

#endif /* __TEST_H__ */