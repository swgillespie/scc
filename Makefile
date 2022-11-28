CC ?= clang
CFLAGS := --std=gnu11 -Wpedantic -Wall -Wextra -g

SOURCES := main.c codegen.c parse.c tokenize.c type.c
OBJECTS := $(SOURCES:.c=.o)

TEST_SOURCES := $(shell find ./tests -name "*.c")
TEST_EXES := $(basename $(TEST_SOURCES))

scc: $(OBJECTS)
	$(CC) $(OBJECTS) -o scc

%.o: %.c scc.h
	$(CC) $(CFLAGS) -c -o $@ $<

test: $(TEST_EXES)
	for exe in $(TEST_EXES); do ./$$exe; done

tests/helpers.o: tests/helpers.s
	$(CC) -c -o $@ $<

tests/%.preproc.c: tests/%.c
	$(CC) -E -P $< > $@

tests/%.s: tests/%.preproc.c scc
	./scc $< > $@ 2> /dev/null

tests/%: tests/%.s tests/helpers.o
	$(CC) -static -o $@ $< tests/helpers.o

clean:
	rm -f $(OBJECTS)
	rm -f $(TEST_EXES)
	rm -f $(TEST_SOURCES:.c=.preproc.c)
	rm -f $(TEST_SOURCES:.c=.s)
	rm -f tests/helpers.o
	rm -f scc