CC ?= clang
CFLAGS := --std=gnu11 -Wpedantic -Wall -Wextra -g

SOURCES := main.c codegen.c parse.c tokenize.c type.c
OBJECTS := $(SOURCES:.c=.o)

TEST_SOURCES := $(shell find ./tests/run -name "*.c")
TEST_EXES := $(basename $(TEST_SOURCES))

scc: $(OBJECTS)
	$(CC) $(OBJECTS) -o scc

%.o: %.c scc.h
	$(CC) $(CFLAGS) -c -o $@ $<

test-run: $(TEST_EXES)
	for exe in $(TEST_EXES); do ./$$exe; done

test-compile: scc
	poetry run lit tests/compile

test: test-compile test-run

tests/run/helpers.o: tests/run/helpers.s
	$(CC) -c -o $@ $<

tests/run/%.preproc.c: tests/run/%.c
	$(CC) -E -P $< > $@

tests/run/%.s: tests/run/%.preproc.c scc
	./scc $< > $@ 2> /dev/null

tests/run/%: tests/run/%.s tests/run/helpers.o
	$(CC) -static -o $@ $< tests/run/helpers.o

clean:
	rm -f $(OBJECTS)
	rm -f $(TEST_EXES)
	rm -f $(TEST_SOURCES:.c=.preproc.c)
	rm -f $(TEST_SOURCES:.c=.s)
	rm -f tests/helpers.o
	rm -f scc