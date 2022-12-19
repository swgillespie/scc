CC := gcc
CFLAGS := --std=gnu11 -Wpedantic -Wall -Wextra -g

SOURCES := main.c codegen.c parse.c tokenize.c type.c
OBJECTS := $(SOURCES:.c=.o)

TEST_SOURCES := tests/run/arith.c \
	tests/run/basics.c \
	tests/run/pointers.c \
	tests/run/calls.c \
	tests/run/types.c

TEST_EXES := $(basename $(TEST_SOURCES))

all: scc test

scc: $(OBJECTS)
	$(CC) $(OBJECTS) -o scc

selfhost: CFLAGS = -D__SCC__=1 -Wpedantic
selfhost: scc

%.o: %.c scc.h
	$(CC) $(CFLAGS) -c -o $@ $<

test-run: $(TEST_EXES)
	for exe in $(TEST_EXES); do ./$$exe; done

test-compile:
	poetry run lit tests/compile --timeout=2

test: test-compile test-run

tests/run/helpers.o: tests/run/helpers.c
	$(CC) -g -O2 -c -o $@ $<

tests/run/%.s: tests/run/%.c
	./scc $< -o $@

tests/run/%: tests/run/%.s tests/run/helpers.o
	$(CC) -static -o $@ $< tests/run/helpers.o

bootstrap: clean
	./bootstrap.sh

bootstrap-test: bootstrap test

clean:
	rm -f $(OBJECTS)
	rm -f $(TEST_EXES)
	rm -f $(TEST_SOURCES:.c=.preproc.c)
	rm -f $(TEST_SOURCES:.c=.s)
	rm -f tests/helpers.o
	rm -f scc