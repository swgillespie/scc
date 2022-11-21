CC ?= clang
CFLAGS := --std=c11 -Wpedantic -Wall -Wextra -g

SOURCES := $(shell find . -name "*.c")
OBJECTS := $(SOURCES:.c=.o)

scc: $(OBJECTS)
	$(CC) $(OBJECTS) -o scc

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

test: scc
	./test.py

clean:
	rm -f $(OBJECTS)
	rm scc