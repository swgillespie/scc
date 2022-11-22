# scc - sean's C compiler

A small C compiler with big dreams. Being built incrementally, feature-by-feature.

```
$ cat input.c
int main() {
    int x = 5;
    int y = 5;
    int z = 2 * 5;
    return x + y - z;
}

$ make
$ ./scc input.c > input.s
$ clang -static -o input input.s
$ ./input
```
