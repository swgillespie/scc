#!/bin/env python3

import subprocess
import sys

from os.path import join
from tempfile import TemporaryDirectory

failed = 0
succeeded = 0

def run_test(src: str, expected: int) -> None:
    global failed, succeeded

    with TemporaryDirectory() as tempdir:
        input_c = join(tempdir, 'input.c')
        input_s = join(tempdir, 'input.s')
        lib_s = join(tempdir, 'lib.s')
        input = join(tempdir, 'input')

        with open(input_c, 'w') as input_file:
            input_file.write(src)
            input_file.flush()

        with open(lib_s, 'w') as lib_file:
            lib_file.write("""
            .global returns_zero
            returns_zero:
                mov $0, %rax
                ret

            .global returns_argument
            returns_argument:
                mov %rdi, %rax
                ret

            .global adds_arguments
            adds_arguments:
                lea (%rdi, %rsi), %rax
                ret

            .global adds_six_args
            adds_six_args:
                add %rsi, %rdi
                add %rdx, %rdi
                add %rcx, %rdi
                add %r8, %rdi
                lea (%rdi, %r9), %rax
                ret
            """)
            lib_file.flush()


        with open(input_s, 'w') as output:
            subprocess.check_call(["./scc", input_c], stdout=output, stderr=subprocess.DEVNULL)
        subprocess.check_call(["cc", "-static", "-o", input, input_s, lib_s])
        result = subprocess.call([input])
        if result != expected:
            print(f"\n{src}\n\nExpected {expected}, got {result}")
            failed += 1
        else:
            print(".", end="")
            sys.stdout.flush()
            succeeded += 1

def main():
    run_test('int main() { return 0; }', 0)
    run_test('int main() { return 1; }', 1)
    run_test('int main() { return 2; }', 2)
    run_test('int main() { return 1+1; }', 2)
    run_test('int main() { return 1+1+2; }', 4)
    run_test('int main() { return 20-3; }', 17)
    run_test('int main() { return 2*3; }', 6)
    run_test('int main() { return 1+1*3; }', 4)
    run_test('int main() { return 6/2; }', 3)
    run_test('int main() { return 6%4; }', 2)
    run_test('int main() { return 1 == 0; }', 0)
    run_test('int main() { return 1 == 1; }', 1)
    run_test('int main() { return 3 < 4; }', 1)
    run_test('int main() { return 3 < 3; }', 0)
    run_test('int main() { return 3 <= 3; }', 1)
    run_test('int main() { return 0 != 0; }', 0)
    run_test('int main() { return 0 != 1; }', 1)
    run_test('int main() { return (1+1)*2; }', 4)
    run_test('int main() { int x = 0; return 0; }', 0)
    run_test('int main() { int x = 0; return x; }', 0)
    run_test('int main() { int x = 1; int y = 2; return x + y; }', 3)
    run_test('int main() {{ int x = 1; int y = 2; return x + y; }}', 3)
    run_test('int main() { int x = 0; if (0) { x = 1; } else { x = 2; } return x; }', 2)
    run_test('int main() { int x = 0; if (1) { x = 1; } else { x = 2; } return x; }', 1)
    run_test('int main() { int x = 0; if (0) { x = 1; } return x; }', 0)
    run_test('int main() { int x = 0; if (1) { x = 1; } return x; }', 1)
    run_test('int main() { int x; x = 0; return 0; }', 0)
    run_test('int main() { int x = 0; for (int i = 0; i < 5; i = i + 1) { x = x + 1; } return x; }', 5)
    run_test('int main() { int x = 0; for (int i = 0; i < 5; i = i + 1) x = x + 1; return x; }', 5)
    run_test('int main() { int x = 0; while (x < 5) x = x + 1; return x; }', 5)
    run_test('int main() { int x = 0; x++; return x; }', 1)
    run_test('int main() { int x = 0; int* y = &x; *y = 5; return x; }', 5)
    run_test('int main() { int returns_zero = 0; return returns_zero; }', 0)
    run_test('int main() { return returns_zero(); }', 0)
    run_test('int main() { return returns_argument(5); }', 5)
    run_test('int main() { return adds_arguments(2, 3); }', 5)
    run_test('int main() { return adds_six_args(1, 2, 3, 4, 5, 6); }', 21)
    run_test('int main() { int x[5]; return 0; }', 0)
    run_test('int main() { int x[5]; return sizeof(x); }', 40)
    run_test('int main() { int x[5]; return sizeof x; }', 40)
    run_test('int main() { int x[2]; x[0] = 1; return x[0]; }', 1)
    run_test('int main() { int x[2]; x[1] = 1; return x[1]; }', 1)
    run_test('int main() { int x[2]; int* y = x; y[0] = 1; return x[0];} ', 1)
    run_test('int main() { int x[2]; int* y = x + 1; y[0] = 1; return x[1];} ', 1)
    print(f"\n\n{succeeded}/{succeeded + failed} passed")
    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()