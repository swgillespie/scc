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
        input = join(tempdir, 'input')

        with open(input_c, 'w') as input_file:
            input_file.write(src)
            input_file.flush()

        with open(input_s, 'w') as output:
            subprocess.check_call(["./scc", input_c], stdout=output)
        subprocess.check_call(["clang", "-static", "-o", input, input_s])
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
    print(f"\n\n{succeeded}/{succeeded + failed} passed")
    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()