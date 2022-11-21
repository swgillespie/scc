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
            succeeded += 1

def main():
    run_test('int main() {}', 0)
    print(f"\n\n{succeeded}/{succeeded + failed} passed")
    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()