#!/bin/bash
#
# A three-stage bootstrap for SCC. SCC is known to bootstrap partially with GCC 11.2.0 but should
# bootstrap just fine with any reasonable C compiler.
#
# Like most bootstraps, it's split into three stages:
#  * Stage 1 - gcc builds scc and produces a stage1 scc compiler
#  * Stage 2 - stage 1 scc builds scc and produces a stage2 scc compiler
#  * Stage 3 - stage 2 scc builds scc and produces a stage3 scc compiler,
#              which should be bit-by-bit identical to the stage 1 compiler
#              if there are no bugs.
#
# SCC can't fully bootstrap yet, so the stage 2 and stage 3 compiles use gcc's stage 1 artifacts
# for files that scc can't handle yet.

set -x -e

STAGE_1="./scratch/bootstrap/stage1"
STAGE_2="./scratch/bootstrap/stage2"
STAGE_3="./scratch/bootstrap/stage3"

stage1() {
    mkdir -p $STAGE_1
    local cflags="--std=gnu11 -Wpedantic -Wall -Wextra -g"
    gcc $cflags -c -o $STAGE_1/main.o main.c
    gcc $cflags -c -o $STAGE_1/codegen.o codegen.c
    gcc $cflags -c -o $STAGE_1/parse.o parse.c
    gcc $cflags -c -o $STAGE_1/tokenize.o tokenize.c
    gcc $cflags -c -o $STAGE_1/type.o type.c
    gcc $cflags -c -o $STAGE_1/builtins.o builtins.c
    gcc -static -o $STAGE_1/scc \
        $STAGE_1/main.o \
        $STAGE_1/codegen.o \
        $STAGE_1/parse.o \
        $STAGE_1/tokenize.o \
        $STAGE_1/type.o \
        $STAGE_1/builtins.o
}

stage2() {
    mkdir -p scratch/bootstrap/stage2
    local scc="$STAGE_1/scc"
    $scc main.c -o $STAGE_2/main.s
    gcc -c -o $STAGE_2/main.o $STAGE_2/main.s
    $scc parse.c -o $STAGE_2/parse.s
    gcc -c -o $STAGE_2/parse.o $STAGE_2/parse.s
    cp $STAGE_1/codegen.o $STAGE_2/codegen.o
    cp $STAGE_1/tokenize.o $STAGE_2/tokenize.o
    cp $STAGE_1/type.o $STAGE_2/type.o
    cp $STAGE_1/builtins.o $STAGE_2/builtins.o
    gcc -static -o $STAGE_2/scc \
        $STAGE_2/main.o \
        $STAGE_2/codegen.o \
        $STAGE_2/parse.o \
        $STAGE_2/tokenize.o \
        $STAGE_2/type.o \
        $STAGE_2/builtins.o
}

stage3() {
    mkdir -p scratch/bootstrap/stage3
    local scc="$STAGE_2/scc"
    $scc main.c -o $STAGE_3/main.s
    gcc -c -o $STAGE_3/main.o $STAGE_3/main.s
    cp $STAGE_2/codegen.o $STAGE_3/codegen.o
    cp $STAGE_2/parse.o $STAGE_3/parse.o
    cp $STAGE_2/tokenize.o $STAGE_3/tokenize.o
    cp $STAGE_2/type.o $STAGE_3/type.o
    cp $STAGE_2/builtins.o $STAGE_3/builtins.o
    gcc -static -o $STAGE_3/scc \
        $STAGE_3/main.o \
        $STAGE_3/codegen.o \
        $STAGE_3/parse.o \
        $STAGE_3/tokenize.o \
        $STAGE_3/type.o \
        $STAGE_3/builtins.o
    diff $STAGE_2/scc $STAGE_3/scc
}

main() {
    mkdir -p scratch/bootstrap
    stage1
    stage2
    stage3
    cp $STAGE_3/scc ./scc
}

main