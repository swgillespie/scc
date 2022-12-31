# scc - sean's C compiler

A small C compiler. scc implements a subset of C11, small enough to compile with a pretty small codebase but large enough for this compiler to compile itself.

Compile with `make scc`, which uses `gcc` to produce a stage 0 compiler. Use
`make bootstrap` to do a three-stage bootstrap and produce a stage 3 compiler. `make test` runs tests with the stage 0 compiler, `make bootstrap-test` runs them with the stage 3 compiler. You'll want to run a `poetry install` once in order to install Python testing dependencies (i.e. llvm's `lit` and `filecheck`).

Note that this compiler does not bootstrap `diag.c`, `diag.c` exists only for pretty diagnostics and does not affect the behavior of the compiler. It uses `stdarg.h`, which `scc` does not implement, so `scc` does not compile it as part of the bootstrap. You can replace it with an implementation that doesn't use `stdarg.h`, but you get bad error messages and the compiletests fail (due to the error messages changing), so I opted to leave it.

`scc`'s codegen is bad and not performant, making it not bad is out of scope for this project.
