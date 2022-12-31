# scc - sean's C compiler

A small C compiler. scc implements a subset of C11, small enough to compile with a pretty small codebase but large enough for this compiler to compile itself.

Compile with `make scc`, which uses `gcc` to produce a stage 0 compiler. Use
`make bootstrap` to do a three-stage bootstrap and produce a stage 3 compiler. `make test` runs tests with the stage 0 compiler, `make bootstrap-test` runs them with the stage 3 compiler. You'll want to run a `poetry install` once in order to install Python testing dependencies (i.e. llvm's `lit` and `filecheck`).

`scc`'s codegen is bad and not performant, making it not bad is out of scope for this project.