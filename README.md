# dnad

This is a fork of https://github.com/joddlehod/dnad.git

In this fork, the dnad repository have been refactored into fypp macros. See https://fypp.readthedocs.io/en/stable/index.html

The benefit of this approach is that interfaces and implementations of mathematical functions can be "injected" into
the modules where they is used. Thereby inlining can be achieved without resorting to inter-procedural optimizations. 

## Tests

`test/gen_tests.py` writes a test of every procedure the module macros emit, with reference values from sympy.
`test/coverage.py` builds them with gcov and fails unless every line of those procedures ran.

    uv run --group dev python test/gen_tests.py
    uv run python test/coverage.py
    fpm test
