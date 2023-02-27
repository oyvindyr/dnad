# dnad

This is a fork of https://github.com/joddlehod/dnad.git

In this fork, the dnad repository have been refactored into fypp macros. See https://fypp.readthedocs.io/en/stable/index.html

The benefit of this approach is that interfaces and implementations of mathematical functions can be "injected" into
the modules where they is used. Thereby inlining can be achieved without resorting to inter-procedural optimizations. 

