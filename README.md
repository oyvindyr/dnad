# dnad

This is a fork of https://github.com/joddlehod/dnad.git

In this fork, the dnad repository have been refactored into fypp macros. See https://fypp.readthedocs.io/en/stable/index.html

The benefit of this approach is that interfaces and implementations of mathematical functions can be "injected" into
the modules where they is used. Thereby inlining can be achieved without resorting to inter-procedural optimizations. 

Alternatively, you can import dnadN_mod.f90 into your module (replace N with integer). These files can be autogenerated by fypp using:
```
fypp -M src/macros -m dnad --define=number_of_derivatives=3 src/dnad_mod.fypp src/dnad3_mod.f90
```

Preprocess unit test with:
```
fypp -I src/macros/overload_implementations -M src/macros -m dnad test/test_hdual_mod.fypp test/test_hdual_mod.f90
```
