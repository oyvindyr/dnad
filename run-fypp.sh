fypp -I src/macros/overload_implementations -M src/macros -m dnad test/test_hdual1_mod.fypp test/test_hdual1_mod.f90
fypp -I src/macros/overload_implementations -M src/macros -m dnad test/test_hdual2_mod.fypp test/test_hdual2_mod.f90
fypp -I src/macros/overload_implementations -M src/macros -m dnad test/test_hdual_chain_mod.fypp test/test_hdual_chain_mod.f90
fypp -I src/macros/overload_implementations -M src/macros -m dnad example/example_hdual_mod.fypp example/example_hdual_mod.f90
fypp -I src/macros/overload_implementations -M src/macros -m dnad example/example_dual_undef_mod.fypp example/example_dual_undef_mod.f90
