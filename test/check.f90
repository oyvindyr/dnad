program check
    use test_hdual1_mod, only : test_hdual1
    use test_hdual2_mod, only : test_hdual2
    use test_hdual_chain_mod, only : test_hdual_chain
    use test_all_stringent_mod, only : test_all_stringent
    use test_all_lenient_mod, only : test_all_lenient
    implicit none

    logical :: is_ok(5)

    is_ok(1) = test_hdual1()
    is_ok(2) = test_hdual2()
    is_ok(3) = test_hdual_chain()
    is_ok(4) = test_all_stringent()
    is_ok(5) = test_all_lenient()

    if (all(is_ok)) then
        print *, "dual/hdual tests: ok"
    else
        print *, "dual/hdual tests: failed"
        error stop 1
    end if

end program
