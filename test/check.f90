program check
    use test_hdual1_mod, only : test_hdual1
    use test_hdual2_mod, only : test_hdual2
    use test_hdual_chain_mod, only : test_hdual_chain
    implicit none

    logical :: is_ok(3)

    is_ok(1) = test_hdual1()
    is_ok(2) = test_hdual2()
    is_ok(3) = test_hdual_chain()

    if (all(is_ok)) then
        print *, "dual/hdual tests: ok"
    else
        print *, "dual/hdual tests: failed"
    end if
    
end program