program check
    use test_ddual_mod, only : test_ddual
    implicit none

    logical :: is_ok

    is_ok = test_ddual()

    if (is_ok) then
        print *, "test_ddual: ok"
    else
        print *, "test_ddual: failed"
    end if
    
end program