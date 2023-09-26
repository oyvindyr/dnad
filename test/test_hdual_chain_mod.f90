module test_hdual_chain_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: test_hdual_chain

    integer, parameter :: ny = 2
    integer, parameter :: nx = 3

    
    type :: dual__y_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(ny) = 0  ! derivatives
    end type
    type :: hdual__y_t
        !! hyper-dual number type
        sequence
        type(dual__y_t) :: d  ! dual number
        real(dp) :: h(ny*(ny + 1)/2) = 0  ! Lower triangular of Hessian
    end type
    type :: dual__x_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(nx) = 0  ! derivatives
    end type
    type :: hdual__x_t
        !! hyper-dual number type
        sequence
        type(dual__x_t) :: d  ! dual number
        real(dp) :: h(nx*(nx + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface  f_of_y
        module procedure f_of_y__x ! Function of y, with derivatives wrt x
        module procedure f_of_y__y ! Function of y, with derivatives wrt y
    end interface

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize__d_x_scalar
        module procedure initialize__d_x_vector
        module procedure initialize__hd_x_scalar
        module procedure initialize__hd_x_vector
        module procedure initialize__d_y_scalar
        module procedure initialize__d_y_vector
        module procedure initialize__hd_y_scalar
        module procedure initialize__hd_y_vector
    end interface
    interface display
        !! Pretty-print one or more dual or hyper dual numbers
        module procedure display__d_x_1input
        module procedure display__d_x_1input_vec
        module procedure display__d_x_1input_mat
        module procedure display__d_x_2input
        module procedure display__hd_x_1input
        module procedure display__hd_x_1input_vec
        module procedure display__hd_x_2input
        module procedure display__d_y_1input
        module procedure display__d_y_1input_vec
        module procedure display__d_y_1input_mat
        module procedure display__d_y_2input
        module procedure display__hd_y_1input
        module procedure display__hd_y_1input_vec
        module procedure display__hd_y_2input
    end interface
    interface fvalue
        !! Extract function value from a dual or hyper-dual number
        module procedure fvalue__d_x
        module procedure fvalue__d_x_r1
        module procedure fvalue__d_x_r2
        module procedure fvalue__hd_x
        module procedure fvalue__hd_x_r1
        module procedure fvalue__hd_x_r2
        module procedure fvalue__d_y
        module procedure fvalue__d_y_r1
        module procedure fvalue__d_y_r2
        module procedure fvalue__hd_y
        module procedure fvalue__hd_y_r1
        module procedure fvalue__hd_y_r2
    end interface

    interface gradient
        !! Extract gradient from a dual or hyper-dual number
        module procedure gradient__d_x
        module procedure gradient__hd_x
        module procedure gradient__d_y
        module procedure gradient__hd_y
    end interface

    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian__hd_x
        module procedure hessian__hd_y
    end interface
    interface jacobi_tr
        !! Extract transpose of Jacobi matrix from a vector of dual or hyper-dual numbers.
        !! - A hyper-dual vector results in a dual matrix.
        !! - A dual vector results in a real matrix.
        module procedure jacobi_tr__d_x
        module procedure jacobi_tr__hd_x
        module procedure jacobi_tr__d_y
        module procedure jacobi_tr__hd_y
    end interface
    interface chain_duals
        !! Generic function for converting a dual or hyper-dual number from one dual-type to another by applying the chain rule of derivation
        module procedure chain_duals__d_y_x
        module procedure chain_duals__hd_y_x
    end interface
    interface assignment (=)
        module procedure assign__dx_i  ! dual=integer, elemental
        module procedure assign__dx_r  ! dual=real, elemental
        module procedure assign__i_dx  ! integer=dual, elemental
        module procedure assign__r_dx  ! real=dual, elemental
        module procedure assign__dy_i  ! dual=integer, elemental
        module procedure assign__dy_r  ! dual=real, elemental
        module procedure assign__i_dy  ! integer=dual, elemental
        module procedure assign__r_dy  ! real=dual, elemental
        module procedure assign__hdx_i  ! dual=integer, elemental
        module procedure assign__hdx_r  ! dual=real, elemental
        module procedure assign__i_hdx  ! integer=dual, elemental
        module procedure assign__r_hdx  ! real=dual, elemental
        module procedure assign__hdy_i  ! dual=integer, elemental
        module procedure assign__hdy_r  ! dual=real, elemental
        module procedure assign__i_hdy  ! integer=dual, elemental
        module procedure assign__r_hdy  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure add__dx   ! +dual number, elemental
        module procedure add__dx_dx       ! dual + dual, elemental
        module procedure add__dx_i       ! dual + integer, elemental
        module procedure add__dx_r       ! dual + real, elemental
        module procedure add__i_dx       ! integer + dual, elemental
        module procedure add__r_dx       ! real + dual, elemental
        module procedure add__dy   ! +dual number, elemental
        module procedure add__dy_dy       ! dual + dual, elemental
        module procedure add__dy_i       ! dual + integer, elemental
        module procedure add__dy_r       ! dual + real, elemental
        module procedure add__i_dy       ! integer + dual, elemental
        module procedure add__r_dy       ! real + dual, elemental
        module procedure add__hdx  ! +dual number, elemental
        module procedure add__hdx_hdx     ! dual + dual, elemental
        module procedure add__hdx_i      ! dual + integer, elemental
        module procedure add__hdx_r      ! dual + real, elemental
        module procedure add__i_hdx      ! integer + dual, elemental
        module procedure add__r_hdx      ! real + dual, elemental
        module procedure add__hdy  ! +dual number, elemental
        module procedure add__hdy_hdy     ! dual + dual, elemental
        module procedure add__hdy_i      ! dual + integer, elemental
        module procedure add__hdy_r      ! dual + real, elemental
        module procedure add__i_hdy      ! integer + dual, elemental
        module procedure add__r_hdy      ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure minus__dx  ! negate a dual number,elemental
        module procedure minus__dx_dx      ! dual -dual,elemental
        module procedure minus__dx_i      ! dual-integer,elemental
        module procedure minus__dx_r      ! dual-real,elemental
        module procedure minus__i_dx      ! integer-dual,elemental
        module procedure minus__r_dx      ! real-dual,elemental
        module procedure minus__dy  ! negate a dual number,elemental
        module procedure minus__dy_dy      ! dual -dual,elemental
        module procedure minus__dy_i      ! dual-integer,elemental
        module procedure minus__dy_r      ! dual-real,elemental
        module procedure minus__i_dy      ! integer-dual,elemental
        module procedure minus__r_dy      ! real-dual,elemental
        module procedure minus__hdx ! negate a dual number,elemental
        module procedure minus__hdx_hdx    ! dual -dual,elemental
        module procedure minus__hdx_i     ! dual-integer,elemental
        module procedure minus__hdx_r     ! dual-real,elemental
        module procedure minus__i_hdx     ! integer-dual,elemental
        module procedure minus__r_hdx     ! real-dual,elemental
        module procedure minus__hdy ! negate a dual number,elemental
        module procedure minus__hdy_hdy    ! dual -dual,elemental
        module procedure minus__hdy_i     ! dual-integer,elemental
        module procedure minus__hdy_r     ! dual-real,elemental
        module procedure minus__i_hdy     ! integer-dual,elemental
        module procedure minus__r_hdy     ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult__dx_dx    ! dual*dual, elemental
        module procedure mult__dx_i    ! dual*integer,elemental
        module procedure mult__dx_r    ! dual*real,elemental
        module procedure mult__i_dx    ! integer*dual,elemental
        module procedure mult__r_dx    ! real*dual,elemental
        module procedure mult__dy_dy    ! dual*dual, elemental
        module procedure mult__dy_i    ! dual*integer,elemental
        module procedure mult__dy_r    ! dual*real,elemental
        module procedure mult__i_dy    ! integer*dual,elemental
        module procedure mult__r_dy    ! real*dual,elemental
        module procedure mult__hdx_hdx  ! dual*dual, elemental
        module procedure mult__hdx_i   ! dual*integer,elemental
        module procedure mult__hdx_r   ! dual*real,elemental
        module procedure mult__i_hdx   ! integer*dual,elemental
        module procedure mult__r_hdx   ! real*dual,elemental
        module procedure mult__hdy_hdy  ! dual*dual, elemental
        module procedure mult__hdy_i   ! dual*integer,elemental
        module procedure mult__hdy_r   ! dual*real,elemental
        module procedure mult__i_hdy   ! integer*dual,elemental
        module procedure mult__r_hdy   ! real*dual,elemental
    end interface
    interface operator (**)
        module procedure pow__dx_i ! dual number to an integer power,elemental
        module procedure pow__dx_r ! dual number to a real power, elemental
        module procedure pow__dx_dx ! dual number to a dual power, elemental
        module procedure pow__dy_i ! dual number to an integer power,elemental
        module procedure pow__dy_r ! dual number to a real power, elemental
        module procedure pow__dy_dy ! dual number to a dual power, elemental
        module procedure pow__hdx_i ! hdual number to an integer power,elemental
        module procedure pow__hdx_r ! hdual number to a real power, elemental
        module procedure pow__hdx_hdx ! hdual number to a hdual power, elemental
        module procedure pow__hdy_i ! hdual number to an integer power,elemental
        module procedure pow__hdy_r ! hdual number to a real power, elemental
        module procedure pow__hdy_hdy ! hdual number to a hdual power, elemental
    end interface
    interface log
        module procedure log__dx ! log of a dual number, elemental
        module procedure log__dy ! log of a dual number, elemental
        module procedure log__hdx ! log of a dual number, elemental
        module procedure log__hdy ! log of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt__dx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt__dy ! obtain the sqrt of a dual number, elemental
        module procedure sqrt__hdx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt__hdy ! obtain the sqrt of a dual number, elemental
    end interface
    
contains

    function test_hdual_chain() result(is_ok)
        logical :: is_ok
           
        real(dp), parameter :: abs_tol = 1.0e-14_dp

        integer :: i

        real(dp) :: x(nx)

        type(hdual__y_t) :: fy
        type(hdual__y_t) :: yy(ny)

        type(hdual__x_t) :: fx
        type(hdual__x_t) :: yx(ny)
        type(hdual__x_t) :: xx(nx)

        type(hdual__x_t) :: fx_chain

        ! random x
        call random_number(x)
        x = 5*x + 0.1_dp

        ! Initialize xx, representing x as a function of x, with derivatives wrt x
        call initialize(xx, x)
        
        ! y as a function of x, with derivatives wrt x
        yx = y_of_x(xx)

        ! y as a function of y, with derivatives wrt y
        call initialize(yy, fvalue(yx))

        ! f as a function of y, with derivatives wrt y
        fy = f_of_y(yy)

        ! f as a function of x, with derivatives wrt x
        fx_chain = chain_duals(fy, yx)

        ! Same as fx_chain, but computed directly
        fx = f_of_x(xx)

        print*," "
        call display(yy, "yy")
        print*," "
        call display(fy, "fy")
        print*," "
        call display(fx, fx_chain, "fx", "fx_chain")


        print*," "
        call display(yx, "yx")
        print*," "
        call display(jacobi_tr(yx), "yxjt")

        is_ok = .true.
        if (abs(fvalue(fx_chain) - fvalue(fx)) > abs_tol) then
            is_ok = .false.
        end if
        if (maxval(abs(gradient(fx_chain) - gradient(fx))) > abs_tol) then
            is_ok = .false.
        end if
        if (maxval(abs(fx_chain%h - fx%h)) > abs_tol) then
            is_ok = .false.
        end if

        


    end function

    function f_of_y__x(y) result(f)
        type(hdual__x_t), intent(in) :: y(ny)
        type(hdual__x_t) :: f
        f = sqrt(y(1))*log(y(2))
    end function
    function f_of_y__y(y) result(f)
        type(hdual__y_t), intent(in) :: y(ny)
        type(hdual__y_t) :: f
        f = sqrt(y(1))*log(y(2))
    end function

    function y_of_x(x) result(y)
        type(hdual__x_t), intent(in) :: x(nx)
        type(hdual__x_t) :: y(ny)
        y(1) = sqrt(x(1)**2 + x(2)**2)
        y(2) = x(3)
    end function

    function f_of_x(x) result(f)
        type(hdual__x_t), intent(in) :: x(nx)
        type(hdual__x_t) :: f
        f = f_of_y(y_of_x(x))
    end function

    pure subroutine initialize__d_x_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual__x_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize__d_x_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual__x_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize__hd_x_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual__x_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%d%f = val
        hdual%d%g = 0
        hdual%h = 0
        hdual%d%g(idiff) = 1

    end subroutine
    pure subroutine initialize__hd_x_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual__x_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%d%f = val(i)
            hdual(i)%d%g = 0
            hdual(i)%h = 0
            hdual(i)%d%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize__d_y_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual__y_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize__d_y_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual__y_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize__hd_y_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual__y_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%d%f = val
        hdual%d%g = 0
        hdual%h = 0
        hdual%d%g(idiff) = 1

    end subroutine
    pure subroutine initialize__hd_y_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual__y_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%d%f = val(i)
            hdual(i)%d%g = 0
            hdual(i)%h = 0
            hdual(i)%d%g(i) = 1
        end do

    end subroutine
    subroutine display__d_x_1input(d, name)
        type(dual__x_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i

        print*, "Function values:"
        print*, " ",name,"%f"
        print '(ES22.14)', d%f

        print*, "Derivatives:"
        print*, " i    ", name, "%g(i)"
        do i = 1, size(d%g)
            print '(I3, ES22.14)' , i, d%g(i)
        end do
    end subroutine
    subroutine display__d_x_1input_vec(d, name)
        type(dual__x_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, p

        print*, "Function values:"
        print*, " ",name,"(p)%f"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%f
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%g(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%g)
                print '(I3, I3, ES22.14)', p, i, d(p)%g(i)
            end do
        end do
    end subroutine
    subroutine display__d_x_1input_mat(d, name)
        type(dual__x_t), intent(in) :: d(:, :)
        character(len=*), intent(in) :: name
        integer :: i, p, q

        print*, "Function values:"
        print*, " ",name,"(p,q)%f"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                print '(I3, I3, ES22.14)', p, q, d(p, q)%f
            end do
        end do

        print*, "Derivatives:"
        print*, " p  q  i    ", name, "(p)%g(i)"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                do i = 1, size(d(1, 1)%g)
                    print '(I3, I3, I3, ES22.14)', p, q, i, d(p,q)%g(i)
                end do
            end do
        end do
    end subroutine
    subroutine display__d_x_2input(d1, d2, name1, name2)
        type(dual__x_t), intent(in) :: d1
        type(dual__x_t), intent(in) :: d2
        character(len=*), intent(in) :: name1
        character(len=*), intent(in) :: name2
        integer :: i

        print*, "Function values:"
        print*, " ",name1, "%f                  ", name2, "%f"
        print '(ES22.14, ES22.14)', d1%f, d2%f

        print*, "Derivatives:"
        print*, " i    ", name1, "%g(i)                ", name2, "%g(i)"
        do i = 1, size(d1%g)
            print '(I3, ES22.14, ES22.14)' , i, d1%g(i), d2%g(i)
        end do
    end subroutine
    subroutine display__hd_x_1input(d, name)
        type(hdual__x_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i, j, k

        print*, "Function values:"
        print*, " ",name,"%d%f"
        print '(ES22.14)', d%d%f

        print*, "Derivatives:"
        print*, " i    ", name, "%d%g(i)"
        do i = 1, size(d%d%g)
            print '(I3, ES22.14)' , i, d%d%g(i)
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " k  i  j   ", name, "%h(k)"
        k = 0
        do j = 1, size(d%d%g)
            do i = j, size(d%d%g)
                k = k + 1
                print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d%h(k)
            end do
        end do

    end subroutine
    subroutine display__hd_x_1input_vec(d, name)
        type(hdual__x_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, j, k, p

        print*, "Function values:"
        print*, " ",name,"(p)%d%f"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%d%f
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%d%g(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%d%g)
                print '(I3, I3, ES22.14)', p, i, d(p)%d%g(i)
            end do
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " p  k  i  j   ", name, "(p)%h(k)"
        do p = 1, size(d)
            k = 0
            do j = 1, size(d(1)%d%g)
                do i = j, size(d(1)%d%g)
                    k = k + 1
                    print '(I3, I3, I3, I3, ES22.14, ES22.14)', p, k, i, j, d(p)%h(k)
                end do
            end do
        end do

    end subroutine
    subroutine display__hd_x_2input(d1, d2, name1, name2)
      type(hdual__x_t), intent(in) :: d1
      type(hdual__x_t), intent(in) :: d2
      character(len=*), intent(in) :: name1
      character(len=*), intent(in) :: name2
      integer :: i, j, k

        print*, "Function values:"
        print*, " ",name1, "%d%f                  ", name2, "%d%f"
        print '(ES22.14, ES22.14)', d1%d%f, d2%d%f

        print*, "Derivatives:"
        print*, " i    ", name1, "%d%g(i)                ", name2, "%d%g(i)"
        do i = 1, size(d1%d%g)
            print '(I3, ES22.14, ES22.14)' , i, d1%d%g(i), d2%d%g(i)
        end do

      print*, "Lower triangular of Hessian matrix, h(i,j)"
      print*, " k  i  j   ", name1, "%h(k)            ", name2, "%h(k)"
      k = 0
      do j = 1, size(d1%d%g)
          do i = j, size(d1%d%g)
              k = k + 1
              print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d1%h(k), d2%h(k)
          end do
      end do

    end subroutine
    subroutine display__d_y_1input(d, name)
        type(dual__y_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i

        print*, "Function values:"
        print*, " ",name,"%f"
        print '(ES22.14)', d%f

        print*, "Derivatives:"
        print*, " i    ", name, "%g(i)"
        do i = 1, size(d%g)
            print '(I3, ES22.14)' , i, d%g(i)
        end do
    end subroutine
    subroutine display__d_y_1input_vec(d, name)
        type(dual__y_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, p

        print*, "Function values:"
        print*, " ",name,"(p)%f"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%f
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%g(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%g)
                print '(I3, I3, ES22.14)', p, i, d(p)%g(i)
            end do
        end do
    end subroutine
    subroutine display__d_y_1input_mat(d, name)
        type(dual__y_t), intent(in) :: d(:, :)
        character(len=*), intent(in) :: name
        integer :: i, p, q

        print*, "Function values:"
        print*, " ",name,"(p,q)%f"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                print '(I3, I3, ES22.14)', p, q, d(p, q)%f
            end do
        end do

        print*, "Derivatives:"
        print*, " p  q  i    ", name, "(p)%g(i)"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                do i = 1, size(d(1, 1)%g)
                    print '(I3, I3, I3, ES22.14)', p, q, i, d(p,q)%g(i)
                end do
            end do
        end do
    end subroutine
    subroutine display__d_y_2input(d1, d2, name1, name2)
        type(dual__y_t), intent(in) :: d1
        type(dual__y_t), intent(in) :: d2
        character(len=*), intent(in) :: name1
        character(len=*), intent(in) :: name2
        integer :: i

        print*, "Function values:"
        print*, " ",name1, "%f                  ", name2, "%f"
        print '(ES22.14, ES22.14)', d1%f, d2%f

        print*, "Derivatives:"
        print*, " i    ", name1, "%g(i)                ", name2, "%g(i)"
        do i = 1, size(d1%g)
            print '(I3, ES22.14, ES22.14)' , i, d1%g(i), d2%g(i)
        end do
    end subroutine
    subroutine display__hd_y_1input(d, name)
        type(hdual__y_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i, j, k

        print*, "Function values:"
        print*, " ",name,"%d%f"
        print '(ES22.14)', d%d%f

        print*, "Derivatives:"
        print*, " i    ", name, "%d%g(i)"
        do i = 1, size(d%d%g)
            print '(I3, ES22.14)' , i, d%d%g(i)
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " k  i  j   ", name, "%h(k)"
        k = 0
        do j = 1, size(d%d%g)
            do i = j, size(d%d%g)
                k = k + 1
                print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d%h(k)
            end do
        end do

    end subroutine
    subroutine display__hd_y_1input_vec(d, name)
        type(hdual__y_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, j, k, p

        print*, "Function values:"
        print*, " ",name,"(p)%d%f"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%d%f
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%d%g(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%d%g)
                print '(I3, I3, ES22.14)', p, i, d(p)%d%g(i)
            end do
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " p  k  i  j   ", name, "(p)%h(k)"
        do p = 1, size(d)
            k = 0
            do j = 1, size(d(1)%d%g)
                do i = j, size(d(1)%d%g)
                    k = k + 1
                    print '(I3, I3, I3, I3, ES22.14, ES22.14)', p, k, i, j, d(p)%h(k)
                end do
            end do
        end do

    end subroutine
    subroutine display__hd_y_2input(d1, d2, name1, name2)
      type(hdual__y_t), intent(in) :: d1
      type(hdual__y_t), intent(in) :: d2
      character(len=*), intent(in) :: name1
      character(len=*), intent(in) :: name2
      integer :: i, j, k

        print*, "Function values:"
        print*, " ",name1, "%d%f                  ", name2, "%d%f"
        print '(ES22.14, ES22.14)', d1%d%f, d2%d%f

        print*, "Derivatives:"
        print*, " i    ", name1, "%d%g(i)                ", name2, "%d%g(i)"
        do i = 1, size(d1%d%g)
            print '(I3, ES22.14, ES22.14)' , i, d1%d%g(i), d2%d%g(i)
        end do

      print*, "Lower triangular of Hessian matrix, h(i,j)"
      print*, " k  i  j   ", name1, "%h(k)            ", name2, "%h(k)"
      k = 0
      do j = 1, size(d1%d%g)
          do i = j, size(d1%d%g)
              k = k + 1
              print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d1%h(k), d2%h(k)
          end do
      end do

    end subroutine


    pure function fvalue__d_x(d) result(f)
        type(dual__x_t), intent(in) :: d
        real(dp) :: f
        f = d%f
    end function
    pure function fvalue__d_x_r1(d) result(f)
        type(dual__x_t), intent(in) :: d(:)
        real(dp) :: f(size(d))
        f = d%f
    end function
    pure function fvalue__d_x_r2(d) result(f)
        type(dual__x_t), intent(in) :: d(:,:)
        real(dp) :: f(size(d,1),size(d,2))
        f = d%f
    end function
    pure function fvalue__hd_x(hd) result(f)
        type(hdual__x_t), intent(in) :: hd
        real(dp) :: f
        f = hd%d%f
    end function
    pure function fvalue__hd_x_r1(hd) result(f)
        type(hdual__x_t), intent(in) :: hd(:)
        real(dp) :: f(size(hd))
        f = hd%d%f
    end function
    pure function fvalue__hd_x_r2(hd) result(f)
        type(hdual__x_t), intent(in) :: hd(:,:)
        real(dp) :: f(size(hd,1),size(hd,2))
        f = hd%d%f
    end function
    pure function fvalue__d_y(d) result(f)
        type(dual__y_t), intent(in) :: d
        real(dp) :: f
        f = d%f
    end function
    pure function fvalue__d_y_r1(d) result(f)
        type(dual__y_t), intent(in) :: d(:)
        real(dp) :: f(size(d))
        f = d%f
    end function
    pure function fvalue__d_y_r2(d) result(f)
        type(dual__y_t), intent(in) :: d(:,:)
        real(dp) :: f(size(d,1),size(d,2))
        f = d%f
    end function
    pure function fvalue__hd_y(hd) result(f)
        type(hdual__y_t), intent(in) :: hd
        real(dp) :: f
        f = hd%d%f
    end function
    pure function fvalue__hd_y_r1(hd) result(f)
        type(hdual__y_t), intent(in) :: hd(:)
        real(dp) :: f(size(hd))
        f = hd%d%f
    end function
    pure function fvalue__hd_y_r2(hd) result(f)
        type(hdual__y_t), intent(in) :: hd(:,:)
        real(dp) :: f(size(hd,1),size(hd,2))
        f = hd%d%f
    end function

    pure function gradient__d_x(d) result(g)
        type(dual__x_t), intent(in) :: d
        real(dp) :: g(size(d%g))
        g = d%g
    end function
    pure function gradient__hd_x(hd) result(g)
        type(hdual__x_t), intent(in) :: hd
        real(dp) :: g(size(hd%d%g))
        g = hd%d%g
    end function
    pure function gradient__d_y(d) result(g)
        type(dual__y_t), intent(in) :: d
        real(dp) :: g(size(d%g))
        g = d%g
    end function
    pure function gradient__hd_y(hd) result(g)
        type(hdual__y_t), intent(in) :: hd
        real(dp) :: g(size(hd%d%g))
        g = hd%d%g
    end function

    pure function hessian__hd_x(hd) result(m)
        type(hdual__x_t), intent(in) :: hd
        real(dp) :: m(size(hd%d%g), size(hd%d%g))
        
        integer i, j, k

        k = 0
        do j = 1, size(hd%d%g)
            k = k + 1
            m(j, j) = hd%h(k)
            do i = j+1, size(hd%d%g)
                k = k + 1
                m(i, j) = hd%h(k)
                m(j, i) = hd%h(k)
            end do
        end do

    end function
    pure function hessian__hd_y(hd) result(m)
        type(hdual__y_t), intent(in) :: hd
        real(dp) :: m(size(hd%d%g), size(hd%d%g))
        
        integer i, j, k

        k = 0
        do j = 1, size(hd%d%g)
            k = k + 1
            m(j, j) = hd%h(k)
            do i = j+1, size(hd%d%g)
                k = k + 1
                m(i, j) = hd%h(k)
                m(j, i) = hd%h(k)
            end do
        end do

    end function
    pure function jacobi_tr__d_x(x) result(y)
        type(dual__x_t), intent(in) :: x(:)
        real(dp) :: y(size(x(1)%g),size(x))

        integer :: j

        do j = 1, size(x)
            y(:,j) = x(j)%g
        end do

    end function
    pure function jacobi_tr__hd_x(x) result(y)
        type(hdual__x_t), intent(in) :: x(:)
        type(dual__x_t) :: y(size(x(1)%d%g),size(x))

        real(dp) :: hess(size(x(1)%d%g), size(x(1)%d%g))
        integer :: i, j

        do j = 1, size(x)
            hess = hessian(x(j))
            do i = 1, size(x(1)%d%g)
                y(i,j)%f = x(j)%d%g(i)
                y(i,j)%g = hess(:,i)
            end do
        end do

    end function
    pure function jacobi_tr__d_y(x) result(y)
        type(dual__y_t), intent(in) :: x(:)
        real(dp) :: y(size(x(1)%g),size(x))

        integer :: j

        do j = 1, size(x)
            y(:,j) = x(j)%g
        end do

    end function
    pure function jacobi_tr__hd_y(x) result(y)
        type(hdual__y_t), intent(in) :: x(:)
        type(dual__y_t) :: y(size(x(1)%d%g),size(x))

        real(dp) :: hess(size(x(1)%d%g), size(x(1)%d%g))
        integer :: i, j

        do j = 1, size(x)
            hess = hessian(x(j))
            do i = 1, size(x(1)%d%g)
                y(i,j)%f = x(j)%d%g(i)
                y(i,j)%g = hess(:,i)
            end do
        end do

    end function
    pure function chain_duals__d_y_x(fy, yx) result(fx)
        !! Convert a function value from type(dual__y_t) to type(dual__x_t)
        !! by applying the chain rule of derivation. Second input yx(:) has type(dual__x_t)
        !! and represents the design variables by which the derivatives of fy is taken with respect to.
        !! Thus, size(yx) needs to be equal to size(fy%g) (size known at compile time).
         
        ! This function has been automatically generated by dnad_chain_duals.fypp
        type(dual__y_t), intent(in) :: fy
        type(dual__x_t), intent(in) :: yx(size(fy%g))
        type(dual__x_t) :: fx
        
        integer :: p, j

        fx%f = fy%f
        fx%g = 0
        do p = 1, size(fy%g)
            do j = 1, size(fx%g)
                fx%g(j) = fx%g(j) + fy%g(p)*yx(p)%g(j)
            end do
        end do


    end function
    pure function chain_duals__hd_y_x(fy, yx) result(fx)
        !! Convert a function value from type(hdual__y_t) to type(hdual__x_t)
        !! by applying the chain rule of derivation. Second input yx(:) has type(hdual__x_t)
        !! and represents the design variables by which the derivatives of fy is taken with respect to.
        !! Thus, size(yx) needs to be equal to size(fy%g) (size known at compile time).
         
        ! This function has been automatically generated by dnad_chain_duals.fypp
        type(hdual__y_t), intent(in) :: fy
        type(hdual__x_t), intent(in) :: yx(size(fy%d%g))
        type(hdual__x_t) :: fx
        
        integer :: p, j
        integer :: i, q, k, nk
        real(dp) :: hfy(size(fy%d%g), size(fy%d%g))
        real(dp) :: tmp

        fx%d%f = fy%d%f
        fx%d%g = 0
        do p = 1, size(fy%d%g)
            do j = 1, size(fx%d%g)
                fx%d%g(j) = fx%d%g(j) + fy%d%g(p)*yx(p)%d%g(j)
            end do
        end do

        nk = size(fx%d%g)*(size(fx%d%g)+1)/2
        fx%h = 0
        do p = 1, size(fy%d%g)
            do k = 1, nk
                fx%h(k) = fx%h(k) + fy%d%g(p)*yx(p)%h(k)
            end do
        end do
        hfy = hessian(fy)
        do q = 1, size(fy%d%g)
            do p = 1, size(fy%d%g)
                k = 0
                do j = 1, size(fx%d%g)
                    tmp = hfy(p,q)*yx(q)%d%g(j)
                    do i = j, size(fx%d%g)
                        k = k + 1
                        fx%h(k) = fx%h(k) + tmp*yx(p)%d%g(i)
                    end do
                end do
            end do
        end do

    end function
    elemental subroutine assign__dx_i(u, i)
        type(dual__x_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__dx_r(u, r)
        type(dual__x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__i_dx(i, v)
        type(dual__x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign__r_dx(r, v)
        type(dual__x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental subroutine assign__dy_i(u, i)
        type(dual__y_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__dy_r(u, r)
        type(dual__y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__i_dy(i, v)
        type(dual__y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign__r_dy(r, v)
        type(dual__y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental subroutine assign__hdx_i(u, i)
        type(hdual__x_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__hdx_r(u, r)
        type(hdual__x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__i_hdx(i, v)
        type(hdual__x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)

    end subroutine
    elemental subroutine assign__r_hdx(r, v)
        type(hdual__x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f

    end subroutine
    elemental subroutine assign__hdy_i(u, i)
        type(hdual__y_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__hdy_r(u, r)
        type(hdual__y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__i_hdy(i, v)
        type(hdual__y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)

    end subroutine
    elemental subroutine assign__r_hdy(r, v)
        type(hdual__y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f

    end subroutine
    elemental function add__dx(u) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add__dx_dx(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add__dx_r(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__r_dx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__dx_i(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__i_dx(u, v) result(res)
        integer, intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__dy(u) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add__dy_dy(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add__dy_r(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__r_dy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__dy_i(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__i_dy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__hdx(u) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__hdx_hdx(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
    end function
    elemental function add__hdx_r(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__r_hdx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add__hdx_i(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__i_hdx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add__hdy(u) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__hdy_hdy(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
    end function
    elemental function add__hdy_r(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__r_hdy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add__hdy_i(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__i_hdy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function minus__dx(u) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t) :: res
        
        res%f = -u%f
        res%g = -u%g
    end function
    elemental function minus__dx_dx(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
    end function
    elemental function minus__dx_r(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__r_dx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__dx_i(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__i_dx(u, v) result(res)
        integer, intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__dy(u) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t) :: res
        
        res%f = -u%f
        res%g = -u%g
    end function
    elemental function minus__dy_dy(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
    end function
    elemental function minus__dy_r(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__r_dy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__dy_i(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__i_dy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__hdx(u) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t) :: res
        
        res%d%f = -u%d%f
        res%d%g = -u%d%g
        res%h = -u%h
    end function
    elemental function minus__hdx_hdx(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f - v%d%f
        res%d%g = u%d%g - v%d%g
        res%h = u%h - v%h
    end function
    elemental function minus__hdx_r(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__r_hdx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function minus__hdx_i(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__i_hdx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function minus__hdy(u) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t) :: res
        
        res%d%f = -u%d%f
        res%d%g = -u%d%g
        res%h = -u%h
    end function
    elemental function minus__hdy_hdy(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f - v%d%f
        res%d%g = u%d%g - v%d%g
        res%h = u%h - v%h
    end function
    elemental function minus__hdy_r(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__r_hdy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function minus__hdy_i(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__i_hdy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function mult__dx_dx(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
    end function
    elemental function mult__dx_r(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__r_dx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__dx_i(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__i_dx(u, v) result(res)
        integer, intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__dy_dy(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
    end function
    elemental function mult__dy_r(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__r_dy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__dy_i(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__i_dy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__hdx_hdx(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f*v%d%f
        res%d%g = u%d%g*v%d%f + u%d%f*v%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = u%h(k)*v%d%f + u%d%g(i)*v%d%g(j) + u%d%g(j)*v%d%g(i) + u%d%f*v%h(k)
            end do
        end do
    end function
    elemental function mult__hdx_r(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__r_hdx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult__hdx_i(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__i_hdx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult__hdy_hdy(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f*v%d%f
        res%d%g = u%d%g*v%d%f + u%d%f*v%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = u%h(k)*v%d%f + u%d%g(i)*v%d%g(j) + u%d%g(j)*v%d%g(i) + u%d%f*v%h(k)
            end do
        end do
    end function
    elemental function mult__hdy_r(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__r_hdy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult__hdy_i(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__i_hdy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function pow__dx_i(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dx_r(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__x_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dx_dx(u, v) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t), intent(in) :: v
        type(dual__x_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

    end function
    elemental function pow__dy_i(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dy_r(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__y_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dy_dy(u, v) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t), intent(in) :: v
        type(dual__y_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

    end function
    elemental function pow__hdx_i(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__x_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow__hdx_r(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__x_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow__hdx_hdx(u, v) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t), intent(in) :: v
        type(hdual__x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%d%f**v%d%f
        t1 = log(u%d%f)
        t2 = 1.0_dp/u%d%f
        t3 = t1*v%d%f + 1
        res%d%f = t0
        res%d%g = t0*(t1*v%d%g + t2*u%d%g*v%d%f)
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t0*(t1*v%h(k) + t2*u%h(k)*v%d%f + t2*u%d%g(j)*(t2*u%d%g(i)*v%d%f*(v%d%f - &
      1) + t3*v%d%g(i)) + v%d%g(j)*(t1**2*v%d%g(i) + t2*t3*u%d%g(i)))
            end do
        end do
    end function
    elemental function pow__hdy_i(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__y_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow__hdy_r(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__y_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow__hdy_hdy(u, v) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t), intent(in) :: v
        type(hdual__y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%d%f**v%d%f
        t1 = log(u%d%f)
        t2 = 1.0_dp/u%d%f
        t3 = t1*v%d%f + 1
        res%d%f = t0
        res%d%g = t0*(t1*v%d%g + t2*u%d%g*v%d%f)
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t0*(t1*v%h(k) + t2*u%h(k)*v%d%f + t2*u%d%g(j)*(t2*u%d%g(i)*v%d%f*(v%d%f - &
      1) + t3*v%d%g(i)) + v%d%g(j)*(t1**2*v%d%g(i) + t2*t3*u%d%g(i)))
            end do
        end do
    end function
    elemental function log__dx(u) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
    end function
    elemental function log__dy(u) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
    end function
    elemental function log__hdx(u) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%d%f
        res%d%f = log(u%d%f)
        res%d%g = t0*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t0*(-t0*u%d%g(i)*u%d%g(j) + u%h(k))
            end do
        end do
    end function
    elemental function log__hdy(u) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%d%f
        res%d%f = log(u%d%f)
        res%d%g = t0*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t0*(-t0*u%d%g(i)*u%d%g(j) + u%h(k))
            end do
        end do
    end function
    elemental function sqrt__dx(u) result(res)
        type(dual__x_t), intent(in) :: u
        type(dual__x_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
    end function

    elemental function sqrt__dy(u) result(res)
        type(dual__y_t), intent(in) :: u
        type(dual__y_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
    end function

    elemental function sqrt__hdx(u) result(res)
        type(hdual__x_t), intent(in) :: u
        type(hdual__x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%d%f)
        t1 = 1.0_dp/t0
        res%d%f = t0
        res%d%g = 0.5_dp*t1*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = (0.25_dp)*t1*(2*u%h(k) - u%d%g(i)*u%d%g(j)/u%d%f)
            end do
        end do
    end function
    elemental function sqrt__hdy(u) result(res)
        type(hdual__y_t), intent(in) :: u
        type(hdual__y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%d%f)
        t1 = 1.0_dp/t0
        res%d%f = t0
        res%d%g = 0.5_dp*t1*u%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = (0.25_dp)*t1*(2*u%h(k) - u%d%g(i)*u%d%g(j)/u%d%f)
            end do
        end do
    end function
    
    
end module