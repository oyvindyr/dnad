module test_hdual_chain_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: test_hdual_chain

    integer, parameter :: ny = 2
    integer, parameter :: nx = 3

    
    type :: dual_y_t
        !! dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(ny) = 0  ! derivatives
    end type
    type :: dual_x_t
        !! dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(nx) = 0  ! derivatives
    end type
    type :: hdual_y_t
        !! hyper-dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(ny) = 0  ! derivatives
        real(dp) :: ddx(ny*(ny + 1)/2) = 0  ! Lower triangular of Hessian
    end type
    type :: hdual_x_t
        !! hyper-dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(nx) = 0  ! derivatives
        real(dp) :: ddx(nx*(nx + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface  f_of_y
        module procedure f_of_y__x ! Function of y, with derivatives wrt x
        module procedure f_of_y__y ! Function of y, with derivatives wrt y
    end interface

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize_dualx_scalar
        module procedure initialize_dualx_vector
        module procedure initialize_dualy_scalar
        module procedure initialize_dualy_vector
        module procedure initialize_hdualx_scalar
        module procedure initialize_hdualx_vector
        module procedure initialize_hdualy_scalar
        module procedure initialize_hdualy_vector
    end interface
    interface display
        !! Pretty-print one or more dual or hyper dual numbers
        module procedure display__dualx_1input
        module procedure display__dualx_1input_vec
        module procedure display__dualx_1input_mat
        module procedure display__dualx_2input
        module procedure display__dualy_1input
        module procedure display__dualy_1input_vec
        module procedure display__dualy_1input_mat
        module procedure display__dualy_2input
        module procedure display__hdualx_1input
        module procedure display__hdualx_1input_vec
        module procedure display__hdualx_2input
        module procedure display__hdualy_1input
        module procedure display__hdualy_1input_vec
        module procedure display__hdualy_2input
    end interface
    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian_hdualx
        module procedure hessian_hdualy
    end interface
    interface jacobi_tr
        !! Extract transpose of Jacobi matrix from a vector of dual or hyper-dual numbers.
        !! - A hyper-dual vector results in a dual matrix.
        !! - A dual vector results in a real matrix.
        module procedure jacobi_tr__dualx
        module procedure jacobi_tr__dualy
        module procedure jacobi_tr__hdualx
        module procedure jacobi_tr__hdualy
    end interface
    interface chain_duals
        !! Generic function for converting a dual or hyper-dual number from one dual-type to another by applying the chain rule of derivation
        module procedure chain_duals__dualy_dualx
        module procedure chain_duals__hdualy_hdualx
    end interface
    interface assignment (=)
        module procedure assign_dualx_i  ! dual=integer, elemental
        module procedure assign_dualx_r  ! dual=real, elemental
        module procedure assign_i_dualx  ! integer=dual, elemental
        module procedure assign_r_dualx  ! real=dual, elemental
        module procedure assign_dualy_i  ! dual=integer, elemental
        module procedure assign_dualy_r  ! dual=real, elemental
        module procedure assign_i_dualy  ! integer=dual, elemental
        module procedure assign_r_dualy  ! real=dual, elemental
        module procedure assign_hdualx_i  ! dual=integer, elemental
        module procedure assign_hdualx_r  ! dual=real, elemental
        module procedure assign_i_hdualx  ! integer=dual, elemental
        module procedure assign_r_hdualx  ! real=dual, elemental
        module procedure assign_hdualy_i  ! dual=integer, elemental
        module procedure assign_hdualy_r  ! dual=real, elemental
        module procedure assign_i_hdualy  ! integer=dual, elemental
        module procedure assign_r_hdualy  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure unary_add_dualx   ! +dual number, elemental
        module procedure add_dualx_dualx       ! dual + dual, elemental
        module procedure add_dualx_i       ! dual + integer, elemental
        module procedure add_dualx_r       ! dual + real, elemental
        module procedure add_i_dualx       ! integer + dual, elemental
        module procedure add_r_dualx       ! real + dual, elemental
        module procedure unary_add_dualy   ! +dual number, elemental
        module procedure add_dualy_dualy       ! dual + dual, elemental
        module procedure add_dualy_i       ! dual + integer, elemental
        module procedure add_dualy_r       ! dual + real, elemental
        module procedure add_i_dualy       ! integer + dual, elemental
        module procedure add_r_dualy       ! real + dual, elemental
        module procedure unary_add_hdualx  ! +dual number, elemental
        module procedure add_hdualx_hdualx     ! dual + dual, elemental
        module procedure add_hdualx_i      ! dual + integer, elemental
        module procedure add_hdualx_r      ! dual + real, elemental
        module procedure add_i_hdualx      ! integer + dual, elemental
        module procedure add_r_hdualx      ! real + dual, elemental
        module procedure unary_add_hdualy  ! +dual number, elemental
        module procedure add_hdualy_hdualy     ! dual + dual, elemental
        module procedure add_hdualy_i      ! dual + integer, elemental
        module procedure add_hdualy_r      ! dual + real, elemental
        module procedure add_i_hdualy      ! integer + dual, elemental
        module procedure add_r_hdualy      ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure unary_minus_dualx  ! negate a dual number,elemental
        module procedure minus_dualx_dualx      ! dual -dual,elemental
        module procedure minus_dualx_i      ! dual-integer,elemental
        module procedure minus_dualx_r      ! dual-real,elemental
        module procedure minus_i_dualx      ! integer-dual,elemental
        module procedure minus_r_dualx      ! real-dual,elemental
        module procedure unary_minus_dualy  ! negate a dual number,elemental
        module procedure minus_dualy_dualy      ! dual -dual,elemental
        module procedure minus_dualy_i      ! dual-integer,elemental
        module procedure minus_dualy_r      ! dual-real,elemental
        module procedure minus_i_dualy      ! integer-dual,elemental
        module procedure minus_r_dualy      ! real-dual,elemental
        module procedure unary_minus_hdualx ! negate a dual number,elemental
        module procedure minus_hdualx_hdualx    ! dual -dual,elemental
        module procedure minus_hdualx_i     ! dual-integer,elemental
        module procedure minus_hdualx_r     ! dual-real,elemental
        module procedure minus_i_hdualx     ! integer-dual,elemental
        module procedure minus_r_hdualx     ! real-dual,elemental
        module procedure unary_minus_hdualy ! negate a dual number,elemental
        module procedure minus_hdualy_hdualy    ! dual -dual,elemental
        module procedure minus_hdualy_i     ! dual-integer,elemental
        module procedure minus_hdualy_r     ! dual-real,elemental
        module procedure minus_i_hdualy     ! integer-dual,elemental
        module procedure minus_r_hdualy     ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult_dualx_dualx    ! dual*dual, elemental
        module procedure mult_dualx_i    ! dual*integer,elemental
        module procedure mult_dualx_r    ! dual*real,elemental
        module procedure mult_i_dualx    ! integer*dual,elemental
        module procedure mult_r_dualx    ! real*dual,elemental
        module procedure mult_dualy_dualy    ! dual*dual, elemental
        module procedure mult_dualy_i    ! dual*integer,elemental
        module procedure mult_dualy_r    ! dual*real,elemental
        module procedure mult_i_dualy    ! integer*dual,elemental
        module procedure mult_r_dualy    ! real*dual,elemental
        module procedure mult_hdualx_hdualx  ! dual*dual, elemental
        module procedure mult_hdualx_i   ! dual*integer,elemental
        module procedure mult_hdualx_r   ! dual*real,elemental
        module procedure mult_i_hdualx   ! integer*dual,elemental
        module procedure mult_r_hdualx   ! real*dual,elemental
        module procedure mult_hdualy_hdualy  ! dual*dual, elemental
        module procedure mult_hdualy_i   ! dual*integer,elemental
        module procedure mult_hdualy_r   ! dual*real,elemental
        module procedure mult_i_hdualy   ! integer*dual,elemental
        module procedure mult_r_hdualy   ! real*dual,elemental
    end interface
    interface operator (**)
        module procedure pow_dualx_i ! dual number to an integer power,elemental
        module procedure pow_dualx_r ! dual number to a real power, elemental
        module procedure pow_dualx_dualx ! dual number to a dual power, elemental
        module procedure pow_dualy_i ! dual number to an integer power,elemental
        module procedure pow_dualy_r ! dual number to a real power, elemental
        module procedure pow_dualy_dualy ! dual number to a dual power, elemental
        module procedure pow_hdualx_i ! hdual number to an integer power,elemental
        module procedure pow_hdualx_r ! hdual number to a real power, elemental
        module procedure pow_hdualx_hdualx ! hdual number to a hdual power, elemental
        module procedure pow_hdualy_i ! hdual number to an integer power,elemental
        module procedure pow_hdualy_r ! hdual number to a real power, elemental
        module procedure pow_hdualy_hdualy ! hdual number to a hdual power, elemental
    end interface
    interface log
        module procedure log_dualx ! log of a dual number, elemental
        module procedure log_dualy ! log of a dual number, elemental
        module procedure log_hdualx ! log of a dual number, elemental
        module procedure log_hdualy ! log of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt_dualx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_dualy ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_hdualx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_hdualy ! obtain the sqrt of a dual number, elemental
    end interface
    
contains

    function test_hdual_chain() result(is_ok)
        logical :: is_ok
           
        real(dp), parameter :: abs_tol = 1.0e-14_dp

        integer :: i

        real(dp) :: x(nx)

        type(hdual_y_t) :: fy
        type(hdual_y_t) :: yy(ny)

        type(hdual_x_t) :: fx
        type(hdual_x_t) :: yx(ny)
        type(hdual_x_t) :: xx(nx)

        type(hdual_x_t) :: fx_chain

        ! random x
        call random_number(x)
        x = 5*x + 0.1_dp

        ! Initialize xx, representing x as a function of x, with derivatives wrt x
        call initialize(xx, x)
        
        ! y as a function of x, with derivatives wrt x
        yx = y_of_x(xx)

        ! y as a function of y, with derivatives wrt y
        call initialize(yy, yx(:)%x)

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
        if (abs(fx_chain%x - fx%x) > abs_tol) then
            is_ok = .false.
        end if
        if (maxval(abs(fx_chain%dx - fx%dx)) > abs_tol) then
            is_ok = .false.
        end if
        if (maxval(abs(fx_chain%ddx - fx%ddx)) > abs_tol) then
            is_ok = .false.
        end if

        


    end function

    function f_of_y__x(y) result(f)
        type(hdual_x_t), intent(in) :: y(ny)
        type(hdual_x_t) :: f
        f = sqrt(y(1))*log(y(2))
    end function
    function f_of_y__y(y) result(f)
        type(hdual_y_t), intent(in) :: y(ny)
        type(hdual_y_t) :: f
        f = sqrt(y(1))*log(y(2))
    end function

    function y_of_x(x) result(y)
        type(hdual_x_t), intent(in) :: x(nx)
        type(hdual_x_t) :: y(ny)
        y(1) = sqrt(x(1)**2 + x(2)**2)
        y(2) = x(3)
    end function

    function f_of_x(x) result(f)
        type(hdual_x_t), intent(in) :: x(nx)
        type(hdual_x_t) :: f
        f = f_of_y(y_of_x(x))
    end function

    pure subroutine initialize_dualx_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual_x_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%x = val
        dual%dx = 0
        dual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_dualx_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual_x_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%x = val(i)
            dual(i)%dx = 0
            dual(i)%dx(i) = 1
        end do

    end subroutine
    pure subroutine initialize_dualy_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual_y_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%x = val
        dual%dx = 0
        dual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_dualy_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual_y_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%x = val(i)
            dual(i)%dx = 0
            dual(i)%dx(i) = 1
        end do

    end subroutine
    pure subroutine initialize_hdualx_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual_x_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%x = val
        hdual%dx = 0
        hdual%ddx = 0
        hdual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_hdualx_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual_x_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%x = val(i)
            hdual(i)%dx = 0
            hdual(i)%ddx = 0
            hdual(i)%dx(i) = 1
        end do

    end subroutine
    pure subroutine initialize_hdualy_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual_y_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%x = val
        hdual%dx = 0
        hdual%ddx = 0
        hdual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_hdualy_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual_y_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%x = val(i)
            hdual(i)%dx = 0
            hdual(i)%ddx = 0
            hdual(i)%dx(i) = 1
        end do

    end subroutine
    subroutine display__dualx_1input(d, name)
        type(dual_x_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i

        print*, "Function values:"
        print*, " ",name,"%x"
        print '(ES22.14)', d%x

        print*, "Derivatives:"
        print*, " i    ", name, "%dx(i)"
        do i = 1, size(d%dx)
            print '(I3, ES22.14)' , i, d%dx(i)
        end do
    end subroutine
    subroutine display__dualx_1input_vec(d, name)
        type(dual_x_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, p

        print*, "Function values:"
        print*, " ",name,"(p)%x"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%x
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%dx(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%dx)
                print '(I3, I3, ES22.14)', p, i, d(p)%dx(i)
            end do
        end do
    end subroutine
    subroutine display__dualx_1input_mat(d, name)
        type(dual_x_t), intent(in) :: d(:, :)
        character(len=*), intent(in) :: name
        integer :: i, p, q

        print*, "Function values:"
        print*, " ",name,"(p,q)%x"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                print '(I3, I3, ES22.14)', p, q, d(p, q)%x
            end do
        end do

        print*, "Derivatives:"
        print*, " p  q  i    ", name, "(p)%dx(i)"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                do i = 1, size(d(1, 1)%dx)
                    print '(I3, I3, I3, ES22.14)', p, q, i, d(p,q)%dx(i)
                end do
            end do
        end do
    end subroutine
    subroutine display__dualx_2input(d1, d2, name1, name2)
        type(dual_x_t), intent(in) :: d1
        type(dual_x_t), intent(in) :: d2
        character(len=*), intent(in) :: name1
        character(len=*), intent(in) :: name2
        integer :: i

        print*, "Function values:"
        print*, " ",name1, "%x                  ", name2, "%x"
        print '(ES22.14, ES22.14)', d1%x, d2%x

        print*, "Derivatives:"
        print*, " i    ", name1, "%dx(i)                ", name2, "%dx(i)"
        do i = 1, size(d1%dx)
            print '(I3, ES22.14, ES22.14)' , i, d1%dx(i), d2%dx(i)
        end do
    end subroutine
    subroutine display__dualy_1input(d, name)
        type(dual_y_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i

        print*, "Function values:"
        print*, " ",name,"%x"
        print '(ES22.14)', d%x

        print*, "Derivatives:"
        print*, " i    ", name, "%dx(i)"
        do i = 1, size(d%dx)
            print '(I3, ES22.14)' , i, d%dx(i)
        end do
    end subroutine
    subroutine display__dualy_1input_vec(d, name)
        type(dual_y_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, p

        print*, "Function values:"
        print*, " ",name,"(p)%x"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%x
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%dx(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%dx)
                print '(I3, I3, ES22.14)', p, i, d(p)%dx(i)
            end do
        end do
    end subroutine
    subroutine display__dualy_1input_mat(d, name)
        type(dual_y_t), intent(in) :: d(:, :)
        character(len=*), intent(in) :: name
        integer :: i, p, q

        print*, "Function values:"
        print*, " ",name,"(p,q)%x"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                print '(I3, I3, ES22.14)', p, q, d(p, q)%x
            end do
        end do

        print*, "Derivatives:"
        print*, " p  q  i    ", name, "(p)%dx(i)"
        do q = 1, size(d, 2)
            do p = 1, size(d, 1)
                do i = 1, size(d(1, 1)%dx)
                    print '(I3, I3, I3, ES22.14)', p, q, i, d(p,q)%dx(i)
                end do
            end do
        end do
    end subroutine
    subroutine display__dualy_2input(d1, d2, name1, name2)
        type(dual_y_t), intent(in) :: d1
        type(dual_y_t), intent(in) :: d2
        character(len=*), intent(in) :: name1
        character(len=*), intent(in) :: name2
        integer :: i

        print*, "Function values:"
        print*, " ",name1, "%x                  ", name2, "%x"
        print '(ES22.14, ES22.14)', d1%x, d2%x

        print*, "Derivatives:"
        print*, " i    ", name1, "%dx(i)                ", name2, "%dx(i)"
        do i = 1, size(d1%dx)
            print '(I3, ES22.14, ES22.14)' , i, d1%dx(i), d2%dx(i)
        end do
    end subroutine

    subroutine display__hdualx_1input(d, name)
        type(hdual_x_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i, j, k

        print*, "Function values:"
        print*, " ",name,"%x"
        print '(ES22.14)', d%x

        print*, "Derivatives:"
        print*, " i    ", name, "%dx(i)"
        do i = 1, size(d%dx)
            print '(I3, ES22.14)' , i, d%dx(i)
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " k  i  j   ", name, "%ddx(k)"
        k = 0
        do j = 1, size(d%dx)
            do i = j, size(d%dx)
                k = k + 1
                print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d%ddx(k)
            end do
        end do

    end subroutine
    subroutine display__hdualx_1input_vec(d, name)
        type(hdual_x_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, j, k, p

        print*, "Function values:"
        print*, " ",name,"(p)%x"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%x
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%dx(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%dx)
                print '(I3, I3, ES22.14)', p, i, d(p)%dx(i)
            end do
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " p  k  i  j   ", name, "(p)%ddx(k)"
        do p = 1, size(d)
            k = 0
            do j = 1, size(d(1)%dx)
                do i = j, size(d(1)%dx)
                    k = k + 1
                    print '(I3, I3, I3, I3, ES22.14, ES22.14)', p, k, i, j, d(p)%ddx(k)
                end do
            end do
        end do

    end subroutine
    subroutine display__hdualx_2input(d1, d2, name1, name2)
      type(hdual_x_t), intent(in) :: d1
      type(hdual_x_t), intent(in) :: d2
      character(len=*), intent(in) :: name1
      character(len=*), intent(in) :: name2
      integer :: i, j, k

        print*, "Function values:"
        print*, " ",name1, "%x                  ", name2, "%x"
        print '(ES22.14, ES22.14)', d1%x, d2%x

        print*, "Derivatives:"
        print*, " i    ", name1, "%dx(i)                ", name2, "%dx(i)"
        do i = 1, size(d1%dx)
            print '(I3, ES22.14, ES22.14)' , i, d1%dx(i), d2%dx(i)
        end do

      print*, "Lower triangular of Hessian matrix, h(i,j)"
      print*, " k  i  j   ", name1, "%ddx(k)            ", name2, "%ddx(k)"
      k = 0
      do j = 1, size(d1%dx)
          do i = j, size(d1%dx)
              k = k + 1
              print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d1%ddx(k), d2%ddx(k)
          end do
      end do

    end subroutine
    subroutine display__hdualy_1input(d, name)
        type(hdual_y_t), intent(in) :: d
        character(len=*), intent(in) :: name
        integer :: i, j, k

        print*, "Function values:"
        print*, " ",name,"%x"
        print '(ES22.14)', d%x

        print*, "Derivatives:"
        print*, " i    ", name, "%dx(i)"
        do i = 1, size(d%dx)
            print '(I3, ES22.14)' , i, d%dx(i)
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " k  i  j   ", name, "%ddx(k)"
        k = 0
        do j = 1, size(d%dx)
            do i = j, size(d%dx)
                k = k + 1
                print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d%ddx(k)
            end do
        end do

    end subroutine
    subroutine display__hdualy_1input_vec(d, name)
        type(hdual_y_t), intent(in) :: d(:)
        character(len=*), intent(in) :: name
        integer :: i, j, k, p

        print*, "Function values:"
        print*, " ",name,"(p)%x"
        do p = 1, size(d)
            print '(I3, ES22.14)', p, d(p)%x
        end do

        print*, "Derivatives:"
        print*, " p   i    ", name, "(p)%dx(i)"
        do p = 1, size(d)
            do i = 1, size(d(1)%dx)
                print '(I3, I3, ES22.14)', p, i, d(p)%dx(i)
            end do
        end do

        print*, "Lower triangular of Hessian matrix, h(i,j)"
        print*, " p  k  i  j   ", name, "(p)%ddx(k)"
        do p = 1, size(d)
            k = 0
            do j = 1, size(d(1)%dx)
                do i = j, size(d(1)%dx)
                    k = k + 1
                    print '(I3, I3, I3, I3, ES22.14, ES22.14)', p, k, i, j, d(p)%ddx(k)
                end do
            end do
        end do

    end subroutine
    subroutine display__hdualy_2input(d1, d2, name1, name2)
      type(hdual_y_t), intent(in) :: d1
      type(hdual_y_t), intent(in) :: d2
      character(len=*), intent(in) :: name1
      character(len=*), intent(in) :: name2
      integer :: i, j, k

        print*, "Function values:"
        print*, " ",name1, "%x                  ", name2, "%x"
        print '(ES22.14, ES22.14)', d1%x, d2%x

        print*, "Derivatives:"
        print*, " i    ", name1, "%dx(i)                ", name2, "%dx(i)"
        do i = 1, size(d1%dx)
            print '(I3, ES22.14, ES22.14)' , i, d1%dx(i), d2%dx(i)
        end do

      print*, "Lower triangular of Hessian matrix, h(i,j)"
      print*, " k  i  j   ", name1, "%ddx(k)            ", name2, "%ddx(k)"
      k = 0
      do j = 1, size(d1%dx)
          do i = j, size(d1%dx)
              k = k + 1
              print '(I3, I3, I3, ES22.14, ES22.14)', k, i, j, d1%ddx(k), d2%ddx(k)
          end do
      end do

    end subroutine

    pure function hessian_hdualx(d) result(m)
        type(hdual_x_t), intent(in) :: d
        real(dp) :: m(size(d%dx), size(d%dx))
        
        integer i, j, k

        k = 0
        do j = 1, size(d%dx)
            k = k + 1
            m(j, j) = d%ddx(k)
            do i = j+1, size(d%dx)
                k = k + 1
                m(i, j) = d%ddx(k)
                m(j, i) = d%ddx(k)
            end do
        end do

    end function
    pure function hessian_hdualy(d) result(m)
        type(hdual_y_t), intent(in) :: d
        real(dp) :: m(size(d%dx), size(d%dx))
        
        integer i, j, k

        k = 0
        do j = 1, size(d%dx)
            k = k + 1
            m(j, j) = d%ddx(k)
            do i = j+1, size(d%dx)
                k = k + 1
                m(i, j) = d%ddx(k)
                m(j, i) = d%ddx(k)
            end do
        end do

    end function
    pure function jacobi_tr__hdualx(x) result(y)
        type(hdual_x_t), intent(in) :: x(:)
        type(dual_x_t) :: y(size(x(1)%dx),size(x))

        real(dp) :: hess(size(x(1)%dx), size(x(1)%dx))
        integer :: i, j

        do j = 1, size(x)
            hess = hessian(x(j))
            do i = 1, size(x(1)%dx)
                y(i,j)%x = x(j)%dx(i)
                y(i,j)%dx = hess(:,i)
            end do
        end do

    end function
    pure function jacobi_tr__hdualy(x) result(y)
        type(hdual_y_t), intent(in) :: x(:)
        type(dual_y_t) :: y(size(x(1)%dx),size(x))

        real(dp) :: hess(size(x(1)%dx), size(x(1)%dx))
        integer :: i, j

        do j = 1, size(x)
            hess = hessian(x(j))
            do i = 1, size(x(1)%dx)
                y(i,j)%x = x(j)%dx(i)
                y(i,j)%dx = hess(:,i)
            end do
        end do

    end function
    pure function jacobi_tr__dualx(x) result(y)
        type(dual_x_t), intent(in) :: x(:)
        real(dp) :: y(size(x(1)%dx),size(x))

        integer :: j

        do j = 1, size(x)
            y(:,j) = x(j)%dx
        end do

    end function
    pure function jacobi_tr__dualy(x) result(y)
        type(dual_y_t), intent(in) :: x(:)
        real(dp) :: y(size(x(1)%dx),size(x))

        integer :: j

        do j = 1, size(x)
            y(:,j) = x(j)%dx
        end do

    end function
    pure function chain_duals__dualy_dualx(fy, yx) result(fx)

        type(dual_y_t), intent(in) :: fy
        type(dual_x_t), intent(in) :: yx(size(fy%dx))
        type(dual_x_t) :: fx
        
        integer :: p, j

        fx%x = fy%x
        fx%dx = 0
        do p = 1, size(fy%dx)
            do j = 1, size(fx%dx)
                fx%dx(j) = fx%dx(j) + fy%dx(p)*yx(p)%dx(j)
            end do
        end do


    end function
    pure function chain_duals__hdualy_hdualx(fy, yx) result(fx)

        type(hdual_y_t), intent(in) :: fy
        type(hdual_x_t), intent(in) :: yx(size(fy%dx))
        type(hdual_x_t) :: fx
        
        integer :: p, j
        integer :: i, q, k, nk
        real(dp) :: hfy(size(fy%dx), size(fy%dx))
        real(dp) :: tmp

        fx%x = fy%x
        fx%dx = 0
        do p = 1, size(fy%dx)
            do j = 1, size(fx%dx)
                fx%dx(j) = fx%dx(j) + fy%dx(p)*yx(p)%dx(j)
            end do
        end do

        nk = size(fx%dx)*(size(fx%dx)+1)/2
        fx%ddx = 0
        do p = 1, size(fy%dx)
            do k = 1, nk
                fx%ddx(k) = fx%ddx(k) + fy%dx(p)*yx(p)%ddx(k)
            end do
        end do
        hfy = hessian(fy)
        do q = 1, size(fy%dx)
            do p = 1, size(fy%dx)
                k = 0
                do j = 1, size(fx%dx)
                    tmp = hfy(p,q)*yx(q)%dx(j)
                    do i = j, size(fx%dx)
                        k = k + 1
                        fx%ddx(k) = fx%ddx(k) + tmp*yx(p)%dx(i)
                    end do
                end do
            end do
        end do

    end function
    elemental subroutine assign_dualx_i(u, i)
        type(dual_x_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_dualx_r(u, r)
        type(dual_x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_dualx(i, v)
        type(dual_x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_dualx(r, v)
        type(dual_x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental subroutine assign_dualy_i(u, i)
        type(dual_y_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_dualy_r(u, r)
        type(dual_y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_dualy(i, v)
        type(dual_y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_dualy(r, v)
        type(dual_y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental subroutine assign_hdualx_i(u, i)
        type(hdual_x_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_hdualx_r(u, r)
        type(hdual_x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_hdualx(i, v)
        type(hdual_x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_hdualx(r, v)
        type(hdual_x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental subroutine assign_hdualy_i(u, i)
        type(hdual_y_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_hdualy_r(u, r)
        type(hdual_y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_hdualy(i, v)
        type(hdual_y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_hdualy(r, v)
        type(hdual_y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental function unary_add_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = u%x
        res%dx = u%dx
    end function
    elemental function add_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
    end function
    elemental function add_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental function add_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental function unary_add_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = u%x
        res%dx = u%dx
    end function
    elemental function add_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
    end function
    elemental function add_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental function add_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental function unary_add_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
    end function
    elemental function add_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function add_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function unary_add_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
    end function
    elemental function add_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function add_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function unary_minus_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
    end function
    elemental function minus_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
    end function
    elemental function minus_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
    end function
    elemental function minus_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
    end function
    elemental function minus_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
    end function
    elemental function minus_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
    end function
    elemental function unary_minus_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
    end function
    elemental function minus_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
    end function
    elemental function minus_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
    end function
    elemental function minus_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
    end function
    elemental function minus_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
    end function
    elemental function minus_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
    end function
    elemental function unary_minus_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        res%ddx = -u%ddx
    end function
    elemental function minus_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        res%ddx = u%ddx - v%ddx
    end function
    elemental function minus_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function minus_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
    end function
    elemental function minus_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function minus_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
    end function
    elemental function unary_minus_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        res%ddx = -u%ddx
    end function
    elemental function minus_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        res%ddx = u%ddx - v%ddx
    end function
    elemental function minus_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function minus_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
    end function
    elemental function minus_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function minus_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
    end function
    elemental function mult_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
    end function
    elemental function mult_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
    end function
    elemental function mult_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
    end function
    elemental function mult_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
    end function
    elemental function mult_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
    end function
    elemental function mult_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
    end function
    elemental function mult_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
    end function
    elemental function mult_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
    end function
    elemental function mult_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
    end function
    elemental function mult_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
    end function
    elemental function mult_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = u%ddx(k)*v%x + u%dx(i)*v%dx(j) + u%dx(j)*v%dx(i) + u%x*v%ddx(k)
            end do
        end do
    end function
    elemental function mult_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function mult_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function mult_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = u%ddx(k)*v%x + u%dx(i)*v%dx(j) + u%dx(j)*v%dx(i) + u%x*v%ddx(k)
            end do
        end do
    end function
    elemental function mult_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function mult_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function pow_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
    end function
    elemental function pow_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
    end function
    elemental function pow_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = u%x**v%x
        res%x = t0
        res%dx = t0*(u%dx*v%x/u%x + v%dx*log(u%x))

    end function
    elemental function pow_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
    end function
    elemental function pow_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
    end function
    elemental function pow_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = u%x**v%x
        res%x = t0
        res%dx = t0*(u%dx*v%x/u%x + v%dx*log(u%x))

    end function
    elemental function pow_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
    end function
    elemental function pow_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
    end function
    elemental function pow_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%x**v%x
        t1 = log(u%x)
        t2 = 1.0_dp/u%x
        t3 = t1*v%x + 1
        res%x = t0
        res%dx = t0*(t1*v%dx + t2*u%dx*v%x)
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = t0*(t1*v%ddx(k) + t2*u%ddx(k)*v%x + t2*u%dx(j)*(t2*u%dx(i)*v%x*(v%x - &
      1) + t3*v%dx(i)) + v%dx(j)*(t1**2*v%dx(i) + t2*t3*u%dx(i)))
            end do
        end do
    end function
    elemental function pow_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
    end function
    elemental function pow_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
    end function
    elemental function pow_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%x**v%x
        t1 = log(u%x)
        t2 = 1.0_dp/u%x
        t3 = t1*v%x + 1
        res%x = t0
        res%dx = t0*(t1*v%dx + t2*u%dx*v%x)
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = t0*(t1*v%ddx(k) + t2*u%ddx(k)*v%x + t2*u%dx(j)*(t2*u%dx(i)*v%x*(v%x - &
      1) + t3*v%dx(i)) + v%dx(j)*(t1**2*v%dx(i) + t2*t3*u%dx(i)))
            end do
        end do
    end function
    elemental function log_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = log(u%x)
        res%dx = u%dx/u%x
    end function
    elemental function log_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = log(u%x)
        res%dx = u%dx/u%x
    end function
    elemental function log_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%x
        res%x = log(u%x)
        res%dx = t0*u%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(i)*u%dx(j) + u%ddx(k))
            end do
        end do
    end function
    elemental function log_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%x
        res%x = log(u%x)
        res%dx = t0*u%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(i)*u%dx(j) + u%ddx(k))
            end do
        end do
    end function
    elemental function sqrt_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%x)
        res%x = t0
        res%dx = 0.5_dp*u%dx/t0
    end function

    elemental function sqrt_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%x)
        res%x = t0
        res%dx = 0.5_dp*u%dx/t0
    end function

    elemental function sqrt_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%x)
        t1 = 1.0_dp/t0
        res%x = t0
        res%dx = 0.5_dp*t1*u%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = (0.25_dp)*t1*(2*u%ddx(k) - u%dx(i)*u%dx(j)/u%x)
            end do
        end do
    end function
    elemental function sqrt_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%x)
        t1 = 1.0_dp/t0
        res%x = t0
        res%dx = 0.5_dp*t1*u%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = (0.25_dp)*t1*(2*u%ddx(k) - u%dx(i)*u%dx(j)/u%x)
            end do
        end do
    end function
    
    
end module