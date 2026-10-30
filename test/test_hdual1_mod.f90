module test_hdual1_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 2

    
    type :: dual__xy_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type
    type :: hdual__xy_t
        !! hyper-dual number type
        sequence
        type(dual__xy_t) :: d  ! dual number
        real(dp) :: h(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize__d_xy_scalar
        module procedure initialize__d_xy_vector
        module procedure initialize__hd_xy_scalar
        module procedure initialize__hd_xy_vector
    end interface
    interface fvalue
        !! Extract function value from a real, dual or hyper-dual number
        module procedure fvalue__r
        module procedure fvalue__r1
        module procedure fvalue__r2
        module procedure fvalue__d_xy
        module procedure fvalue__d_xy_r1
        module procedure fvalue__d_xy_r2
        module procedure fvalue__hd_xy
        module procedure fvalue__hd_xy_r1
        module procedure fvalue__hd_xy_r2
    end interface

    interface downgrade
        !! Downgrade a hyper-dual number to a dual number, a dual number to a real, or a real to itself.
        module procedure fvalue__r   ! Real to real
        module procedure fvalue__r1  ! Real to real (rank-1)
        module procedure fvalue__r2  ! Real to real (rank-2)
        module procedure fvalue__d_xy    ! Dual to real
        module procedure fvalue__d_xy_r1 ! Dual to real (rank-1)
        module procedure fvalue__d_xy_r2 ! Dual to real (rank-2)
        module procedure downgrade__hd_xy ! Hyper-dual to dual
    end interface

    interface gradient
        !! Extract gradient from a dual or hyper-dual number
        module procedure gradient__d_xy
        module procedure gradient__hd_xy
    end interface

    interface gradient_d
        !! Extract gradient from a dual or hyper-dual number. In the hyper-dual case, the gradient is dual valued.
        module procedure gradient__d_xy ! Same implementations as gradient()
        module procedure gradient_d__hd_xy
    end interface

    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian__hd_xy
    end interface
    interface assignment (=)
        !! Assign an integer or a real to a dual number, or the function value to them
        module procedure assign__dxy_i
        module procedure assign__dxy_r
        module procedure assign__i_dxy
        module procedure assign__r_dxy
        module procedure assign__hdxy_i
        module procedure assign__hdxy_r
        module procedure assign__i_hdxy
        module procedure assign__r_hdxy
    end interface
    interface operator (+)
        !! Unary plus and addition
        module procedure add__dxy
        module procedure add__dxy_dxy
        module procedure add__dxy_i
        module procedure add__dxy_r
        module procedure add__i_dxy
        module procedure add__r_dxy
        module procedure add__hdxy
        module procedure add__hdxy_hdxy
        module procedure add__hdxy_i
        module procedure add__hdxy_r
        module procedure add__i_hdxy
        module procedure add__r_hdxy
    end interface
    interface operator (-)
        !! Negation and subtraction
        module procedure minus__dxy
        module procedure minus__dxy_dxy
        module procedure minus__dxy_i
        module procedure minus__dxy_r
        module procedure minus__i_dxy
        module procedure minus__r_dxy
        module procedure minus__hdxy
        module procedure minus__hdxy_hdxy
        module procedure minus__hdxy_i
        module procedure minus__hdxy_r
        module procedure minus__i_hdxy
        module procedure minus__r_hdxy
    end interface
    interface operator (*)
        !! Multiplication
        module procedure mult__dxy_dxy
        module procedure mult__dxy_i
        module procedure mult__dxy_r
        module procedure mult__i_dxy
        module procedure mult__r_dxy
        module procedure mult__hdxy_hdxy
        module procedure mult__hdxy_i
        module procedure mult__hdxy_r
        module procedure mult__i_hdxy
        module procedure mult__r_hdxy
    end interface
    interface operator (/)
        !! Division
        module procedure div__dxy_dxy
        module procedure div__dxy_r
        module procedure div__r_dxy
        module procedure div__dxy_i
        module procedure div__i_dxy
        module procedure div__hdxy_hdxy
        module procedure div__hdxy_r
        module procedure div__r_hdxy
        module procedure div__hdxy_i
        module procedure div__i_hdxy
    end interface
    interface operator (**)
        !! Power
        module procedure pow__dxy_i
        module procedure pow__dxy_r
        module procedure pow__dxy_dxy
        module procedure pow__hdxy_i
        module procedure pow__hdxy_r
        module procedure pow__hdxy_hdxy
    end interface
    interface hypot
        !! Euclidean distance sqrt(x**2 + y**2)
        module procedure hypot__dxy_dxy
        module procedure hypot__dxy_r
        module procedure hypot__r_dxy
        module procedure hypot__hdxy_hdxy
        module procedure hypot__hdxy_r
        module procedure hypot__r_hdxy
    end interface
    interface operator (==)
        !! Equality of the function values
        module procedure eq__dxy_dxy
        module procedure eq__dxy_i
        module procedure eq__dxy_r
        module procedure eq__i_dxy
        module procedure eq__r_dxy
        module procedure eq__hdxy_hdxy
        module procedure eq__hdxy_i
        module procedure eq__hdxy_r
        module procedure eq__i_hdxy
        module procedure eq__r_hdxy
    end interface
    interface operator (<=)
        !! Less than or equal of the function values
        module procedure le__dxy_dxy
        module procedure le__dxy_i
        module procedure le__dxy_r
        module procedure le__i_dxy
        module procedure le__r_dxy
        module procedure le__hdxy_hdxy
        module procedure le__hdxy_i
        module procedure le__hdxy_r
        module procedure le__i_hdxy
        module procedure le__r_hdxy
    end interface
    interface operator (<)
        !! Less than of the function values
        module procedure lt__dxy_dxy
        module procedure lt__dxy_i
        module procedure lt__dxy_r
        module procedure lt__i_dxy
        module procedure lt__r_dxy
        module procedure lt__hdxy_hdxy
        module procedure lt__hdxy_i
        module procedure lt__hdxy_r
        module procedure lt__i_hdxy
        module procedure lt__r_hdxy
    end interface
    interface operator (>=)
        !! Greater than or equal of the function values
        module procedure ge__dxy_dxy
        module procedure ge__dxy_i
        module procedure ge__dxy_r
        module procedure ge__i_dxy
        module procedure ge__r_dxy
        module procedure ge__hdxy_hdxy
        module procedure ge__hdxy_i
        module procedure ge__hdxy_r
        module procedure ge__i_hdxy
        module procedure ge__r_hdxy
    end interface
    interface operator (>)
        !! Greater than of the function values
        module procedure gt__dxy_dxy
        module procedure gt__dxy_i
        module procedure gt__dxy_r
        module procedure gt__i_dxy
        module procedure gt__r_dxy
        module procedure gt__hdxy_hdxy
        module procedure gt__hdxy_i
        module procedure gt__hdxy_r
        module procedure gt__i_hdxy
        module procedure gt__r_hdxy
    end interface
    interface operator (/=)
        !! Inequality of the function values
        module procedure ne__dxy_dxy
        module procedure ne__dxy_i
        module procedure ne__dxy_r
        module procedure ne__i_dxy
        module procedure ne__r_dxy
        module procedure ne__hdxy_hdxy
        module procedure ne__hdxy_i
        module procedure ne__hdxy_r
        module procedure ne__i_hdxy
        module procedure ne__r_hdxy
    end interface
    interface abs
        !! Absolute value
        module procedure abs__dxy
    end interface
    interface dabs
        !! Absolute value (old Fortran name)
        module procedure abs__dxy
    end interface
    interface acos
        !! Arccosine
        module procedure acos__dxy
    end interface
    interface asin
        !! Arcsine
        module procedure asin__dxy
    end interface
    interface atan
        !! Arctangent
        module procedure atan__dxy
    end interface
    interface atan2
        !! Arctangent of y/x in the quadrant of (x, y)
        module procedure atan2__dxy
    end interface
    interface cos
        !! Cosine
        module procedure cos__dxy
    end interface
    interface dcos
        !! Cosine (old Fortran name)
        module procedure cos__dxy
    end interface
    interface dot_product
        !! Dot product of two dual vectors
        module procedure dot_product__dxy_dxy
    end interface
    interface exp
        !! Exponential
        module procedure exp__dxy
    end interface
    interface int
        !! Integer part of the function value
        module procedure int__dxy
    end interface
    interface log
        !! Natural logarithm
        module procedure log__dxy
        module procedure log__hdxy
    end interface
    interface log10
        !! Base-10 logarithm
        module procedure log10__dxy
    end interface
    interface matmul
        !! Matrix product of dual matrices and vectors
        module procedure matmul__dxy_dxy
        module procedure matmul__dxy_v
        module procedure matmul__v_dxy
    end interface
    interface max
        !! The larger of two numbers
        module procedure max__dxy_dxy
        module procedure max__dxy_i
        module procedure max__dxy_r
        module procedure max__r_dxy
    end interface
    interface dmax1
        !! The largest of two to five dual numbers
        module procedure dmax1__dxy_dxy
    end interface
    interface maxval
        !! The largest element of a dual vector
        module procedure maxval__dxy
    end interface
    interface min
        !! The smaller of two numbers
        module procedure min__dxy_dxy
        module procedure min__dxy_r
    end interface
    interface dmin1
        !! The smallest of two to four dual numbers
        module procedure dmin1__dxy_dxy
    end interface
    interface minval
        !! The smallest element of a dual vector
        module procedure minval__dxy
    end interface
    interface nint
        !! Nearest integer to the function value
        module procedure nint__dxy
    end interface
    interface sign
        !! abs(a) with the sign of b
        module procedure sign__dxy_dxy
        module procedure sign__r_dxy
    end interface
    interface sin
        !! Sine
        module procedure sin__dxy
    end interface
    interface dsin
        !! Sine (old Fortran name)
        module procedure sin__dxy
    end interface
    interface tan
        !! Tangent
        module procedure tan__dxy
    end interface
    interface dtan
        !! Tangent (old Fortran name)
        module procedure tan__dxy
    end interface
    interface sqrt
        !! Square root
        module procedure sqrt__dxy
        module procedure sqrt__hdxy
    end interface
    interface sum
        !! Sum of a dual vector
        module procedure sum__dxy
    end interface
    interface maxloc
        !! Location of the largest element of a dual vector
        module procedure maxloc__dxy
    end interface
    
contains

    function test_hdual1() result(is_ok)
        logical :: is_ok
           
        real(dp), parameter :: abs_tol = 1.0e-14_dp

        integer, parameter :: nval = 5

        integer :: i

        real(dp), dimension(nval) :: x, y
        real(dp), dimension(nval) :: fvd, fvh, fv_fasit
        real(dp), dimension(2, nval) :: dfd, dfh, df_fasit
        real(dp), dimension(2, 2, nval) :: ddfh, ddf_fasit

        type(dual__xy_t), dimension(nval) :: xd, yd, fd
        type(hdual__xy_t), dimension(nval) :: xhd, yhd, fhd

        call random_number(x)
        x = 5*x + 0.1_dp
        call random_number(y)
        y = 5*y + 0.1_dp


        ! Initialize dual inputs:
        do i = 1, nval

            call initialize(xd(i), x(i), 1)
            call initialize(yd(i), y(i), 2)
            
            call initialize(xhd(i), x(i), 1)
            call initialize(yhd(i), y(i), 2)

        end do

        fd = test_function_dual(xd, yd)
        fhd = test_function_hdual(xhd, yhd)

        ! Extract function value, gradient and hessian for all values:
        do i = 1, nval

            fvd(i) = fd(i)
            dfd(:, i) = gradient(fd(i))

            fvh(i) = fhd(i)
            dfh(:, i) = gradient(fhd(i))
            ddfh(:, :, i) = hessian(fhd(i))

        end do

        ! Fasit:
        do i = 1, nval
            call test_function_analytic(x(i), y(i), fv_fasit(i), df_fasit(:, i), ddf_fasit(:, :, i))
        end do

        if ((maxval(abs(fvd - fv_fasit)) < abs_tol) .and. &
                (maxval(abs(fvh - fv_fasit)) < abs_tol) .and. &
                (maxval(abs(dfd - df_fasit)) < abs_tol) .and. &
                (maxval(abs(dfh - df_fasit)) < abs_tol) .and. &
                (maxval(abs(ddfh - ddf_fasit)) < abs_tol)) then
            is_ok = .true.
        else
            is_ok = .false.
            print*,"f_diff:"
            print*, fvh - fv_fasit
            print*,"df_diff:"
            print*, dfh - df_fasit
            print*,"ddf_diff:"
            print*, ddfh - ddf_fasit
        end if


        if (is_ok) then
            print *, "test_hdual: ok"
        else
            print *, "test_hdual: failed"
        end if

        ! print*, "f:"
        ! print*, f
        ! print*, "f_fasit:"
        ! print*, f_fasit
        ! print*, "df:"
        ! print*, df
        ! print*, "df_fasit:"
        ! print*, df_fasit
        ! print*, "ddf:"
        ! print*, ddf
        ! print*, "ddf_fasit:"
        ! print*, ddf_fasit
      
    end function

    elemental function test_function_dual(x, y) result(f)
        type(dual__xy_t), intent(in) :: x, y
        type(dual__xy_t) :: f
        f = sqrt(x)*log(y)
    end function
    elemental function test_function_hdual(x, y) result(f)
        type(hdual__xy_t), intent(in) :: x, y
        type(hdual__xy_t) :: f
        f = sqrt(x)*log(y)
    end function

    pure subroutine test_function_analytic(x, y, f, df, ddf)
        real(dp), intent(in) :: x, y
        real(dp), intent(out) :: f           ! function value
        real(dp), intent(out) :: df(2)       ! gradient
        real(dp), intent(out) :: ddf(2, 2)   ! Hessian
        f = sqrt(x)*log(y)
        df(1) = log(y)/(2*sqrt(x))  ! df/dx
        df(2) = sqrt(x)/y           ! df/dy
        ddf(1, 1) = -log(y)/(4*x**1.5_dp)  ! d**2f/dx**2
        ddf(2, 2) = -sqrt(x)/(y**2)        ! d**2f/dy**2
        ddf(2, 1) = 1/(2*sqrt(x)*y)        ! d**2f/dxdy
        ddf(1, 2) = ddf(2, 1)              ! d**2f/dydx
    end subroutine

    pure subroutine initialize__d_xy_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual__xy_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize__d_xy_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual__xy_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize__hd_xy_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual__xy_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%d%f = val
        hdual%d%g = 0
        hdual%h = 0
        hdual%d%g(idiff) = 1

    end subroutine
    pure subroutine initialize__hd_xy_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual__xy_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%d%f = val(i)
            hdual(i)%d%g = 0
            hdual(i)%h = 0
            hdual(i)%d%g(i) = 1
        end do

    end subroutine

    pure function fvalue__r(fi) result(f)
        real(dp), intent(in) :: fi
        real(dp) :: f
        f = fi
    end function
    pure function fvalue__r1(fi) result(f)
        real(dp), intent(in) :: fi(:)
        real(dp) :: f(size(fi))
        f = fi
    end function
    pure function fvalue__r2(fi) result(f)
        real(dp), intent(in) :: fi(:,:)
        real(dp) :: f(size(fi, 1), size(fi, 2))
        f = fi
    end function
    pure function fvalue__d_xy(d) result(f)
        type(dual__xy_t), intent(in) :: d
        real(dp) :: f
        f = d%f
    end function
    pure function fvalue__d_xy_r1(d) result(f)
        type(dual__xy_t), intent(in) :: d(:)
        real(dp) :: f(size(d))
        f = d%f
    end function
    pure function fvalue__d_xy_r2(d) result(f)
        type(dual__xy_t), intent(in) :: d(:,:)
        real(dp) :: f(size(d,1),size(d,2))
        f = d%f
    end function
    pure function fvalue__hd_xy(hd) result(f)
        type(hdual__xy_t), intent(in) :: hd
        real(dp) :: f
        f = hd%d%f
    end function
    pure function fvalue__hd_xy_r1(hd) result(f)
        type(hdual__xy_t), intent(in) :: hd(:)
        real(dp) :: f(size(hd))
        f = hd%d%f
    end function
    pure function fvalue__hd_xy_r2(hd) result(f)
        type(hdual__xy_t), intent(in) :: hd(:,:)
        real(dp) :: f(size(hd,1),size(hd,2))
        f = hd%d%f
    end function

    pure function downgrade__hd_xy(hd) result(d)
        type(hdual__xy_t), intent(in) :: hd
        type(dual__xy_t) :: d
        d = hd%d
    end function

    pure function gradient__d_xy(d) result(g)
        type(dual__xy_t), intent(in) :: d
        real(dp) :: g(size(d%g))
        g = d%g
    end function
    pure function gradient__hd_xy(hd) result(g)
        type(hdual__xy_t), intent(in) :: hd
        real(dp) :: g(size(hd%d%g))
        g = hd%d%g
    end function
    pure function gradient_d__hd_xy(hd) result(g)
        type(hdual__xy_t), intent(in) :: hd
        type(dual__xy_t) :: g(size(hd%d%g))

        real(dp) :: hess(size(hd%d%g), size(hd%d%g))
        integer :: i

        hess = hessian(hd)
        do i = 1, size(hd%d%g)
            g(i)%f = hd%d%g(i)
            g(i)%g = hess(:,i)
        end do
    end function

    pure function hessian__hd_xy(hd) result(m)
        type(hdual__xy_t), intent(in) :: hd
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
    elemental subroutine assign__dxy_i(u, i)
        type(dual__xy_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__dxy_r(u, r)
        type(dual__xy_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__i_dxy(i, v)
        type(dual__xy_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign__r_dxy(r, v)
        type(dual__xy_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental subroutine assign__hdxy_i(u, i)
        type(hdual__xy_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__hdxy_r(u, r)
        type(hdual__xy_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__i_hdxy(i, v)
        type(hdual__xy_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)

    end subroutine
    elemental subroutine assign__r_hdxy(r, v)
        type(hdual__xy_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f

    end subroutine
    elemental function add__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__r_dxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__dxy_i(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__i_dxy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__hdxy(u) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
    end function
    elemental function add__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__r_hdxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add__hdxy_i(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__i_hdxy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function minus__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        
        res%f = -u%f
        res%g = -u%g
    end function
    elemental function minus__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
    end function
    elemental function minus__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__r_dxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__dxy_i(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__i_dxy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__hdxy(u) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t) :: res
        
        res%d%f = -u%d%f
        res%d%g = -u%d%g
        res%h = -u%h
    end function
    elemental function minus__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f - v%d%f
        res%d%g = u%d%g - v%d%g
        res%h = u%h - v%h
    end function
    elemental function minus__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__r_hdxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function minus__hdxy_i(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__i_hdxy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function mult__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
    end function
    elemental function mult__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__r_dxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__dxy_i(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__i_dxy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
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
    elemental function mult__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__r_hdxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult__hdxy_i(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__i_hdxy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function div__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = t0*u%f
        res%f = t1
        res%g = t0*(-t1*v%g + u%g)
    end function
    elemental function div__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
    end function
    elemental function div__r_dxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
    end function
    elemental function div__dxy_i(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
    end function
    elemental function div__i_dxy(u, v) result(res)
        integer, intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
    end function
    elemental function div__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%d%f
        t1 = t0*u%d%f
        res%d%f = t1
        res%d%g = t0*(-t1*v%d%g + u%d%g)
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t0*(-t0*u%d%g(j)*v%d%g(i) - t0*v%d%g(j)*(-2*t1*v%d%g(i) + u%d%g(i)) - t1* &
      v%h(k) + u%h(k))
            end do
        end do
    end function
    elemental function div__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
    end function
    elemental function div__r_hdxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%d%f
        t1 = u/v%d%f**2
        res%d%f = t0*u
        res%d%g = -t1*v%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t1*(2*t0*v%d%g(i)*v%d%g(j) - v%h(k))
            end do
        end do
    end function
    elemental function div__hdxy_i(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
    end function
    elemental function div__i_hdxy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%d%f
        t1 = u/v%d%f**2
        res%d%f = t0*u
        res%d%g = -t1*v%d%g
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t1*(2*t0*v%d%g(i)*v%d%g(j) - v%h(k))
            end do
        end do
    end function
    elemental function pow__dxy_i(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

    end function
    elemental function pow__hdxy_i(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__xy_t) :: res
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
    elemental function pow__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
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
    elemental function pow__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
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
    elemental function hypot__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u%f, v%f)
        res%f = t0
        res%g = (u%g*u%f + v%g*v%f)/t0
    end function
    elemental function hypot__dxy_r(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u%f, v)
        res%f = t0
        res%g = u%g*u%f/t0
    end function
    elemental function hypot__r_dxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u, v%f)
        res%f = t0
        res%g = v%g*v%f/t0
    end function
    elemental function hypot__hdxy_hdxy(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3,t4,t5
        
        t0 = u%d%f**2
        t1 = v%d%f**2
        t2 = t0 + t1
        t3 = sqrt(t2)
        t4 = 1.0_dp/t3
        t5 = 1.0_dp/t2
        res%d%f = t3
        res%d%g = t4*(u%d%g*u%d%f + v%d%g*v%d%f)

        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t4*(u%h(k)*u%d%f - u%d%g(j)*(t5*u%d%f*v%d%g(i)*v%d%f + u%d%g(i)*(t0*t5 - 1)) &
      + v%h(k)*v%d%f - v%d%g(j)*(t5*u%d%g(i)*u%d%f*v%d%f + v%d%g(i)*(t1*t5 - 1)))
            end do
        end do
    end function
    elemental function hypot__hdxy_r(u, v) result(res)
        type(hdual__xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%d%f**2
        t1 = t0 + v**2
        t2 = sqrt(t1)
        t3 = 1.0_dp/t2
        res%d%f = t2
        res%d%g = t3*u%d%g*u%d%f
        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t3*(u%h(k)*u%d%f - u%d%g(i)*u%d%g(j)*(t0/t1 - 1))
            end do
        end do
    end function
    elemental function hypot__r_hdxy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__xy_t), intent(in) :: v
        type(hdual__xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = v%d%f**2
        t1 = t0 + u**2
        t2 = sqrt(t1)
        t3 = 1.0_dp/t2
        res%d%f = t2
        res%d%g = t3*v%d%g*v%d%f

        k = 0
        do j = 1, size(res%d%g)
            do i = j, size(res%d%g)
                k = k + 1
                res%h(k) = t3*(v%h(k)*v%d%f - v%d%g(i)*v%d%g(j)*(t0/t1 - 1))
            end do
        end do
    end function
    elemental  function eq__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f == rhs%f)

    end function
    elemental  function eq__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    elemental  function eq__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    elemental  function eq__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    elemental  function eq__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    elemental  function eq__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f == rhs%d%f)

    end function
    elemental  function eq__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    elemental  function eq__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    elemental  function eq__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    elemental  function eq__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    elemental  function le__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f <= rhs%f)

    end function
    elemental  function le__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    elemental  function le__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    elemental  function le__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    elemental  function le__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    elemental  function le__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f <= rhs%d%f)

    end function
    elemental  function le__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    elemental  function le__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    elemental  function le__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    elemental  function le__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    elemental  function lt__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f < rhs%f)

    end function
    elemental  function lt__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    elemental  function lt__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    elemental  function lt__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    elemental  function lt__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    elemental  function lt__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f < rhs%d%f)

    end function
    elemental  function lt__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    elemental  function lt__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    elemental  function lt__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    elemental  function lt__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    elemental  function ge__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f >= rhs%f)

    end function
    elemental  function ge__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    elemental  function ge__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    elemental  function ge__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    elemental  function ge__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    elemental  function ge__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f >= rhs%d%f)

    end function
    elemental  function ge__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    elemental  function ge__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    elemental  function ge__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    elemental  function ge__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    elemental  function gt__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f > rhs%f)

    end function
    elemental  function gt__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    elemental  function gt__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    elemental  function gt__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    elemental  function gt__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    elemental  function gt__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f > rhs%d%f)

    end function
    elemental  function gt__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    elemental  function gt__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    elemental  function gt__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    elemental  function gt__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    elemental  function ne__dxy_dxy(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f /= rhs%f)

    end function
    elemental  function ne__dxy_i(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    elemental  function ne__dxy_r(lhs, rhs) result(res)
         type(dual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    elemental  function ne__i_dxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    elemental  function ne__r_dxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    elemental  function ne__hdxy_hdxy(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f /= rhs%d%f)

    end function
    elemental  function ne__hdxy_i(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    elemental  function ne__hdxy_r(lhs, rhs) result(res)
         type(hdual__xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    elemental  function ne__i_hdxy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    elemental  function ne__r_hdxy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    elemental  function abs__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        integer :: i

        res%f = abs(u%f)
        res%g = sign(1.0_dp, u%f)*u%g

    end function
    elemental  function acos__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = acos(u%f)
    
        res%g = -u%g / sqrt(1.0_dp - u%f**2)

    end function
    elemental  function asin__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = asin(u%f)
        res%g = u%g / sqrt(1.0_dp - u%f**2)

    end function
    elemental  function atan__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = atan(u%f)
        res%g = u%g / (1.0_dp + u%f**2)

    end function
    elemental  function atan2__dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u, v
        type(dual__xy_t) :: res

        real(dp) :: usq_plus_vsq

        res%f = atan2(u%f, v%f)

        usq_plus_vsq = u%f**2 + v%f**2
        res%g = v%f / usq_plus_vsq * u%g - u%f / usq_plus_vsq * v%g

    end function
    elemental  function cos__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = cos(u%f)
        res%g = -sin(u%f) * u%g

    end function
    pure  function dot_product__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u(:), v(:)
        type(dual__xy_t) :: res

        integer :: i

        res%f = dot_product(u%f, v%f)
        do i = 1, size(res%g)
            res%g(i) = dot_product(u%f, v%g(i)) + dot_product(v%f, u%g(i))
        end do

    end function
    elemental  function exp__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%f)
        res%f = exp_x
        res%g = u%g * exp_x

    end function
    elemental  function int__dxy(u) result(res)
         type(dual__xy_t), intent(in) :: u
         integer :: res

         res = int(u%f)

    end function
    elemental function log__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
    end function
    elemental function log__hdxy(u) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t) :: res
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
    elemental  function log10__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%f * log(10.0_dp))
        res%f = log10(u%f)
        res%g = u%g * inv

    end function
    pure  function matmul__dxy_dxy(u,v) result(res)
        type(dual__xy_t), intent(in) :: u(:,:), v(:,:)
        type(dual__xy_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1,1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    pure  function matmul__dxy_v(u, v) result(res)
        type(dual__xy_t), intent(in) :: u(:,:), v(:)
        type(dual__xy_t) :: res(size(u,1))
        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    pure  function matmul__v_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u(:), v(:,:)
        type(dual__xy_t) :: res(size(v, 2))
        integer::i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    elemental  function max__dxy_dxy(val1, val2) result(res)
        type(dual__xy_t), intent(in) :: val1, val2
        type(dual__xy_t) :: res

        if (val1%f > val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    elemental  function max__dxy_i(u, i) result(res)
        type(dual__xy_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual__xy_t) :: res

        if (u%f > i) then
            res = u
        else
            res = i
        endif

    end function
    elemental  function max__dxy_r(u, r) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__xy_t) :: res

        if (u%f > r) then
            res = u
        else
            res = r
        endif

    end function
     elemental  function max__r_dxy(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        if (u%f > n) then
            res = u
        else
            res = n
        endif

    end function
    elemental  function dmax1__dxy_dxy(val1, val2, val3, val4,val5) result(res)
        type(dual__xy_t), intent(in) :: val1, val2
        type(dual__xy_t), intent(in), optional :: val3, val4,val5
        type(dual__xy_t) :: res

        if (val1%f > val2%f) then
            res = val1
        else
            res = val2
        endif
        if(present(val3))then
           if(res%f < val3%f) res = val3
        endif
        if(present(val4))then
           if(res%f < val4%f) res = val4
        endif
        if(present(val5))then
           if(res%f < val5%f) res = val5
        endif

    end function
    pure  function maxval__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__xy_t) :: res

        iloc=maxloc(u%f)
        res=u(iloc(1))

    end function
    elemental  function min__dxy_dxy(val1, val2) result(res)
        type(dual__xy_t), intent(in) :: val1, val2
        type(dual__xy_t) :: res

        if (val1%f < val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    elemental  function min__dxy_r(u, r) result(res)
        type(dual__xy_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__xy_t) :: res

        if (u%f < r) then
            res = u
        else
            res = r
        endif

    end function
    elemental  function dmin1__dxy_dxy(val1, val2, val3, val4) result(res)
        type(dual__xy_t), intent(in) :: val1, val2
        type(dual__xy_t), intent(in), optional :: val3, val4
        type(dual__xy_t) :: res

        if (val1%f < val2%f) then
            res = val1
        else
            res = val2
        endif
        if(present(val3))then
           if(res%f > val3%f) res = val3
        endif
        if(present(val4))then
           if(res%f > val4%f) res = val4
        endif

    end function
    pure  function minval__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__xy_t) :: res

        iloc=minloc(u%f)
        res=u(iloc(1))

    end function
    elemental  function nint__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        integer :: res

        res=nint(u%f)

    end function
    elemental  function sign__dxy_dxy(val1, val2) result(res)
        type(dual__xy_t), intent(in) :: val1, val2
        type(dual__xy_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    elemental  function sign__r_dxy(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual__xy_t), intent(in) :: val2
        type(dual__xy_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    elemental  function sin__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = sin(u%f)
        res%g = cos(u%f) * u%g

    end function
    elemental  function tan__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res

        res%f = tan(u%f)
        res%g = u%g / cos(u%f)**2

    end function
    elemental function sqrt__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
    end function

    elemental function sqrt__hdxy(u) result(res)
        type(hdual__xy_t), intent(in) :: u
        type(hdual__xy_t) :: res
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
    pure  function sum__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u(:)
        type(dual__xy_t) :: res
        integer :: i

        res%f = sum(u%f)
        do i = 1, size(res%g)
            res%g(i) = sum(u%g(i))
        end do

    end function
    pure  function maxloc__dxy(array) result(ind)
        type(dual__xy_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%f)

    end function
    
    
end module