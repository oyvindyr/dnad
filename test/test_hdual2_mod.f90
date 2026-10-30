module test_hdual2_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 3

    
    type :: dual__uvw_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type
    type :: hdual__uvw_t
        !! hyper-dual number type
        sequence
        type(dual__uvw_t) :: d  ! dual number
        real(dp) :: h(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize__d_uvw_scalar
        module procedure initialize__d_uvw_vector
        module procedure initialize__hd_uvw_scalar
        module procedure initialize__hd_uvw_vector
    end interface
    interface fvalue
        !! Extract function value from a real, dual or hyper-dual number
        module procedure fvalue__r
        module procedure fvalue__r1
        module procedure fvalue__r2
        module procedure fvalue__d_uvw
        module procedure fvalue__d_uvw_r1
        module procedure fvalue__d_uvw_r2
        module procedure fvalue__hd_uvw
        module procedure fvalue__hd_uvw_r1
        module procedure fvalue__hd_uvw_r2
    end interface

    interface downgrade
        !! Downgrade a hyper-dual number to a dual number, a dual number to a real, or a real to itself.
        module procedure fvalue__r   ! Real to real
        module procedure fvalue__r1  ! Real to real (rank-1)
        module procedure fvalue__r2  ! Real to real (rank-2)
        module procedure fvalue__d_uvw    ! Dual to real
        module procedure fvalue__d_uvw_r1 ! Dual to real (rank-1)
        module procedure fvalue__d_uvw_r2 ! Dual to real (rank-2)
        module procedure downgrade__hd_uvw ! Hyper-dual to dual
    end interface

    interface gradient
        !! Extract gradient from a dual or hyper-dual number
        module procedure gradient__d_uvw
        module procedure gradient__hd_uvw
    end interface

    interface gradient_d
        !! Extract gradient from a dual or hyper-dual number. In the hyper-dual case, the gradient is dual valued.
        module procedure gradient__d_uvw ! Same implementations as gradient()
        module procedure gradient_d__hd_uvw
    end interface

    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian__hd_uvw
    end interface
    interface assignment (=)
        !! Assign an integer or a real to a dual number, or the function value to them
        module procedure assign__duvw_i
        module procedure assign__duvw_r
        module procedure assign__i_duvw
        module procedure assign__r_duvw
        module procedure assign__hduvw_i
        module procedure assign__hduvw_r
        module procedure assign__i_hduvw
        module procedure assign__r_hduvw
    end interface
    interface operator (+)
        !! Unary plus and addition
        module procedure add__duvw
        module procedure add__duvw_duvw
        module procedure add__duvw_i
        module procedure add__duvw_r
        module procedure add__i_duvw
        module procedure add__r_duvw
        module procedure add__hduvw
        module procedure add__hduvw_hduvw
        module procedure add__hduvw_i
        module procedure add__hduvw_r
        module procedure add__i_hduvw
        module procedure add__r_hduvw
    end interface
    interface operator (-)
        !! Negation and subtraction
        module procedure minus__duvw
        module procedure minus__duvw_duvw
        module procedure minus__duvw_i
        module procedure minus__duvw_r
        module procedure minus__i_duvw
        module procedure minus__r_duvw
        module procedure minus__hduvw
        module procedure minus__hduvw_hduvw
        module procedure minus__hduvw_i
        module procedure minus__hduvw_r
        module procedure minus__i_hduvw
        module procedure minus__r_hduvw
    end interface
    interface operator (*)
        !! Multiplication
        module procedure mult__duvw_duvw
        module procedure mult__duvw_i
        module procedure mult__duvw_r
        module procedure mult__i_duvw
        module procedure mult__r_duvw
        module procedure mult__hduvw_hduvw
        module procedure mult__hduvw_i
        module procedure mult__hduvw_r
        module procedure mult__i_hduvw
        module procedure mult__r_hduvw
    end interface
    interface operator (/)
        !! Division
        module procedure div__duvw_duvw
        module procedure div__duvw_r
        module procedure div__r_duvw
        module procedure div__duvw_i
        module procedure div__i_duvw
        module procedure div__hduvw_hduvw
        module procedure div__hduvw_r
        module procedure div__r_hduvw
        module procedure div__hduvw_i
        module procedure div__i_hduvw
    end interface
    interface operator (**)
        !! Power
        module procedure pow__duvw_i
        module procedure pow__duvw_r
        module procedure pow__duvw_duvw
        module procedure pow__hduvw_i
        module procedure pow__hduvw_r
        module procedure pow__hduvw_hduvw
    end interface
    interface hypot
        !! Euclidean distance sqrt(x**2 + y**2)
        module procedure hypot__duvw_duvw
        module procedure hypot__duvw_r
        module procedure hypot__r_duvw
        module procedure hypot__hduvw_hduvw
        module procedure hypot__hduvw_r
        module procedure hypot__r_hduvw
    end interface
    interface operator (==)
        !! Equality of the function values
        module procedure eq__duvw_duvw
        module procedure eq__duvw_i
        module procedure eq__duvw_r
        module procedure eq__i_duvw
        module procedure eq__r_duvw
        module procedure eq__hduvw_hduvw
        module procedure eq__hduvw_i
        module procedure eq__hduvw_r
        module procedure eq__i_hduvw
        module procedure eq__r_hduvw
    end interface
    interface operator (<=)
        !! Less than or equal of the function values
        module procedure le__duvw_duvw
        module procedure le__duvw_i
        module procedure le__duvw_r
        module procedure le__i_duvw
        module procedure le__r_duvw
        module procedure le__hduvw_hduvw
        module procedure le__hduvw_i
        module procedure le__hduvw_r
        module procedure le__i_hduvw
        module procedure le__r_hduvw
    end interface
    interface operator (<)
        !! Less than of the function values
        module procedure lt__duvw_duvw
        module procedure lt__duvw_i
        module procedure lt__duvw_r
        module procedure lt__i_duvw
        module procedure lt__r_duvw
        module procedure lt__hduvw_hduvw
        module procedure lt__hduvw_i
        module procedure lt__hduvw_r
        module procedure lt__i_hduvw
        module procedure lt__r_hduvw
    end interface
    interface operator (>=)
        !! Greater than or equal of the function values
        module procedure ge__duvw_duvw
        module procedure ge__duvw_i
        module procedure ge__duvw_r
        module procedure ge__i_duvw
        module procedure ge__r_duvw
        module procedure ge__hduvw_hduvw
        module procedure ge__hduvw_i
        module procedure ge__hduvw_r
        module procedure ge__i_hduvw
        module procedure ge__r_hduvw
    end interface
    interface operator (>)
        !! Greater than of the function values
        module procedure gt__duvw_duvw
        module procedure gt__duvw_i
        module procedure gt__duvw_r
        module procedure gt__i_duvw
        module procedure gt__r_duvw
        module procedure gt__hduvw_hduvw
        module procedure gt__hduvw_i
        module procedure gt__hduvw_r
        module procedure gt__i_hduvw
        module procedure gt__r_hduvw
    end interface
    interface operator (/=)
        !! Inequality of the function values
        module procedure ne__duvw_duvw
        module procedure ne__duvw_i
        module procedure ne__duvw_r
        module procedure ne__i_duvw
        module procedure ne__r_duvw
        module procedure ne__hduvw_hduvw
        module procedure ne__hduvw_i
        module procedure ne__hduvw_r
        module procedure ne__i_hduvw
        module procedure ne__r_hduvw
    end interface
    interface abs
        !! Absolute value
        module procedure abs__duvw
    end interface
    interface dabs
        !! Absolute value (old Fortran name)
        module procedure abs__duvw
    end interface
    interface acos
        !! Arccosine
        module procedure acos__duvw
    end interface
    interface asin
        !! Arcsine
        module procedure asin__duvw
    end interface
    interface atan
        !! Arctangent
        module procedure atan__duvw
    end interface
    interface atan2
        !! Arctangent of y/x in the quadrant of (x, y)
        module procedure atan2__duvw
    end interface
    interface cos
        !! Cosine
        module procedure cos__duvw
    end interface
    interface dcos
        !! Cosine (old Fortran name)
        module procedure cos__duvw
    end interface
    interface dot_product
        !! Dot product of two dual vectors
        module procedure dot_product__duvw_duvw
    end interface
    interface exp
        !! Exponential
        module procedure exp__duvw
    end interface
    interface int
        !! Integer part of the function value
        module procedure int__duvw
    end interface
    interface log
        !! Natural logarithm
        module procedure log__duvw
        module procedure log__hduvw
    end interface
    interface log10
        !! Base-10 logarithm
        module procedure log10__duvw
    end interface
    interface matmul
        !! Matrix product of dual matrices and vectors
        module procedure matmul__duvw_duvw
        module procedure matmul__duvw_v
        module procedure matmul__v_duvw
    end interface
    interface max
        !! The larger of two numbers
        module procedure max__duvw_duvw
        module procedure max__duvw_i
        module procedure max__duvw_r
        module procedure max__r_duvw
    end interface
    interface dmax1
        !! The largest of two to five dual numbers
        module procedure dmax1__duvw_duvw
    end interface
    interface maxval
        !! The largest element of a dual vector
        module procedure maxval__duvw
    end interface
    interface min
        !! The smaller of two numbers
        module procedure min__duvw_duvw
        module procedure min__duvw_r
    end interface
    interface dmin1
        !! The smallest of two to four dual numbers
        module procedure dmin1__duvw_duvw
    end interface
    interface minval
        !! The smallest element of a dual vector
        module procedure minval__duvw
    end interface
    interface nint
        !! Nearest integer to the function value
        module procedure nint__duvw
    end interface
    interface sign
        !! abs(a) with the sign of b
        module procedure sign__duvw_duvw
        module procedure sign__r_duvw
    end interface
    interface sin
        !! Sine
        module procedure sin__duvw
    end interface
    interface dsin
        !! Sine (old Fortran name)
        module procedure sin__duvw
    end interface
    interface tan
        !! Tangent
        module procedure tan__duvw
    end interface
    interface dtan
        !! Tangent (old Fortran name)
        module procedure tan__duvw
    end interface
    interface sqrt
        !! Square root
        module procedure sqrt__duvw
        module procedure sqrt__hduvw
    end interface
    interface sum
        !! Sum of a dual vector
        module procedure sum__duvw
    end interface
    interface maxloc
        !! Location of the largest element of a dual vector
        module procedure maxloc__duvw
    end interface
    
contains

    function test_hdual2() result(is_ok)
        logical :: is_ok
           
        real(dp), parameter :: abs_tol = 1.0e-12_dp

        integer, parameter :: nval = 5

        integer :: i

        real(dp), dimension(nval) :: u, v, w
        real(dp), dimension(nval) :: r1, r2, r3
        integer, dimension(nval) :: i1, i2, i3
        real(dp), dimension(nval) :: fvd, fvh, fv_fasit
        real(dp), dimension(num_deriv, nval) :: dfd, dfh, df_fasit
        real(dp), dimension(num_deriv, num_deriv, nval) :: ddfh, ddf_fasit

        type(dual__uvw_t), dimension(nval) :: ud, vd, wd, fd
        type(hdual__uvw_t), dimension(nval) :: uhd, vhd, whd, fhd

        call random_number(u)
        u = 5*u + 0.1_dp
        call random_number(v)
        v = 5*v + 0.1_dp
        call random_number(w)
        w = 5*w + 0.1_dp
        call random_number(r1)
        r1 = 5*r1 + 0.1_dp
        call random_number(r2)
        r2 = 5*r2 + 0.1_dp
        call random_number(r3)
        r3 = 5*r3 + 0.1_dp
        i1 = ceiling(r1)
        i2 = ceiling(r2)
        i3 = ceiling(r3)


        ! Initialize dual inputs:
        do i = 1, nval

            call initialize(ud(i), u(i), 1)
            call initialize(vd(i), v(i), 2)
            call initialize(wd(i), w(i), 3)

            call initialize(uhd(i), u(i), 1)
            call initialize(vhd(i), v(i), 2)
            call initialize(whd(i), w(i), 3)

        end do

        fd = test_function_dual(ud, vd, wd, r1, r2, r3, i1, i2, i3)
        fhd = test_function_hdual(uhd, vhd, whd, r1, r2, r3, i1, i2, i3)

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
            call test_function_analytic(u(i), v(i), w(i), r1(i), r2(i), r3(i), i1(i), i2(i), i3(i), &
                    fv_fasit(i), df_fasit(:, i), ddf_fasit(:, :, i))
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
            print *, "test_hdual2: ok"
        else
            print *, "test_hdual2: failed"
        end if

        print*,'Hess matrix: '
        do i = 1, num_deriv
            print*,ddfh(i,:,1)
        end do
      
    end function

    elemental function test_function_dual(u, v, w, r1, r2, r3, i1, i2, i3) result(f)
        type(dual__uvw_t), intent(in) :: u, v, w
        real(dp), intent(in) :: r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        type(dual__uvw_t) :: f
        f = i3/(2*v + 1) + r3/(2*u + 1) - u/(2*w + 1) + u/(2*i2 + 1) + u**i1 + u &
            **r1 - u**v + sqrt(v)*log(u) + w/(2*r2 + 1) + (i1*v - i2*u + r1*u &
            - r2*w)**2
    end function
    elemental function test_function_hdual(u, v, w, r1, r2, r3, i1, i2, i3) result(f)
        type(hdual__uvw_t), intent(in) :: u, v, w
        real(dp), intent(in) :: r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        type(hdual__uvw_t) :: f
        f = i3/(2*v + 1) + r3/(2*u + 1) - u/(2*w + 1) + u/(2*i2 + 1) + u**i1 + u &
            **r1 - u**v + sqrt(v)*log(u) + w/(2*r2 + 1) + (i1*v - i2*u + r1*u &
            - r2*w)**2
    end function

    pure subroutine test_function_analytic(u, v, w, r1, r2, r3, i1, i2, i3, f, df, ddf)
        !! Test function with analytic gradient and Hessian. (Thanks to Sympy fortran-generator!)
        real(dp), intent(in) :: u, v, w, r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        real(dp), intent(out) :: f           ! function value
        real(dp), intent(out) :: df(num_deriv)       ! gradient
        real(dp), intent(out) :: ddf(num_deriv, num_deriv)   ! Hessian
        f = i3/(2*v + 1) + r3/(2*u + 1) - u/(2*w + 1) + u/(2*i2 + 1) + u**i1 + u &
            **r1 - u**v + sqrt(v)*log(u) + w/(2*r2 + 1) + (i1*v - i2*u + r1*u &
            - r2*w)**2
        df(1) = i1*u**i1/u + r1*u**r1/u - 2*r3/(2*u + 1)**2 + (-2*i2 + 2*r1)* &
            (i1*v - i2*u + r1*u - r2*w) - 1/(2*w + 1) + 1d0/(2*i2 + 1) - u**v &
            *v/u + sqrt(v)/u
        df(2) = 2*i1*(i1*v - i2*u + r1*u - r2*w) - 2*i3/(2*v + 1)**2 - u**v* &
            log(u) + (1.0d0/2.0d0)*log(u)/sqrt(v)
        df(3) = -2*r2*(i1*v - i2*u + r1*u - r2*w) + 2*u/(2*w + 1)**2 + 1d0/(2 &
            *r2 + 1)
        ddf(1, 1) = i1**2*u**i1/u**2 - i1*u**i1/u**2 + r1**2*u**r1/u**2 - r1*u** &
            r1/u**2 + 8*r3/(2*u + 1)**3 + (-2*i2 + 2*r1)*(-i2 + r1) - u**v*v &
            **2/u**2 + u**v*v/u**2 - sqrt(v)/u**2
        ddf(2, 1) = i1*(-2*i2 + 2*r1) - u**v*v*log(u)/u - u**v/u + (1.0d0/2.0d0) &
            /(u*sqrt(v))
        ddf(3, 1) = -r2*(-2*i2 + 2*r1) + 2/(2*w + 1)**2
        ddf(1, 2) = i1*(-2*i2 + 2*r1) - u**v*v*log(u)/u - u**v/u + (1.0d0/2.0d0) &
            /(u*sqrt(v))
        ddf(2, 2) = 2*i1**2 + 8*i3/(2*v + 1)**3 - u**v*log(u)**2 - 1.0d0/4.0d0* &
            log(u)/v**(3.0d0/2.0d0)
        ddf(3, 2) = -2*i1*r2
        ddf(1, 3) = -r2*(-2*i2 + 2*r1) + 2/(2*w + 1)**2
        ddf(2, 3) = -2*i1*r2
        ddf(3, 3) = 2*r2**2 - 8*u/(2*w + 1)**3
    end subroutine

    pure subroutine initialize__d_uvw_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual__uvw_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize__d_uvw_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual__uvw_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize__hd_uvw_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual__uvw_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%d%f = val
        hdual%d%g = 0
        hdual%h = 0
        hdual%d%g(idiff) = 1

    end subroutine
    pure subroutine initialize__hd_uvw_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual__uvw_t), intent(out) :: hdual(:)
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
    pure function fvalue__d_uvw(d) result(f)
        type(dual__uvw_t), intent(in) :: d
        real(dp) :: f
        f = d%f
    end function
    pure function fvalue__d_uvw_r1(d) result(f)
        type(dual__uvw_t), intent(in) :: d(:)
        real(dp) :: f(size(d))
        f = d%f
    end function
    pure function fvalue__d_uvw_r2(d) result(f)
        type(dual__uvw_t), intent(in) :: d(:,:)
        real(dp) :: f(size(d,1),size(d,2))
        f = d%f
    end function
    pure function fvalue__hd_uvw(hd) result(f)
        type(hdual__uvw_t), intent(in) :: hd
        real(dp) :: f
        f = hd%d%f
    end function
    pure function fvalue__hd_uvw_r1(hd) result(f)
        type(hdual__uvw_t), intent(in) :: hd(:)
        real(dp) :: f(size(hd))
        f = hd%d%f
    end function
    pure function fvalue__hd_uvw_r2(hd) result(f)
        type(hdual__uvw_t), intent(in) :: hd(:,:)
        real(dp) :: f(size(hd,1),size(hd,2))
        f = hd%d%f
    end function

    pure function downgrade__hd_uvw(hd) result(d)
        type(hdual__uvw_t), intent(in) :: hd
        type(dual__uvw_t) :: d
        d = hd%d
    end function

    pure function gradient__d_uvw(d) result(g)
        type(dual__uvw_t), intent(in) :: d
        real(dp) :: g(size(d%g))
        g = d%g
    end function
    pure function gradient__hd_uvw(hd) result(g)
        type(hdual__uvw_t), intent(in) :: hd
        real(dp) :: g(size(hd%d%g))
        g = hd%d%g
    end function
    pure function gradient_d__hd_uvw(hd) result(g)
        type(hdual__uvw_t), intent(in) :: hd
        type(dual__uvw_t) :: g(size(hd%d%g))

        real(dp) :: hess(size(hd%d%g), size(hd%d%g))
        integer :: i

        hess = hessian(hd)
        do i = 1, size(hd%d%g)
            g(i)%f = hd%d%g(i)
            g(i)%g = hess(:,i)
        end do
    end function

    pure function hessian__hd_uvw(hd) result(m)
        type(hdual__uvw_t), intent(in) :: hd
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
    elemental subroutine assign__duvw_i(u, i)
        type(dual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__duvw_r(u, r)
        type(dual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__i_duvw(i, v)
        type(dual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign__r_duvw(r, v)
        type(dual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental subroutine assign__hduvw_i(u, i)
        type(hdual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__hduvw_r(u, r)
        type(hdual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign__i_hduvw(i, v)
        type(hdual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)

    end subroutine
    elemental subroutine assign__r_hduvw(r, v)
        type(hdual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f

    end subroutine
    elemental function add__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
    end function
    elemental function add__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function minus__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = -u%f
        res%g = -u%g
    end function
    elemental function minus__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
    end function
    elemental function minus__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v
        res%g = u%g
    end function
    elemental function minus__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
    end function
    elemental function minus__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        
        res%d%f = -u%d%f
        res%d%g = -u%d%g
        res%h = -u%h
    end function
    elemental function minus__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v%d%f
        res%d%g = u%d%g - v%d%g
        res%h = u%h - v%h
    end function
    elemental function minus__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function minus__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function minus__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
    end function
    elemental function mult__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
    end function
    elemental function mult__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
    end function
    elemental function mult__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
    end function
    elemental function mult__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function mult__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function div__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = t0*u%f
        res%f = t1
        res%g = t0*(-t1*v%g + u%g)
    end function
    elemental function div__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
    end function
    elemental function div__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
    end function
    elemental function div__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
    end function
    elemental function div__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
    end function
    elemental function div__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function div__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
    end function
    elemental function div__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function div__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
    end function
    elemental function div__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function pow__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
    end function
    elemental function pow__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

    end function
    elemental function pow__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function pow__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function pow__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function hypot__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u%f, v%f)
        res%f = t0
        res%g = (u%g*u%f + v%g*v%f)/t0
    end function
    elemental function hypot__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u%f, v)
        res%f = t0
        res%g = u%g*u%f/t0
    end function
    elemental function hypot__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = hypot(u, v%f)
        res%f = t0
        res%g = v%g*v%f/t0
    end function
    elemental function hypot__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function hypot__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental function hypot__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
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
    elemental  function eq__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f == rhs%f)

    end function
    elemental  function eq__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    elemental  function eq__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    elemental  function eq__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    elemental  function eq__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    elemental  function eq__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f == rhs%d%f)

    end function
    elemental  function eq__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    elemental  function eq__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    elemental  function eq__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    elemental  function eq__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    elemental  function le__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f <= rhs%f)

    end function
    elemental  function le__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    elemental  function le__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    elemental  function le__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    elemental  function le__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    elemental  function le__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f <= rhs%d%f)

    end function
    elemental  function le__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    elemental  function le__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    elemental  function le__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    elemental  function le__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    elemental  function lt__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f < rhs%f)

    end function
    elemental  function lt__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    elemental  function lt__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    elemental  function lt__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    elemental  function lt__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    elemental  function lt__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f < rhs%d%f)

    end function
    elemental  function lt__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    elemental  function lt__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    elemental  function lt__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    elemental  function lt__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    elemental  function ge__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f >= rhs%f)

    end function
    elemental  function ge__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    elemental  function ge__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    elemental  function ge__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    elemental  function ge__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    elemental  function ge__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f >= rhs%d%f)

    end function
    elemental  function ge__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    elemental  function ge__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    elemental  function ge__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    elemental  function ge__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    elemental  function gt__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f > rhs%f)

    end function
    elemental  function gt__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    elemental  function gt__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    elemental  function gt__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    elemental  function gt__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    elemental  function gt__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f > rhs%d%f)

    end function
    elemental  function gt__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    elemental  function gt__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    elemental  function gt__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    elemental  function gt__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    elemental  function ne__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f /= rhs%f)

    end function
    elemental  function ne__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    elemental  function ne__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    elemental  function ne__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    elemental  function ne__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    elemental  function ne__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f /= rhs%d%f)

    end function
    elemental  function ne__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    elemental  function ne__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    elemental  function ne__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    elemental  function ne__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    elemental  function abs__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        integer :: i

        res%f = abs(u%f)
        res%g = sign(1.0_dp, u%f)*u%g

    end function
    elemental  function acos__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = acos(u%f)
    
        res%g = -u%g / sqrt(1.0_dp - u%f**2)

    end function
    elemental  function asin__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = asin(u%f)
        res%g = u%g / sqrt(1.0_dp - u%f**2)

    end function
    elemental  function atan__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = atan(u%f)
        res%g = u%g / (1.0_dp + u%f**2)

    end function
    elemental  function atan2__duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u, v
        type(dual__uvw_t) :: res

        real(dp) :: usq_plus_vsq

        res%f = atan2(u%f, v%f)

        usq_plus_vsq = u%f**2 + v%f**2
        res%g = v%f / usq_plus_vsq * u%g - u%f / usq_plus_vsq * v%g

    end function
    elemental  function cos__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = cos(u%f)
        res%g = -sin(u%f) * u%g

    end function
    pure  function dot_product__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:), v(:)
        type(dual__uvw_t) :: res

        integer :: i

        res%f = dot_product(u%f, v%f)
        do i = 1, size(res%g)
            res%g(i) = dot_product(u%f, v%g(i)) + dot_product(v%f, u%g(i))
        end do

    end function
    elemental  function exp__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%f)
        res%f = exp_x
        res%g = u%g * exp_x

    end function
    elemental  function int__duvw(u) result(res)
         type(dual__uvw_t), intent(in) :: u
         integer :: res

         res = int(u%f)

    end function
    elemental function log__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
    end function
    elemental function log__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
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
    elemental  function log10__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%f * log(10.0_dp))
        res%f = log10(u%f)
        res%g = u%g * inv

    end function
    pure  function matmul__duvw_duvw(u,v) result(res)
        type(dual__uvw_t), intent(in) :: u(:,:), v(:,:)
        type(dual__uvw_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1,1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    pure  function matmul__duvw_v(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:,:), v(:)
        type(dual__uvw_t) :: res(size(u,1))
        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    pure  function matmul__v_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:), v(:,:)
        type(dual__uvw_t) :: res(size(v, 2))
        integer::i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    elemental  function max__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val1%f > val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    elemental  function max__duvw_i(u, i) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual__uvw_t) :: res

        if (u%f > i) then
            res = u
        else
            res = i
        endif

    end function
    elemental  function max__duvw_r(u, r) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__uvw_t) :: res

        if (u%f > r) then
            res = u
        else
            res = r
        endif

    end function
     elemental  function max__r_duvw(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        if (u%f > n) then
            res = u
        else
            res = n
        endif

    end function
    elemental  function dmax1__duvw_duvw(val1, val2, val3, val4,val5) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t), intent(in), optional :: val3, val4,val5
        type(dual__uvw_t) :: res

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
    pure  function maxval__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__uvw_t) :: res

        iloc=maxloc(u%f)
        res=u(iloc(1))

    end function
    elemental  function min__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val1%f < val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    elemental  function min__duvw_r(u, r) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__uvw_t) :: res

        if (u%f < r) then
            res = u
        else
            res = r
        endif

    end function
    elemental  function dmin1__duvw_duvw(val1, val2, val3, val4) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t), intent(in), optional :: val3, val4
        type(dual__uvw_t) :: res

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
    pure  function minval__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__uvw_t) :: res

        iloc=minloc(u%f)
        res=u(iloc(1))

    end function
    elemental  function nint__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer :: res

        res=nint(u%f)

    end function
    elemental  function sign__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    elemental  function sign__r_duvw(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual__uvw_t), intent(in) :: val2
        type(dual__uvw_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    elemental  function sin__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = sin(u%f)
        res%g = cos(u%f) * u%g

    end function
    elemental  function tan__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = tan(u%f)
        res%g = u%g / cos(u%f)**2

    end function
    elemental function sqrt__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
    end function

    elemental function sqrt__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
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
    pure  function sum__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        type(dual__uvw_t) :: res
        integer :: i

        res%f = sum(u%f)
        do i = 1, size(res%g)
            res%g(i) = sum(u%g(i))
        end do

    end function
    pure  function maxloc__duvw(array) result(ind)
        type(dual__uvw_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%f)

    end function
    
    
end module