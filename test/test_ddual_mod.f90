module test_ddual_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 2

    

    type :: dual_xy
        !! dual number type
        sequence
        real(dp) :: x  ! functional value
        real(dp) :: dx(num_deriv)  ! derivative
    end type

    type :: ddual_xy
        !! double dual number type with dual dx component
        sequence
        real(dp) :: x  ! functional value
        type(dual_xy) :: dx(num_deriv)  ! derivative
    end type


    interface assignment (=)
        module procedure assign_d_i  ! dual=integer, elemental
        module procedure assign_d_r  ! dual=real, elemental
        module procedure assign_i_d  ! integer=dual, elemental
        module procedure assign_r_d  ! real=dual, elemental
        module procedure assign_dd_i  ! dual=integer, elemental
        module procedure assign_dd_r  ! dual=real, elemental
        module procedure assign_i_dd  ! integer=dual, elemental
        module procedure assign_r_dd  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure add_unary_d   ! +dual number, elemental
        module procedure add_d_d       ! dual + dual, elemental
        module procedure add_d_i       ! dual + integer, elemental
        module procedure add_d_r       ! dual + real, elemental
        module procedure add_i_d       ! integer + dual, elemental
        module procedure add_r_d       ! real + dual, elemental
        module procedure add_unary_dd  ! +dual number, elemental
        module procedure add_dd_dd     ! dual + dual, elemental
        module procedure add_dd_i      ! dual + integer, elemental
        module procedure add_dd_r      ! dual + real, elemental
        module procedure add_i_dd      ! integer + dual, elemental
        module procedure add_r_dd      ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure minus_unary_d  ! negate a dual number,elemental
        module procedure minus_d_d      ! dual -dual,elemental
        module procedure minus_d_i      ! dual-integer,elemental
        module procedure minus_d_r      ! dual-real,elemental
        module procedure minus_i_d      ! integer-dual,elemental
        module procedure minus_r_d      ! real-dual,elemental
        module procedure minus_unary_dd ! negate a dual number,elemental
        module procedure minus_dd_dd    ! dual -dual,elemental
        module procedure minus_dd_i     ! dual-integer,elemental
        module procedure minus_dd_r     ! dual-real,elemental
        module procedure minus_i_dd     ! integer-dual,elemental
        module procedure minus_r_dd     ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult_d_d    ! dual*dual, elemental
        module procedure mult_d_i    ! dual*integer,elemental
        module procedure mult_d_r    ! dual*real,elemental
        module procedure mult_i_d    ! integer*dual,elemental
        module procedure mult_r_d    ! real*dual,elemental
        module procedure mult_dd_dd  ! dual*dual, elemental
        module procedure mult_dd_i   ! dual*integer,elemental
        module procedure mult_dd_r   ! dual*real,elemental
        module procedure mult_i_dd   ! integer*dual,elemental
        module procedure mult_r_dd   ! real*dual,elemental
    end interface
    interface operator (/)
        module procedure div_dd ! dual/dual,elemental
        module procedure div_di ! dual/integer, elemental
        module procedure div_dr ! dual/real,emental
        module procedure div_id ! integer/dual, elemental
        module procedure div_rd ! real/dual, elemental
    end interface
    interface operator (**)
        module procedure pow_i ! dual number to an integer power,elemental
        module procedure pow_r ! dual number to a real power, elemental
        module procedure pow_d ! dual number to a dual power, elemental
    end interface
    interface log
        module procedure log_d ! log of a dual number, elemental
        module procedure log_dd ! log of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt_d ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_dd ! obtain the sqrt of a dual number, elemental
    end interface
    
contains

    function test_ddual() result(is_ok)
        logical :: is_ok
           
        integer, parameter :: nval = 1

        integer :: i

        real(dp), dimension(nval) :: x, y
        real(dp), dimension(nval) :: f, f_fasit
        real(dp), dimension(2, nval) :: df, df_fasit
        real(dp), dimension(2, 2, nval) :: ddf, ddf_fasit

        type(ddual_xy), dimension(nval) :: xd, yd, fd

        call random_number(x)
        call random_number(y)

        ! Initialize dual inputs:
        do i = 1, nval
            xd(i)%x = x(i)
            xd(i)%dx(1)%x       = 1.0_dp    ! dx/dx
            xd(i)%dx(1)%dx(1:2) = [1.0_dp, 0.0_dp]         ! [d**2x/dx**2, d**2x/dxdy] 
            xd(i)%dx(2)%x       = 0         ! dx/dy
            xd(i)%dx(2)%dx(1:2) = [0.0_dp, 1.0_dp]         ! [d**2x/dxdy, d**2x/dydy] 

            yd(i)%x = y(i)
            yd(i)%dx(1)%x       = 0         ! dy/dx
            yd(i)%dx(1)%dx(1:2) = [1.0_dp, 0.0_dp]         ! [d**2y/dx**2, d**2y/dxdy] 
            yd(i)%dx(2)%x       = 1.0_dp    ! dy/dy
            yd(i)%dx(2)%dx(1:2) = [0.0_dp, 1.0_dp]         ! [d**2y/dxdy, d**2y/dy**2] 
        end do

        fd = test_function_ddual(xd, yd)

        ! Extract function value, gradient and hessian for all values:
        do i = 1, nval
            f(i) = fd(i)
            df(1, i) = fd(i)%dx(1)%x
            df(2, i) = fd(i)%dx(2)%x
            ddf(1, 1, i) = fd(i)%dx(1)%dx(1)
            ddf(2, 1, i) = fd(i)%dx(2)%dx(1)
            ddf(1, 2, i) = fd(i)%dx(1)%dx(2)
            ddf(2, 2, i) = fd(i)%dx(2)%dx(2)
        end do

        ! Fasit:
        do i = 1, nval
            call test_function_analytic(x(i), y(i), f_fasit(i), df_fasit(:, i), ddf_fasit(:, :, i))
        end do

        print*, "f:"
        print*, f
        print*, "f_fasit:"
        print*, f_fasit
        print*, "df:"
        print*, df
        print*, "df_fasit:"
        print*, df_fasit
        print*, "ddf:"
        print*, ddf
        print*, "ddf_fasit:"
        print*, ddf_fasit
      
    end function

    elemental function test_function_ddual(x, y) result(f)
        type(ddual_xy), intent(in) :: x, y
        type(ddual_xy) :: f
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
        ddf(1, 2) = ddf(1, 2)              ! d**2f/dydx
    end subroutine

    elemental subroutine assign_d_i(u, i)
         type(dual_xy), intent(out) :: u
         integer, intent(in) :: i

         u%x = real(i, dp)  ! This is faster than direct assignment
         u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_d_r(u, r)
        type(dual_xy), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_d(i, v)
         type(dual_xy), intent(in) :: v
         integer, intent(out) :: i

         i = int(v%x)

    end subroutine
    elemental subroutine assign_r_d(r, v)
         type(dual_xy), intent(in) :: v
         real(dp), intent(out) :: r

         r = v%x

    end subroutine
    elemental subroutine assign_dd_i(u, i)
         type(ddual_xy), intent(out) :: u
         integer, intent(in) :: i

         u%x = real(i, dp)  ! This is faster than direct assignment
         u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_dd_r(u, r)
        type(ddual_xy), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_dd(i, v)
         type(ddual_xy), intent(in) :: v
         integer, intent(out) :: i

         i = int(v%x)

    end subroutine
    elemental subroutine assign_r_dd(r, v)
         type(ddual_xy), intent(in) :: v
         real(dp), intent(out) :: r

         r = v%x

    end subroutine
    elemental function add_unary_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res = u  ! Faster than assigning component wise

    end function
    elemental function add_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u, v
        type(dual_xy) :: res

        res%x = u%x + v%x
        res%dx = u%dx + v%dx

    end function
    elemental function add_d_i(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        res%x = real(i, dp) + u%x
        res%dx = u%dx

    end function
    elemental function add_d_r(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        res%x = r + u%x
        res%dx = u%dx

    end function
    elemental function add_i_d(i, v) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        res%x = real(i, dp) + v%x
        res%dx = v%dx

    end function
    elemental function add_r_d(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        res%x = r + v%x
        res%dx = v%dx

    end function
    elemental function add_unary_dd(u) result(res)
        type(ddual_xy), intent(in) :: u
        type(ddual_xy) :: res

        res = u  ! Faster than assigning component wise

    end function
    elemental function add_dd_dd(u, v) result(res)
        type(ddual_xy), intent(in) :: u, v
        type(ddual_xy) :: res

        res%x = u%x + v%x
        res%dx = u%dx + v%dx

    end function
    elemental function add_dd_i(u, i) result(res)
        type(ddual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(ddual_xy) :: res

        res%x = real(i, dp) + u%x
        res%dx = u%dx

    end function
    elemental function add_dd_r(u, r) result(res)
        type(ddual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(ddual_xy) :: res

        res%x = r + u%x
        res%dx = u%dx

    end function
    elemental function add_i_dd(i, v) result(res)
        integer, intent(in) :: i
        type(ddual_xy), intent(in) :: v
        type(ddual_xy) :: res

        res%x = real(i, dp) + v%x
        res%dx = v%dx

    end function
    elemental function add_r_dd(r, v) result(res)
        real(dp), intent(in) :: r
        type(ddual_xy), intent(in) :: v
        type(ddual_xy) :: res

        res%x = r + v%x
        res%dx = v%dx

    end function
    elemental function minus_unary_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = -u%x
        res%dx = -u%dx

    end function
    elemental function minus_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u, v
        type(dual_xy) :: res

        res%x = u%x - v%x
        res%dx = u%dx - v%dx

    end function
    elemental function minus_d_i(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        res%x = u%x - real(i, dp)
        res%dx = u%dx

    end function
    elemental function minus_d_r(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp),intent(in) :: r
        type(dual_xy) :: res

        res%x = u%x - r
        res%dx = u%dx

    end function
    elemental function minus_i_d(i, v) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        res%x = real(i, dp) - v%x
        res%dx = -v%dx

    end function
    elemental function minus_r_d(r, v) result(res)
         real(dp), intent(in) :: r
         type(dual_xy), intent(in) :: v
         type(dual_xy) :: res

        res%x = r - v%x
        res%dx = -v%dx

    end function
    elemental function minus_unary_dd(u) result(res)
        type(ddual_xy), intent(in) :: u
        type(ddual_xy) :: res

        res%x = -u%x
        res%dx = -u%dx

    end function
    elemental function minus_dd_dd(u, v) result(res)
        type(ddual_xy), intent(in) :: u, v
        type(ddual_xy) :: res

        res%x = u%x - v%x
        res%dx = u%dx - v%dx

    end function
    elemental function minus_dd_i(u, i) result(res)
        type(ddual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(ddual_xy) :: res

        res%x = u%x - real(i, dp)
        res%dx = u%dx

    end function
    elemental function minus_dd_r(u, r) result(res)
        type(ddual_xy), intent(in) :: u
        real(dp),intent(in) :: r
        type(ddual_xy) :: res

        res%x = u%x - r
        res%dx = u%dx

    end function
    elemental function minus_i_dd(i, v) result(res)
        integer, intent(in) :: i
        type(ddual_xy), intent(in) :: v
        type(ddual_xy) :: res

        res%x = real(i, dp) - v%x
        res%dx = -v%dx

    end function
    elemental function minus_r_dd(r, v) result(res)
         real(dp), intent(in) :: r
         type(ddual_xy), intent(in) :: v
         type(ddual_xy) :: res

        res%x = r - v%x
        res%dx = -v%dx

    end function
    elemental function mult_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u, v
        type(dual_xy) :: res

        res%x = u%x * v%x
        res%dx = u%x * v%dx + v%x * u%dx

    end function
    elemental function mult_d_i(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * u%x
        res%dx = r * u%dx

    end function
    elemental function mult_d_r(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        res%x = u%x * r
        res%dx = u%dx * r

    end function
    elemental function mult_i_d(i, v) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * v%x
        res%dx = r * v%dx

    end function
    elemental function mult_r_d(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        res%x = r * v%x
        res%dx = r * v%dx

    end function
    elemental function mult_dd_dd(u, v) result(res)
        type(ddual_xy), intent(in) :: u, v
        type(ddual_xy) :: res

        res%x = u%x * v%x
        res%dx = u%x * v%dx + v%x * u%dx

    end function
    elemental function mult_dd_i(u, i) result(res)
        type(ddual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(ddual_xy) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * u%x
        res%dx = r * u%dx

    end function
    elemental function mult_dd_r(u, r) result(res)
        type(ddual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(ddual_xy) :: res

        res%x = u%x * r
        res%dx = u%dx * r

    end function
    elemental function mult_i_dd(i, v) result(res)
        integer, intent(in) :: i
        type(ddual_xy), intent(in) :: v
        type(ddual_xy) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * v%x
        res%dx = r * v%dx

    end function
    elemental function mult_r_dd(r, v) result(res)
        real(dp), intent(in) :: r
        type(ddual_xy), intent(in) :: v
        type(ddual_xy) :: res

        res%x = r * v%x
        res%dx = r * v%dx

    end function
    elemental function div_dd(u, v) result(res)
        type(dual_xy), intent(in) :: u, v
        type(dual_xy) :: res

        res%x = u%x / v%x
        res%dx = (u%dx - res%x * v%dx) / v%x

    end function div_dd
    elemental function div_di(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        res%x = u%x / i
        res%dx = u%dx / i

    end function div_di
    elemental function div_dr(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        res%x = u%x / r
        res%dx = u%dx / r

    end function div_dr
    elemental function div_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = real(i, dp) * inv
        res%dx = -res%x * inv * v%dx

    end function div_id
    elemental function div_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = r * inv
        res%dx = -res%x * inv * v%dx

    end function div_rd
    elemental function pow_i(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (i - 1)
        res%x = u%x * pow_x
        res%dx = real(i, dp) * pow_x * u%dx

    end function pow_i
    elemental function pow_r(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (r - 1.0_dp)
        res%x = u%x * pow_x
        res%dx = r * pow_x * u%dx

    end function pow_r
    elemental function pow_d(u, v) result(res)
        type(dual_xy), intent(in)::u, v
        type(dual_xy) :: res

        res%x = u%x ** v%x
        res%dx = res%x * (v%x / u%x * u%dx + log(u%x) * v%dx)

    end function pow_d
    elemental function log_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = log(u%x)
        res%dx = u%dx / u%x

    end function
    elemental function log_dd(u) result(res)
        type(ddual_xy), intent(in) :: u
        type(ddual_xy) :: res

        res%x = log(u%x)
        res%dx = u%dx / u%x

    end function
    elemental function sqrt_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = sqrt(u%x)

        res%dx = 0.5_dp * u%dx / res%x

    end function
    elemental function sqrt_dd(u) result(res)
        type(ddual_xy), intent(in) :: u
        type(ddual_xy) :: res

        res%x = sqrt(u%x)

        res%dx = 0.5_dp * u%dx / res%x

    end function

    
end module