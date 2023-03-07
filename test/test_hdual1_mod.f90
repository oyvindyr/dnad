module test_hdual1_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 2

    ! Implementation call counters to show test coverage:
    integer :: assign_d_i_counter = 0
    integer :: assign_d_r_counter = 0
    integer :: assign_i_d_counter = 0
    integer :: assign_r_d_counter = 0
    integer :: assign_hd_i_counter = 0
    integer :: assign_hd_r_counter = 0
    integer :: assign_i_hd_counter = 0
    integer :: assign_r_hd_counter = 0
    integer :: unary_add_d_counter = 0
    integer :: add_d_d_counter = 0
    integer :: add_d_i_counter = 0
    integer :: add_d_r_counter = 0
    integer :: add_i_d_counter = 0
    integer :: add_r_d_counter = 0
    integer :: unary_add_hd_counter = 0
    integer :: add_hd_hd_counter = 0
    integer :: add_hd_i_counter = 0
    integer :: add_hd_r_counter = 0
    integer :: add_i_hd_counter = 0
    integer :: add_r_hd_counter = 0
    integer :: unary_minus_d_counter = 0
    integer :: minus_d_d_counter = 0
    integer :: minus_d_i_counter = 0
    integer :: minus_d_r_counter = 0
    integer :: minus_i_d_counter = 0
    integer :: minus_r_d_counter = 0
    integer :: unary_minus_hd_counter = 0
    integer :: minus_hd_hd_counter = 0
    integer :: minus_hd_i_counter = 0
    integer :: minus_hd_r_counter = 0
    integer :: minus_i_hd_counter = 0
    integer :: minus_r_hd_counter = 0
    integer :: mult_d_d_counter = 0
    integer :: mult_d_i_counter = 0
    integer :: mult_d_r_counter = 0
    integer :: mult_i_d_counter = 0
    integer :: mult_r_d_counter = 0
    integer :: mult_hd_hd_counter = 0
    integer :: mult_hd_i_counter = 0
    integer :: mult_hd_r_counter = 0
    integer :: mult_i_hd_counter = 0
    integer :: mult_r_hd_counter = 0
    integer :: div_d_d_counter = 0
    integer :: div_d_r_counter = 0
    integer :: div_r_d_counter = 0
    integer :: div_d_i_counter = 0
    integer :: div_i_d_counter = 0
    integer :: div_hd_hd_counter = 0
    integer :: div_hd_r_counter = 0
    integer :: div_r_hd_counter = 0
    integer :: div_hd_i_counter = 0
    integer :: div_i_hd_counter = 0
    integer :: pow_d_i_counter = 0
    integer :: pow_d_r_counter = 0
    integer :: pow_d_d_counter = 0
    integer :: pow_hd_i_counter = 0
    integer :: pow_hd_r_counter = 0
    integer :: pow_hd_hd_counter = 0
    integer :: eq_dd_counter = 0
    integer :: eq_di_counter = 0
    integer :: eq_dr_counter = 0
    integer :: eq_id_counter = 0
    integer :: eq_rd_counter = 0
    integer :: le_dd_counter = 0
    integer :: le_di_counter = 0
    integer :: le_dr_counter = 0
    integer :: le_id_counter = 0
    integer :: le_rd_counter = 0
    integer :: lt_dd_counter = 0
    integer :: lt_di_counter = 0
    integer :: lt_dr_counter = 0
    integer :: lt_id_counter = 0
    integer :: lt_rd_counter = 0
    integer :: ge_dd_counter = 0
    integer :: ge_di_counter = 0
    integer :: ge_dr_counter = 0
    integer :: ge_id_counter = 0
    integer :: ge_rd_counter = 0
    integer :: gt_dd_counter = 0
    integer :: gt_di_counter = 0
    integer :: gt_dr_counter = 0
    integer :: gt_id_counter = 0
    integer :: gt_rd_counter = 0
    integer :: ne_dd_counter = 0
    integer :: ne_di_counter = 0
    integer :: ne_dr_counter = 0
    integer :: ne_id_counter = 0
    integer :: ne_rd_counter = 0
    integer :: abs_d_counter = 0
    integer :: acos_d_counter = 0
    integer :: asin_d_counter = 0
    integer :: atan_d_counter = 0
    integer :: atan2_d_counter = 0
    integer :: cos_d_counter = 0
    integer :: dot_product_dd_counter = 0
    integer :: exp_d_counter = 0
    integer :: int_d_counter = 0
    integer :: log_d_counter = 0
    integer :: log_hd_counter = 0
    integer :: log10_d_counter = 0
    integer :: matmul_dd_counter = 0
    integer :: matmul_dv_counter = 0
    integer :: matmul_vd_counter = 0
    integer :: max_dd_counter = 0
    integer :: max_di_counter = 0
    integer :: max_dr_counter = 0
    integer :: max_rd_counter = 0
    integer :: dmax1_dd_counter = 0
    integer :: maxval_d_counter = 0
    integer :: min_dd_counter = 0
    integer :: min_dr_counter = 0
    integer :: dmin1_dd_counter = 0
    integer :: minval_d_counter = 0
    integer :: nint_d_counter = 0
    integer :: sin_d_counter = 0
    integer :: tan_d_counter = 0
    integer :: sqrt_d_counter = 0
    integer :: sqrt_hd_counter = 0
    integer :: sum_d_counter = 0
    integer :: maxloc_d_counter = 0

    
    type :: dual_xy
        !! dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivative
    end type
    type :: hdual_xy
        !! hyper-dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivative
        real(dp) :: ddx(num_deriv, num_deriv) = 0  ! derivative
    end type

    interface assignment (=)
        module procedure assign_d_i  ! dual=integer, elemental
        module procedure assign_d_r  ! dual=real, elemental
        module procedure assign_i_d  ! integer=dual, elemental
        module procedure assign_r_d  ! real=dual, elemental
        module procedure assign_hd_i  ! dual=integer, elemental
        module procedure assign_hd_r  ! dual=real, elemental
        module procedure assign_i_hd  ! integer=dual, elemental
        module procedure assign_r_hd  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure unary_add_d   ! +dual number, elemental
        module procedure add_d_d       ! dual + dual, elemental
        module procedure add_d_i       ! dual + integer, elemental
        module procedure add_d_r       ! dual + real, elemental
        module procedure add_i_d       ! integer + dual, elemental
        module procedure add_r_d       ! real + dual, elemental
        module procedure unary_add_hd  ! +dual number, elemental
        module procedure add_hd_hd     ! dual + dual, elemental
        module procedure add_hd_i      ! dual + integer, elemental
        module procedure add_hd_r      ! dual + real, elemental
        module procedure add_i_hd      ! integer + dual, elemental
        module procedure add_r_hd      ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure unary_minus_d  ! negate a dual number,elemental
        module procedure minus_d_d      ! dual -dual,elemental
        module procedure minus_d_i      ! dual-integer,elemental
        module procedure minus_d_r      ! dual-real,elemental
        module procedure minus_i_d      ! integer-dual,elemental
        module procedure minus_r_d      ! real-dual,elemental
        module procedure unary_minus_hd ! negate a dual number,elemental
        module procedure minus_hd_hd    ! dual -dual,elemental
        module procedure minus_hd_i     ! dual-integer,elemental
        module procedure minus_hd_r     ! dual-real,elemental
        module procedure minus_i_hd     ! integer-dual,elemental
        module procedure minus_r_hd     ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult_d_d    ! dual*dual, elemental
        module procedure mult_d_i    ! dual*integer,elemental
        module procedure mult_d_r    ! dual*real,elemental
        module procedure mult_i_d    ! integer*dual,elemental
        module procedure mult_r_d    ! real*dual,elemental
        module procedure mult_hd_hd  ! dual*dual, elemental
        module procedure mult_hd_i   ! dual*integer,elemental
        module procedure mult_hd_r   ! dual*real,elemental
        module procedure mult_i_hd   ! integer*dual,elemental
        module procedure mult_r_hd   ! real*dual,elemental
    end interface
    interface operator (/)
        module procedure div_d_d    ! dual/dual, elemental
        module procedure div_d_r    ! dual/real, elemental
        module procedure div_r_d    ! real/dual, elemental
        module procedure div_d_i    ! dual/integer, elemental
        module procedure div_i_d    ! integer/dual, elemental
        module procedure div_hd_hd  ! hdual/hdual, elemental
        module procedure div_hd_r   ! hdual/real, elemental
        module procedure div_r_hd   ! real/hdual, elemental
        module procedure div_hd_i   ! hdual/integer, elemental
        module procedure div_i_hd   ! integer/hdual, elemental
    end interface
    interface operator (**)
        module procedure pow_d_i ! dual number to an integer power,elemental
        module procedure pow_d_r ! dual number to a real power, elemental
        module procedure pow_d_d ! dual number to a dual power, elemental
        module procedure pow_hd_i ! hdual number to an integer power,elemental
        module procedure pow_hd_r ! hdual number to a real power, elemental
        module procedure pow_hd_hd ! hdual number to a hdual power, elemental
    end interface
    interface operator (==)
        module procedure eq_dd ! compare two dual numbers, elemental
        module procedure eq_di ! compare a dual and an integer, elemental
        module procedure eq_dr ! compare a dual and a real, elemental
        module procedure eq_id ! compare integer with a dual number, elemental
        module procedure eq_rd ! compare a real with a dual number, elemental
    end interface
    interface operator (<=)
        module procedure le_dd  ! compare two dual numbers, elemental
        module procedure le_di  ! compare a dual and an integer, elemental
        module procedure le_dr  ! compare a dual and a real,elemental
        module procedure le_id ! compare integer with a dual number, elemental
        module procedure le_rd ! compare a real with a dual number, elemental
    end interface
    interface operator (<)
        module procedure lt_dd  !compare two dual numbers, elemental
        module procedure lt_di  !compare a dual and an integer, elemental
        module procedure lt_dr  !compare dual with a real, elemental
        module procedure lt_id ! compare integer with a dual number, elemental
        module procedure lt_rd ! compare a real with a dual number, elemental
    end interface
    interface operator (>=)
        module procedure ge_dd ! compare two dual numbers, elemental
        module procedure ge_di ! compare dual with integer, elemental
        module procedure ge_dr ! compare dual with a real number, elemental
        module procedure ge_id ! compare integer with a dual number, elemental
        module procedure ge_rd ! compare a real with a dual number, elemental
    end interface
    interface operator (>)
        module procedure gt_dd  !compare two dual numbers, elemental
        module procedure gt_di  !compare a dual and an integer, elemental
        module procedure gt_dr  !compare dual with a real, elemental
        module procedure gt_id ! compare integer with a dual number, elemental
        module procedure gt_rd ! compare a real with a dual number, elemental
    end interface
    interface operator (/=)
        module procedure ne_dd  !compare two dual numbers, elemental
        module procedure ne_di  !compare a dual and an integer, elemental
        module procedure ne_dr  !compare dual with a real, elemental
        module procedure ne_id ! compare integer with a dual number, elemental
        module procedure ne_rd ! compare a real with a dual number, elemental
    end interface
    interface abs
        module procedure abs_d  ! absolute value of a dual number, elemental
    end interface
    interface dabs
        module procedure abs_d ! same as abs, used for some old fortran commands
    end interface
    interface acos
        module procedure acos_d ! arccosine of a dual number, elemental
    end interface
    interface asin
        module procedure asin_d ! arcsine of a dual number, elemental
    end interface
    interface atan
        module procedure atan_d ! arctan of a dual number, elemental
    end interface
    interface atan2
        module procedure atan2_d ! arctan of a dual number, elemental
    end interface
    interface cos
        module procedure cos_d ! cosine of a dual number, elemental
    end interface
    interface dcos
        module procedure cos_d ! cosine of a dual number, elemental
    end interface
    interface dot_product
        module procedure dot_product_dd ! dot product two dual number vectors
    end interface
    interface exp
        module procedure exp_d ! exponential of a dual number, elemental
    end interface
    interface int
        module procedure int_d ! integer part of a dual number, elemental
    end interface
    interface log
        module procedure log_d ! log of a dual number, elemental
        module procedure log_hd ! log of a dual number, elemental
    end interface
    interface log10
        module procedure log10_d ! log of a dual number, elemental
    end interface
    interface matmul
        module procedure matmul_dd ! multiply two dual matrices
        module procedure matmul_dv ! multiply a dual matrix with a dual vector
        module procedure matmul_vd ! multiply a dual vector with a dual matrix
    end interface
    interface max
        module procedure max_dd ! max of two dual numbers, elemental
        module procedure max_di ! max of a dual number and an integer, elemental
        module procedure max_dr ! max of a dual number and a real, elemental
        module procedure max_rd ! max of a real,and a dual number,  elemental
    end interface
    interface dmax1
        module procedure dmax1_dd ! max of from two to four dual numbers, elemental
    end interface
    interface maxval
        module procedure maxval_d ! maxval of a dual number vector
    end interface
    interface min
        module procedure min_dd ! min of from two to four dual numbers, elemental
        module procedure min_dr ! min of a dual and a real, elemental
    end interface
    interface dmin1
        module procedure dmin1_dd ! min of from two to four dual numbers, elemental
    end interface
    interface minval
        module procedure minval_d ! obtain the maxval  of a dual number vectgor
    end interface
    interface nint
        module procedure nint_d ! nearest integer to the argument, elemental
    end interface
    interface  sign
      module procedure  sign_dd ! sign(a,b) with two dual numbers, elemental
      module procedure  sign_rd ! sign(a,b) with a real and a dual, elemental
    end interface
    interface sin
        module procedure sin_d ! obtain sine of a dual number, elemental
    end interface
    interface dsin
        module procedure sin_d ! obtain sine of a dual number, elemental
    end interface
    interface tan
        module procedure tan_d ! obtain sine of a dual number, elemental
    end interface
    interface dtan
        module procedure tan_d ! obtain sine of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt_d ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_hd ! obtain the sqrt of a dual number, elemental
    end interface
    interface sum
        module procedure sum_d ! sum a dual array
    end interface
    interface maxloc
        module procedure maxloc_d ! location of max in a dual array
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

        type(dual_xy), dimension(nval) :: xd, yd, fd
        type(hdual_xy), dimension(nval) :: xhd, yhd, fhd

        call random_number(x)
        x = 5*x + 0.1_dp
        call random_number(y)
        y = 5*y + 0.1_dp


        ! Initialize dual inputs:
        do i = 1, nval
            xd(i)%x  = x(i)
            xd(i)%dx = [1.0_dp, 0.0_dp]
            
            yd(i)%x  = y(i)
            yd(i)%dx = [0.0_dp, 1.0_dp]

            xhd(i)%x  = x(i)
            xhd(i)%dx = [1.0_dp, 0.0_dp]
            xhd(i)%ddx = 0.0_dp

            yhd(i)%x  = y(i)
            yhd(i)%dx = [0.0_dp, 1.0_dp]
            yhd(i)%ddx = 0.0_dp

        end do

        fd = test_function_dual(xd, yd)
        fhd = test_function_hdual(xhd, yhd)

        ! Extract function value, gradient and hessian for all values:
        do i = 1, nval

            fvd(i) = fd(i)
            dfd(:, i) = fd(i)%dx

            fvh(i) = fhd(i)
            dfh(:, i) = fhd(i)%dx
            ddfh(:, :, i) = fhd(i)%ddx

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

        print*, "assign_d_i_counter: ", assign_d_i_counter
        print*, "assign_d_r_counter: ", assign_d_r_counter
        print*, "assign_i_d_counter: ", assign_i_d_counter
        print*, "assign_r_d_counter: ", assign_r_d_counter
        print*, "assign_hd_i_counter: ", assign_hd_i_counter
        print*, "assign_hd_r_counter: ", assign_hd_r_counter
        print*, "assign_i_hd_counter: ", assign_i_hd_counter
        print*, "assign_r_hd_counter: ", assign_r_hd_counter
        print*, "unary_add_d_counter: ", unary_add_d_counter
        print*, "add_d_d_counter: ", add_d_d_counter
        print*, "add_d_i_counter: ", add_d_i_counter
        print*, "add_d_r_counter: ", add_d_r_counter
        print*, "add_i_d_counter: ", add_i_d_counter
        print*, "add_r_d_counter: ", add_r_d_counter
        print*, "unary_add_hd_counter: ", unary_add_hd_counter
        print*, "add_hd_hd_counter: ", add_hd_hd_counter
        print*, "add_hd_i_counter: ", add_hd_i_counter
        print*, "add_hd_r_counter: ", add_hd_r_counter
        print*, "add_i_hd_counter: ", add_i_hd_counter
        print*, "add_r_hd_counter: ", add_r_hd_counter
        print*, "unary_minus_d_counter: ", unary_minus_d_counter
        print*, "minus_d_d_counter: ", minus_d_d_counter
        print*, "minus_d_i_counter: ", minus_d_i_counter
        print*, "minus_d_r_counter: ", minus_d_r_counter
        print*, "minus_i_d_counter: ", minus_i_d_counter
        print*, "minus_r_d_counter: ", minus_r_d_counter
        print*, "unary_minus_hd_counter: ", unary_minus_hd_counter
        print*, "minus_hd_hd_counter: ", minus_hd_hd_counter
        print*, "minus_hd_i_counter: ", minus_hd_i_counter
        print*, "minus_hd_r_counter: ", minus_hd_r_counter
        print*, "minus_i_hd_counter: ", minus_i_hd_counter
        print*, "minus_r_hd_counter: ", minus_r_hd_counter
        print*, "mult_d_d_counter: ", mult_d_d_counter
        print*, "mult_d_i_counter: ", mult_d_i_counter
        print*, "mult_d_r_counter: ", mult_d_r_counter
        print*, "mult_i_d_counter: ", mult_i_d_counter
        print*, "mult_r_d_counter: ", mult_r_d_counter
        print*, "mult_hd_hd_counter: ", mult_hd_hd_counter
        print*, "mult_hd_i_counter: ", mult_hd_i_counter
        print*, "mult_hd_r_counter: ", mult_hd_r_counter
        print*, "mult_i_hd_counter: ", mult_i_hd_counter
        print*, "mult_r_hd_counter: ", mult_r_hd_counter
        print*, "div_d_d_counter: ", div_d_d_counter
        print*, "div_d_r_counter: ", div_d_r_counter
        print*, "div_r_d_counter: ", div_r_d_counter
        print*, "div_d_i_counter: ", div_d_i_counter
        print*, "div_i_d_counter: ", div_i_d_counter
        print*, "div_hd_hd_counter: ", div_hd_hd_counter
        print*, "div_hd_r_counter: ", div_hd_r_counter
        print*, "div_r_hd_counter: ", div_r_hd_counter
        print*, "div_hd_i_counter: ", div_hd_i_counter
        print*, "div_i_hd_counter: ", div_i_hd_counter
        print*, "pow_d_i_counter: ", pow_d_i_counter
        print*, "pow_d_r_counter: ", pow_d_r_counter
        print*, "pow_d_d_counter: ", pow_d_d_counter
        print*, "pow_hd_i_counter: ", pow_hd_i_counter
        print*, "pow_hd_r_counter: ", pow_hd_r_counter
        print*, "pow_hd_hd_counter: ", pow_hd_hd_counter
        print*, "eq_dd_counter: ", eq_dd_counter
        print*, "eq_di_counter: ", eq_di_counter
        print*, "eq_dr_counter: ", eq_dr_counter
        print*, "eq_id_counter: ", eq_id_counter
        print*, "eq_rd_counter: ", eq_rd_counter
        print*, "le_dd_counter: ", le_dd_counter
        print*, "le_di_counter: ", le_di_counter
        print*, "le_dr_counter: ", le_dr_counter
        print*, "le_id_counter: ", le_id_counter
        print*, "le_rd_counter: ", le_rd_counter
        print*, "lt_dd_counter: ", lt_dd_counter
        print*, "lt_di_counter: ", lt_di_counter
        print*, "lt_dr_counter: ", lt_dr_counter
        print*, "lt_id_counter: ", lt_id_counter
        print*, "lt_rd_counter: ", lt_rd_counter
        print*, "ge_dd_counter: ", ge_dd_counter
        print*, "ge_di_counter: ", ge_di_counter
        print*, "ge_dr_counter: ", ge_dr_counter
        print*, "ge_id_counter: ", ge_id_counter
        print*, "ge_rd_counter: ", ge_rd_counter
        print*, "gt_dd_counter: ", gt_dd_counter
        print*, "gt_di_counter: ", gt_di_counter
        print*, "gt_dr_counter: ", gt_dr_counter
        print*, "gt_id_counter: ", gt_id_counter
        print*, "gt_rd_counter: ", gt_rd_counter
        print*, "ne_dd_counter: ", ne_dd_counter
        print*, "ne_di_counter: ", ne_di_counter
        print*, "ne_dr_counter: ", ne_dr_counter
        print*, "ne_id_counter: ", ne_id_counter
        print*, "ne_rd_counter: ", ne_rd_counter
        print*, "abs_d_counter: ", abs_d_counter
        print*, "acos_d_counter: ", acos_d_counter
        print*, "asin_d_counter: ", asin_d_counter
        print*, "atan_d_counter: ", atan_d_counter
        print*, "atan2_d_counter: ", atan2_d_counter
        print*, "cos_d_counter: ", cos_d_counter
        print*, "dot_product_dd_counter: ", dot_product_dd_counter
        print*, "exp_d_counter: ", exp_d_counter
        print*, "int_d_counter: ", int_d_counter
        print*, "log_d_counter: ", log_d_counter
        print*, "log_hd_counter: ", log_hd_counter
        print*, "log10_d_counter: ", log10_d_counter
        print*, "matmul_dd_counter: ", matmul_dd_counter
        print*, "matmul_dv_counter: ", matmul_dv_counter
        print*, "matmul_vd_counter: ", matmul_vd_counter
        print*, "max_dd_counter: ", max_dd_counter
        print*, "max_di_counter: ", max_di_counter
        print*, "max_dr_counter: ", max_dr_counter
        print*, "max_rd_counter: ", max_rd_counter
        print*, "dmax1_dd_counter: ", dmax1_dd_counter
        print*, "maxval_d_counter: ", maxval_d_counter
        print*, "min_dd_counter: ", min_dd_counter
        print*, "min_dr_counter: ", min_dr_counter
        print*, "dmin1_dd_counter: ", dmin1_dd_counter
        print*, "minval_d_counter: ", minval_d_counter
        print*, "nint_d_counter: ", nint_d_counter
        print*, "sin_d_counter: ", sin_d_counter
        print*, "tan_d_counter: ", tan_d_counter
        print*, "sqrt_d_counter: ", sqrt_d_counter
        print*, "sqrt_hd_counter: ", sqrt_hd_counter
        print*, "sum_d_counter: ", sum_d_counter
        print*, "maxloc_d_counter: ", maxloc_d_counter

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

    impure elemental function test_function_dual(x, y) result(f)
        type(dual_xy), intent(in) :: x, y
        type(dual_xy) :: f
        f = sqrt(x)*log(y)
    end function
    impure elemental function test_function_hdual(x, y) result(f)
        type(hdual_xy), intent(in) :: x, y
        type(hdual_xy) :: f
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

    impure elemental  subroutine assign_d_i(u, i)
        type(dual_xy), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        assign_d_i_counter = assign_d_i_counter + 1

    end subroutine
    impure elemental  subroutine assign_d_r(u, r)
        type(dual_xy), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        assign_d_r_counter = assign_d_r_counter + 1

    end subroutine
    impure elemental  subroutine assign_i_d(i, v)
        type(dual_xy), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_d_counter = assign_i_d_counter + 1

    end subroutine
    impure elemental  subroutine assign_r_d(r, v)
        type(dual_xy), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_d_counter = assign_r_d_counter + 1

    end subroutine
    impure elemental  subroutine assign_hd_i(u, i)
        type(hdual_xy), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_i_counter = assign_hd_i_counter + 1

    end subroutine
    impure elemental  subroutine assign_hd_r(u, r)
        type(hdual_xy), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_r_counter = assign_hd_r_counter + 1

    end subroutine
    impure elemental  subroutine assign_i_hd(i, v)
        type(hdual_xy), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_hd_counter = assign_i_hd_counter + 1

    end subroutine
    impure elemental  subroutine assign_r_hd(r, v)
        type(hdual_xy), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_hd_counter = assign_r_hd_counter + 1

    end subroutine
    impure elemental function unary_add_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res
        res%x = u%x
        res%dx = u%dx
        unary_add_d_counter = unary_add_d_counter + 1
    end function
    impure elemental function add_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        add_d_d_counter = add_d_d_counter + 1
    end function
    impure elemental function add_d_r(u, v) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x + v
        res%dx = u%dx
        add_d_r_counter = add_d_r_counter + 1
    end function
    impure elemental function add_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u + v%x
        res%dx = v%dx
        add_r_d_counter = add_r_d_counter + 1
    end function
    impure elemental function add_d_i(u, v) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x + v
        res%dx = u%dx
        add_d_i_counter = add_d_i_counter + 1
    end function
    impure elemental function add_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u + v%x
        res%dx = v%dx
        add_i_d_counter = add_i_d_counter + 1
    end function
    impure elemental function unary_add_hd(u) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x
        res%dx = u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)
        end do
        unary_add_hd_counter = unary_add_hd_counter + 1
    end function
    impure elemental function add_hd_hd(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j) + v%ddx(:, j)
        end do
        add_hd_hd_counter = add_hd_hd_counter + 1
    end function
    impure elemental function add_hd_r(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x + v
        res%dx = u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)
        end do
        add_hd_r_counter = add_hd_r_counter + 1
    end function
    impure elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u + v%x
        res%dx = v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = v%ddx(:, j)
        end do
        add_r_hd_counter = add_r_hd_counter + 1
    end function
    impure elemental function add_hd_i(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x + v
        res%dx = u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)
        end do
        add_hd_i_counter = add_hd_i_counter + 1
    end function
    impure elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u + v%x
        res%dx = v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = v%ddx(:, j)
        end do
        add_i_hd_counter = add_i_hd_counter + 1
    end function
    impure elemental function unary_minus_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res
        res%x = -u%x
        res%dx = -u%dx
        unary_minus_d_counter = unary_minus_d_counter + 1
    end function
    impure elemental function minus_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        minus_d_d_counter = minus_d_d_counter + 1
    end function
    impure elemental function minus_d_r(u, v) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x - v
        res%dx = u%dx
        minus_d_r_counter = minus_d_r_counter + 1
    end function
    impure elemental function minus_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u - v%x
        res%dx = -v%dx
        minus_r_d_counter = minus_r_d_counter + 1
    end function
    impure elemental function minus_d_i(u, v) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x - v
        res%dx = u%dx
        minus_d_i_counter = minus_d_i_counter + 1
    end function
    impure elemental function minus_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u - v%x
        res%dx = -v%dx
        minus_i_d_counter = minus_i_d_counter + 1
    end function
    impure elemental function unary_minus_hd(u) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy) :: res
        integer :: j
        res%x = -u%x
        res%dx = -u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = -u%ddx(:, j)
        end do
        unary_minus_hd_counter = unary_minus_hd_counter + 1
    end function
    impure elemental function minus_hd_hd(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j) - v%ddx(:, j)
        end do
        minus_hd_hd_counter = minus_hd_hd_counter + 1
    end function
    impure elemental function minus_hd_r(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x - v
        res%dx = u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)
        end do
        minus_hd_r_counter = minus_hd_r_counter + 1
    end function
    impure elemental function minus_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u - v%x
        res%dx = -v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = -v%ddx(:, j)
        end do
        minus_r_hd_counter = minus_r_hd_counter + 1
    end function
    impure elemental function minus_hd_i(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x - v
        res%dx = u%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)
        end do
        minus_hd_i_counter = minus_hd_i_counter + 1
    end function
    impure elemental function minus_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u - v%x
        res%dx = -v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = -v%ddx(:, j)
        end do
        minus_i_hd_counter = minus_i_hd_counter + 1
    end function
    impure elemental function mult_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        mult_d_d_counter = mult_d_d_counter + 1
    end function
    impure elemental function mult_d_r(u, v) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_r_counter = mult_d_r_counter + 1
    end function
    impure elemental function mult_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u*v%x
        res%dx = u*v%dx
        mult_r_d_counter = mult_r_d_counter + 1
    end function
    impure elemental function mult_d_i(u, v) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_i_counter = mult_d_i_counter + 1
    end function
    impure elemental function mult_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u*v%x
        res%dx = u*v%dx
        mult_i_d_counter = mult_i_d_counter + 1
    end function
    impure elemental function mult_hd_hd(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*v%x + u%dx*v%dx(j) + u%dx(j)*v%dx + u%x*v%ddx(:, j)
        end do
        mult_hd_hd_counter = mult_hd_hd_counter + 1
    end function
    impure elemental function mult_hd_r(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x*v
        res%dx = u%dx*v
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*v
        end do
        mult_hd_r_counter = mult_hd_r_counter + 1
    end function
    impure elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u*v%x
        res%dx = u*v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u*v%ddx(:, j)
        end do
        mult_r_hd_counter = mult_r_hd_counter + 1
    end function
    impure elemental function mult_hd_i(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x*v
        res%dx = u%dx*v
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*v
        end do
        mult_hd_i_counter = mult_hd_i_counter + 1
    end function
    impure elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u*v%x
        res%dx = u*v%dx
        do j = 1, num_deriv
            res%ddx(:, j) = u*v%ddx(:, j)
        end do
        mult_i_hd_counter = mult_i_hd_counter + 1
    end function
    impure elemental function div_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x/v%x
        res%dx = u%dx/v%x - u%x*v%dx/v%x**2
        div_d_d_counter = div_d_d_counter + 1
    end function
    impure elemental function div_d_r(u, v) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x/v
        res%dx = u%dx/v
        div_d_r_counter = div_d_r_counter + 1
    end function
    impure elemental function div_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_r_d_counter = div_r_d_counter + 1
    end function
    impure elemental function div_d_i(u, v) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x/v
        res%dx = u%dx/v
        div_d_i_counter = div_d_i_counter + 1
    end function
    impure elemental function div_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_i_d_counter = div_i_d_counter + 1
    end function
    impure elemental function div_hd_hd(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x/v%x
        res%dx = u%dx/v%x - u%x*v%dx/v%x**2
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)/v%x - u%dx(j)*v%dx/v%x**2 - u%x*v%ddx(:, j)/v%x**2 + &
      v%dx(j)*(-u%dx/v%x**2 + 2*u%x*v%dx/v%x**3)
        end do
        div_hd_hd_counter = div_hd_hd_counter + 1
    end function
    impure elemental function div_hd_r(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x/v
        res%dx = u%dx/v
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)/v
        end do
        div_hd_r_counter = div_hd_r_counter + 1
    end function
    impure elemental function div_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        do j = 1, num_deriv
            res%ddx(:, j) = -u*v%ddx(:, j)/v%x**2 + 2*u*v%dx*v%dx(j)/v%x**3
        end do
        div_r_hd_counter = div_r_hd_counter + 1
    end function
    impure elemental function div_hd_i(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x/v
        res%dx = u%dx/v
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)/v
        end do
        div_hd_i_counter = div_hd_i_counter + 1
    end function
    impure elemental function div_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        do j = 1, num_deriv
            res%ddx(:, j) = -u*v%ddx(:, j)/v%x**2 + 2*u*v%dx*v%dx(j)/v%x**3
        end do
        div_i_hd_counter = div_i_hd_counter + 1
    end function
    impure elemental function pow_d_i(u, v) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x**v
        res%dx = u%dx*u%x**v*v/u%x
        pow_d_i_counter = pow_d_i_counter + 1
    end function
    impure elemental function pow_d_r(u, v) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x**v
        res%dx = u%dx*u%x**v*v/u%x
        pow_d_r_counter = pow_d_r_counter + 1
    end function
    impure elemental function pow_d_d(u, v) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy), intent(in) :: v
        type(dual_xy) :: res
        res%x = u%x**v%x
        res%dx = u%dx*u%x**v%x*v%x/u%x + u%x**v%x*v%dx*log(u%x)
        pow_d_d_counter = pow_d_d_counter + 1
    end function
    impure elemental function pow_hd_i(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x**v
        res%dx = u%dx*u%x**v*v/u%x
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*u%x**v*v/u%x + u%dx*u%dx(j)*(u%x**v*v**2/ &
      u%x**2 - u%x**v*v/u%x**2)
        end do
        pow_hd_i_counter = pow_hd_i_counter + 1
    end function
    impure elemental function pow_hd_r(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x**v
        res%dx = u%dx*u%x**v*v/u%x
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*u%x**v*v/u%x + u%dx*u%dx(j)*(u%x**v*v**2/ &
      u%x**2 - u%x**v*v/u%x**2)
        end do
        pow_hd_r_counter = pow_hd_r_counter + 1
    end function
    impure elemental function pow_hd_hd(u, v) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy), intent(in) :: v
        type(hdual_xy) :: res
        integer :: j
        res%x = u%x**v%x
        res%dx = u%dx*u%x**v%x*v%x/u%x + u%x**v%x*v%dx*log(u%x)
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)*u%x**v%x*v%x/u%x + u%dx(j)*(u%dx*(u%x**v%x*v%x**2/ &
      u%x**2 - u%x**v%x*v%x/u%x**2) + v%dx*(u%x**v%x*v%x*log(u%x)/u%x &
      + u%x**v%x/u%x)) + u%x**v%x*v%ddx(:, j)*log(u%x) + v%dx(j)*(u%dx*(u%x** &
      v%x*v%x*log(u%x)/u%x + u%x**v%x/u%x) + u%x**v%x*v%dx*log(u%x)**2 &
      )
        end do
        pow_hd_hd_counter = pow_hd_hd_counter + 1
    end function
    impure elemental  function eq_dd(lhs, rhs) result(res)
         type(dual_xy), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function eq_dd
    impure elemental  function eq_di(lhs, rhs) result(res)
         type(dual_xy), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == real(rhs, dp))

    end function eq_di
    impure elemental  function eq_dr(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical::res

        res = (lhs%x == rhs)

    end function eq_dr
    impure elemental  function eq_id(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_id
    impure elemental  function eq_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_rd
    impure elemental  function le_dd(lhs, rhs) result(res)
         type(dual_xy), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function le_dd
    impure elemental  function le_di(lhs, rhs) result(res)
         type(dual_xy), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_di
    impure elemental  function le_dr(lhs, rhs) result(res)
         type(dual_xy), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_dr
    impure elemental  function le_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (i <= rhs%x)

    end function le_id
    impure elemental  function le_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function le_rd
    impure elemental  function lt_dd(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x < rhs%x)

    end function lt_dd
    impure elemental  function lt_di(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_di
    impure elemental  function lt_dr(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_dr
    impure elemental  function lt_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (i < rhs%x)

    end function lt_id
    impure elemental  function lt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function lt_rd
    impure elemental  function ge_dd(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x >= rhs%x)

    end function ge_dd
    impure elemental  function ge_di(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_di
    impure elemental  function ge_dr(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_dr
    impure elemental  function ge_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: rhs
        logical :: res

        res = (i >= rhs%x)

    end function ge_id
    impure elemental  function ge_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function ge_rd
    impure elemental  function gt_dd(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x > rhs%x)

    end function gt_dd
    impure elemental  function gt_di(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_di
    impure elemental  function gt_dr(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_dr
    impure elemental  function gt_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: rhs
        logical :: res

        res = (i > rhs%x)

    end function gt_id
    impure elemental  function gt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function gt_rd
    impure elemental  function ne_dd(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x /= rhs%x)

    end function ne_dd
    impure elemental  function ne_di(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_di
    impure elemental  function ne_dr(lhs, rhs) result(res)
        type(dual_xy), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_dr
    impure elemental  function ne_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual_xy), intent(in) :: rhs
        logical :: res

        res = (i /= rhs%x)

    end function ne_id
    impure elemental  function ne_rd(lhs, rhs) result(res)
        real(dp), intent(in) :: lhs
        type(dual_xy), intent(in) :: rhs
        logical :: res

        res = (lhs /= rhs%x)

    end function ne_rd
    impure elemental  function abs_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res
        integer :: i

        res%x = abs(u%x)
        res%dx = abs(u%dx)

    end function abs_d
    impure elemental  function acos_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = acos(u%x)
    
        res%dx = -u%dx / sqrt(1.0_dp - u%x**2)

    end function acos_d
    impure elemental  function asin_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = asin(u%x)
        res%dx = u%dx / sqrt(1.0_dp - u%x**2)

    end function asin_d
    impure elemental  function atan_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function atan_d
    impure elemental  function atan2_d(u, v) result(res)
        type(dual_xy), intent(in) :: u, v
        type(dual_xy) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function atan2_d
    impure elemental  function cos_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d
      function dot_product_dd(u, v) result(res)
        type(dual_xy), intent(in) :: u(:), v(:)
        type(dual_xy) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function dot_product_dd
    impure elemental  function exp_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function exp_d
    impure elemental  function int_d(u) result(res)
         type(dual_xy), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function int_d
    impure elemental function log_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res
        res%x = log(u%x)
        res%dx = u%dx/u%x
        log_d_counter = log_d_counter + 1
    end function
    impure elemental function log_hd(u) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy) :: res
        integer :: j
        res%x = log(u%x)
        res%dx = u%dx/u%x
        do j = 1, num_deriv
            res%ddx(:, j) = u%ddx(:, j)/u%x - u%dx*u%dx(j)/u%x**2
        end do
        log_hd_counter = log_hd_counter + 1
    end function
    impure elemental  function log10_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function log10_d
      function matmul_dd(u,v) result(res)
        type(dual_xy), intent(in) :: u(:,:), v(:,:)
        type(dual_xy) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dd
      function matmul_dv(u, v) result(res)
        type(dual_xy), intent(in) :: u(:,:), v(:)
        type(dual_xy) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dv
      function matmul_vd(u, v) result(res)
        type(dual_xy), intent(in) :: u(:), v(:,:)
        type(dual_xy) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_vd
    impure elemental  function max_dd(val1, val2) result(res)
        type(dual_xy), intent(in) :: val1, val2
        type(dual_xy) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif

    end function max_dd
    impure elemental  function max_di(u, i) result(res)
        type(dual_xy), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function max_di
    impure elemental  function max_dr(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function max_dr
     impure elemental  function max_rd(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function max_rd
    impure elemental  function dmax1_dd(val1, val2, val3, val4,val5) result(res)
        type(dual_xy), intent(in) :: val1, val2
        type(dual_xy), intent(in), optional :: val3, val4,val5
        type(dual_xy) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif
        if(present(val3))then
           if(res%x < val3%x) res = val3
        endif
        if(present(val4))then
           if(res%x < val4%x) res = val4
        endif
        if(present(val5))then
           if(res%x < val5%x) res = val5
        endif

    end function dmax1_dd
      function maxval_d(u) result(res)
        type(dual_xy), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_xy) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function maxval_d
    impure elemental  function min_dd(val1, val2) result(res)
        type(dual_xy), intent(in) :: val1, val2
        type(dual_xy) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif

    end function min_dd
    impure elemental  function min_dr(u, r) result(res)
        type(dual_xy), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function min_dr
    impure elemental  function dmin1_dd(val1, val2, val3, val4) result(res)
        type(dual_xy), intent(in) :: val1, val2
        type(dual_xy), intent(in), optional :: val3, val4
        type(dual_xy) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif
        if(present(val3))then
           if(res%x > val3%x) res = val3
        endif
        if(present(val4))then
           if(res%x > val4%x) res = val4
        endif

    end function dmin1_dd
      function minval_d(u) result(res)
        type(dual_xy), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_xy) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function minval_d
    impure elemental  function nint_d(u) result(res)
        type(dual_xy), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function nint_d
    impure elemental  function sign_dd(val1, val2) result(res)
        type(dual_xy), intent(in) :: val1, val2
        type(dual_xy) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function sign_dd
    impure elemental  function sign_rd(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual_xy), intent(in) :: val2
        type(dual_xy) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function sign_rd
    impure elemental  function sin_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d
    impure elemental  function tan_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d
    impure elemental function sqrt_d(u) result(res)
        type(dual_xy), intent(in) :: u
        type(dual_xy) :: res
        res%x = sqrt(u%x)
        res%dx = 0.5_dp*u%dx/sqrt(u%x)
        sqrt_d_counter = sqrt_d_counter + 1
    end function
    impure elemental function sqrt_hd(u) result(res)
        type(hdual_xy), intent(in) :: u
        type(hdual_xy) :: res
        integer :: j
        res%x = sqrt(u%x)
        res%dx = 0.5_dp*u%dx/sqrt(u%x)
        do j = 1, num_deriv
            res%ddx(:, j) = 0.5_dp*u%ddx(:, j)/sqrt(u%x) - 0.25_dp*u%dx*u%dx(j)/ &
      u%x**1.5_dp
        end do
        sqrt_hd_counter = sqrt_hd_counter + 1
    end function
      function sum_d(u) result(res)
        type(dual_xy), intent(in) :: u(:)
        type(dual_xy) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, num_deriv
            res%dx(i) = sum(u%dx(i))
        end do

    end function sum_d
      function maxloc_d(array) result(ind)
        type(dual_xy), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function maxloc_d
    
    
end module