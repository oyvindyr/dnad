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

    
    type :: dual_xy_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type
    type :: hdual_xy_t
        !! hyper-dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
        real(dp) :: h(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize_d_scalar
        module procedure initialize_d_vector
        module procedure initialize_hd_scalar
        module procedure initialize_hd_vector
    end interface
    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian_hd
    end interface
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
        module procedure eq_d_d ! compare two dual numbers, elemental
        module procedure eq_d_i ! compare a dual and an integer, elemental
        module procedure eq_d_r ! compare a dual and a real, elemental
        module procedure eq_i_d ! compare integer with a dual number, elemental
        module procedure eq_r_d ! compare a real with a dual number, elemental
        module procedure eq_hd_hd ! compare two hdual numbers, elemental
        module procedure eq_hd_i ! compare a hdual and an integer, elemental
        module procedure eq_hd_r ! compare a hdual and a real, elemental
        module procedure eq_i_hd ! compare integer with a hdual number, elemental
        module procedure eq_r_hd ! compare a real with a hdual number, elemental
    end interface
    interface operator (<=)
        module procedure le_d_d ! compare two dual numbers, elemental
        module procedure le_d_i ! compare a dual and an integer, elemental
        module procedure le_d_r ! compare a dual and a real, elemental
        module procedure le_i_d ! compare integer with a dual number, elemental
        module procedure le_r_d ! compare a real with a dual number, elemental
        module procedure le_hd_hd ! compare two hdual numbers, elemental
        module procedure le_hd_i ! compare a hdual and an integer, elemental
        module procedure le_hd_r ! compare a hdual and a real, elemental
        module procedure le_i_hd ! compare integer with a hdual number, elemental
        module procedure le_r_hd ! compare a real with a hdual number, elemental
    end interface
    interface operator (<)
        module procedure lt_d_d ! compare two dual numbers, elemental
        module procedure lt_d_i ! compare a dual and an integer, elemental
        module procedure lt_d_r ! compare a dual and a real, elemental
        module procedure lt_i_d ! compare integer with a dual number, elemental
        module procedure lt_r_d ! compare a real with a dual number, elemental
        module procedure lt_hd_hd ! compare two hdual numbers, elemental
        module procedure lt_hd_i ! compare a hdual and an integer, elemental
        module procedure lt_hd_r ! compare a hdual and a real, elemental
        module procedure lt_i_hd ! compare integer with a hdual number, elemental
        module procedure lt_r_hd ! compare a real with a hdual number, elemental
    end interface
    interface operator (>=)
        module procedure ge_d_d ! compare two dual numbers, elemental
        module procedure ge_d_i ! compare a dual and an integer, elemental
        module procedure ge_d_r ! compare a dual and a real, elemental
        module procedure ge_i_d ! compare integer with a dual number, elemental
        module procedure ge_r_d ! compare a real with a dual number, elemental
        module procedure ge_hd_hd ! compare two hdual numbers, elemental
        module procedure ge_hd_i ! compare a hdual and an integer, elemental
        module procedure ge_hd_r ! compare a hdual and a real, elemental
        module procedure ge_i_hd ! compare integer with a hdual number, elemental
        module procedure ge_r_hd ! compare a real with a hdual number, elemental
    end interface
    interface operator (>)
        module procedure gt_d_d ! compare two dual numbers, elemental
        module procedure gt_d_i ! compare a dual and an integer, elemental
        module procedure gt_d_r ! compare a dual and a real, elemental
        module procedure gt_i_d ! compare integer with a dual number, elemental
        module procedure gt_r_d ! compare a real with a dual number, elemental
        module procedure gt_hd_hd ! compare two hdual numbers, elemental
        module procedure gt_hd_i ! compare a hdual and an integer, elemental
        module procedure gt_hd_r ! compare a hdual and a real, elemental
        module procedure gt_i_hd ! compare integer with a hdual number, elemental
        module procedure gt_r_hd ! compare a real with a hdual number, elemental
    end interface
    interface operator (/=)
        module procedure ne_d_d ! compare two dual numbers, elemental
        module procedure ne_d_i ! compare a dual and an integer, elemental
        module procedure ne_d_r ! compare a dual and a real, elemental
        module procedure ne_i_d ! compare integer with a dual number, elemental
        module procedure ne_r_d ! compare a real with a dual number, elemental
        module procedure ne_hd_hd ! compare two hdual numbers, elemental
        module procedure ne_hd_i ! compare a hdual and an integer, elemental
        module procedure ne_hd_r ! compare a hdual and a real, elemental
        module procedure ne_i_hd ! compare integer with a hdual number, elemental
        module procedure ne_r_hd ! compare a real with a hdual number, elemental
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
        module procedure dot_product_d_d ! dot product two dual number vectors
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
        module procedure matmul_d_d ! multiply two dual matrices
        module procedure matmul_d_v ! multiply a dual matrix with a dual vector
        module procedure matmul_v_d ! multiply a dual vector with a dual matrix
    end interface
    interface max
        module procedure max_d_d ! max of two dual numbers, elemental
        module procedure max_d_i ! max of a dual number and an integer, elemental
        module procedure max_d_r ! max of a dual number and a real, elemental
        module procedure max_r_d ! max of a real,and a dual number,  elemental
    end interface
    interface dmax1
        module procedure dmax1_d_d ! max of from two to four dual numbers, elemental
    end interface
    interface maxval
        module procedure maxval_d ! maxval of a dual number vector
    end interface
    interface min
        module procedure min_d_d ! min of from two to four dual numbers, elemental
        module procedure min_d_r ! min of a dual and a real, elemental
    end interface
    interface dmin1
        module procedure dmin1_d_d ! min of from two to four dual numbers, elemental
    end interface
    interface minval
        module procedure minval_d ! obtain the maxval  of a dual number vectgor
    end interface
    interface nint
        module procedure nint_d ! nearest integer to the argument, elemental
    end interface
    interface  sign
        module procedure  sign_d_d ! sign(a,b) with two dual numbers, elemental
        module procedure  sign_r_d ! sign(a,b) with a real and a dual, elemental
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

        type(dual_xy_t), dimension(nval) :: xd, yd, fd
        type(hdual_xy_t), dimension(nval) :: xhd, yhd, fhd

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
            dfd(:, i) = fd(i)%g

            fvh(i) = fhd(i)
            dfh(:, i) = fhd(i)%g
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
        type(dual_xy_t), intent(in) :: x, y
        type(dual_xy_t) :: f
        f = sqrt(x)*log(y)
    end function
    impure elemental function test_function_hdual(x, y) result(f)
        type(hdual_xy_t), intent(in) :: x, y
        type(hdual_xy_t) :: f
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

    pure subroutine initialize_d_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual_xy_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize_d_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual_xy_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    pure subroutine initialize_hd_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual_xy_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%f = val
        hdual%g = 0
        hdual%h = 0
        hdual%g(idiff) = 1

    end subroutine
    pure subroutine initialize_hd_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual_xy_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%f = val(i)
            hdual(i)%g = 0
            hdual(i)%h = 0
            hdual(i)%g(i) = 1
        end do

    end subroutine
    pure function hessian_hd(d) result(m)
        type(hdual_xy_t), intent(in) :: d
        real(dp) :: m(size(d%g), size(d%g))
        
        integer i, j, k

        k = 0
        do j = 1, size(d%g)
            k = k + 1
            m(j, j) = d%h(k)
            do i = j+1, size(d%g)
                k = k + 1
                m(i, j) = d%h(k)
                m(j, i) = d%h(k)
            end do
        end do

    end function
    impure elemental subroutine assign_d_i(u, i)
        type(dual_xy_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp
        assign_d_i_counter = assign_d_i_counter + 1

    end subroutine
    impure elemental subroutine assign_d_r(u, r)
        type(dual_xy_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp
        assign_d_r_counter = assign_d_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_d(i, v)
        type(dual_xy_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)
        assign_i_d_counter = assign_i_d_counter + 1

    end subroutine
    impure elemental subroutine assign_r_d(r, v)
        type(dual_xy_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f
        assign_r_d_counter = assign_r_d_counter + 1

    end subroutine
    impure elemental subroutine assign_hd_i(u, i)
        type(hdual_xy_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp
        u%h = 0.0_dp
        assign_hd_i_counter = assign_hd_i_counter + 1

    end subroutine
    impure elemental subroutine assign_hd_r(u, r)
        type(hdual_xy_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp
        u%h = 0.0_dp
        assign_hd_r_counter = assign_hd_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_hd(i, v)
        type(hdual_xy_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)
        assign_i_hd_counter = assign_i_hd_counter + 1

    end subroutine
    impure elemental subroutine assign_r_hd(r, v)
        type(hdual_xy_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f
        assign_r_hd_counter = assign_r_hd_counter + 1

    end subroutine
    impure elemental function unary_add_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res
        
        res%f = u%f
        res%g = u%g
        unary_add_d_counter = unary_add_d_counter + 1
    end function
    impure elemental function add_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
        add_d_d_counter = add_d_d_counter + 1
    end function
    impure elemental function add_d_r(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        add_d_r_counter = add_d_r_counter + 1
    end function
    impure elemental function add_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        add_r_d_counter = add_r_d_counter + 1
    end function
    impure elemental function add_d_i(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        add_d_i_counter = add_d_i_counter + 1
    end function
    impure elemental function add_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        add_i_d_counter = add_i_d_counter + 1
    end function
    impure elemental function unary_add_hd(u) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t) :: res
        
        res%f = u%f
        res%g = u%g
        res%h = u%h
        unary_add_hd_counter = unary_add_hd_counter + 1
    end function
    impure elemental function add_hd_hd(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
        res%h = u%h + v%h
        add_hd_hd_counter = add_hd_hd_counter + 1
    end function
    impure elemental function add_hd_r(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        res%h = u%h
        add_hd_r_counter = add_hd_r_counter + 1
    end function
    impure elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        res%h = v%h
        add_r_hd_counter = add_r_hd_counter + 1
    end function
    impure elemental function add_hd_i(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        res%h = u%h
        add_hd_i_counter = add_hd_i_counter + 1
    end function
    impure elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        res%h = v%h
        add_i_hd_counter = add_i_hd_counter + 1
    end function
    impure elemental function unary_minus_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res
        
        res%f = -u%f
        res%g = -u%g
        unary_minus_d_counter = unary_minus_d_counter + 1
    end function
    impure elemental function minus_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
        minus_d_d_counter = minus_d_d_counter + 1
    end function
    impure elemental function minus_d_r(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        minus_d_r_counter = minus_d_r_counter + 1
    end function
    impure elemental function minus_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        minus_r_d_counter = minus_r_d_counter + 1
    end function
    impure elemental function minus_d_i(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        minus_d_i_counter = minus_d_i_counter + 1
    end function
    impure elemental function minus_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        minus_i_d_counter = minus_i_d_counter + 1
    end function
    impure elemental function unary_minus_hd(u) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t) :: res
        
        res%f = -u%f
        res%g = -u%g
        res%h = -u%h
        unary_minus_hd_counter = unary_minus_hd_counter + 1
    end function
    impure elemental function minus_hd_hd(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
        res%h = u%h - v%h
        minus_hd_hd_counter = minus_hd_hd_counter + 1
    end function
    impure elemental function minus_hd_r(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        res%h = u%h
        minus_hd_r_counter = minus_hd_r_counter + 1
    end function
    impure elemental function minus_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        res%h = -v%h
        minus_r_hd_counter = minus_r_hd_counter + 1
    end function
    impure elemental function minus_hd_i(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        res%h = u%h
        minus_hd_i_counter = minus_hd_i_counter + 1
    end function
    impure elemental function minus_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        res%h = -v%h
        minus_i_hd_counter = minus_i_hd_counter + 1
    end function
    impure elemental function mult_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
        mult_d_d_counter = mult_d_d_counter + 1
    end function
    impure elemental function mult_d_r(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        mult_d_r_counter = mult_d_r_counter + 1
    end function
    impure elemental function mult_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        mult_r_d_counter = mult_r_d_counter + 1
    end function
    impure elemental function mult_d_i(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        mult_d_i_counter = mult_d_i_counter + 1
    end function
    impure elemental function mult_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        mult_i_d_counter = mult_i_d_counter + 1
    end function
    impure elemental function mult_hd_hd(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = u%h(k)*v%f + u%g(i)*v%g(j) + u%g(j)*v%g(i) + u%f*v%h(k)
            end do
        end do
        mult_hd_hd_counter = mult_hd_hd_counter + 1
    end function
    impure elemental function mult_hd_r(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        res%h = u%h*v
        mult_hd_r_counter = mult_hd_r_counter + 1
    end function
    impure elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        res%h = u*v%h
        mult_r_hd_counter = mult_r_hd_counter + 1
    end function
    impure elemental function mult_hd_i(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        res%h = u%h*v
        mult_hd_i_counter = mult_hd_i_counter + 1
    end function
    impure elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        res%h = u*v%h
        mult_i_hd_counter = mult_i_hd_counter + 1
    end function
    impure elemental function div_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = t0*u%f
        res%f = t1
        res%g = t0*(-t1*v%g + u%g)
        div_d_d_counter = div_d_d_counter + 1
    end function
    impure elemental function div_d_r(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        div_d_r_counter = div_d_r_counter + 1
    end function
    impure elemental function div_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
        div_r_d_counter = div_r_d_counter + 1
    end function
    impure elemental function div_d_i(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        div_d_i_counter = div_d_i_counter + 1
    end function
    impure elemental function div_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
        div_i_d_counter = div_i_d_counter + 1
    end function
    impure elemental function div_hd_hd(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = t0*u%f
        res%f = t1
        res%g = t0*(-t1*v%g + u%g)
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t0*(-t0*u%g(j)*v%g(i) - t0*v%g(j)*(-2*t1*v%g(i) + u%g(i)) - t1* &
      v%h(k) + u%h(k))
            end do
        end do
        div_hd_hd_counter = div_hd_hd_counter + 1
    end function
    impure elemental function div_hd_r(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        res%h = t0*u%h
        div_hd_r_counter = div_hd_r_counter + 1
    end function
    impure elemental function div_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = u/v%f**2
        res%f = t0*u
        res%g = -t1*v%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t1*(2*t0*v%g(i)*v%g(j) - v%h(k))
            end do
        end do
        div_r_hd_counter = div_r_hd_counter + 1
    end function
    impure elemental function div_hd_i(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        res%h = t0*u%h
        div_hd_i_counter = div_hd_i_counter + 1
    end function
    impure elemental function div_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = u/v%f**2
        res%f = t0*u
        res%g = -t1*v%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t1*(2*t0*v%g(i)*v%g(j) - v%h(k))
            end do
        end do
        div_i_hd_counter = div_i_hd_counter + 1
    end function
    impure elemental function pow_d_i(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
        pow_d_i_counter = pow_d_i_counter + 1
    end function
    impure elemental function pow_d_r(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_xy_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
        pow_d_r_counter = pow_d_r_counter + 1
    end function
    impure elemental function pow_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t), intent(in) :: v
        type(dual_xy_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

        pow_d_d_counter = pow_d_d_counter + 1
    end function
    impure elemental function pow_hd_i(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*v*u%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = v*(u%g(i)*u%g(j)*(v - 1)*u%f**(v-2) + u%h(k)*u%f**(v-1))
            end do
        end do
        pow_hd_i_counter = pow_hd_i_counter + 1
    end function
    impure elemental function pow_hd_r(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*v*u%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = v*(u%g(i)*u%g(j)*(v - 1)*u%f**(v-2) + u%h(k)*u%f**(v-1))
            end do
        end do
        pow_hd_r_counter = pow_hd_r_counter + 1
    end function
    impure elemental function pow_hd_hd(u, v) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t), intent(in) :: v
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%f**v%f
        t1 = log(u%f)
        t2 = 1.0_dp/u%f
        t3 = t1*v%f + 1
        res%f = t0
        res%g = t0*(t1*v%g + t2*u%g*v%f)
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t0*(t1*v%h(k) + t2*u%h(k)*v%f + t2*u%g(j)*(t2*u%g(i)*v%f*(v%f - &
      1) + t3*v%g(i)) + v%g(j)*(t1**2*v%g(i) + t2*t3*u%g(i)))
            end do
        end do
        pow_hd_hd_counter = pow_hd_hd_counter + 1
    end function
    impure elemental  function eq_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f == rhs%f)

    end function
    impure elemental  function eq_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function eq_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function eq_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f == rhs%f)

    end function
    impure elemental  function eq_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function eq_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function le_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f <= rhs%f)

    end function
    impure elemental  function le_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function le_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function le_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f <= rhs%f)

    end function
    impure elemental  function le_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function le_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function lt_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f < rhs%f)

    end function
    impure elemental  function lt_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function lt_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function lt_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f < rhs%f)

    end function
    impure elemental  function lt_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function lt_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function ge_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f >= rhs%f)

    end function
    impure elemental  function ge_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function ge_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function ge_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f >= rhs%f)

    end function
    impure elemental  function ge_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function ge_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function gt_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f > rhs%f)

    end function
    impure elemental  function gt_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function gt_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function gt_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f > rhs%f)

    end function
    impure elemental  function gt_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function gt_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function ne_d_d(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f /= rhs%f)

    end function
    impure elemental  function ne_d_i(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne_d_r(lhs, rhs) result(res)
         type(dual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function ne_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function ne_hd_hd(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f /= rhs%f)

    end function
    impure elemental  function ne_hd_i(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne_hd_r(lhs, rhs) result(res)
         type(hdual_xy_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function ne_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_xy_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function abs_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res
        integer :: i

        ! We need this!
        res%f = abs(u%f)
        res%g = abs(u%g)

    end function
    impure elemental  function acos_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = acos(u%f)
    
        res%g = -u%g / sqrt(1.0_dp - u%f**2)

    end function
    impure elemental  function asin_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = asin(u%f)
        res%g = u%g / sqrt(1.0_dp - u%f**2)

    end function
    impure elemental  function atan_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = atan(u%f)
        res%g = u%g / (1.0_dp + u%f**2)

    end function
    impure elemental  function atan2_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u, v
        type(dual_xy_t) :: res

        real(dp) :: usq_plus_vsq

        res%f = atan2(u%f, v%f)

        usq_plus_vsq = u%f**2 + v%f**2
        res%g = v%f / usq_plus_vsq * u%g - u%f / usq_plus_vsq * v%g

    end function
    impure elemental  function cos_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = cos(u%f)
        res%g = -sin(u%f) * u%g

    end function
      function dot_product_d_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u(:), v(:)
        type(dual_xy_t) :: res

        integer :: i

        res%f = dot_product(u%f, v%f)
        do i = 1, num_deriv
            res%g(i) = dot_product(u%f, v%g(i)) + dot_product(v%f, u%g(i))
        end do

    end function
    impure elemental  function exp_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%f)
        res%f = exp_x
        res%g = u%g * exp_x

    end function
    impure elemental  function int_d(u) result(res)
         type(dual_xy_t), intent(in) :: u
         integer :: res

         res = int(u%f)

    end function
    impure elemental function log_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
        log_d_counter = log_d_counter + 1
    end function
    impure elemental function log_hd(u) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%f
        res%f = log(u%f)
        res%g = t0*u%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t0*(-t0*u%g(i)*u%g(j) + u%h(k))
            end do
        end do
        log_hd_counter = log_hd_counter + 1
    end function
    impure elemental  function log10_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%f * log(10.0_dp))
        res%f = log10(u%f)
        res%g = u%g * inv

    end function
      function matmul_d_d(u,v) result(res)
        type(dual_xy_t), intent(in) :: u(:,:), v(:,:)
        type(dual_xy_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, num_deriv
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
      function matmul_d_v(u, v) result(res)
        type(dual_xy_t), intent(in) :: u(:,:), v(:)
        type(dual_xy_t) :: res(size(u,1))
        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, num_deriv
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
      function matmul_v_d(u, v) result(res)
        type(dual_xy_t), intent(in) :: u(:), v(:,:)
        type(dual_xy_t) :: res(size(v, 2))
        integer::i

        res%f = matmul(u%f, v%f)
        do i = 1, num_deriv
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    impure elemental  function max_d_d(val1, val2) result(res)
        type(dual_xy_t), intent(in) :: val1, val2
        type(dual_xy_t) :: res

        if (val1%f > val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function max_d_i(u, i) result(res)
        type(dual_xy_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual_xy_t) :: res

        if (u%f > i) then
            res = u
        else
            res = i
        endif

    end function
    impure elemental  function max_d_r(u, r) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy_t) :: res

        if (u%f > r) then
            res = u
        else
            res = r
        endif

    end function
     impure elemental  function max_r_d(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        if (u%f > n) then
            res = u
        else
            res = n
        endif

    end function
    impure elemental  function dmax1_d_d(val1, val2, val3, val4,val5) result(res)
        type(dual_xy_t), intent(in) :: val1, val2
        type(dual_xy_t), intent(in), optional :: val3, val4,val5
        type(dual_xy_t) :: res

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
      function maxval_d(u) result(res)
        type(dual_xy_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_xy_t) :: res

        iloc=maxloc(u%f)
        res=u(iloc(1))

    end function
    impure elemental  function min_d_d(val1, val2) result(res)
        type(dual_xy_t), intent(in) :: val1, val2
        type(dual_xy_t) :: res

        if (val1%f < val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function min_d_r(u, r) result(res)
        type(dual_xy_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_xy_t) :: res

        if (u%f < r) then
            res = u
        else
            res = r
        endif

    end function
    impure elemental  function dmin1_d_d(val1, val2, val3, val4) result(res)
        type(dual_xy_t), intent(in) :: val1, val2
        type(dual_xy_t), intent(in), optional :: val3, val4
        type(dual_xy_t) :: res

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
      function minval_d(u) result(res)
        type(dual_xy_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_xy_t) :: res

        iloc=minloc(u%f)
        res=u(iloc(1))

    end function
    impure elemental  function nint_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        integer :: res

        res=nint(u%f)

    end function
    impure elemental  function sign_d_d(val1, val2) result(res)
        type(dual_xy_t), intent(in) :: val1, val2
        type(dual_xy_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    impure elemental  function sign_r_d(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual_xy_t), intent(in) :: val2
        type(dual_xy_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    impure elemental  function sin_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = sin(u%f)
        res%g = cos(u%f) * u%g

    end function
    impure elemental  function tan_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res

        res%f = tan(u%f)
        res%g = u%g / cos(u%f)**2

    end function
    impure elemental function sqrt_d(u) result(res)
        type(dual_xy_t), intent(in) :: u
        type(dual_xy_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
        sqrt_d_counter = sqrt_d_counter + 1
    end function

    impure elemental function sqrt_hd(u) result(res)
        type(hdual_xy_t), intent(in) :: u
        type(hdual_xy_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%f)
        t1 = 1.0_dp/t0
        res%f = t0
        res%g = 0.5_dp*t1*u%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = (0.25_dp)*t1*(2*u%h(k) - u%g(i)*u%g(j)/u%f)
            end do
        end do
        sqrt_hd_counter = sqrt_hd_counter + 1
    end function
      function sum_d(u) result(res)
        type(dual_xy_t), intent(in) :: u(:)
        type(dual_xy_t) :: res
        integer :: i

        res%f = sum(u%f)
        do i = 1, num_deriv
            res%g(i) = sum(u%g(i))
        end do

    end function
      function maxloc_d(array) result(ind)
        type(dual_xy_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%f)

    end function
    
    
end module