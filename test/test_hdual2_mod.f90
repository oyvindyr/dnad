module test_hdual2_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 3

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

    
    type :: dual_uvw
        !! dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivatives
    end type
    type :: hdual_uvw
        !! hyper-dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivatives
        real(dp) :: ddx(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface hessian ! Extract Hessian from a hyper-dual number
        module procedure hessian_matrix
    end interface

    interface initialize ! Initialize a dual or hyper dual number
        module procedure initialize_dual
        module procedure initialize_hdual
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

        type(dual_uvw), dimension(nval) :: ud, vd, wd, fd
        type(hdual_uvw), dimension(nval) :: uhd, vhd, whd, fhd

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
            dfd(:, i) = fd(i)%dx

            fvh(i) = fhd(i)
            dfh(:, i) = fhd(i)%dx
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
            print *, "test_hdual2: ok"
        else
            print *, "test_hdual2: failed"
        end if

        print*,'Hess matrix: '
        do i = 1, num_deriv
            print*,ddfh(i,:,1)
        end do
      
    end function

    impure elemental function test_function_dual(u, v, w, r1, r2, r3, i1, i2, i3) result(f)
        !! Test function for dual numbers. It is "impure" only because we use global counter variables to
        !! check test coverage. In "normal" use, it would be pure.
        type(dual_uvw), intent(in) :: u, v, w
        real(dp), intent(in) :: r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        type(dual_uvw) :: f
        f = i3/(2*v + 1) + r3/(2*u + 1) - u/(2*w + 1) + u/(2*i2 + 1) + u**i1 + u &
            **r1 - u**v + sqrt(v)*log(u) + w/(2*r2 + 1) + (i1*v - i2*u + r1*u &
            - r2*w)**2
    end function
    impure elemental function test_function_hdual(u, v, w, r1, r2, r3, i1, i2, i3) result(f)
        !! Test function for dual numbers. It is "impure" only because we use global counter variables to
        !! check test coverage. In "normal" use, it would be pure.
        type(hdual_uvw), intent(in) :: u, v, w
        real(dp), intent(in) :: r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        type(hdual_uvw) :: f
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

    pure subroutine initialize_dual(dual, val, idiff)
        type(dual_uvw), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%x = val
        dual%dx = 0
        dual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_hdual(hdual, val, idiff)
        type(hdual_uvw), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%x = val
        hdual%dx = 0
        hdual%ddx = 0
        hdual%dx(idiff) = 1

    end subroutine
    pure function hessian_matrix(d) result(m)
        type(hdual_uvw), intent(in) :: d
        real(dp) :: m(num_deriv, num_deriv)
        
        integer i, j, k

        k = 0
        do j = 1, num_deriv
            k = k + 1
            m(j, j) = d%ddx(k)
            do i = j+1, num_deriv
                k = k + 1
                m(i, j) = d%ddx(k)
                m(j, i) = d%ddx(k)
            end do
        end do
        ! m = d%ddx

    end function
    impure elemental subroutine assign_d_i(u, i)
        type(dual_uvw), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        assign_d_i_counter = assign_d_i_counter + 1

    end subroutine
    impure elemental subroutine assign_d_r(u, r)
        type(dual_uvw), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        assign_d_r_counter = assign_d_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_d(i, v)
        type(dual_uvw), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_d_counter = assign_i_d_counter + 1

    end subroutine
    impure elemental subroutine assign_r_d(r, v)
        type(dual_uvw), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_d_counter = assign_r_d_counter + 1

    end subroutine
    impure elemental subroutine assign_hd_i(u, i)
        type(hdual_uvw), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_i_counter = assign_hd_i_counter + 1

    end subroutine
    impure elemental subroutine assign_hd_r(u, r)
        type(hdual_uvw), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_r_counter = assign_hd_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_hd(i, v)
        type(hdual_uvw), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_hd_counter = assign_i_hd_counter + 1

    end subroutine
    impure elemental subroutine assign_r_hd(r, v)
        type(hdual_uvw), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_hd_counter = assign_r_hd_counter + 1

    end subroutine
    impure elemental function unary_add_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res
        
        res%x = u%x
        res%dx = u%dx
        unary_add_d_counter = unary_add_d_counter + 1
    end function
    impure elemental function add_d_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        add_d_d_counter = add_d_d_counter + 1
    end function
    impure elemental function add_d_r(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_r_counter = add_d_r_counter + 1
    end function
    impure elemental function add_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_r_d_counter = add_r_d_counter + 1
    end function
    impure elemental function add_d_i(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_i_counter = add_d_i_counter + 1
    end function
    impure elemental function add_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_i_d_counter = add_i_d_counter + 1
    end function
    impure elemental function unary_add_hd(u) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
        unary_add_hd_counter = unary_add_hd_counter + 1
    end function
    impure elemental function add_hd_hd(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
        add_hd_hd_counter = add_hd_hd_counter + 1
    end function
    impure elemental function add_hd_r(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_r_counter = add_hd_r_counter + 1
    end function
    impure elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_r_hd_counter = add_r_hd_counter + 1
    end function
    impure elemental function add_hd_i(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_i_counter = add_hd_i_counter + 1
    end function
    impure elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_i_hd_counter = add_i_hd_counter + 1
    end function
    impure elemental function unary_minus_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        unary_minus_d_counter = unary_minus_d_counter + 1
    end function
    impure elemental function minus_d_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        minus_d_d_counter = minus_d_d_counter + 1
    end function
    impure elemental function minus_d_r(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_r_counter = minus_d_r_counter + 1
    end function
    impure elemental function minus_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_r_d_counter = minus_r_d_counter + 1
    end function
    impure elemental function minus_d_i(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_i_counter = minus_d_i_counter + 1
    end function
    impure elemental function minus_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_i_d_counter = minus_i_d_counter + 1
    end function
    impure elemental function unary_minus_hd(u) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        res%ddx = -u%ddx
        unary_minus_hd_counter = unary_minus_hd_counter + 1
    end function
    impure elemental function minus_hd_hd(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        res%ddx = u%ddx - v%ddx
        minus_hd_hd_counter = minus_hd_hd_counter + 1
    end function
    impure elemental function minus_hd_r(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_r_counter = minus_hd_r_counter + 1
    end function
    impure elemental function minus_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_r_hd_counter = minus_r_hd_counter + 1
    end function
    impure elemental function minus_hd_i(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_i_counter = minus_hd_i_counter + 1
    end function
    impure elemental function minus_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_i_hd_counter = minus_i_hd_counter + 1
    end function
    impure elemental function mult_d_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        mult_d_d_counter = mult_d_d_counter + 1
    end function
    impure elemental function mult_d_r(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_r_counter = mult_d_r_counter + 1
    end function
    impure elemental function mult_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_r_d_counter = mult_r_d_counter + 1
    end function
    impure elemental function mult_d_i(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_i_counter = mult_d_i_counter + 1
    end function
    impure elemental function mult_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_i_d_counter = mult_i_d_counter + 1
    end function
    impure elemental function mult_hd_hd(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = u%ddx(k)*v%x + u%dx(i)*v%dx(j) + u%dx(j)*v%dx(i) + u%x*v%ddx(k)
            end do
        end do
        mult_hd_hd_counter = mult_hd_hd_counter + 1
    end function
    impure elemental function mult_hd_r(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_r_counter = mult_hd_r_counter + 1
    end function
    impure elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_r_hd_counter = mult_r_hd_counter + 1
    end function
    impure elemental function mult_hd_i(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_i_counter = mult_hd_i_counter + 1
    end function
    impure elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_i_hd_counter = mult_i_hd_counter + 1
    end function
    impure elemental function div_d_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        div_d_d_counter = div_d_d_counter + 1
    end function
    impure elemental function div_d_r(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_r_counter = div_d_r_counter + 1
    end function
    impure elemental function div_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_r_d_counter = div_r_d_counter + 1
    end function
    impure elemental function div_d_i(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_i_counter = div_d_i_counter + 1
    end function
    impure elemental function div_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_i_d_counter = div_i_d_counter + 1
    end function
    impure elemental function div_hd_hd(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(j)*v%dx(i) - t0*v%dx(j)*(-2*t1*v%dx(i) + u%dx(i)) - t1* &
      v%ddx(k) + u%ddx(k))
            end do
        end do
        div_hd_hd_counter = div_hd_hd_counter + 1
    end function
    impure elemental function div_hd_r(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_r_counter = div_hd_r_counter + 1
    end function
    impure elemental function div_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_r_hd_counter = div_r_hd_counter + 1
    end function
    impure elemental function div_hd_i(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_i_counter = div_hd_i_counter + 1
    end function
    impure elemental function div_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_i_hd_counter = div_i_hd_counter + 1
    end function
    impure elemental function pow_d_i(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_i_counter = pow_d_i_counter + 1
    end function
    impure elemental function pow_d_r(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_r_counter = pow_d_r_counter + 1
    end function
    impure elemental function pow_d_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw), intent(in) :: v
        type(dual_uvw) :: res
        real(dp) :: t0
        
        t0 = u%x**v%x
        res%x = t0
        res%dx = t0*(u%dx*v%x/u%x + v%dx*log(u%x))

        pow_d_d_counter = pow_d_d_counter + 1
    end function
    impure elemental function pow_hd_i(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
        pow_hd_i_counter = pow_hd_i_counter + 1
    end function
    impure elemental function pow_hd_r(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*v*u%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = v*(u%dx(i)*u%dx(j)*(v - 1)*u%x**(v-2) + u%ddx(k)*u%x**(v-1))
            end do
        end do
        pow_hd_r_counter = pow_hd_r_counter + 1
    end function
    impure elemental function pow_hd_hd(u, v) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw), intent(in) :: v
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0,t1,t2,t3
        
        t0 = u%x**v%x
        t1 = log(u%x)
        t2 = 1.0_dp/u%x
        t3 = t1*v%x + 1
        res%x = t0
        res%dx = t0*(t1*v%dx + t2*u%dx*v%x)
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = t0*(t1*v%ddx(k) + t2*u%ddx(k)*v%x + t2*u%dx(j)*(t2*u%dx(i)*v%x*(v%x - &
      1) + t3*v%dx(i)) + v%dx(j)*(t1**2*v%dx(i) + t2*t3*u%dx(i)))
            end do
        end do
        pow_hd_hd_counter = pow_hd_hd_counter + 1
    end function
    impure elemental  function eq_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function le_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function lt_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function ge_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function gt_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function ne_d_d(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_d_i(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_d_r(lhs, rhs) result(res)
         type(dual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_d(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_d(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_hd_hd(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_hd_i(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_hd_r(lhs, rhs) result(res)
         type(hdual_uvw), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_hd(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_hd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_uvw), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function abs_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res
        integer :: i

        ! We need this!
        res%x = abs(u%x)
        res%dx = abs(u%dx)

    end function abs_d
    impure elemental  function acos_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = acos(u%x)
    
        res%dx = -u%dx / sqrt(1.0_dp - u%x**2)

    end function acos_d
    impure elemental  function asin_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = asin(u%x)
        res%dx = u%dx / sqrt(1.0_dp - u%x**2)

    end function asin_d
    impure elemental  function atan_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function atan_d
    impure elemental  function atan2_d(u, v) result(res)
        type(dual_uvw), intent(in) :: u, v
        type(dual_uvw) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function atan2_d
    impure elemental  function cos_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d
      function dot_product_dd(u, v) result(res)
        type(dual_uvw), intent(in) :: u(:), v(:)
        type(dual_uvw) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function dot_product_dd
    impure elemental  function exp_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function exp_d
    impure elemental  function int_d(u) result(res)
         type(dual_uvw), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function int_d
    impure elemental function log_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res
        
        res%x = log(u%x)
        res%dx = u%dx/u%x
        log_d_counter = log_d_counter + 1
    end function
    impure elemental function log_hd(u) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%x
        res%x = log(u%x)
        res%dx = t0*u%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(i)*u%dx(j) + u%ddx(k))
            end do
        end do
        log_hd_counter = log_hd_counter + 1
    end function
    impure elemental  function log10_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function log10_d
      function matmul_dd(u,v) result(res)
        type(dual_uvw), intent(in) :: u(:,:), v(:,:)
        type(dual_uvw) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dd
      function matmul_dv(u, v) result(res)
        type(dual_uvw), intent(in) :: u(:,:), v(:)
        type(dual_uvw) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dv
      function matmul_vd(u, v) result(res)
        type(dual_uvw), intent(in) :: u(:), v(:,:)
        type(dual_uvw) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_vd
    impure elemental  function max_dd(val1, val2) result(res)
        type(dual_uvw), intent(in) :: val1, val2
        type(dual_uvw) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif

    end function max_dd
    impure elemental  function max_di(u, i) result(res)
        type(dual_uvw), intent(in) :: u
        integer, intent(in) :: i
        type(dual_uvw) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function max_di
    impure elemental  function max_dr(u, r) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_uvw) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function max_dr
     impure elemental  function max_rd(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function max_rd
    impure elemental  function dmax1_dd(val1, val2, val3, val4,val5) result(res)
        type(dual_uvw), intent(in) :: val1, val2
        type(dual_uvw), intent(in), optional :: val3, val4,val5
        type(dual_uvw) :: res

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
        type(dual_uvw), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_uvw) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function maxval_d
    impure elemental  function min_dd(val1, val2) result(res)
        type(dual_uvw), intent(in) :: val1, val2
        type(dual_uvw) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif

    end function min_dd
    impure elemental  function min_dr(u, r) result(res)
        type(dual_uvw), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_uvw) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function min_dr
    impure elemental  function dmin1_dd(val1, val2, val3, val4) result(res)
        type(dual_uvw), intent(in) :: val1, val2
        type(dual_uvw), intent(in), optional :: val3, val4
        type(dual_uvw) :: res

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
        type(dual_uvw), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_uvw) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function minval_d
    impure elemental  function nint_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function nint_d
    impure elemental  function sign_dd(val1, val2) result(res)
        type(dual_uvw), intent(in) :: val1, val2
        type(dual_uvw) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function sign_dd
    impure elemental  function sign_rd(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual_uvw), intent(in) :: val2
        type(dual_uvw) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function sign_rd
    impure elemental  function sin_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d
    impure elemental  function tan_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d
    impure elemental function sqrt_d(u) result(res)
        type(dual_uvw), intent(in) :: u
        type(dual_uvw) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%x)
        res%x = t0
        res%dx = 0.5_dp*u%dx/t0
        sqrt_d_counter = sqrt_d_counter + 1
    end function

    impure elemental function sqrt_hd(u) result(res)
        type(hdual_uvw), intent(in) :: u
        type(hdual_uvw) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%x)
        t1 = 1.0_dp/t0
        res%x = t0
        res%dx = 0.5_dp*t1*u%dx
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%ddx(k) = (0.25_dp)*t1*(2*u%ddx(k) - u%dx(i)*u%dx(j)/u%x)
            end do
        end do
        sqrt_hd_counter = sqrt_hd_counter + 1
    end function
      function sum_d(u) result(res)
        type(dual_uvw), intent(in) :: u(:)
        type(dual_uvw) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, num_deriv
            res%dx(i) = sum(u%dx(i))
        end do

    end function sum_d
      function maxloc_d(array) result(ind)
        type(dual_uvw), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function maxloc_d
    
    
end module