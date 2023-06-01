module test_hdual_chain_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: test_hdual_chain

    integer, parameter :: ny = 2
    integer, parameter :: nx = 3


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
    interface operator (/)
        module procedure div_dualx_dualx    ! dual/dual, elemental
        module procedure div_dualx_r    ! dual/real, elemental
        module procedure div_r_dualx    ! real/dual, elemental
        module procedure div_dualx_i    ! dual/integer, elemental
        module procedure div_i_dualx    ! integer/dual, elemental
        module procedure div_dualy_dualy    ! dual/dual, elemental
        module procedure div_dualy_r    ! dual/real, elemental
        module procedure div_r_dualy    ! real/dual, elemental
        module procedure div_dualy_i    ! dual/integer, elemental
        module procedure div_i_dualy    ! integer/dual, elemental
        module procedure div_hdualx_hdualx  ! hdual/hdual, elemental
        module procedure div_hdualx_r   ! hdual/real, elemental
        module procedure div_r_hdualx   ! real/hdual, elemental
        module procedure div_hdualx_i   ! hdual/integer, elemental
        module procedure div_i_hdualx   ! integer/hdual, elemental
        module procedure div_hdualy_hdualy  ! hdual/hdual, elemental
        module procedure div_hdualy_r   ! hdual/real, elemental
        module procedure div_r_hdualy   ! real/hdual, elemental
        module procedure div_hdualy_i   ! hdual/integer, elemental
        module procedure div_i_hdualy   ! integer/hdual, elemental
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
    interface operator (==)
        module procedure eq_dualx_dualx ! compare two dual numbers, elemental
        module procedure eq_dualx_i ! compare a dual and an integer, elemental
        module procedure eq_dualx_r ! compare a dual and a real, elemental
        module procedure eq_i_dualx ! compare integer with a dual number, elemental
        module procedure eq_r_dualx ! compare a real with a dual number, elemental
        module procedure eq_dualy_dualy ! compare two dual numbers, elemental
        module procedure eq_dualy_i ! compare a dual and an integer, elemental
        module procedure eq_dualy_r ! compare a dual and a real, elemental
        module procedure eq_i_dualy ! compare integer with a dual number, elemental
        module procedure eq_r_dualy ! compare a real with a dual number, elemental
        module procedure eq_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure eq_hdualx_i ! compare a hdual and an integer, elemental
        module procedure eq_hdualx_r ! compare a hdual and a real, elemental
        module procedure eq_i_hdualx ! compare integer with a hdual number, elemental
        module procedure eq_r_hdualx ! compare a real with a hdual number, elemental
        module procedure eq_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure eq_hdualy_i ! compare a hdual and an integer, elemental
        module procedure eq_hdualy_r ! compare a hdual and a real, elemental
        module procedure eq_i_hdualy ! compare integer with a hdual number, elemental
        module procedure eq_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface operator (<=)
        module procedure le_dualx_dualx ! compare two dual numbers, elemental
        module procedure le_dualx_i ! compare a dual and an integer, elemental
        module procedure le_dualx_r ! compare a dual and a real, elemental
        module procedure le_i_dualx ! compare integer with a dual number, elemental
        module procedure le_r_dualx ! compare a real with a dual number, elemental
        module procedure le_dualy_dualy ! compare two dual numbers, elemental
        module procedure le_dualy_i ! compare a dual and an integer, elemental
        module procedure le_dualy_r ! compare a dual and a real, elemental
        module procedure le_i_dualy ! compare integer with a dual number, elemental
        module procedure le_r_dualy ! compare a real with a dual number, elemental
        module procedure le_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure le_hdualx_i ! compare a hdual and an integer, elemental
        module procedure le_hdualx_r ! compare a hdual and a real, elemental
        module procedure le_i_hdualx ! compare integer with a hdual number, elemental
        module procedure le_r_hdualx ! compare a real with a hdual number, elemental
        module procedure le_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure le_hdualy_i ! compare a hdual and an integer, elemental
        module procedure le_hdualy_r ! compare a hdual and a real, elemental
        module procedure le_i_hdualy ! compare integer with a hdual number, elemental
        module procedure le_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface operator (<)
        module procedure lt_dualx_dualx ! compare two dual numbers, elemental
        module procedure lt_dualx_i ! compare a dual and an integer, elemental
        module procedure lt_dualx_r ! compare a dual and a real, elemental
        module procedure lt_i_dualx ! compare integer with a dual number, elemental
        module procedure lt_r_dualx ! compare a real with a dual number, elemental
        module procedure lt_dualy_dualy ! compare two dual numbers, elemental
        module procedure lt_dualy_i ! compare a dual and an integer, elemental
        module procedure lt_dualy_r ! compare a dual and a real, elemental
        module procedure lt_i_dualy ! compare integer with a dual number, elemental
        module procedure lt_r_dualy ! compare a real with a dual number, elemental
        module procedure lt_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure lt_hdualx_i ! compare a hdual and an integer, elemental
        module procedure lt_hdualx_r ! compare a hdual and a real, elemental
        module procedure lt_i_hdualx ! compare integer with a hdual number, elemental
        module procedure lt_r_hdualx ! compare a real with a hdual number, elemental
        module procedure lt_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure lt_hdualy_i ! compare a hdual and an integer, elemental
        module procedure lt_hdualy_r ! compare a hdual and a real, elemental
        module procedure lt_i_hdualy ! compare integer with a hdual number, elemental
        module procedure lt_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface operator (>=)
        module procedure ge_dualx_dualx ! compare two dual numbers, elemental
        module procedure ge_dualx_i ! compare a dual and an integer, elemental
        module procedure ge_dualx_r ! compare a dual and a real, elemental
        module procedure ge_i_dualx ! compare integer with a dual number, elemental
        module procedure ge_r_dualx ! compare a real with a dual number, elemental
        module procedure ge_dualy_dualy ! compare two dual numbers, elemental
        module procedure ge_dualy_i ! compare a dual and an integer, elemental
        module procedure ge_dualy_r ! compare a dual and a real, elemental
        module procedure ge_i_dualy ! compare integer with a dual number, elemental
        module procedure ge_r_dualy ! compare a real with a dual number, elemental
        module procedure ge_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure ge_hdualx_i ! compare a hdual and an integer, elemental
        module procedure ge_hdualx_r ! compare a hdual and a real, elemental
        module procedure ge_i_hdualx ! compare integer with a hdual number, elemental
        module procedure ge_r_hdualx ! compare a real with a hdual number, elemental
        module procedure ge_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure ge_hdualy_i ! compare a hdual and an integer, elemental
        module procedure ge_hdualy_r ! compare a hdual and a real, elemental
        module procedure ge_i_hdualy ! compare integer with a hdual number, elemental
        module procedure ge_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface operator (>)
        module procedure gt_dualx_dualx ! compare two dual numbers, elemental
        module procedure gt_dualx_i ! compare a dual and an integer, elemental
        module procedure gt_dualx_r ! compare a dual and a real, elemental
        module procedure gt_i_dualx ! compare integer with a dual number, elemental
        module procedure gt_r_dualx ! compare a real with a dual number, elemental
        module procedure gt_dualy_dualy ! compare two dual numbers, elemental
        module procedure gt_dualy_i ! compare a dual and an integer, elemental
        module procedure gt_dualy_r ! compare a dual and a real, elemental
        module procedure gt_i_dualy ! compare integer with a dual number, elemental
        module procedure gt_r_dualy ! compare a real with a dual number, elemental
        module procedure gt_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure gt_hdualx_i ! compare a hdual and an integer, elemental
        module procedure gt_hdualx_r ! compare a hdual and a real, elemental
        module procedure gt_i_hdualx ! compare integer with a hdual number, elemental
        module procedure gt_r_hdualx ! compare a real with a hdual number, elemental
        module procedure gt_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure gt_hdualy_i ! compare a hdual and an integer, elemental
        module procedure gt_hdualy_r ! compare a hdual and a real, elemental
        module procedure gt_i_hdualy ! compare integer with a hdual number, elemental
        module procedure gt_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface operator (/=)
        module procedure ne_dualx_dualx ! compare two dual numbers, elemental
        module procedure ne_dualx_i ! compare a dual and an integer, elemental
        module procedure ne_dualx_r ! compare a dual and a real, elemental
        module procedure ne_i_dualx ! compare integer with a dual number, elemental
        module procedure ne_r_dualx ! compare a real with a dual number, elemental
        module procedure ne_dualy_dualy ! compare two dual numbers, elemental
        module procedure ne_dualy_i ! compare a dual and an integer, elemental
        module procedure ne_dualy_r ! compare a dual and a real, elemental
        module procedure ne_i_dualy ! compare integer with a dual number, elemental
        module procedure ne_r_dualy ! compare a real with a dual number, elemental
        module procedure ne_hdualx_hdualx ! compare two hdual numbers, elemental
        module procedure ne_hdualx_i ! compare a hdual and an integer, elemental
        module procedure ne_hdualx_r ! compare a hdual and a real, elemental
        module procedure ne_i_hdualx ! compare integer with a hdual number, elemental
        module procedure ne_r_hdualx ! compare a real with a hdual number, elemental
        module procedure ne_hdualy_hdualy ! compare two hdual numbers, elemental
        module procedure ne_hdualy_i ! compare a hdual and an integer, elemental
        module procedure ne_hdualy_r ! compare a hdual and a real, elemental
        module procedure ne_i_hdualy ! compare integer with a hdual number, elemental
        module procedure ne_r_hdualy ! compare a real with a hdual number, elemental
    end interface
    interface abs
        module procedure abs_dualx  ! absolute value of a dual number, elemental
        module procedure abs_dualy  ! absolute value of a dual number, elemental
    end interface
    interface dabs
        module procedure abs_dualx ! same as abs, used for some old fortran commands
        module procedure abs_dualy ! same as abs, used for some old fortran commands
    end interface
    interface acos
        module procedure acos_dualx ! arccosine of a dual number, elemental
        module procedure acos_dualy ! arccosine of a dual number, elemental
    end interface
    interface asin
        module procedure asin_dualx ! arcsine of a dual number, elemental
        module procedure asin_dualy ! arcsine of a dual number, elemental
    end interface
    interface atan
        module procedure atan_dualx ! arctan of a dual number, elemental
        module procedure atan_dualy ! arctan of a dual number, elemental
    end interface
    interface atan2
        module procedure atan2_dualx ! arctan of a dual number, elemental
        module procedure atan2_dualy ! arctan of a dual number, elemental
    end interface
    interface cos
        module procedure cos_dualx ! cosine of a dual number, elemental
        module procedure cos_dualy ! cosine of a dual number, elemental
    end interface
    interface dcos
        module procedure cos_dualx ! cosine of a dual number, elemental
        module procedure cos_dualy ! cosine of a dual number, elemental
    end interface
    interface dot_product
        module procedure dot_product_dualx_dualx ! dot product two dual number vectors
        module procedure dot_product_dualy_dualy ! dot product two dual number vectors
    end interface
    interface exp
        module procedure exp_dualx ! exponential of a dual number, elemental
        module procedure exp_dualy ! exponential of a dual number, elemental
    end interface
    interface int
        module procedure int_dualx ! integer part of a dual number, elemental
        module procedure int_dualy ! integer part of a dual number, elemental
    end interface
    interface log
        module procedure log_dualx ! log of a dual number, elemental
        module procedure log_dualy ! log of a dual number, elemental
        module procedure log_hdualx ! log of a dual number, elemental
        module procedure log_hdualy ! log of a dual number, elemental
    end interface
    interface log10
        module procedure log10_dualx ! log of a dual number, elemental
        module procedure log10_dualy ! log of a dual number, elemental
    end interface
    interface matmul
        module procedure matmul_dualx_dualx ! multiply two dual matrices
        module procedure matmul_dualx_v ! multiply a dual matrix with a dual vector
        module procedure matmul_v_dualx ! multiply a dual vector with a dual matrix
        module procedure matmul_dualy_dualy ! multiply two dual matrices
        module procedure matmul_dualy_v ! multiply a dual matrix with a dual vector
        module procedure matmul_v_dualy ! multiply a dual vector with a dual matrix
    end interface
    interface max
        module procedure max_dualx_dualx ! max of two dual numbers, elemental
        module procedure max_dualx_i ! max of a dual number and an integer, elemental
        module procedure max_dualx_r ! max of a dual number and a real, elemental
        module procedure max_r_dualx ! max of a real,and a dual number,  elemental
        module procedure max_dualy_dualy ! max of two dual numbers, elemental
        module procedure max_dualy_i ! max of a dual number and an integer, elemental
        module procedure max_dualy_r ! max of a dual number and a real, elemental
        module procedure max_r_dualy ! max of a real,and a dual number,  elemental
    end interface
    interface dmax1
        module procedure dmax1_dualx_dualx ! max of from two to four dual numbers, elemental
        module procedure dmax1_dualy_dualy ! max of from two to four dual numbers, elemental
    end interface
    interface maxval
        module procedure maxval_dualx ! maxval of a dual number vector
        module procedure maxval_dualy ! maxval of a dual number vector
    end interface
    interface min
        module procedure min_dualx_dualx ! min of from two to four dual numbers, elemental
        module procedure min_dualx_r ! min of a dual and a real, elemental
        module procedure min_dualy_dualy ! min of from two to four dual numbers, elemental
        module procedure min_dualy_r ! min of a dual and a real, elemental
    end interface
    interface dmin1
        module procedure dmin1_dualx_dualx ! min of from two to four dual numbers, elemental
        module procedure dmin1_dualy_dualy ! min of from two to four dual numbers, elemental
    end interface
    interface minval
        module procedure minval_dualx ! obtain the maxval  of a dual number vectgor
        module procedure minval_dualy ! obtain the maxval  of a dual number vectgor
    end interface
    interface nint
        module procedure nint_dualx ! nearest integer to the argument, elemental
        module procedure nint_dualy ! nearest integer to the argument, elemental
    end interface
    interface  sign
        module procedure  sign_dualx_dualx ! sign(a,b) with two dual numbers, elemental
        module procedure  sign_r_dualx ! sign(a,b) with a real and a dual, elemental
        module procedure  sign_dualy_dualy ! sign(a,b) with two dual numbers, elemental
        module procedure  sign_r_dualy ! sign(a,b) with a real and a dual, elemental
    end interface
    interface sin
        module procedure sin_dualx ! obtain sine of a dual number, elemental
        module procedure sin_dualy ! obtain sine of a dual number, elemental
    end interface
    interface dsin
        module procedure sin_dualx ! obtain sine of a dual number, elemental
        module procedure sin_dualy ! obtain sine of a dual number, elemental
    end interface
    interface tan
        module procedure tan_dualx ! obtain sine of a dual number, elemental
        module procedure tan_dualy ! obtain sine of a dual number, elemental
    end interface
    interface dtan
        module procedure tan_dualx ! obtain sine of a dual number, elemental
        module procedure tan_dualy ! obtain sine of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt_dualx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_dualy ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_hdualx ! obtain the sqrt of a dual number, elemental
        module procedure sqrt_hdualy ! obtain the sqrt of a dual number, elemental
    end interface
    interface sum
        module procedure sum_dualx ! sum a dual array
        module procedure sum_dualy ! sum a dual array
    end interface
    interface maxloc
        module procedure maxloc_dualx ! location of max in a dual array
        module procedure maxloc_dualy ! location of max in a dual array
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
    impure elemental subroutine assign_dualx_i(u, i)
        type(dual_x_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        assign_d_i_counter = assign_d_i_counter + 1

    end subroutine
    impure elemental subroutine assign_dualx_r(u, r)
        type(dual_x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        assign_d_r_counter = assign_d_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_dualx(i, v)
        type(dual_x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_d_counter = assign_i_d_counter + 1

    end subroutine
    impure elemental subroutine assign_r_dualx(r, v)
        type(dual_x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_d_counter = assign_r_d_counter + 1

    end subroutine
    impure elemental subroutine assign_dualy_i(u, i)
        type(dual_y_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        assign_d_i_counter = assign_d_i_counter + 1

    end subroutine
    impure elemental subroutine assign_dualy_r(u, r)
        type(dual_y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        assign_d_r_counter = assign_d_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_dualy(i, v)
        type(dual_y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_d_counter = assign_i_d_counter + 1

    end subroutine
    impure elemental subroutine assign_r_dualy(r, v)
        type(dual_y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_d_counter = assign_r_d_counter + 1

    end subroutine
    impure elemental subroutine assign_hdualx_i(u, i)
        type(hdual_x_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_i_counter = assign_hd_i_counter + 1

    end subroutine
    impure elemental subroutine assign_hdualx_r(u, r)
        type(hdual_x_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_r_counter = assign_hd_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_hdualx(i, v)
        type(hdual_x_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_hd_counter = assign_i_hd_counter + 1

    end subroutine
    impure elemental subroutine assign_r_hdualx(r, v)
        type(hdual_x_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_hd_counter = assign_r_hd_counter + 1

    end subroutine
    impure elemental subroutine assign_hdualy_i(u, i)
        type(hdual_y_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_i_counter = assign_hd_i_counter + 1

    end subroutine
    impure elemental subroutine assign_hdualy_r(u, r)
        type(hdual_y_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp
        assign_hd_r_counter = assign_hd_r_counter + 1

    end subroutine
    impure elemental subroutine assign_i_hdualy(i, v)
        type(hdual_y_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)
        assign_i_hd_counter = assign_i_hd_counter + 1

    end subroutine
    impure elemental subroutine assign_r_hdualy(r, v)
        type(hdual_y_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x
        assign_r_hd_counter = assign_r_hd_counter + 1

    end subroutine
    impure elemental function unary_add_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        unary_add_d_counter = unary_add_d_counter + 1
    end function
    impure elemental function add_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        add_d_d_counter = add_d_d_counter + 1
    end function
    impure elemental function add_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_r_counter = add_d_r_counter + 1
    end function
    impure elemental function add_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_r_d_counter = add_r_d_counter + 1
    end function
    impure elemental function add_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_i_counter = add_d_i_counter + 1
    end function
    impure elemental function add_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_i_d_counter = add_i_d_counter + 1
    end function
    impure elemental function unary_add_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        unary_add_d_counter = unary_add_d_counter + 1
    end function
    impure elemental function add_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        add_d_d_counter = add_d_d_counter + 1
    end function
    impure elemental function add_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_r_counter = add_d_r_counter + 1
    end function
    impure elemental function add_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_r_d_counter = add_r_d_counter + 1
    end function
    impure elemental function add_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        add_d_i_counter = add_d_i_counter + 1
    end function
    impure elemental function add_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        add_i_d_counter = add_i_d_counter + 1
    end function
    impure elemental function unary_add_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
        unary_add_hd_counter = unary_add_hd_counter + 1
    end function
    impure elemental function add_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
        add_hd_hd_counter = add_hd_hd_counter + 1
    end function
    impure elemental function add_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_r_counter = add_hd_r_counter + 1
    end function
    impure elemental function add_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_r_hd_counter = add_r_hd_counter + 1
    end function
    impure elemental function add_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_i_counter = add_hd_i_counter + 1
    end function
    impure elemental function add_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_i_hd_counter = add_i_hd_counter + 1
    end function
    impure elemental function unary_add_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
        unary_add_hd_counter = unary_add_hd_counter + 1
    end function
    impure elemental function add_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
        add_hd_hd_counter = add_hd_hd_counter + 1
    end function
    impure elemental function add_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_r_counter = add_hd_r_counter + 1
    end function
    impure elemental function add_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_r_hd_counter = add_r_hd_counter + 1
    end function
    impure elemental function add_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
        add_hd_i_counter = add_hd_i_counter + 1
    end function
    impure elemental function add_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
        add_i_hd_counter = add_i_hd_counter + 1
    end function
    impure elemental function unary_minus_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        unary_minus_d_counter = unary_minus_d_counter + 1
    end function
    impure elemental function minus_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        minus_d_d_counter = minus_d_d_counter + 1
    end function
    impure elemental function minus_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_r_counter = minus_d_r_counter + 1
    end function
    impure elemental function minus_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_r_d_counter = minus_r_d_counter + 1
    end function
    impure elemental function minus_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_i_counter = minus_d_i_counter + 1
    end function
    impure elemental function minus_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_i_d_counter = minus_i_d_counter + 1
    end function
    impure elemental function unary_minus_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        unary_minus_d_counter = unary_minus_d_counter + 1
    end function
    impure elemental function minus_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        minus_d_d_counter = minus_d_d_counter + 1
    end function
    impure elemental function minus_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_r_counter = minus_d_r_counter + 1
    end function
    impure elemental function minus_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_r_d_counter = minus_r_d_counter + 1
    end function
    impure elemental function minus_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        minus_d_i_counter = minus_d_i_counter + 1
    end function
    impure elemental function minus_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        minus_i_d_counter = minus_i_d_counter + 1
    end function
    impure elemental function unary_minus_hdualx(u) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        res%ddx = -u%ddx
        unary_minus_hd_counter = unary_minus_hd_counter + 1
    end function
    impure elemental function minus_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        res%ddx = u%ddx - v%ddx
        minus_hd_hd_counter = minus_hd_hd_counter + 1
    end function
    impure elemental function minus_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_r_counter = minus_hd_r_counter + 1
    end function
    impure elemental function minus_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_r_hd_counter = minus_r_hd_counter + 1
    end function
    impure elemental function minus_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_i_counter = minus_hd_i_counter + 1
    end function
    impure elemental function minus_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_i_hd_counter = minus_i_hd_counter + 1
    end function
    impure elemental function unary_minus_hdualy(u) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t) :: res
        
        res%x = -u%x
        res%dx = -u%dx
        res%ddx = -u%ddx
        unary_minus_hd_counter = unary_minus_hd_counter + 1
    end function
    impure elemental function minus_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v%x
        res%dx = u%dx - v%dx
        res%ddx = u%ddx - v%ddx
        minus_hd_hd_counter = minus_hd_hd_counter + 1
    end function
    impure elemental function minus_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_r_counter = minus_hd_r_counter + 1
    end function
    impure elemental function minus_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_r_hd_counter = minus_r_hd_counter + 1
    end function
    impure elemental function minus_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x - v
        res%dx = u%dx
        res%ddx = u%ddx
        minus_hd_i_counter = minus_hd_i_counter + 1
    end function
    impure elemental function minus_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u - v%x
        res%dx = -v%dx
        res%ddx = -v%ddx
        minus_i_hd_counter = minus_i_hd_counter + 1
    end function
    impure elemental function mult_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        mult_d_d_counter = mult_d_d_counter + 1
    end function
    impure elemental function mult_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_r_counter = mult_d_r_counter + 1
    end function
    impure elemental function mult_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_r_d_counter = mult_r_d_counter + 1
    end function
    impure elemental function mult_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_i_counter = mult_d_i_counter + 1
    end function
    impure elemental function mult_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_i_d_counter = mult_i_d_counter + 1
    end function
    impure elemental function mult_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v%x
        res%dx = u%dx*v%x + u%x*v%dx
        mult_d_d_counter = mult_d_d_counter + 1
    end function
    impure elemental function mult_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_r_counter = mult_d_r_counter + 1
    end function
    impure elemental function mult_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_r_d_counter = mult_r_d_counter + 1
    end function
    impure elemental function mult_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        mult_d_i_counter = mult_d_i_counter + 1
    end function
    impure elemental function mult_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        mult_i_d_counter = mult_i_d_counter + 1
    end function
    impure elemental function mult_hdualx_hdualx(u, v) result(res)
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
        mult_hd_hd_counter = mult_hd_hd_counter + 1
    end function
    impure elemental function mult_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_r_counter = mult_hd_r_counter + 1
    end function
    impure elemental function mult_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_r_hd_counter = mult_r_hd_counter + 1
    end function
    impure elemental function mult_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_i_counter = mult_hd_i_counter + 1
    end function
    impure elemental function mult_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_i_hd_counter = mult_i_hd_counter + 1
    end function
    impure elemental function mult_hdualy_hdualy(u, v) result(res)
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
        mult_hd_hd_counter = mult_hd_hd_counter + 1
    end function
    impure elemental function mult_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_r_counter = mult_hd_r_counter + 1
    end function
    impure elemental function mult_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_r_hd_counter = mult_r_hd_counter + 1
    end function
    impure elemental function mult_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
        mult_hd_i_counter = mult_hd_i_counter + 1
    end function
    impure elemental function mult_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
        mult_i_hd_counter = mult_i_hd_counter + 1
    end function
    impure elemental function div_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        div_d_d_counter = div_d_d_counter + 1
    end function
    impure elemental function div_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_r_counter = div_d_r_counter + 1
    end function
    impure elemental function div_r_dualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_r_d_counter = div_r_d_counter + 1
    end function
    impure elemental function div_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_i_counter = div_d_i_counter + 1
    end function
    impure elemental function div_i_dualx(u, v) result(res)
        integer, intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_i_d_counter = div_i_d_counter + 1
    end function
    impure elemental function div_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        div_d_d_counter = div_d_d_counter + 1
    end function
    impure elemental function div_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_r_counter = div_d_r_counter + 1
    end function
    impure elemental function div_r_dualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_r_d_counter = div_r_d_counter + 1
    end function
    impure elemental function div_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        div_d_i_counter = div_d_i_counter + 1
    end function
    impure elemental function div_i_dualy(u, v) result(res)
        integer, intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u/v%x
        res%dx = -u*v%dx/v%x**2
        div_i_d_counter = div_i_d_counter + 1
    end function
    impure elemental function div_hdualx_hdualx(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(j)*v%dx(i) - t0*v%dx(j)*(-2*t1*v%dx(i) + u%dx(i)) - t1* &
      v%ddx(k) + u%ddx(k))
            end do
        end do
        div_hd_hd_counter = div_hd_hd_counter + 1
    end function
    impure elemental function div_hdualx_r(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_x_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_r_counter = div_hd_r_counter + 1
    end function
    impure elemental function div_r_hdualx(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_r_hd_counter = div_r_hd_counter + 1
    end function
    impure elemental function div_hdualx_i(u, v) result(res)
        type(hdual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_x_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_i_counter = div_hd_i_counter + 1
    end function
    impure elemental function div_i_hdualx(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_x_t), intent(in) :: v
        type(hdual_x_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, nx
            do i = j, nx
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_i_hd_counter = div_i_hd_counter + 1
    end function
    impure elemental function div_hdualy_hdualy(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = t0*u%x
        res%x = t1
        res%dx = t0*(-t1*v%dx + u%dx)
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = t0*(-t0*u%dx(j)*v%dx(i) - t0*v%dx(j)*(-2*t1*v%dx(i) + u%dx(i)) - t1* &
      v%ddx(k) + u%ddx(k))
            end do
        end do
        div_hd_hd_counter = div_hd_hd_counter + 1
    end function
    impure elemental function div_hdualy_r(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_y_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_r_counter = div_hd_r_counter + 1
    end function
    impure elemental function div_r_hdualy(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_r_hd_counter = div_r_hd_counter + 1
    end function
    impure elemental function div_hdualy_i(u, v) result(res)
        type(hdual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_y_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%x = t0*u%x
        res%dx = t0*u%dx
        res%ddx = t0*u%ddx
        div_hd_i_counter = div_hd_i_counter + 1
    end function
    impure elemental function div_i_hdualy(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_y_t), intent(in) :: v
        type(hdual_y_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%x
        t1 = u/v%x**2
        res%x = t0*u
        res%dx = -t1*v%dx
        k = 0
        do j = 1, ny
            do i = j, ny
                k = k + 1
                res%ddx(k) = t1*(2*t0*v%dx(i)*v%dx(j) - v%ddx(k))
            end do
        end do
        div_i_hd_counter = div_i_hd_counter + 1
    end function
    impure elemental function pow_dualx_i(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_i_counter = pow_d_i_counter + 1
    end function
    impure elemental function pow_dualx_r(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_x_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_r_counter = pow_d_r_counter + 1
    end function
    impure elemental function pow_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t), intent(in) :: v
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = u%x**v%x
        res%x = t0
        res%dx = t0*(u%dx*v%x/u%x + v%dx*log(u%x))

        pow_d_d_counter = pow_d_d_counter + 1
    end function
    impure elemental function pow_dualy_i(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_i_counter = pow_d_i_counter + 1
    end function
    impure elemental function pow_dualy_r(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_y_t) :: res
        
        res%x = u%x**v
        res%dx = u%x**(v - 1)*u%dx*v
        pow_d_r_counter = pow_d_r_counter + 1
    end function
    impure elemental function pow_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t), intent(in) :: v
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = u%x**v%x
        res%x = t0
        res%dx = t0*(u%dx*v%x/u%x + v%dx*log(u%x))

        pow_d_d_counter = pow_d_d_counter + 1
    end function
    impure elemental function pow_hdualx_i(u, v) result(res)
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
        pow_hd_i_counter = pow_hd_i_counter + 1
    end function
    impure elemental function pow_hdualx_r(u, v) result(res)
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
        pow_hd_r_counter = pow_hd_r_counter + 1
    end function
    impure elemental function pow_hdualx_hdualx(u, v) result(res)
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
        pow_hd_hd_counter = pow_hd_hd_counter + 1
    end function
    impure elemental function pow_hdualy_i(u, v) result(res)
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
        pow_hd_i_counter = pow_hd_i_counter + 1
    end function
    impure elemental function pow_hdualy_r(u, v) result(res)
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
        pow_hd_r_counter = pow_hd_r_counter + 1
    end function
    impure elemental function pow_hdualy_hdualy(u, v) result(res)
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
        pow_hd_hd_counter = pow_hd_hd_counter + 1
    end function
    impure elemental  function eq_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function
    impure elemental  function eq_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x == rhs)

    end function
    impure elemental  function eq_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function eq_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function
    impure elemental  function le_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function
    impure elemental  function le_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function
    impure elemental  function le_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function le_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function
    impure elemental  function lt_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x < rhs%x)

    end function
    impure elemental  function lt_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x < rhs)

    end function
    impure elemental  function lt_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function lt_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function
    impure elemental  function ge_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x >= rhs%x)

    end function
    impure elemental  function ge_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x >= rhs)

    end function
    impure elemental  function ge_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function ge_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function
    impure elemental  function gt_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x > rhs%x)

    end function
    impure elemental  function gt_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x > rhs)

    end function
    impure elemental  function gt_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function gt_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function
    impure elemental  function ne_dualx_dualx(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_dualx_i(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_dualx_r(lhs, rhs) result(res)
         type(dual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_dualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_dualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_dualy_dualy(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_dualy_i(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_dualy_r(lhs, rhs) result(res)
         type(dual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_dualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_dualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_hdualx_hdualx(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_hdualx_i(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_hdualx_r(lhs, rhs) result(res)
         type(hdual_x_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_hdualx(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_hdualx(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_x_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_hdualy_hdualy(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x /= rhs%x)

    end function
    impure elemental  function ne_hdualy_i(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_hdualy_r(lhs, rhs) result(res)
         type(hdual_y_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x /= rhs)

    end function
    impure elemental  function ne_i_hdualy(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function ne_r_hdualy(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual_y_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%x)

    end function
    impure elemental  function abs_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        integer :: i

        ! We need this!
        res%x = abs(u%x)
        res%dx = abs(u%dx)

    end function
    impure elemental  function abs_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        integer :: i

        ! We need this!
        res%x = abs(u%x)
        res%dx = abs(u%dx)

    end function
    impure elemental  function acos_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = acos(u%x)
    
        res%dx = -u%dx / sqrt(1.0_dp - u%x**2)

    end function
    impure elemental  function acos_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = acos(u%x)
    
        res%dx = -u%dx / sqrt(1.0_dp - u%x**2)

    end function
    impure elemental  function asin_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = asin(u%x)
        res%dx = u%dx / sqrt(1.0_dp - u%x**2)

    end function
    impure elemental  function asin_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = asin(u%x)
        res%dx = u%dx / sqrt(1.0_dp - u%x**2)

    end function
    impure elemental  function atan_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function
    impure elemental  function atan_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function
    impure elemental  function atan2_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u, v
        type(dual_x_t) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function
    impure elemental  function atan2_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u, v
        type(dual_y_t) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function
    impure elemental  function cos_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function
    impure elemental  function cos_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function
      function dot_product_dualx_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u(:), v(:)
        type(dual_x_t) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, nx
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function
      function dot_product_dualy_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u(:), v(:)
        type(dual_y_t) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, ny
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function
    impure elemental  function exp_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function
    impure elemental  function exp_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function
    impure elemental  function int_dualx(u) result(res)
         type(dual_x_t), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function
    impure elemental  function int_dualy(u) result(res)
         type(dual_y_t), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function
    impure elemental function log_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        
        res%x = log(u%x)
        res%dx = u%dx/u%x
        log_d_counter = log_d_counter + 1
    end function
    impure elemental function log_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        
        res%x = log(u%x)
        res%dx = u%dx/u%x
        log_d_counter = log_d_counter + 1
    end function
    impure elemental function log_hdualx(u) result(res)
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
        log_hd_counter = log_hd_counter + 1
    end function
    impure elemental function log_hdualy(u) result(res)
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
        log_hd_counter = log_hd_counter + 1
    end function
    impure elemental  function log10_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function
    impure elemental  function log10_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function
      function matmul_dualx_dualx(u,v) result(res)
        type(dual_x_t), intent(in) :: u(:,:), v(:,:)
        type(dual_x_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, nx
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
      function matmul_dualx_v(u, v) result(res)
        type(dual_x_t), intent(in) :: u(:,:), v(:)
        type(dual_x_t) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, nx
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
      function matmul_v_dualx(u, v) result(res)
        type(dual_x_t), intent(in) :: u(:), v(:,:)
        type(dual_x_t) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, nx
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
      function matmul_dualy_dualy(u,v) result(res)
        type(dual_y_t), intent(in) :: u(:,:), v(:,:)
        type(dual_y_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, ny
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
      function matmul_dualy_v(u, v) result(res)
        type(dual_y_t), intent(in) :: u(:,:), v(:)
        type(dual_y_t) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, ny
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
      function matmul_v_dualy(u, v) result(res)
        type(dual_y_t), intent(in) :: u(:), v(:,:)
        type(dual_y_t) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, ny
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function
    impure elemental  function max_dualx_dualx(val1, val2) result(res)
        type(dual_x_t), intent(in) :: val1, val2
        type(dual_x_t) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function max_dualx_i(u, i) result(res)
        type(dual_x_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual_x_t) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function
    impure elemental  function max_dualx_r(u, r) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_x_t) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function
     impure elemental  function max_r_dualx(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function
    impure elemental  function max_dualy_dualy(val1, val2) result(res)
        type(dual_y_t), intent(in) :: val1, val2
        type(dual_y_t) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function max_dualy_i(u, i) result(res)
        type(dual_y_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual_y_t) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function
    impure elemental  function max_dualy_r(u, r) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_y_t) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function
     impure elemental  function max_r_dualy(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function
    impure elemental  function dmax1_dualx_dualx(val1, val2, val3, val4,val5) result(res)
        type(dual_x_t), intent(in) :: val1, val2
        type(dual_x_t), intent(in), optional :: val3, val4,val5
        type(dual_x_t) :: res

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

    end function
    impure elemental  function dmax1_dualy_dualy(val1, val2, val3, val4,val5) result(res)
        type(dual_y_t), intent(in) :: val1, val2
        type(dual_y_t), intent(in), optional :: val3, val4,val5
        type(dual_y_t) :: res

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

    end function
      function maxval_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_x_t) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function
      function maxval_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_y_t) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function
    impure elemental  function min_dualx_dualx(val1, val2) result(res)
        type(dual_x_t), intent(in) :: val1, val2
        type(dual_x_t) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function min_dualx_r(u, r) result(res)
        type(dual_x_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_x_t) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function
    impure elemental  function min_dualy_dualy(val1, val2) result(res)
        type(dual_y_t), intent(in) :: val1, val2
        type(dual_y_t) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function min_dualy_r(u, r) result(res)
        type(dual_y_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual_y_t) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function
    impure elemental  function dmin1_dualx_dualx(val1, val2, val3, val4) result(res)
        type(dual_x_t), intent(in) :: val1, val2
        type(dual_x_t), intent(in), optional :: val3, val4
        type(dual_x_t) :: res

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

    end function
    impure elemental  function dmin1_dualy_dualy(val1, val2, val3, val4) result(res)
        type(dual_y_t), intent(in) :: val1, val2
        type(dual_y_t), intent(in), optional :: val3, val4
        type(dual_y_t) :: res

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

    end function
      function minval_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_x_t) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function
      function minval_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual_y_t) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function
    impure elemental  function nint_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function
    impure elemental  function nint_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function
    impure elemental  function sign_dualx_dualx(val1, val2) result(res)
        type(dual_x_t), intent(in) :: val1, val2
        type(dual_x_t) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    impure elemental  function sign_r_dualx(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual_x_t), intent(in) :: val2
        type(dual_x_t) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    impure elemental  function sign_dualy_dualy(val1, val2) result(res)
        type(dual_y_t), intent(in) :: val1, val2
        type(dual_y_t) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    impure elemental  function sign_r_dualy(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual_y_t), intent(in) :: val2
        type(dual_y_t) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    impure elemental  function sin_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function
    impure elemental  function sin_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function
    impure elemental  function tan_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function
    impure elemental  function tan_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function
    impure elemental function sqrt_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u
        type(dual_x_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%x)
        res%x = t0
        res%dx = 0.5_dp*u%dx/t0
        sqrt_d_counter = sqrt_d_counter + 1
    end function

    impure elemental function sqrt_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u
        type(dual_y_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%x)
        res%x = t0
        res%dx = 0.5_dp*u%dx/t0
        sqrt_d_counter = sqrt_d_counter + 1
    end function

    impure elemental function sqrt_hdualx(u) result(res)
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
        sqrt_hd_counter = sqrt_hd_counter + 1
    end function
    impure elemental function sqrt_hdualy(u) result(res)
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
        sqrt_hd_counter = sqrt_hd_counter + 1
    end function
      function sum_dualx(u) result(res)
        type(dual_x_t), intent(in) :: u(:)
        type(dual_x_t) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, nx
            res%dx(i) = sum(u%dx(i))
        end do

    end function
      function sum_dualy(u) result(res)
        type(dual_y_t), intent(in) :: u(:)
        type(dual_y_t) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, ny
            res%dx(i) = sum(u%dx(i))
        end do

    end function
      function maxloc_dualx(array) result(ind)
        type(dual_x_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function
      function maxloc_dualy(array) result(ind)
        type(dual_y_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function
    
    
end module