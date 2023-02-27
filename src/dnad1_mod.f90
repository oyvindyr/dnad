!******************************************************************************
!* Dual Number Automatic Differentiation (DNAD) of Fortran Codes
!*-----------------------------------------------------------------------------
!* COPYRIGHT (c) Joshua Hodson, All rights reserved, you are free to copy,
!* modify, or translate this code to other languages such as c/c++. This is a
!* fork of the original Fortran DNAD module developed by Dr. Wenbin Yu. See
!* original copyright information below. You can download the original version
!* at https://cdmhub.org/resources/374
!*
!* COPYRIGHT (c) Wenbin Yu, All rights reserved, you are free to copy,
!* modify or translate this code to other languages such as c/c++. If
!* you find a bug please let me know through wenbinyu.heaven@gmail.com. If
!* you added new functions and want to share with others, please let me know
!* too. You are welcome to share your successful stories with us through
!* http://groups.google.com/group/hifi-comp.
!******************************************************************************
!* Acknowledgements
!*-----------------------------------------------------------------------------
!* The development of DNAD is supported, in part, by the Chief Scientist
!* Innovative Research Fund at AFRL/RB WPAFB, and by Department of Army
!* SBIR (Topic A08-022) through Advanced Dynamics Inc. The views and
!* conclusions contained herein are those of the authors and should not be
!* interpreted as necessarily representing the official policies or
!* endorsement, either expressed or implied, of the funding agency.
!*
!* Additional development of DNAD has been supported under a Department of
!* Energy (DOE) Nuclear Energy University Program (NEUP) Graduate Fellowship.
!* Any opinions, findings, conclusions or recommendations expressed in this
!* publication are those of the authors and do not necessarily reflect the
!* views of the Department of Energy Office of Nuclear Energy.
!******************************************************************************
!* Citation
!*-----------------------------------------------------------------------------
!* Your citation of the following two papers is appreciated:
!* Yu, W. and Blair, M.: "DNAD, a Simple Tool for Automatic Differentiation of
!* Fortran Codes Using Dual Numbers," Computer Physics Communications, vol.
!* 184, 2013, pp. 1446-1452.
!*
!* Spall, R. and Yu, W.: "Imbedded Dual-Number Automatic Differentiation for
!* CFD Sensitivity Analysis," Journal of Fluids Engineering, vol. 135, 2013,
!* 014501.
!******************************************************************************
!* Quick Start Guide
!*-----------------------------------------------------------------------------
!* To integrate DNAD into an existing Fortran program, do the following:
!*
!*   1. Include the DNAD module in the source files by adding "use dnadmod" to
!*      the beginning of all modules, global functions, and global subroutines
!*      that include definitions of floating-point variables.
!*   2. Redefine all floating-point variables as type(dual). This can be done
!*      using precompiler directives so that the integration can be turned on
!*      or off at compile-time, eliminating the need for maintaining two
!*      separate code bases for the same project.
!*   3. All I/O involving floating-point variables will need to be examined.
!*      A method will need to be determined for inputting and outputting
!*      derivative values. This customization is typically unique for each
!*      piece of software and needs to be determined on a case-by-case basis.
!*   4. When compiling DNAD, use the compiler option "-Dndv=#", where # is the
!*      number of design variables desired. This sizes the derivative array
!*      that is stored with each floating point number.
!*   5. When compiling DNAD, use compiler options to specify precision. If no
!*      compiler options are specified, DNAD will default to single-precision
!*      floating-point arithmetic. Most popular Fortran compilers provide
!*      options for specifying precision at compile-time so that it does not
!*      have to be hard-coded into the source code. For example, use the
!*      "-fdefault-real-8" compiler in gfortran or the "-r8" compiler option
!*      with Intel Fortran to compile DNAD as double-precision.
!*   6. Modify the compilation process for the target software to include the
!*      DNAD module in the resulting executable or library.
!******************************************************************************
!* Change Log
!*-----------------------------------------------------------------------------
!*
!*  2016-04-29  Joshua Hodson
!*  - Updated copyright, acknowledgments, and quick start guide.
!*  - Removed overloads for single-precision reals.
!*  - Added tan, dtan, atan, and atan2 intrinsic function overloads.
!*  - Removed macro for precision and defined all floating-point variables as
!*    default real. Compiler options can now be used to set precision.
!*  - Added checks for undefined derivatives when only constants are used in
!*    the calculation (i.e. all partial derivatives are zero). This limits the
!*    perpetuation of NaN values in the code.
!*  - Combined the header and source files into a single file.
!*
!*  2015-07-29  Joshua Hodson
!*  - Added maxloc intrinsic function overload.
!*  - Converted UPPERCASE to lowercase for readability.
!*  - Added macros for defining precision and number of design variables.
!*  - Renamed module from Dual_Num_Auto_Diff to dnadmod
!*  - Renamed dual number type from DUAL_NUM to dual
!*  - Renamed components of dual number type from (xp_ad_, xp_ad_) to (x, dx)
!*
!*  2014-06-05  Wenbin Yu
!*  - Forked from original DNAD repository, see https://cdmhub.org/resources/374
!*
!******************************************************************************





module dnad_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64

    integer, parameter :: num_deriv = 1

    ! Workaround to make pure functions produce NaN's deliberately:
    real(dp) :: negative_one = -1.0_dp

    public assignment (=)
    public operator (+)
    public operator (-)
    public operator (*)
    public operator (/)
    public operator (**)
    public operator (==)
    public operator (<=)
    public operator (<)
    public operator (>=)
    public operator (>)
    public operator (/=)
    public abs
    public dabs
    public acos
    public asin
    public atan
    public atan2
    public cos
    public dcos
    public dot_product
    public exp
    public int
    public log
    public log10
    public matmul
    public max
    public dmax1
    public maxval
    public min
    public dmin1
    public minval
    public nint
    public  sign
    public sin
    public dsin
    public tan
    public dtan
    public sqrt
    public sum
    public maxloc
 
    

    type :: dual1
        sequence
        real(dp) :: x  ! functional value
        real(dp) :: dx(num_deriv)  ! derivative
    end type


    interface assignment (=)
        module procedure assign_di  ! dual=integer, elemental
        module procedure assign_dr  ! dual=real, elemental
        module procedure assign_id  ! integer=dual, elemental
        module procedure assign_rd  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure add_d   ! +dual number, elemental
        module procedure add_dd  ! dual + dual, elemental
        module procedure add_di  ! dual + integer, elemental
        module procedure add_dr  ! dual + real, elemental
        module procedure add_id  ! integer + dual, elemental
        module procedure add_rd  ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure minus_d   ! negate a dual number,elemental
        module procedure minus_dd  ! dual -dual,elemental
        module procedure minus_di  ! dual-integer,elemental
        module procedure minus_dr  ! dual-real,elemental
        module procedure minus_id  ! integer-dual,elemental
        module procedure minus_rd  ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult_dd    ! dual*dual, elemental
        module procedure mult_di    ! dual*integer,elemental
        module procedure mult_dr    ! dual*real,elemental
        module procedure mult_id    ! integer*dual,elemental
        module procedure mult_rd    ! real*dual,elemental
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
    end interface
    interface sum
        module procedure sum_d ! sum a dual array
    end interface
    interface maxloc
        module procedure maxloc_d ! location of max in a dual array
    end interface

contains

    elemental subroutine assign_di(u, i)
         type(dual1), intent(out) :: u
         integer, intent(in) :: i

         u%x = real(i, dp)  ! This is faster than direct assignment
         u%dx = 0.0_dp

    end subroutine assign_di
    elemental subroutine assign_dr(u, r)
        type(dual1), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine assign_dr
    elemental subroutine assign_id(i, v)
         type(dual1), intent(in) :: v
         integer, intent(out) :: i

         i = int(v%x)

    end subroutine assign_id
    elemental subroutine assign_rd(r, v)
         type(dual1), intent(in) :: v
         real(dp), intent(out) :: r

         r = v%x

    end subroutine assign_rd
    elemental function add_d(u) result(res)
         type(dual1), intent(in) :: u
         type(dual1) :: res

         res = u  ! Faster than assigning component wise

    end function add_d
    elemental function add_dd(u, v) result(res)
         type(dual1), intent(in) :: u, v
         type(dual1) :: res

         res%x = u%x + v%x
         res%dx = u%dx + v%dx

    end function add_dd
    elemental function add_di(u, i) result(res)
         type(dual1), intent(in) :: u
         integer, intent(in) :: i
         type(dual1) :: res

         res%x = real(i, dp) + u%x
         res%dx = u%dx

    end function add_di
    elemental function add_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        res%x = r + u%x
        res%dx = u%dx

    end function add_dr
    elemental function add_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: v
        type(dual1) :: res

        res%x = real(i, dp) + v%x
        res%dx = v%dx

    end function add_id
    elemental function add_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual1), intent(in) :: v
        type(dual1) :: res

        res%x = r + v%x
        res%dx = v%dx

    end function add_rd
    elemental function minus_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = -u%x
        res%dx = -u%dx

    end function minus_d
    elemental function minus_dd(u, v) result(res)
        type(dual1), intent(in) :: u, v
        type(dual1) :: res

        res%x = u%x - v%x
        res%dx = u%dx - v%dx

    end function minus_dd
    elemental function minus_di(u, i) result(res)
        type(dual1), intent(in) :: u
        integer, intent(in) :: i
        type(dual1) :: res

        res%x = u%x - real(i, dp)
        res%dx = u%dx

    end function minus_di
    elemental function minus_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp),intent(in) :: r
        type(dual1) :: res

        res%x = u%x - r
        res%dx = u%dx

    end function minus_dr
    elemental function minus_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: v
        type(dual1) :: res

        res%x = real(i, dp) - v%x
        res%dx = -v%dx

    end function minus_id
    elemental function minus_rd(r, v) result(res)
         real(dp), intent(in) :: r
         type(dual1), intent(in) :: v
         type(dual1) :: res

        res%x = r - v%x
        res%dx = -v%dx

    end function minus_rd
    elemental function mult_dd(u, v) result(res)
        type(dual1), intent(in) :: u, v
        type(dual1) :: res

        res%x = u%x * v%x
        res%dx = u%x * v%dx + v%x * u%dx

    end function mult_dd
    elemental function mult_di(u, i) result(res)
        type(dual1), intent(in) :: u
        integer, intent(in) :: i
        type(dual1) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * u%x
        res%dx = r * u%dx

    end function mult_di
    elemental function mult_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        res%x = u%x * r
        res%dx = u%dx * r

    end function mult_dr
    elemental function mult_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: v
        type(dual1) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_id
    elemental function mult_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual1), intent(in) :: v
        type(dual1) :: res

        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_rd
    elemental function div_dd(u, v) result(res)
        type(dual1), intent(in) :: u, v
        type(dual1) :: res

        res%x = u%x / v%x
        res%dx = (u%dx - res%x * v%dx) / v%x

    end function div_dd
    elemental function div_di(u, i) result(res)
        type(dual1), intent(in) :: u
        integer, intent(in) :: i
        type(dual1) :: res

        res%x = u%x / i
        res%dx = u%dx / i

    end function div_di
    elemental function div_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        res%x = u%x / r
        res%dx = u%dx / r

    end function div_dr
    elemental function div_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: v
        type(dual1) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = real(i, dp) * inv
        res%dx = -res%x * inv * v%dx

    end function div_id
    elemental function div_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual1), intent(in) :: v
        type(dual1) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = r * inv
        res%dx = -res%x * inv * v%dx

    end function div_rd
    elemental function pow_i(u, i) result(res)
        type(dual1), intent(in) :: u
        integer, intent(in) :: i
        type(dual1) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (i - 1)
        res%x = u%x * pow_x
        res%dx = real(i, dp) * pow_x * u%dx

    end function pow_i
    elemental function pow_r(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (r - 1.0_dp)
        res%x = u%x * pow_x
        res%dx = r * pow_x * u%dx

    end function pow_r
    elemental function pow_d(u, v) result(res)
        type(dual1), intent(in)::u, v
        type(dual1) :: res

        res%x = u%x ** v%x
        res%dx = res%x * (v%x / u%x * u%dx + log(u%x) * v%dx)

    end function pow_d
    elemental function eq_dd(lhs, rhs) result(res)
         type(dual1), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function eq_dd
    elemental function eq_di(lhs, rhs) result(res)
         type(dual1), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == real(rhs, dp))

    end function eq_di
    elemental function eq_dr(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical::res

        res = (lhs%x == rhs)

    end function eq_dr
    elemental function eq_id(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_id
    elemental function eq_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_rd
    elemental function le_dd(lhs, rhs) result(res)
         type(dual1), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function le_dd
    elemental function le_di(lhs, rhs) result(res)
         type(dual1), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_di
    elemental function le_dr(lhs, rhs) result(res)
         type(dual1), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_dr
    elemental function le_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (i <= rhs%x)

    end function le_id
    elemental function le_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function le_rd
    elemental function lt_dd(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x < rhs%x)

    end function lt_dd
    elemental function lt_di(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_di
    elemental function lt_dr(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_dr
    elemental function lt_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (i < rhs%x)

    end function lt_id
    elemental function lt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function lt_rd
    elemental function ge_dd(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x >= rhs%x)

    end function ge_dd
    elemental function ge_di(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_di
    elemental function ge_dr(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_dr
    elemental function ge_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: rhs
        logical :: res

        res = (i >= rhs%x)

    end function ge_id
    elemental function ge_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function ge_rd
    elemental function gt_dd(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x > rhs%x)

    end function gt_dd
    elemental function gt_di(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_di
    elemental function gt_dr(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_dr
    elemental function gt_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: rhs
        logical :: res

        res = (i > rhs%x)

    end function gt_id
    elemental function gt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual1), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function gt_rd
    elemental function ne_dd(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x /= rhs%x)

    end function ne_dd
    elemental function ne_di(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_di
    elemental function ne_dr(lhs, rhs) result(res)
        type(dual1), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_dr
    elemental function ne_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual1), intent(in) :: rhs
        logical :: res

        res = (i /= rhs%x)

    end function ne_id
    elemental function ne_rd(lhs, rhs) result(res)
        real(dp), intent(in) :: lhs
        type(dual1), intent(in) :: rhs
        logical :: res

        res = (lhs /= rhs%x)

    end function ne_rd
    elemental function abs_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res
        integer :: i

        if (u%x == 0) then
            res%x = 0.0_dp
            do i = 1, num_deriv
                if (u%dx(i) .eq. 0.0_dp) then
                    res%dx(i) = 0.0_dp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
        else 
            res%x = abs(u%x)
            res%dx = abs(u%dx)
        end if

    end function abs_d
    elemental function acos_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = acos(u%x)
    
        if (u%x == 1.0_dp .or. u%x == -1.0_dp) then
            res%dx = set_Nan()  ! Undefined derivative
        else
            res%dx = -u%dx / sqrt(1.0_dp - u%x**2)
        end if

    end function acos_d
    elemental function asin_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = asin(u%x)
        if (u%x == 1.0_dp .or. u%x == -1.0_dp) then
            res%dx = set_NaN()  ! Undefined derivative
        else
            res%dx = u%dx / sqrt(1.0_dp - u%x**2)
        end if

    end function asin_d
    elemental function atan_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function atan_d
    elemental function atan2_d(u, v) result(res)
        type(dual1), intent(in) :: u, v
        type(dual1) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function atan2_d
    elemental function cos_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d
    pure function dot_product_dd(u, v) result(res)
        type(dual1), intent(in) :: u(:), v(:)
        type(dual1) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function dot_product_dd
    elemental function exp_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function exp_d
    elemental function int_d(u) result(res)
         type(dual1), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function int_d
    elemental function log_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = log(u%x)
        res%dx = u%dx / u%x

    end function log_d
    elemental function log10_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function log10_d
    pure function matmul_dd(u,v) result(res)
        type(dual1), intent(in) :: u(:,:), v(:,:)
        type(dual1) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dd
    pure function matmul_dv(u, v) result(res)
        type(dual1), intent(in) :: u(:,:), v(:)
        type(dual1) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dv
    pure function matmul_vd(u, v) result(res)
        type(dual1), intent(in) :: u(:), v(:,:)
        type(dual1) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, num_deriv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_vd
    elemental function max_dd(val1, val2) result(res)
        type(dual1), intent(in) :: val1, val2
        type(dual1) :: res

        if (val1%x > val2%x) then
            res = val1
        else
            res = val2
        endif

    end function max_dd
    elemental function max_di(u, i) result(res)
        type(dual1), intent(in) :: u
        integer, intent(in) :: i
        type(dual1) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function max_di
    elemental function max_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function max_dr
     elemental function max_rd(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual1), intent(in) :: u
        type(dual1) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function max_rd
    elemental function dmax1_dd(val1, val2, val3, val4,val5) result(res)
        type(dual1), intent(in) :: val1, val2
        type(dual1), intent(in), optional :: val3, val4,val5
        type(dual1) :: res

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
    pure function maxval_d(u) result(res)
        type(dual1), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual1) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function maxval_d
    elemental function min_dd(val1, val2) result(res)
        type(dual1), intent(in) :: val1, val2
        type(dual1) :: res

        if (val1%x < val2%x) then
            res = val1
        else
            res = val2
        endif

    end function min_dd
    elemental function min_dr(u, r) result(res)
        type(dual1), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual1) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function min_dr
    elemental function dmin1_dd(val1, val2, val3, val4) result(res)
        type(dual1), intent(in) :: val1, val2
        type(dual1), intent(in), optional :: val3, val4
        type(dual1) :: res

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
    pure function minval_d(u) result(res)
        type(dual1), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual1) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function minval_d
    elemental function nint_d(u) result(res)
        type(dual1), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function nint_d
    elemental function sign_dd(val1, val2) result(res)
        type(dual1), intent(in) :: val1, val2
        type(dual1) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function sign_dd
    elemental function sign_rd(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual1), intent(in) :: val2
        type(dual1) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function sign_rd
    elemental function sin_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d
    elemental function tan_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d
    elemental function sqrt_d(u) result(res)
        type(dual1), intent(in) :: u
        type(dual1) :: res
        integer :: i

        res%x = sqrt(u%x)

        if (res%x .ne. 0.0_dp) then
            res%dx = 0.5_dp * u%dx / res%x
        else
            do i = 1, num_deriv
                if (u%dx(i) .eq. 0.0_dp) then
                    res%dx(i) = 0.0_dp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
        end if

    end function sqrt_d
    pure function sum_d(u) result(res)
        type(dual1), intent(in) :: u(:)
        type(dual1) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, num_deriv
            res%dx(i) = sum(u%dx(i))
        end do

    end function sum_d
    pure function maxloc_d(array) result(ind)
        type(dual1), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function maxloc_d

    ! Workaround to make pure functions produce NaN's deliberately:
    elemental function set_NaN() result(res)
        real(dp) :: res
        res = sqrt(negative_one)
    end function set_NaN

end module
