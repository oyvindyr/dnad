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
!*   2. Redefine all floating-point variables as type(dual2). This can be done
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
!*  - Renamed dual2 number type from DUAL_NUM to dual2
!*  - Renamed components of dual2 number type from (xp_ad_, xp_ad_) to (x, dx)
!*
!*  2014-06-05  Wenbin Yu
!*  - Forked from original DNAD repository, see https://cdmhub.org/resources/374
!*
!******************************************************************************

module dnad2_mod

    implicit none

    integer, parameter :: wp = kind(0.d0)
    integer, parameter :: ndv = 2 ! ndv_literal will be replaced by pre-processor

    private

    real(wp) :: negative_one = -1.0_wp
    type,public:: dual2  ! make this private will create difficulty to use the
                        ! original write/read commands, hence x and dx are
                        ! variables which can be accessed using D%x and D%dx in
                        ! other units using this module in which D is defined
                        ! as type(dual2).
        sequence
        real(wp) :: x  ! functional value
        real(wp) :: dx(ndv)  ! derivative
    end type dual2


!******** Interfaces for operator overloading
    public assignment (=)
    interface assignment (=)
        module procedure assign_di  ! dual2=integer, elemental
        module procedure assign_dr  ! dual2=real, elemental
        module procedure assign_id  ! integer=dual2, elemental
    end interface


    public operator (+)
    interface operator (+)
        module procedure add_d   ! +dual2 number, elemental
        module procedure add_dd  ! dual2 + dual2, elemental
        module procedure add_di  ! dual2 + integer, elemental
        module procedure add_dr  ! dual2 + real, elemental
        module procedure add_id  ! integer + dual2, elemental
        module procedure add_rd  ! real + dual2, elemental
    end interface

    public operator (-)
    interface operator (-)
        module procedure minus_d   ! negate a dual2 number,elemental
        module procedure minus_dd  ! dual2 -dual2,elemental
        module procedure minus_di  ! dual2-integer,elemental
        module procedure minus_dr  ! dual2-real,elemental
        module procedure minus_id  ! integer-dual2,elemental
        module procedure minus_rd  ! real-dual2,elemental
    end interface

    public operator (*)
    interface operator (*)
        module procedure mult_dd    ! dual2*dual2, elemental
        module procedure mult_di    ! dual2*integer,elemental
        module procedure mult_dr    ! dual2*real,elemental
        module procedure mult_id    ! integer*dual2,elemental
        module procedure mult_rd    ! real*dual2,elemental
    end interface

    public operator (/)
    interface operator (/)
        module procedure div_dd ! dual2/dual2,elemental
        module procedure div_di ! dual2/integer, elemental
        module procedure div_dr ! dual2/real,emental
        module procedure div_id ! integer/dual2, elemental
        module procedure div_rd ! real/dual2, elemental
    end interface

    public operator (**)
    interface operator (**)
        module procedure pow_i ! dual2 number to an integer power,elemental
        module procedure pow_r ! dual2 number to a real power, elemental
        module procedure pow_d ! dual2 number to a dual2 power, elemental
    end interface

    public operator (==)
    interface operator (==)
        module procedure eq_dd ! compare two dual2 numbers, elemental
        module procedure eq_di ! compare a dual2 and an integer, elemental
        module procedure eq_dr ! compare a dual2 and a real, elemental
        module procedure eq_id ! compare integer with a dual2 number, elemental
        module procedure eq_rd ! compare a real with a dual2 number, elemental
    end interface

    public operator (<=)
    interface operator (<=)
        module procedure le_dd  ! compare two dual2 numbers, elemental
        module procedure le_di  ! compare a dual2 and an integer, elemental
        module procedure le_dr  ! compare a dual2 and a real,elemental
        module procedure le_id ! compare integer with a dual2 number, elemental
        module procedure le_rd ! compare a real with a dual2 number, elemental
    end interface

    public operator (<)
    interface operator (<)
        module procedure lt_dd  !compare two dual2 numbers, elemental
        module procedure lt_di  !compare a dual2 and an integer, elemental
        module procedure lt_dr  !compare dual2 with a real, elemental
        module procedure lt_id ! compare integer with a dual2 number, elemental
        module procedure lt_rd ! compare a real with a dual2 number, elemental
    end interface

    public operator (>=)
    interface operator (>=)
        module procedure ge_dd ! compare two dual2 numbers, elemental
        module procedure ge_di ! compare dual2 with integer, elemental
        module procedure ge_dr ! compare dual2 with a real number, elemental
        module procedure ge_id ! compare integer with a dual2 number, elemental
        module procedure ge_rd ! compare a real with a dual2 number, elemental
    end interface

    public operator (>)
    interface operator (>)
        module procedure gt_dd  !compare two dual2 numbers, elemental
        module procedure gt_di  !compare a dual2 and an integer, elemental
        module procedure gt_dr  !compare dual2 with a real, elemental
        module procedure gt_id ! compare integer with a dual2 number, elemental
        module procedure gt_rd ! compare a real with a dual2 number, elemental
    end interface

    public operator (/=)
    interface operator (/=)
        module procedure ne_dd  !compare two dual2 numbers, elemental
        module procedure ne_di  !compare a dual2 and an integer, elemental
        module procedure ne_dr  !compare dual2 with a real, elemental
        module procedure ne_id ! compare integer with a dual2 number, elemental
        module procedure ne_rd ! compare a real with a dual2 number, elemental
    end interface


!------------------------------------------------
! Interfaces for intrinsic functions overloading
!------------------------------------------------
    public abs
    interface abs
        module procedure abs_d  ! absolute value of a dual2 number, elemental
    end interface
    
    public dabs
    interface dabs
        module procedure abs_d ! same as abs, used for some old fortran commands
    end interface
    
    public acos
    interface acos
        module procedure acos_d ! arccosine of a dual2 number, elemental
    end interface
    
    public asin
    interface asin
        module procedure asin_d ! arcsine of a dual2 number, elemental
    end interface
    
    public atan
    interface atan
        module procedure atan_d ! arctan of a dual2 number, elemental
    end interface
    
    public atan2
    interface atan2
        module procedure atan2_d ! arctan of a dual2 number, elemental
    end interface
    
    public cos
    interface cos
        module procedure cos_d ! cosine of a dual2 number, elemental
    end interface
    
    public dcos
    interface dcos
        module procedure cos_d ! cosine of a dual2 number, elemental
    end interface
    
    public dot_product
    interface dot_product
        module procedure dot_product_dd ! dot product two dual2 number vectors
    end interface
    
    public exp
    interface exp
        module procedure exp_d ! exponential of a dual2 number, elemental
    end interface
    
    public int
    interface int
        module procedure int_d ! integer part of a dual2 number, elemental
    end interface
    
    public log
    interface log
        module procedure log_d ! log of a dual2 number, elemental
    end interface
    
    public log10
    interface log10
        module procedure log10_d ! log of a dual2 number, elemental
    end interface
    
    public matmul
    interface matmul
        module procedure matmul_dd ! multiply two dual2 matrices
        module procedure matmul_dv ! multiply a dual2 matrix with a dual2 vector
        module procedure matmul_vd ! multiply a dual2 vector with a dual2 matrix
    end interface
    
    
    public max
    interface max
        module procedure max_dd ! max of from two to four dual2 numbers, elemental
        module procedure max_di ! max of a dual2 number and an integer, elemental
        module procedure max_dr ! max of a dual2 number and a real, elemental
        module procedure max_rd ! max of a real,and a dual2 number,  elemental
    end interface
    
    public dmax1
    interface dmax1
        module procedure max_dd ! max of from two to four dual2 numbers, elemental
    end interface
    
    public maxval
    interface maxval
        module procedure maxval_d ! maxval of a dual2 number vector
    end interface
    
    public min
    interface min
        module procedure min_dd ! min of from two to four dual2 numbers, elemental
        module procedure min_dr ! min of a dual2 and a real, elemental
    end interface
    
    public dmin1
    interface dmin1
        module procedure min_dd ! min of from two to four dual2 numbers, elemental
    end interface
    
    public minval
    interface minval
        module procedure minval_d ! obtain the maxval  of a dual2 number vectgor
    end interface
    
    public nint
    interface nint
        module procedure nint_d ! nearest integer to the argument, elemental
    end interface
    
    public sign
    interface  sign
      module procedure  sign_dd ! sign(a,b) with two dual2 numbers, elemental
      module procedure  sign_rd ! sign(a,b) with a real and a dual2, elemental
    end interface
    
    public sin
    interface sin
        module procedure sin_d ! obtain sine of a dual2 number, elemental
    end interface
    
    public dsin
    interface dsin
        module procedure sin_d ! obtain sine of a dual2 number, elemental
    end interface
    
    public tan
    interface tan
        module procedure tan_d ! obtain sine of a dual2 number, elemental
    end interface
    
    public dtan
    interface dtan
        module procedure tan_d ! obtain sine of a dual2 number, elemental
    end interface
    
    public sqrt
    interface sqrt
        module procedure sqrt_d ! obtain the sqrt of a dual2 number, elemental
    end interface
    
    public sum
    interface sum
        module procedure sum_d ! sum a dual2 array
    end interface
    
    public maxloc
    interface maxloc
        module procedure maxloc_d ! location of max in a dual2 array
    end interface

contains

!*********Begin: functions/subroutines for overloading operators

!******* Begin: (=)
!---------------------

    !-----------------------------------------
    ! dual2 = integer
    ! <u, du> = <i, 0>
    !-----------------------------------------
    elemental subroutine assign_di(u, i)
         type(dual2), intent(out) :: u
         integer, intent(in) :: i

         u%x = real(i, wp)  ! This is faster than direct assignment
         u%dx = 0.0_wp

    end subroutine assign_di


    !-----------------------------------------
    ! dual2 = real(double)
    ! <u, du> = <r, 0>
    !-----------------------------------------
    elemental subroutine assign_dr(u, r)
        type(dual2), intent(out) :: u
        real(wp), intent(in) :: r

        u%x = r
        u%dx = 0.0_wp

    end subroutine assign_dr


    !-----------------------------------------
    ! integer = dual2
    ! i = <u, du>
    !-----------------------------------------
    elemental subroutine assign_id(i, v)
         type(dual2), intent(in) :: v
         integer, intent(out) :: i

         i = int(v%x)

    end subroutine assign_id

!******* End: (=)
!---------------------


!******* Begin: (+)
!---------------------

    !-----------------------------------------
    ! Unary positive
    ! <res, dres> = +<u, du>
    !-----------------------------------------
    elemental function add_d(u) result(res)
         type(dual2), intent(in) :: u
         type(dual2) :: res

         res = u  ! Faster than assigning component wise

    end function add_d


    !-----------------------------------------
    ! dual2 + dual2
    ! <res, dres> = <u, du> + <v, dv> = <u + v, du + dv>
    !-----------------------------------------
    elemental function add_dd(u, v) result(res)
         type(dual2), intent(in) :: u, v
         type(dual2) :: res

         res%x = u%x + v%x
         res%dx = u%dx + v%dx

    end function add_dd


    !-----------------------------------------
    ! dual2 + integer
    ! <res, dres> = <u, du> + i = <u + i, du>
    !-----------------------------------------
    elemental function add_di(u, i) result(res)
         type(dual2), intent(in) :: u
         integer, intent(in) :: i
         type(dual2) :: res

         res%x = real(i, wp) + u%x
         res%dx = u%dx

    end function add_di


    !-----------------------------------------
    ! dual2 + double
    ! <res, dres> = <u, du> + <r, 0> = <u + r, du>
    !-----------------------------------------
    elemental function add_dr(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2) :: res

        res%x = r + u%x
        res%dx = u%dx

    end function add_dr


    !-----------------------------------------
    ! integer + dual2
    ! <res, dres> = <i, 0> + <v, dv> = <i + v, dv>
    !-----------------------------------------
    elemental function add_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: v
        type(dual2) :: res

        res%x = real(i, wp) + v%x
        res%dx = v%dx

    end function add_id


    !-----------------------------------------
    ! double + dual2
    ! <res, dres> = <r, 0> + <v, dv> = <r + v, dv>
    !-----------------------------------------
    elemental function add_rd(r, v) result(res)
        real(wp), intent(in) :: r
        type(dual2), intent(in) :: v
        type(dual2) :: res

        res%x = r + v%x
        res%dx = v%dx

    end function add_rd

!******* End: (+)
!---------------------


!******* Begin: (-)
!---------------------

    !-------------------------------------------------
    ! negate a dual2
    ! <res, dres> = -<u, du>
    !-------------------------------------------------
    elemental function minus_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = -u%x
        res%dx = -u%dx

    end function minus_d


    !-------------------------------------------------
    ! dual2 - dual2
    ! <res, dres> = <u, du> - <v, dv> = <u - v, du - dv>
    !-------------------------------------------------
    elemental function minus_dd(u, v) result(res)
        type(dual2), intent(in) :: u, v
        type(dual2) :: res

        res%x = u%x - v%x
        res%dx = u%dx - v%dx

    end function minus_dd

    !-------------------------------------------------
    ! dual2 - integer
    ! <res, dres> = <u, du> - i = <u - i, du>
    !-------------------------------------------------
    elemental function minus_di(u, i) result(res)
        type(dual2), intent(in) :: u
        integer, intent(in) :: i
        type(dual2) :: res

        res%x = u%x - real(i, wp)
        res%dx = u%dx

    end function minus_di

    
    !-------------------------------------------------
    ! dual2 - double
    ! <res, dres> = <u, du> - r = <u - r, du>
    !-------------------------------------------------
    elemental function minus_dr(u, r) result(res)
        type (dual2), intent(in) :: u
        real(wp),intent(in) :: r
        type(dual2) :: res

        res%x = u%x - r
        res%dx = u%dx

    end function minus_dr


    !-------------------------------------------------
    ! integer - dual2
    ! <res, dres> = i - <v, dv> = <i - v, -dv>
    !-------------------------------------------------
    elemental function minus_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: v
        type(dual2) :: res

        res%x = real(i, wp) - v%x
        res%dx = -v%dx

    end function minus_id


    !-------------------------------------------------
    ! double - dual2
    ! <res, dres> = r - <v, dv> = <r - v, -dv>
    !-------------------------------------------------
    elemental function minus_rd(r, v) result(res)
         real(wp), intent(in) :: r
         type(dual2), intent(in) :: v
         type(dual2) :: res

        res%x = r - v%x
        res%dx = -v%dx

    end function minus_rd

!******* END: (-)
!---------------------


!******* BEGIN: (*)
!---------------------

    !----------------------------------------
    ! dual2 * dual2
    ! <res, dres> = <u, du> * <v, dv> = <u * v, u * dv + v * du>
    !----------------------------------------
    elemental function mult_dd(u, v) result(res)
        type(dual2), intent(in) :: u, v
        type(dual2) :: res

        res%x = u%x * v%x
        res%dx = u%x * v%dx + v%x * u%dx

    end function mult_dd


    !-----------------------------------------
    ! dual2 * integer
    ! <res, dres> = <u, du> * i = <u * i, du * i>
    !-----------------------------------------
    elemental function mult_di(u, i) result(res)
        type(dual2), intent(in) :: u
        integer, intent(in) :: i
        type(dual2) :: res

        real(wp) :: r

        r = real(i, wp)
        res%x = r * u%x
        res%dx = r * u%dx

    end function mult_di

    !-----------------------------------------
    ! dual2 * double
    ! <res, dres> = <u, du> * r = <u * r, du * r>
    !----------------------------------------
    elemental function mult_dr(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2) :: res

        res%x = u%x * r
        res%dx = u%dx * r

    end function mult_dr


    !-----------------------------------------
    ! integer * dual2
    ! <res, dres> = i * <v, dv> = <i * v, i * dv>
    !-----------------------------------------
    elemental function mult_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: v
        type(dual2) :: res

        real(wp) :: r

        r = real(i, wp)
        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_id


    !-----------------------------------------
    ! double * dual2
    ! <res, dres> = r * <v, dv> = <r * v, r * dv>
    !-----------------------------------------
    elemental function mult_rd(r, v) result(res)
        real(wp), intent(in) :: r
        type(dual2), intent(in) :: v
        type(dual2) :: res

        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_rd

!******* END: (*)
!---------------------


!******* BEGIN: (/)
!---------------------

    !-----------------------------------------
    ! dual2 / dual2
    ! <res, dres> = <u, du> / <v, dv> = <u / v, du / v - u * dv / v^2>
    !-----------------------------------------
    elemental function div_dd(u, v) result(res)
        type(dual2), intent(in) :: u, v
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / v%x
        res%x = u%x * inv
        res%dx = (u%dx - res%x * v%dx) * inv

    end function div_dd


    !-----------------------------------------
    ! dual2 / integer
    ! <res, dres> = <u, du> / i = <u / i, du / i>
    !-----------------------------------------
    elemental function div_di(u, i) result(res)
        type(dual2), intent(in) :: u
        integer, intent(in) :: i
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / real(i, wp)
        res%x = u%x * inv
        res%dx = u%dx * inv

    end function div_di


    !-----------------------------------------
    ! dual2 / double
    ! <res, dres> = <u, du> / r = <u / r, du / r>
    !----------------------------------------
    elemental function div_dr(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2):: res

        real(wp) :: inv

        inv = 1.0_wp / r
        res%x = u%x * inv
        res%dx = u%dx * inv

    end function div_dr


    !-----------------------------------------
    ! integer / dual2
    ! <res, dres> = i / <v, dv> = <i / v, -i / v^2 * du>
    !-----------------------------------------
    elemental function div_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: v
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / v%x
        res%x = real(i, wp) * inv
        res%dx = -res%x * inv * v%dx

    end function div_id


    !-----------------------------------------
    ! double / dual2
    ! <res, dres> = r / <u, du> = <r / u, -r / u^2 * du>
    !-----------------------------------------
    elemental function div_rd(r, v) result(res)
        real(wp), intent(in) :: r
        type(dual2), intent(in) :: v
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / v%x
        res%x = r * inv
        res%dx = -res%x * inv * v%dx

    end function div_rd

!******* END: (/)
!---------------------

!******* BEGIN: (**)
!---------------------

    !-----------------------------------------
    ! power(dual2, integer)
    ! <res, dres> = <u, du> ^ i = <u ^ i, i * u ^ (i - 1) * du>
    !-----------------------------------------
    elemental function pow_i(u, i) result(res)
        type(dual2), intent(in) :: u
        integer, intent(in) :: i
        type(dual2) :: res

        real(wp) :: pow_x

        pow_x = u%x ** (i - 1)
        res%x = u%x * pow_x
        res%dx = real(i, wp) * pow_x * u%dx

    end function pow_i

    !-----------------------------------------
    ! power(dual2, double)
    ! <res, dres> = <u, du> ^ r = <u ^ r, r * u ^ (r - 1) * du>
    !-----------------------------------------
    elemental function pow_r(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2) :: res

        real(wp) :: pow_x

        pow_x = u%x ** (r - 1.0_wp)
        res%x = u%x * pow_x
        res%dx = r * pow_x * u%dx

    end function pow_r

    !-----------------------------------------
    ! POWER dual2 numbers to a dual2 power
    ! <res, dres> = <u, du> ^ <v, dv>
    !     = <u ^ v, u ^ v * (v / u * du + Log(u) * dv)>
    !-----------------------------------------
    elemental function pow_d(u, v) result(res)
        type(dual2), intent(in)::u, v
        type(dual2) :: res

        res%x = u%x ** v%x
        res%dx = res%x * (v%x / u%x * u%dx + log(u%x) * v%dx)

    end function pow_d

!******* END: (**)
!---------------------


!******* BEGIN: (==)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function eq_dd(lhs, rhs) result(res)
         type(dual2), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function eq_dd


    !-----------------------------------------
    ! compare a dual2 with an integer,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function eq_di(lhs, rhs) result(res)
         type(dual2), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == real(rhs, wp))

    end function eq_di


    !-----------------------------------------
    ! compare a dual2 number with a real number,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function eq_dr(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        real(wp), intent(in) :: rhs
        logical::res

        res = (lhs%x == rhs)

    end function eq_dr


    !-----------------------------------------
    ! compare an integer with a dual2,
    ! simply compare the functional value.
    !----------------------------------------
    elemental function eq_id(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_id


    !-----------------------------------------
    ! compare a real with a dual2,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function eq_rd(lhs, rhs) result(res)
         real(wp), intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_rd

!******* END: (==)
!---------------------


!******* BEGIN: (<=)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function le_dd(lhs, rhs) result(res)
         type(dual2), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function le_dd


    !-----------------------------------------
    ! compare a dual2 with an integer,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function le_di(lhs, rhs) result(res)
         type(dual2), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_di


    !-----------------------------------------
    ! compare a dual2 number with a real number,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function le_dr(lhs, rhs) result(res)
         type(dual2), intent(in) :: lhs
         real(wp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_dr


    !-----------------------------------------
    ! compare a dual2 number with an integer,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function le_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (i <= rhs%x)

    end function le_id


    !-----------------------------------------
    ! compare a real with a dual2,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function le_rd(lhs, rhs) result(res)
         real(wp), intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function le_rd

!******* END: (<=)
!---------------------

!******* BEGIN: (<)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function lt_dd(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x < rhs%x)

    end function lt_dd

    !-----------------------------------------
    ! compare a dual2 with an integer,
    ! simply compare the functional value.
    !-----------------------------------------
    elemental function lt_di(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_di


    !-----------------------------------------
    ! compare a dual2 number with a real number, simply compare
    ! the functional value.
    !----------------------------------------
    elemental function lt_dr(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        real(wp), intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_dr


    !-----------------------------------------
    ! compare a dual2 number with an integer
    !-----------------------------------------
    elemental function lt_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (i < rhs%x)

    end function lt_id


    !-----------------------------------------
    ! compare a real with a dual2
    !----------------------------------------
    elemental function lt_rd(lhs, rhs) result(res)
         real(wp), intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function lt_rd

!******* END: (<)
!---------------------

!******* BEGIN: (>=)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers, simply compare
    ! the functional value.
    !----------------------------------------
    elemental function ge_dd(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x >= rhs%x)

    end function ge_dd


    !-----------------------------------------
    ! compare a dual2 with an integer
    !-----------------------------------------
    elemental function ge_di(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_di


    !-----------------------------------------
    ! compare a dual2 number with a real number, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function ge_dr(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        real(wp), intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_dr


    !-----------------------------------------
    ! compare a dual2 number with an integer
    !-----------------------------------------
    elemental function ge_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: rhs
        logical :: res

        res = (i >= rhs%x)

    end function ge_id


    !-----------------------------------------
    ! compare a real with a dual2
    !-----------------------------------------
    elemental function ge_rd(lhs, rhs) result(res)
         real(wp), intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function ge_rd

!******* END: (>=)
!---------------------

!******* BEGIN: (>)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function gt_dd(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x > rhs%x)

    end function gt_dd


    !-----------------------------------------
    ! compare a dual2 with an integer
    !-----------------------------------------
    elemental function gt_di(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_di


    !-----------------------------------------
    ! compare a dual2 number with a real number, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function gt_dr(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        real(wp), intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_dr


    !-----------------------------------------
    ! compare a dual2 number with an integer
    !-----------------------------------------
    elemental function gt_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: rhs
        logical :: res

        res = (i > rhs%x)

    end function gt_id


    !-----------------------------------------
    ! compare a real with a dual2
    !-----------------------------------------
    elemental function gt_rd(lhs, rhs) result(res)
         real(wp), intent(in) :: lhs
         type(dual2), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function gt_rd

!******* END: (>)
!---------------------

!******* BEGIN: (/=)
!---------------------
    !-----------------------------------------
    ! compare two dual2 numbers, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function ne_dd(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x /= rhs%x)

    end function ne_dd


    !-----------------------------------------
    ! compare a dual2 with an integer
    !-----------------------------------------
    elemental function ne_di(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_di


    !-----------------------------------------
    ! compare a dual2 number with a real number, simply compare
    ! the functional value.
    !-----------------------------------------
    elemental function ne_dr(lhs, rhs) result(res)
        type(dual2), intent(in) :: lhs
        real(wp), intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_dr


    !-----------------------------------------
    ! compare a dual2 number with an integer
    !-----------------------------------------
    elemental function ne_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual2), intent(in) :: rhs
        logical :: res

        res = (i /= rhs%x)

    end function ne_id


    !-----------------------------------------
    ! compare a real with a dual2
    !-----------------------------------------
    elemental function ne_rd(lhs, rhs) result(res)
        real(wp), intent(in) :: lhs
        type(dual2), intent(in) :: rhs
        logical :: res

        res = (lhs /= rhs%x)

    end function ne_rd

!******* END: (/=)
!---------------------

    !---------------------------------------------------
    ! Absolute value of dual2 numbers
    ! <res, dres> = abs(<u, du>) = <abs(u), du * sign(u)>
    !---------------------------------------------------
    elemental function abs_d(u) result(res)
         type(dual2), intent(in) :: u
         type(dual2) :: res
         integer :: i

         if(u%x > 0) then
            res%x = u%x
            res%dx = u%dx
         else if (u%x < 0) then
            res%x = -u%x
            res%dx = -u%dx
         else
            res%x = 0.0_wp
            do i = 1, ndv
                if (u%dx(i) .eq. 0.0_wp) then
                    res%dx(i) = 0.0_wp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
         endif

    end function abs_d


    !-----------------------------------------
    ! ACOS of dual2 numbers
    ! <res, dres> = acos(<u, du>) = <acos(u), -du / sqrt(1 - u^2)>
    !----------------------------------------
    elemental function acos_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = acos(u%x)
        if (u%x == 1.0_wp .or. u%x == -1.0_wp) then
            res%dx = set_Nan()  ! Undefined derivative
        else
            res%dx = -u%dx / sqrt(1.0_wp - u%x**2)
        end if

    end function acos_d


    !-----------------------------------------
    ! ASIN of dual2 numbers
    ! <res, dres> = asin(<u, du>) = <asin(u), du / sqrt(1 - u^2)>
    !----------------------------------------
    elemental function asin_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = asin(u%x)
        if (u%x == 1.0_wp .or. u%x == -1.0_wp) then
            res%dx = set_NaN()  ! Undefined derivative
        else
            res%dx = u%dx / sqrt(1.0_wp - u%x**2)
        end if

    end function asin_d


    !-----------------------------------------
    ! ATAN of dual2 numbers
    ! <res, dres> = atan(<u, du>) = <atan(u), du / (1 + u^2)>
    !----------------------------------------
    elemental function atan_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_wp + u%x**2)

    end function atan_d


    !-----------------------------------------
    ! ATAN2 of dual2 numbers
    ! <res, dres> = atan2(<u, du>, <v, dv>)
    !             = <atan2(u, v), v / (u^2 + v^2) * du - u / (u^2 + v^2) * dv>
    !----------------------------------------
    elemental function atan2_d(u, v) result(res)
        type(dual2), intent(in) :: u, v
        type(dual2) :: res

        real(wp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function atan2_d


    !-----------------------------------------
    ! COS of dual2 numbers
    ! <res, dres> = cos(<u, du>) = <cos(u), -sin(u) * du>
    !----------------------------------------
    elemental function cos_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d


    !-----------------------------------------
    ! DOT PRODUCT two dual2 number vectors
    ! <res, dres> = <u, du> . <v, dv> = <u . v, u . dv + v . du>
    !-----------------------------------------
    pure function dot_product_dd(u, v) result(res)
        type(dual2), intent(in) :: u(:), v(:)
        type(dual2) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, ndv
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function dot_product_dd


    !-----------------------------------------
    ! EXPONENTIAL OF dual2 numbers
    ! <res, dres> = exp(<u, du>) = <exp(u), exp(u) * du>
    !-----------------------------------------
    elemental function exp_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        real(wp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function exp_d


    !-----------------------------------------
    ! Convert dual2 to integer
    ! i = int(<u, du>) = int(u)
    !----------------------------------------
    elemental function int_d(u) result(res)
         type(dual2), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function int_d


    !-----------------------------------------
    ! LOG OF dual2 numbers,defined for u%x>0 only
    ! the error control should be done in the original code
    ! in other words, if u%x<=0, it is not possible to obtain LOG.
    ! <res, dres> = log(<u, du>) = <log(u), du / u>
    !----------------------------------------
    elemental function log_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / u%x
        res%x = log(u%x)
        res%dx = u%dx * inv

    end function log_d


    !-----------------------------------------
    ! LOG10 OF dual2 numbers,defined for u%x>0 only
    ! the error control should be done in the original code
    ! in other words, if u%x<=0, it is not possible to obtain LOG.
    ! <res, dres> = log10(<u, du>) = <log10(u), du / (u * log(10))>
    ! LOG<u,up>=<LOG(u),up/u>
    !----------------------------------------
    elemental function log10_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        real(wp) :: inv

        inv = 1.0_wp / (u%x * log(10.0_wp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function log10_d


    !-----------------------------------------
    ! MULTIPLY two dual2 number matrices
    ! <res, dres> = <u, du> . <v, dv> = <u . v, du . v + u . dv>
    !----------------------------------------
    pure function matmul_dd(u,v) result(res)
        type(dual2), intent(in) :: u(:,:), v(:,:)
        type(dual2) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, ndv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dd


    !-----------------------------------------
    ! MULTIPLY a dual2 number matrix with a dual2 number
    ! vector
    !
    ! <u,up>.<v,vp>=<u.v,up.v+u.vp>
    !----------------------------------------
    pure function matmul_dv(u, v) result(res)
        type(dual2), intent(in) :: u(:,:), v(:)
        type(dual2) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, ndv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dv


    !-----------------------------------------
    ! MULTIPLY a dual2 vector with a  dual2 matrix
    !
    ! <u,up>.<v,vp>=<u.v,up.v+u.vp>
    !----------------------------------------
    pure function matmul_vd(u, v) result(res)
        type(dual2), intent(in) :: u(:), v(:,:)
        type(dual2) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, ndv
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_vd

    !-----------------------------------------
    ! Obtain the max of 2 to 5 dual2 numbers
    !----------------------------------------
    elemental function max_dd(val1, val2, val3, val4,val5) result(res)
        type(dual2), intent(in) :: val1, val2
        type(dual2), intent(in), optional :: val3, val4,val5
        type(dual2) :: res

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

    end function max_dd


    !-----------------------------------------
    ! Obtain the max of a dual2 number and an integer
    !----------------------------------------
    elemental function max_di(u, i) result(res)
        type(dual2), intent(in) :: u
        integer, intent(in) :: i
        type(dual2) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function max_di

    !-----------------------------------------
    ! Obtain the max of a dual2 number and a real number
    !----------------------------------------
    elemental function max_dr(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function max_dr


    !---------------------------------------------------
    ! Obtain the max of a real and a dual2
    !---------------------------------------------------
     elemental function max_rd(n, u) result(res)
        real(wp), intent(in) :: n
        type(dual2), intent(in) :: u
        type(dual2) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function max_rd


    !-----------------------------------------
    ! Obtain the max value of vector u
    !----------------------------------------
    pure function maxval_d(u) result(res)
        type(dual2), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual2) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function maxval_d


    !-----------------------------------------
    ! Obtain the min of 2 to 4 dual2 numbers
    !----------------------------------------
    elemental function min_dd(val1, val2, val3, val4) result(res)
        type(dual2), intent(in) :: val1, val2
        type(dual2), intent(in), optional :: val3, val4
        type(dual2) :: res

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

    end function min_dd


    !-----------------------------------------
    ! Obtain the min of a dual2 and a double
    !----------------------------------------
    elemental function min_dr(u, r) result(res)
        type(dual2), intent(in) :: u
        real(wp), intent(in) :: r
        type(dual2) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function min_dr


  !-----------------------------------------
    ! Obtain the min value of vector u
    !----------------------------------------
    pure function minval_d(u) result(res)
        type(dual2), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual2) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function minval_d

    
    !------------------------------------------------------
    !Returns the nearest integer to u%x, ELEMENTAL
    !------------------------------------------------------
    elemental function nint_d(u) result(res)
        type(dual2), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function nint_d


    !----------------------------------------------------------------
    ! SIGN(a,b) with two dual2 numbers as inputs,
    ! the result will be |a| if b%x>=0, -|a| if b%x<0,ELEMENTAL
    !----------------------------------------------------------------
    elemental function sign_dd(val1, val2) result(res)
        type(dual2), intent(in) :: val1, val2
        type(dual2) :: res

        if (val2%x < 0.0_wp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function sign_dd


    !----------------------------------------------------------------
    ! SIGN(a,b) with one real and one dual2 number as inputs,
    ! the result will be |a| if b%x>=0, -|a| if b%x<0,ELEMENTAL
    !----------------------------------------------------------------
    elemental function sign_rd(val1, val2) result(res)
        real(wp), intent(in) :: val1
        type(dual2), intent(in) :: val2
        type(dual2) :: res

        if (val2%x < 0.0_wp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function sign_rd


    !-----------------------------------------
    ! SIN of dual2 numbers
    ! <res, dres> = sin(<u, du>) = <sin(u), cos(u) * du>
    !----------------------------------------
    elemental function sin_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d


    !-----------------------------------------
    ! TAN of dual2 numbers
    ! <res, dres> = tan(<u, du>) = <tan(u), du / cos(u)^2>
    !----------------------------------------
    elemental function tan_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d


    !-----------------------------------------
    ! SQRT of dual2 numbers
    ! <res, dres> = sqrt(<u, du>) = <sqrt(u), du / (2 * sqrt(u))>
    !----------------------------------------
    elemental function sqrt_d(u) result(res)
        type(dual2), intent(in) :: u
        type(dual2) :: res
        integer :: i

        res%x = sqrt(u%x)

        if (res%x .ne. 0.0_wp) then
            res%dx = 0.5_wp * u%dx / res%x
        else
            do i = 1, ndv
                if (u%dx(i) .eq. 0.0_wp) then
                    res%dx(i) = 0.0_wp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
        end if

    end function sqrt_d


    !-----------------------------------------
    ! Sum of a dual2 array
    !-----------------------------------------
    pure function sum_d(u) result(res)
        type(dual2), intent(in) :: u(:)
        type(dual2) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, ndv
            res%dx(i) = sum(u%dx(i))
        end do

    end function sum_d


    !-----------------------------------------
    ! Find the location of the max value in an
    ! array of dual2 numbers
    !-----------------------------------------
    pure function maxloc_d(array) result(ind)
        type(dual2), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function maxloc_d


    elemental function set_NaN() result(res)
        real(wp) :: res

        res = sqrt(negative_one)

    end function set_NaN

end module dnad2_mod