module test_hdual2_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: num_deriv = 3

    ! Implementation call counters to show test coverage:
    integer :: assign__d_i_counter = 0
    integer :: assign__d_r_counter = 0
    integer :: assign__i_d_counter = 0
    integer :: assign__r_d_counter = 0
    integer :: assign__hd_i_counter = 0
    integer :: assign__hd_r_counter = 0
    integer :: assign__i_hd_counter = 0
    integer :: assign__r_hd_counter = 0
    integer :: add__d_counter = 0
    integer :: add__d_d_counter = 0
    integer :: add__d_i_counter = 0
    integer :: add__d_r_counter = 0
    integer :: add__i_d_counter = 0
    integer :: add__r_d_counter = 0
    integer :: add__hd_counter = 0
    integer :: add__hd_hd_counter = 0
    integer :: add__hd_i_counter = 0
    integer :: add__hd_r_counter = 0
    integer :: add__i_hd_counter = 0
    integer :: add__r_hd_counter = 0
    integer :: minus__d_counter = 0
    integer :: minus__d_d_counter = 0
    integer :: minus__d_i_counter = 0
    integer :: minus__d_r_counter = 0
    integer :: minus__i_d_counter = 0
    integer :: minus__r_d_counter = 0
    integer :: minus__hd_counter = 0
    integer :: minus__hd_hd_counter = 0
    integer :: minus__hd_i_counter = 0
    integer :: minus__hd_r_counter = 0
    integer :: minus__i_hd_counter = 0
    integer :: minus__r_hd_counter = 0
    integer :: mult__d_d_counter = 0
    integer :: mult__d_i_counter = 0
    integer :: mult__d_r_counter = 0
    integer :: mult__i_d_counter = 0
    integer :: mult__r_d_counter = 0
    integer :: mult__hd_hd_counter = 0
    integer :: mult__hd_i_counter = 0
    integer :: mult__hd_r_counter = 0
    integer :: mult__i_hd_counter = 0
    integer :: mult__r_hd_counter = 0
    integer :: div__d_d_counter = 0
    integer :: div__d_r_counter = 0
    integer :: div__r_d_counter = 0
    integer :: div__d_i_counter = 0
    integer :: div__i_d_counter = 0
    integer :: div__hd_hd_counter = 0
    integer :: div__hd_r_counter = 0
    integer :: div__r_hd_counter = 0
    integer :: div__hd_i_counter = 0
    integer :: div__i_hd_counter = 0
    integer :: pow__d_i_counter = 0
    integer :: pow__d_r_counter = 0
    integer :: pow__d_d_counter = 0
    integer :: pow__hd_i_counter = 0
    integer :: pow__hd_r_counter = 0
    integer :: pow__hd_hd_counter = 0
    integer :: eq__d_d_counter = 0
    integer :: eq__d_i_counter = 0
    integer :: eq__d_r_counter = 0
    integer :: eq__i_d_counter = 0
    integer :: eq__r_d_counter = 0
    integer :: le__d_d_counter = 0
    integer :: le__d_i_counter = 0
    integer :: le__d_r_counter = 0
    integer :: le__i_d_counter = 0
    integer :: le__r_d_counter = 0
    integer :: lt__d_d_counter = 0
    integer :: lt__d_i_counter = 0
    integer :: lt__d_r_counter = 0
    integer :: lt__i_d_counter = 0
    integer :: lt__r_d_counter = 0
    integer :: ge__d_d_counter = 0
    integer :: ge__d_i_counter = 0
    integer :: ge__d_r_counter = 0
    integer :: ge__i_d_counter = 0
    integer :: ge__r_d_counter = 0
    integer :: gt__d_d_counter = 0
    integer :: gt__d_i_counter = 0
    integer :: gt__d_r_counter = 0
    integer :: gt__i_d_counter = 0
    integer :: gt__r_d_counter = 0
    integer :: ne__d_d_counter = 0
    integer :: ne__d_i_counter = 0
    integer :: ne__d_r_counter = 0
    integer :: ne__i_d_counter = 0
    integer :: ne__r_d_counter = 0
    integer :: eq__hd_hd_counter = 0
    integer :: eq__hd_i_counter = 0
    integer :: eq__hd_r_counter = 0
    integer :: eq__i_hd_counter = 0
    integer :: eq__r_hd_counter = 0
    integer :: le__hd_hd_counter = 0
    integer :: le__hd_i_counter = 0
    integer :: le__hd_r_counter = 0
    integer :: le__i_hd_counter = 0
    integer :: le__r_hd_counter = 0
    integer :: lt__hd_hd_counter = 0
    integer :: lt__hd_i_counter = 0
    integer :: lt__hd_r_counter = 0
    integer :: lt__i_hd_counter = 0
    integer :: lt__r_hd_counter = 0
    integer :: ge__hd_hd_counter = 0
    integer :: ge__hd_i_counter = 0
    integer :: ge__hd_r_counter = 0
    integer :: ge__i_hd_counter = 0
    integer :: ge__r_hd_counter = 0
    integer :: gt__hd_hd_counter = 0
    integer :: gt__hd_i_counter = 0
    integer :: gt__hd_r_counter = 0
    integer :: gt__i_hd_counter = 0
    integer :: gt__r_hd_counter = 0
    integer :: ne__hd_hd_counter = 0
    integer :: ne__hd_i_counter = 0
    integer :: ne__hd_r_counter = 0
    integer :: ne__i_hd_counter = 0
    integer :: ne__r_hd_counter = 0
    integer :: abs__d_counter = 0
    integer :: acos__d_counter = 0
    integer :: asin__d_counter = 0
    integer :: atan__d_counter = 0
    integer :: atan2__d_counter = 0
    integer :: cos__d_counter = 0
    integer :: dot_product__dd_counter = 0
    integer :: exp__d_counter = 0
    integer :: int__d_counter = 0
    integer :: log__d_counter = 0
    integer :: log__hd_counter = 0
    integer :: log10__d_counter = 0
    integer :: matmul__dd_counter = 0
    integer :: matmul__dv_counter = 0
    integer :: matmul__vd_counter = 0
    integer :: max__dd_counter = 0
    integer :: max__di_counter = 0
    integer :: max__dr_counter = 0
    integer :: max__rd_counter = 0
    integer :: dmax1__dd_counter = 0
    integer :: maxval__d_counter = 0
    integer :: min__dd_counter = 0
    integer :: min__dr_counter = 0
    integer :: dmin1__dd_counter = 0
    integer :: minval__d_counter = 0
    integer :: nint__d_counter = 0
    integer :: sin__d_counter = 0
    integer :: tan__d_counter = 0
    integer :: sqrt__d_counter = 0
    integer :: sqrt__hd_counter = 0
    integer :: sum__d_counter = 0
    integer :: maxloc__d_counter = 0

    
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
        !! Extract function value from a dual or hyper-dual number
        module procedure fvalue__d_uvw
        module procedure fvalue__d_uvw_r1
        module procedure fvalue__d_uvw_r2
        module procedure fvalue__hd_uvw
        module procedure fvalue__hd_uvw_r1
        module procedure fvalue__hd_uvw_r2
    end interface

    interface gradient
        !! Extract gradient from a dual or hyper-dual number
        module procedure gradient__d_uvw
        module procedure gradient__hd_uvw
    end interface

    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian__hd_uvw
    end interface
    interface assignment (=)
        module procedure assign__duvw_i  ! dual=integer, elemental
        module procedure assign__duvw_r  ! dual=real, elemental
        module procedure assign__i_duvw  ! integer=dual, elemental
        module procedure assign__r_duvw  ! real=dual, elemental
        module procedure assign__hduvw_i  ! dual=integer, elemental
        module procedure assign__hduvw_r  ! dual=real, elemental
        module procedure assign__i_hduvw  ! integer=dual, elemental
        module procedure assign__r_hduvw  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure add__duvw   ! +dual number, elemental
        module procedure add__duvw_duvw       ! dual + dual, elemental
        module procedure add__duvw_i       ! dual + integer, elemental
        module procedure add__duvw_r       ! dual + real, elemental
        module procedure add__i_duvw       ! integer + dual, elemental
        module procedure add__r_duvw       ! real + dual, elemental
        module procedure add__hduvw  ! +dual number, elemental
        module procedure add__hduvw_hduvw     ! dual + dual, elemental
        module procedure add__hduvw_i      ! dual + integer, elemental
        module procedure add__hduvw_r      ! dual + real, elemental
        module procedure add__i_hduvw      ! integer + dual, elemental
        module procedure add__r_hduvw      ! real + dual, elemental
    end interface
    interface operator (-)
        module procedure minus__duvw  ! negate a dual number,elemental
        module procedure minus__duvw_duvw      ! dual -dual,elemental
        module procedure minus__duvw_i      ! dual-integer,elemental
        module procedure minus__duvw_r      ! dual-real,elemental
        module procedure minus__i_duvw      ! integer-dual,elemental
        module procedure minus__r_duvw      ! real-dual,elemental
        module procedure minus__hduvw ! negate a dual number,elemental
        module procedure minus__hduvw_hduvw    ! dual -dual,elemental
        module procedure minus__hduvw_i     ! dual-integer,elemental
        module procedure minus__hduvw_r     ! dual-real,elemental
        module procedure minus__i_hduvw     ! integer-dual,elemental
        module procedure minus__r_hduvw     ! real-dual,elemental
    end interface
    interface operator (*)
        module procedure mult__duvw_duvw    ! dual*dual, elemental
        module procedure mult__duvw_i    ! dual*integer,elemental
        module procedure mult__duvw_r    ! dual*real,elemental
        module procedure mult__i_duvw    ! integer*dual,elemental
        module procedure mult__r_duvw    ! real*dual,elemental
        module procedure mult__hduvw_hduvw  ! dual*dual, elemental
        module procedure mult__hduvw_i   ! dual*integer,elemental
        module procedure mult__hduvw_r   ! dual*real,elemental
        module procedure mult__i_hduvw   ! integer*dual,elemental
        module procedure mult__r_hduvw   ! real*dual,elemental
    end interface
    interface operator (/)
        module procedure div__duvw_duvw    ! dual/dual, elemental
        module procedure div__duvw_r    ! dual/real, elemental
        module procedure div__r_duvw    ! real/dual, elemental
        module procedure div__duvw_i    ! dual/integer, elemental
        module procedure div__i_duvw    ! integer/dual, elemental
        module procedure div__hduvw_hduvw  ! hdual/hdual, elemental
        module procedure div__hduvw_r   ! hdual/real, elemental
        module procedure div__r_hduvw   ! real/hdual, elemental
        module procedure div__hduvw_i   ! hdual/integer, elemental
        module procedure div__i_hduvw   ! integer/hdual, elemental
    end interface
    interface operator (**)
        module procedure pow__duvw_i ! dual number to an integer power,elemental
        module procedure pow__duvw_r ! dual number to a real power, elemental
        module procedure pow__duvw_duvw ! dual number to a dual power, elemental
        module procedure pow__hduvw_i ! hdual number to an integer power,elemental
        module procedure pow__hduvw_r ! hdual number to a real power, elemental
        module procedure pow__hduvw_hduvw ! hdual number to a hdual power, elemental
    end interface
    interface operator (==)
        module procedure eq__duvw_duvw ! compare two dual numbers, elemental
        module procedure eq__duvw_i ! compare a dual and an integer, elemental
        module procedure eq__duvw_r ! compare a dual and a real, elemental
        module procedure eq__i_duvw ! compare integer with a dual number, elemental
        module procedure eq__r_duvw ! compare a real with a dual number, elemental
        module procedure eq__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure eq__hduvw_i ! compare a hdual and an integer, elemental
        module procedure eq__hduvw_r ! compare a hdual and a real, elemental
        module procedure eq__i_hduvw ! compare integer with a hdual number, elemental
        module procedure eq__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface operator (<=)
        module procedure le__duvw_duvw ! compare two dual numbers, elemental
        module procedure le__duvw_i ! compare a dual and an integer, elemental
        module procedure le__duvw_r ! compare a dual and a real, elemental
        module procedure le__i_duvw ! compare integer with a dual number, elemental
        module procedure le__r_duvw ! compare a real with a dual number, elemental
        module procedure le__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure le__hduvw_i ! compare a hdual and an integer, elemental
        module procedure le__hduvw_r ! compare a hdual and a real, elemental
        module procedure le__i_hduvw ! compare integer with a hdual number, elemental
        module procedure le__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface operator (<)
        module procedure lt__duvw_duvw ! compare two dual numbers, elemental
        module procedure lt__duvw_i ! compare a dual and an integer, elemental
        module procedure lt__duvw_r ! compare a dual and a real, elemental
        module procedure lt__i_duvw ! compare integer with a dual number, elemental
        module procedure lt__r_duvw ! compare a real with a dual number, elemental
        module procedure lt__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure lt__hduvw_i ! compare a hdual and an integer, elemental
        module procedure lt__hduvw_r ! compare a hdual and a real, elemental
        module procedure lt__i_hduvw ! compare integer with a hdual number, elemental
        module procedure lt__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface operator (>=)
        module procedure ge__duvw_duvw ! compare two dual numbers, elemental
        module procedure ge__duvw_i ! compare a dual and an integer, elemental
        module procedure ge__duvw_r ! compare a dual and a real, elemental
        module procedure ge__i_duvw ! compare integer with a dual number, elemental
        module procedure ge__r_duvw ! compare a real with a dual number, elemental
        module procedure ge__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure ge__hduvw_i ! compare a hdual and an integer, elemental
        module procedure ge__hduvw_r ! compare a hdual and a real, elemental
        module procedure ge__i_hduvw ! compare integer with a hdual number, elemental
        module procedure ge__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface operator (>)
        module procedure gt__duvw_duvw ! compare two dual numbers, elemental
        module procedure gt__duvw_i ! compare a dual and an integer, elemental
        module procedure gt__duvw_r ! compare a dual and a real, elemental
        module procedure gt__i_duvw ! compare integer with a dual number, elemental
        module procedure gt__r_duvw ! compare a real with a dual number, elemental
        module procedure gt__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure gt__hduvw_i ! compare a hdual and an integer, elemental
        module procedure gt__hduvw_r ! compare a hdual and a real, elemental
        module procedure gt__i_hduvw ! compare integer with a hdual number, elemental
        module procedure gt__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface operator (/=)
        module procedure ne__duvw_duvw ! compare two dual numbers, elemental
        module procedure ne__duvw_i ! compare a dual and an integer, elemental
        module procedure ne__duvw_r ! compare a dual and a real, elemental
        module procedure ne__i_duvw ! compare integer with a dual number, elemental
        module procedure ne__r_duvw ! compare a real with a dual number, elemental
        module procedure ne__hduvw_hduvw ! compare two hdual numbers, elemental
        module procedure ne__hduvw_i ! compare a hdual and an integer, elemental
        module procedure ne__hduvw_r ! compare a hdual and a real, elemental
        module procedure ne__i_hduvw ! compare integer with a hdual number, elemental
        module procedure ne__r_hduvw ! compare a real with a hdual number, elemental
    end interface
    interface abs
        module procedure abs__duvw  ! absolute value of a dual number, elemental
    end interface
    interface dabs
        module procedure abs__duvw ! same as abs, used for some old fortran commands
    end interface
    interface acos
        module procedure acos__duvw ! arccosine of a dual number, elemental
    end interface
    interface asin
        module procedure asin__duvw ! arcsine of a dual number, elemental
    end interface
    interface atan
        module procedure atan__duvw ! arctan of a dual number, elemental
    end interface
    interface atan2
        module procedure atan2__duvw ! arctan of a dual number, elemental
    end interface
    interface cos
        module procedure cos__duvw ! cosine of a dual number, elemental
    end interface
    interface dcos
        module procedure cos__duvw ! cosine of a dual number, elemental
    end interface
    interface dot_product
        module procedure dot_product__duvw_duvw ! dot product two dual number vectors
    end interface
    interface exp
        module procedure exp__duvw ! exponential of a dual number, elemental
    end interface
    interface int
        module procedure int__duvw ! integer part of a dual number, elemental
    end interface
    interface log
        module procedure log__duvw ! log of a dual number, elemental
        module procedure log__hduvw ! log of a dual number, elemental
    end interface
    interface log10
        module procedure log10__duvw ! log of a dual number, elemental
    end interface
    interface matmul
        module procedure matmul__duvw_duvw ! multiply two dual matrices
        module procedure matmul__duvw_v ! multiply a dual matrix with a dual vector
        module procedure matmul__v_duvw ! multiply a dual vector with a dual matrix
    end interface
    interface max
        module procedure max__duvw_duvw ! max of two dual numbers, elemental
        module procedure max__duvw_i ! max of a dual number and an integer, elemental
        module procedure max__duvw_r ! max of a dual number and a real, elemental
        module procedure max__r_duvw ! max of a real,and a dual number,  elemental
    end interface
    interface dmax1
        module procedure dmax1__duvw_duvw ! max of from two to four dual numbers, elemental
    end interface
    interface maxval
        module procedure maxval__duvw ! maxval of a dual number vector
    end interface
    interface min
        module procedure min__duvw_duvw ! min of from two to four dual numbers, elemental
        module procedure min__duvw_r ! min of a dual and a real, elemental
    end interface
    interface dmin1
        module procedure dmin1__duvw_duvw ! min of from two to four dual numbers, elemental
    end interface
    interface minval
        module procedure minval__duvw ! obtain the maxval  of a dual number vectgor
    end interface
    interface nint
        module procedure nint__duvw ! nearest integer to the argument, elemental
    end interface
    interface  sign
        module procedure  sign__duvw_duvw ! sign(a,b) with two dual numbers, elemental
        module procedure  sign__r_duvw ! sign(a,b) with a real and a dual, elemental
    end interface
    interface sin
        module procedure sin__duvw ! obtain sine of a dual number, elemental
    end interface
    interface dsin
        module procedure sin__duvw ! obtain sine of a dual number, elemental
    end interface
    interface tan
        module procedure tan__duvw ! obtain sine of a dual number, elemental
    end interface
    interface dtan
        module procedure tan__duvw ! obtain sine of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt__duvw ! obtain the sqrt of a dual number, elemental
        module procedure sqrt__hduvw ! obtain the sqrt of a dual number, elemental
    end interface
    interface sum
        module procedure sum__duvw ! sum a dual array
    end interface
    interface maxloc
        module procedure maxloc__duvw ! location of max in a dual array
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

        print*, "assign__d_i_counter: ", assign__d_i_counter
        print*, "assign__d_r_counter: ", assign__d_r_counter
        print*, "assign__i_d_counter: ", assign__i_d_counter
        print*, "assign__r_d_counter: ", assign__r_d_counter
        print*, "assign__hd_i_counter: ", assign__hd_i_counter
        print*, "assign__hd_r_counter: ", assign__hd_r_counter
        print*, "assign__i_hd_counter: ", assign__i_hd_counter
        print*, "assign__r_hd_counter: ", assign__r_hd_counter
        print*, "add__d_counter: ", add__d_counter
        print*, "add__d_d_counter: ", add__d_d_counter
        print*, "add__d_i_counter: ", add__d_i_counter
        print*, "add__d_r_counter: ", add__d_r_counter
        print*, "add__i_d_counter: ", add__i_d_counter
        print*, "add__r_d_counter: ", add__r_d_counter
        print*, "add__hd_counter: ", add__hd_counter
        print*, "add__hd_hd_counter: ", add__hd_hd_counter
        print*, "add__hd_i_counter: ", add__hd_i_counter
        print*, "add__hd_r_counter: ", add__hd_r_counter
        print*, "add__i_hd_counter: ", add__i_hd_counter
        print*, "add__r_hd_counter: ", add__r_hd_counter
        print*, "minus__d_counter: ", minus__d_counter
        print*, "minus__d_d_counter: ", minus__d_d_counter
        print*, "minus__d_i_counter: ", minus__d_i_counter
        print*, "minus__d_r_counter: ", minus__d_r_counter
        print*, "minus__i_d_counter: ", minus__i_d_counter
        print*, "minus__r_d_counter: ", minus__r_d_counter
        print*, "minus__hd_counter: ", minus__hd_counter
        print*, "minus__hd_hd_counter: ", minus__hd_hd_counter
        print*, "minus__hd_i_counter: ", minus__hd_i_counter
        print*, "minus__hd_r_counter: ", minus__hd_r_counter
        print*, "minus__i_hd_counter: ", minus__i_hd_counter
        print*, "minus__r_hd_counter: ", minus__r_hd_counter
        print*, "mult__d_d_counter: ", mult__d_d_counter
        print*, "mult__d_i_counter: ", mult__d_i_counter
        print*, "mult__d_r_counter: ", mult__d_r_counter
        print*, "mult__i_d_counter: ", mult__i_d_counter
        print*, "mult__r_d_counter: ", mult__r_d_counter
        print*, "mult__hd_hd_counter: ", mult__hd_hd_counter
        print*, "mult__hd_i_counter: ", mult__hd_i_counter
        print*, "mult__hd_r_counter: ", mult__hd_r_counter
        print*, "mult__i_hd_counter: ", mult__i_hd_counter
        print*, "mult__r_hd_counter: ", mult__r_hd_counter
        print*, "div__d_d_counter: ", div__d_d_counter
        print*, "div__d_r_counter: ", div__d_r_counter
        print*, "div__r_d_counter: ", div__r_d_counter
        print*, "div__d_i_counter: ", div__d_i_counter
        print*, "div__i_d_counter: ", div__i_d_counter
        print*, "div__hd_hd_counter: ", div__hd_hd_counter
        print*, "div__hd_r_counter: ", div__hd_r_counter
        print*, "div__r_hd_counter: ", div__r_hd_counter
        print*, "div__hd_i_counter: ", div__hd_i_counter
        print*, "div__i_hd_counter: ", div__i_hd_counter
        print*, "pow__d_i_counter: ", pow__d_i_counter
        print*, "pow__d_r_counter: ", pow__d_r_counter
        print*, "pow__d_d_counter: ", pow__d_d_counter
        print*, "pow__hd_i_counter: ", pow__hd_i_counter
        print*, "pow__hd_r_counter: ", pow__hd_r_counter
        print*, "pow__hd_hd_counter: ", pow__hd_hd_counter
        print*, "eq__d_d_counter: ", eq__d_d_counter
        print*, "eq__d_i_counter: ", eq__d_i_counter
        print*, "eq__d_r_counter: ", eq__d_r_counter
        print*, "eq__i_d_counter: ", eq__i_d_counter
        print*, "eq__r_d_counter: ", eq__r_d_counter
        print*, "le__d_d_counter: ", le__d_d_counter
        print*, "le__d_i_counter: ", le__d_i_counter
        print*, "le__d_r_counter: ", le__d_r_counter
        print*, "le__i_d_counter: ", le__i_d_counter
        print*, "le__r_d_counter: ", le__r_d_counter
        print*, "lt__d_d_counter: ", lt__d_d_counter
        print*, "lt__d_i_counter: ", lt__d_i_counter
        print*, "lt__d_r_counter: ", lt__d_r_counter
        print*, "lt__i_d_counter: ", lt__i_d_counter
        print*, "lt__r_d_counter: ", lt__r_d_counter
        print*, "ge__d_d_counter: ", ge__d_d_counter
        print*, "ge__d_i_counter: ", ge__d_i_counter
        print*, "ge__d_r_counter: ", ge__d_r_counter
        print*, "ge__i_d_counter: ", ge__i_d_counter
        print*, "ge__r_d_counter: ", ge__r_d_counter
        print*, "gt__d_d_counter: ", gt__d_d_counter
        print*, "gt__d_i_counter: ", gt__d_i_counter
        print*, "gt__d_r_counter: ", gt__d_r_counter
        print*, "gt__i_d_counter: ", gt__i_d_counter
        print*, "gt__r_d_counter: ", gt__r_d_counter
        print*, "ne__d_d_counter: ", ne__d_d_counter
        print*, "ne__d_i_counter: ", ne__d_i_counter
        print*, "ne__d_r_counter: ", ne__d_r_counter
        print*, "ne__i_d_counter: ", ne__i_d_counter
        print*, "ne__r_d_counter: ", ne__r_d_counter
        print*, "eq__hd_hd_counter: ", eq__hd_hd_counter
        print*, "eq__hd_i_counter: ", eq__hd_i_counter
        print*, "eq__hd_r_counter: ", eq__hd_r_counter
        print*, "eq__i_hd_counter: ", eq__i_hd_counter
        print*, "eq__r_hd_counter: ", eq__r_hd_counter
        print*, "le__hd_hd_counter: ", le__hd_hd_counter
        print*, "le__hd_i_counter: ", le__hd_i_counter
        print*, "le__hd_r_counter: ", le__hd_r_counter
        print*, "le__i_hd_counter: ", le__i_hd_counter
        print*, "le__r_hd_counter: ", le__r_hd_counter
        print*, "lt__hd_hd_counter: ", lt__hd_hd_counter
        print*, "lt__hd_i_counter: ", lt__hd_i_counter
        print*, "lt__hd_r_counter: ", lt__hd_r_counter
        print*, "lt__i_hd_counter: ", lt__i_hd_counter
        print*, "lt__r_hd_counter: ", lt__r_hd_counter
        print*, "ge__hd_hd_counter: ", ge__hd_hd_counter
        print*, "ge__hd_i_counter: ", ge__hd_i_counter
        print*, "ge__hd_r_counter: ", ge__hd_r_counter
        print*, "ge__i_hd_counter: ", ge__i_hd_counter
        print*, "ge__r_hd_counter: ", ge__r_hd_counter
        print*, "gt__hd_hd_counter: ", gt__hd_hd_counter
        print*, "gt__hd_i_counter: ", gt__hd_i_counter
        print*, "gt__hd_r_counter: ", gt__hd_r_counter
        print*, "gt__i_hd_counter: ", gt__i_hd_counter
        print*, "gt__r_hd_counter: ", gt__r_hd_counter
        print*, "ne__hd_hd_counter: ", ne__hd_hd_counter
        print*, "ne__hd_i_counter: ", ne__hd_i_counter
        print*, "ne__hd_r_counter: ", ne__hd_r_counter
        print*, "ne__i_hd_counter: ", ne__i_hd_counter
        print*, "ne__r_hd_counter: ", ne__r_hd_counter
        print*, "abs__d_counter: ", abs__d_counter
        print*, "acos__d_counter: ", acos__d_counter
        print*, "asin__d_counter: ", asin__d_counter
        print*, "atan__d_counter: ", atan__d_counter
        print*, "atan2__d_counter: ", atan2__d_counter
        print*, "cos__d_counter: ", cos__d_counter
        print*, "dot_product__dd_counter: ", dot_product__dd_counter
        print*, "exp__d_counter: ", exp__d_counter
        print*, "int__d_counter: ", int__d_counter
        print*, "log__d_counter: ", log__d_counter
        print*, "log__hd_counter: ", log__hd_counter
        print*, "log10__d_counter: ", log10__d_counter
        print*, "matmul__dd_counter: ", matmul__dd_counter
        print*, "matmul__dv_counter: ", matmul__dv_counter
        print*, "matmul__vd_counter: ", matmul__vd_counter
        print*, "max__dd_counter: ", max__dd_counter
        print*, "max__di_counter: ", max__di_counter
        print*, "max__dr_counter: ", max__dr_counter
        print*, "max__rd_counter: ", max__rd_counter
        print*, "dmax1__dd_counter: ", dmax1__dd_counter
        print*, "maxval__d_counter: ", maxval__d_counter
        print*, "min__dd_counter: ", min__dd_counter
        print*, "min__dr_counter: ", min__dr_counter
        print*, "dmin1__dd_counter: ", dmin1__dd_counter
        print*, "minval__d_counter: ", minval__d_counter
        print*, "nint__d_counter: ", nint__d_counter
        print*, "sin__d_counter: ", sin__d_counter
        print*, "tan__d_counter: ", tan__d_counter
        print*, "sqrt__d_counter: ", sqrt__d_counter
        print*, "sqrt__hd_counter: ", sqrt__hd_counter
        print*, "sum__d_counter: ", sum__d_counter
        print*, "maxloc__d_counter: ", maxloc__d_counter

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
        type(dual__uvw_t), intent(in) :: u, v, w
        real(dp), intent(in) :: r1, r2, r3
        integer, intent(in) :: i1, i2, i3
        type(dual__uvw_t) :: f
        f = i3/(2*v + 1) + r3/(2*u + 1) - u/(2*w + 1) + u/(2*i2 + 1) + u**i1 + u &
            **r1 - u**v + sqrt(v)*log(u) + w/(2*r2 + 1) + (i1*v - i2*u + r1*u &
            - r2*w)**2
    end function
    impure elemental function test_function_hdual(u, v, w, r1, r2, r3, i1, i2, i3) result(f)
        !! Test function for dual numbers. It is "impure" only because we use global counter variables to
        !! check test coverage. In "normal" use, it would be pure.
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
    impure elemental subroutine assign__duvw_i(u, i)
        type(dual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp
        assign__d_i_counter = assign__d_i_counter + 1

    end subroutine
    impure elemental subroutine assign__duvw_r(u, r)
        type(dual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp
        assign__d_r_counter = assign__d_r_counter + 1

    end subroutine
    impure elemental subroutine assign__i_duvw(i, v)
        type(dual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)
        assign__i_d_counter = assign__i_d_counter + 1

    end subroutine
    impure elemental subroutine assign__r_duvw(r, v)
        type(dual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f
        assign__r_d_counter = assign__r_d_counter + 1

    end subroutine
    impure elemental subroutine assign__hduvw_i(u, i)
        type(hdual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp
        assign__hd_i_counter = assign__hd_i_counter + 1

    end subroutine
    impure elemental subroutine assign__hduvw_r(u, r)
        type(hdual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp
        assign__hd_r_counter = assign__hd_r_counter + 1

    end subroutine
    impure elemental subroutine assign__i_hduvw(i, v)
        type(hdual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)
        assign__i_hd_counter = assign__i_hd_counter + 1

    end subroutine
    impure elemental subroutine assign__r_hduvw(r, v)
        type(hdual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f
        assign__r_hd_counter = assign__r_hd_counter + 1

    end subroutine
    impure elemental function add__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = u%f
        res%g = u%g
        add__d_counter = add__d_counter + 1
    end function
    impure elemental function add__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
        add__d_d_counter = add__d_d_counter + 1
    end function
    impure elemental function add__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        add__d_r_counter = add__d_r_counter + 1
    end function
    impure elemental function add__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        add__r_d_counter = add__r_d_counter + 1
    end function
    impure elemental function add__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        add__d_i_counter = add__d_i_counter + 1
    end function
    impure elemental function add__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        add__i_d_counter = add__i_d_counter + 1
    end function
    impure elemental function add__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
        add__hd_counter = add__hd_counter + 1
    end function
    impure elemental function add__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
        add__hd_hd_counter = add__hd_hd_counter + 1
    end function
    impure elemental function add__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
        add__hd_r_counter = add__hd_r_counter + 1
    end function
    impure elemental function add__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
        add__r_hd_counter = add__r_hd_counter + 1
    end function
    impure elemental function add__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
        add__hd_i_counter = add__hd_i_counter + 1
    end function
    impure elemental function add__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
        add__i_hd_counter = add__i_hd_counter + 1
    end function
    impure elemental function minus__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = -u%f
        res%g = -u%g
        minus__d_counter = minus__d_counter + 1
    end function
    impure elemental function minus__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v%f
        res%g = u%g - v%g
        minus__d_d_counter = minus__d_d_counter + 1
    end function
    impure elemental function minus__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        minus__d_r_counter = minus__d_r_counter + 1
    end function
    impure elemental function minus__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        minus__r_d_counter = minus__r_d_counter + 1
    end function
    impure elemental function minus__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f - v
        res%g = u%g
        minus__d_i_counter = minus__d_i_counter + 1
    end function
    impure elemental function minus__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u - v%f
        res%g = -v%g
        minus__i_d_counter = minus__i_d_counter + 1
    end function
    impure elemental function minus__hduvw(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        
        res%d%f = -u%d%f
        res%d%g = -u%d%g
        res%h = -u%h
        minus__hd_counter = minus__hd_counter + 1
    end function
    impure elemental function minus__hduvw_hduvw(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v%d%f
        res%d%g = u%d%g - v%d%g
        res%h = u%h - v%h
        minus__hd_hd_counter = minus__hd_hd_counter + 1
    end function
    impure elemental function minus__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
        minus__hd_r_counter = minus__hd_r_counter + 1
    end function
    impure elemental function minus__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
        minus__r_hd_counter = minus__r_hd_counter + 1
    end function
    impure elemental function minus__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f - v
        res%d%g = u%d%g
        res%h = u%h
        minus__hd_i_counter = minus__hd_i_counter + 1
    end function
    impure elemental function minus__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u - v%d%f
        res%d%g = -v%d%g
        res%h = -v%h
        minus__i_hd_counter = minus__i_hd_counter + 1
    end function
    impure elemental function mult__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
        mult__d_d_counter = mult__d_d_counter + 1
    end function
    impure elemental function mult__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        mult__d_r_counter = mult__d_r_counter + 1
    end function
    impure elemental function mult__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        mult__r_d_counter = mult__r_d_counter + 1
    end function
    impure elemental function mult__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        mult__d_i_counter = mult__d_i_counter + 1
    end function
    impure elemental function mult__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        mult__i_d_counter = mult__i_d_counter + 1
    end function
    impure elemental function mult__hduvw_hduvw(u, v) result(res)
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
        mult__hd_hd_counter = mult__hd_hd_counter + 1
    end function
    impure elemental function mult__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
        mult__hd_r_counter = mult__hd_r_counter + 1
    end function
    impure elemental function mult__r_hduvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
        mult__r_hd_counter = mult__r_hd_counter + 1
    end function
    impure elemental function mult__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
        mult__hd_i_counter = mult__hd_i_counter + 1
    end function
    impure elemental function mult__i_hduvw(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
        mult__i_hd_counter = mult__i_hd_counter + 1
    end function
    impure elemental function div__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0,t1
        
        t0 = 1.0_dp/v%f
        t1 = t0*u%f
        res%f = t1
        res%g = t0*(-t1*v%g + u%g)
        div__d_d_counter = div__d_d_counter + 1
    end function
    impure elemental function div__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        div__d_r_counter = div__d_r_counter + 1
    end function
    impure elemental function div__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
        div__r_d_counter = div__r_d_counter + 1
    end function
    impure elemental function div__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%f = t0*u%f
        res%g = t0*u%g
        div__d_i_counter = div__d_i_counter + 1
    end function
    impure elemental function div__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u/v%f
        res%g = -u*v%g/v%f**2
        div__i_d_counter = div__i_d_counter + 1
    end function
    impure elemental function div__hduvw_hduvw(u, v) result(res)
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
        div__hd_hd_counter = div__hd_hd_counter + 1
    end function
    impure elemental function div__hduvw_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
        div__hd_r_counter = div__hd_r_counter + 1
    end function
    impure elemental function div__r_hduvw(u, v) result(res)
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
        div__r_hd_counter = div__r_hd_counter + 1
    end function
    impure elemental function div__hduvw_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = 1.0_dp/v
        res%d%f = t0*u%d%f
        res%d%g = t0*u%d%g
        res%h = t0*u%h
        div__hd_i_counter = div__hd_i_counter + 1
    end function
    impure elemental function div__i_hduvw(u, v) result(res)
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
        div__i_hd_counter = div__i_hd_counter + 1
    end function
    impure elemental function pow__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
        pow__d_i_counter = pow__d_i_counter + 1
    end function
    impure elemental function pow__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f**v
        res%g = u%f**(v - 1)*u%g*v
        pow__d_r_counter = pow__d_r_counter + 1
    end function
    impure elemental function pow__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = u%f**v%f
        res%f = t0
        res%g = t0*(u%g*v%f/u%f + v%g*log(u%f))

        pow__d_d_counter = pow__d_d_counter + 1
    end function
    impure elemental function pow__hduvw_i(u, v) result(res)
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
        pow__hd_i_counter = pow__hd_i_counter + 1
    end function
    impure elemental function pow__hduvw_r(u, v) result(res)
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
        pow__hd_r_counter = pow__hd_r_counter + 1
    end function
    impure elemental function pow__hduvw_hduvw(u, v) result(res)
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
        pow__hd_hd_counter = pow__hd_hd_counter + 1
    end function
    impure elemental  function eq__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f == rhs%f)

    end function
    impure elemental  function eq__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f == rhs)

    end function
    impure elemental  function eq__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function eq__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%f)

    end function
    impure elemental  function eq__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f == rhs%d%f)

    end function
    impure elemental  function eq__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    impure elemental  function eq__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f == rhs)

    end function
    impure elemental  function eq__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    impure elemental  function eq__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%d%f)

    end function
    impure elemental  function le__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f <= rhs%f)

    end function
    impure elemental  function le__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f <= rhs)

    end function
    impure elemental  function le__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function le__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%f)

    end function
    impure elemental  function le__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f <= rhs%d%f)

    end function
    impure elemental  function le__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    impure elemental  function le__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f <= rhs)

    end function
    impure elemental  function le__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    impure elemental  function le__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%d%f)

    end function
    impure elemental  function lt__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f < rhs%f)

    end function
    impure elemental  function lt__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f < rhs)

    end function
    impure elemental  function lt__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function lt__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%f)

    end function
    impure elemental  function lt__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f < rhs%d%f)

    end function
    impure elemental  function lt__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    impure elemental  function lt__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f < rhs)

    end function
    impure elemental  function lt__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    impure elemental  function lt__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%d%f)

    end function
    impure elemental  function ge__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f >= rhs%f)

    end function
    impure elemental  function ge__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f >= rhs)

    end function
    impure elemental  function ge__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function ge__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%f)

    end function
    impure elemental  function ge__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f >= rhs%d%f)

    end function
    impure elemental  function ge__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    impure elemental  function ge__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f >= rhs)

    end function
    impure elemental  function ge__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    impure elemental  function ge__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%d%f)

    end function
    impure elemental  function gt__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f > rhs%f)

    end function
    impure elemental  function gt__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f > rhs)

    end function
    impure elemental  function gt__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function gt__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%f)

    end function
    impure elemental  function gt__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f > rhs%d%f)

    end function
    impure elemental  function gt__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    impure elemental  function gt__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f > rhs)

    end function
    impure elemental  function gt__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    impure elemental  function gt__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%d%f)

    end function
    impure elemental  function ne__duvw_duvw(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%f /= rhs%f)

    end function
    impure elemental  function ne__duvw_i(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne__duvw_r(lhs, rhs) result(res)
         type(dual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%f /= rhs)

    end function
    impure elemental  function ne__i_duvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function ne__r_duvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%f)

    end function
    impure elemental  function ne__hduvw_hduvw(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%d%f /= rhs%d%f)

    end function
    impure elemental  function ne__hduvw_i(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    impure elemental  function ne__hduvw_r(lhs, rhs) result(res)
         type(hdual__uvw_t), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%d%f /= rhs)

    end function
    impure elemental  function ne__i_hduvw(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    impure elemental  function ne__r_hduvw(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(hdual__uvw_t), intent(in) :: rhs
         logical :: res

         res = (lhs /= rhs%d%f)

    end function
    impure elemental  function abs__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        integer :: i

        if (u%f >= 0) then
            res%f = u%f
            res%g = u%g
        else
            res%f = -u%f
            res%g = -u%g
        end if

    end function
    impure elemental  function acos__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = acos(u%f)
    
        res%g = -u%g / sqrt(1.0_dp - u%f**2)

    end function
    impure elemental  function asin__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = asin(u%f)
        res%g = u%g / sqrt(1.0_dp - u%f**2)

    end function
    impure elemental  function atan__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = atan(u%f)
        res%g = u%g / (1.0_dp + u%f**2)

    end function
    impure elemental  function atan2__duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u, v
        type(dual__uvw_t) :: res

        real(dp) :: usq_plus_vsq

        res%f = atan2(u%f, v%f)

        usq_plus_vsq = u%f**2 + v%f**2
        res%g = v%f / usq_plus_vsq * u%g - u%f / usq_plus_vsq * v%g

    end function
    impure elemental  function cos__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = cos(u%f)
        res%g = -sin(u%f) * u%g

    end function
      function dot_product__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:), v(:)
        type(dual__uvw_t) :: res

        integer :: i

        res%f = dot_product(u%f, v%f)
        do i = 1, size(res%g)
            res%g(i) = dot_product(u%f, v%g(i)) + dot_product(v%f, u%g(i))
        end do

    end function
    impure elemental  function exp__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        real(dp) :: exp_x

        exp_x = exp(u%f)
        res%f = exp_x
        res%g = u%g * exp_x

    end function
    impure elemental  function int__duvw(u) result(res)
         type(dual__uvw_t), intent(in) :: u
         integer :: res

         res = int(u%f)

    end function
    impure elemental function log__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = log(u%f)
        res%g = u%g/u%f
        log__d_counter = log__d_counter + 1
    end function
    impure elemental function log__hduvw(u) result(res)
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
        log__hd_counter = log__hd_counter + 1
    end function
    impure elemental  function log10__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%f * log(10.0_dp))
        res%f = log10(u%f)
        res%g = u%g * inv

    end function
      function matmul__duvw_duvw(u,v) result(res)
        type(dual__uvw_t), intent(in) :: u(:,:), v(:,:)
        type(dual__uvw_t) :: res(size(u,1), size(v,2))

        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1,1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
      function matmul__duvw_v(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:,:), v(:)
        type(dual__uvw_t) :: res(size(u,1))
        integer :: i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
      function matmul__v_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u(:), v(:,:)
        type(dual__uvw_t) :: res(size(v, 2))
        integer::i

        res%f = matmul(u%f, v%f)
        do i = 1, size(res(1)%g)
            res%g(i) = matmul(u%g(i), v%f) + matmul(u%f, v%g(i))
        end do

    end function
    impure elemental  function max__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val1%f > val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function max__duvw_i(u, i) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: i
        type(dual__uvw_t) :: res

        if (u%f > i) then
            res = u
        else
            res = i
        endif

    end function
    impure elemental  function max__duvw_r(u, r) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__uvw_t) :: res

        if (u%f > r) then
            res = u
        else
            res = r
        endif

    end function
     impure elemental  function max__r_duvw(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        if (u%f > n) then
            res = u
        else
            res = n
        endif

    end function
    impure elemental  function dmax1__duvw_duvw(val1, val2, val3, val4,val5) result(res)
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
      function maxval__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__uvw_t) :: res

        iloc=maxloc(u%f)
        res=u(iloc(1))

    end function
    impure elemental  function min__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val1%f < val2%f) then
            res = val1
        else
            res = val2
        endif

    end function
    impure elemental  function min__duvw_r(u, r) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual__uvw_t) :: res

        if (u%f < r) then
            res = u
        else
            res = r
        endif

    end function
    impure elemental  function dmin1__duvw_duvw(val1, val2, val3, val4) result(res)
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
      function minval__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual__uvw_t) :: res

        iloc=minloc(u%f)
        res=u(iloc(1))

    end function
    impure elemental  function nint__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer :: res

        res=nint(u%f)

    end function
    impure elemental  function sign__duvw_duvw(val1, val2) result(res)
        type(dual__uvw_t), intent(in) :: val1, val2
        type(dual__uvw_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function
    impure elemental  function sign__r_duvw(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual__uvw_t), intent(in) :: val2
        type(dual__uvw_t) :: res

        if (val2%f < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function
    impure elemental  function sin__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = sin(u%f)
        res%g = cos(u%f) * u%g

    end function
    impure elemental  function tan__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

        res%f = tan(u%f)
        res%g = u%g / cos(u%f)**2

    end function
    impure elemental function sqrt__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        real(dp) :: t0
        
        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
        sqrt__d_counter = sqrt__d_counter + 1
    end function

    impure elemental function sqrt__hduvw(u) result(res)
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
        sqrt__hd_counter = sqrt__hd_counter + 1
    end function
      function sum__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u(:)
        type(dual__uvw_t) :: res
        integer :: i

        res%f = sum(u%f)
        do i = 1, size(res%g)
            res%g(i) = sum(u%g(i))
        end do

    end function
      function maxloc__duvw(array) result(ind)
        type(dual__uvw_t), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%f)

    end function
    
    
end module