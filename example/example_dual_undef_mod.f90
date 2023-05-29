module example_dual_undef_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: example_dual_undef

    integer, parameter :: num_deriv = 3 ! Number of derivatives

    ! Workaround to make pure functions produce NaN's deliberately:
    real(dp) :: negative_one = -1.0_dp
    
    type :: dual_uvw_t
        !! dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivatives
    end type


    interface initialize ! Initialize a dual or hyper dual number
        module procedure initialize_d
    end interface
  
    interface assignment (=)
        module procedure assign_d_i  ! dual=integer, elemental
        module procedure assign_d_r  ! dual=real, elemental
        module procedure assign_i_d  ! integer=dual, elemental
        module procedure assign_r_d  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure unary_add_d   ! +dual number, elemental
        module procedure add_d_d       ! dual + dual, elemental
        module procedure add_d_i       ! dual + integer, elemental
        module procedure add_d_r       ! dual + real, elemental
        module procedure add_i_d       ! integer + dual, elemental
        module procedure add_r_d       ! real + dual, elemental
    end interface
    interface acos
        module procedure acos_d ! arccosine of a dual number, elemental
    end interface
    
contains

    subroutine example_dual_undef()

        type(dual_uvw_t) :: uhd, vhd, whd, fhd

        call initialize(uhd, 1.0_dp, 1)
        call initialize(vhd, 1.0_dp, 2)
        call initialize(whd, 1.0_dp, 3)

        ! Function to compute value, gradient and Hessian for:
        fhd = test_function(uhd, vhd, whd)


        print *, "Function value:"
        print *, fhd%x

        print *, "Gradient:"
        print *, fhd%dx

    end subroutine

    elemental function test_function(u, v, w) result(f)
        type(dual_uvw_t), intent(in) :: u, v, w
        type(dual_uvw_t) :: f
        f = acos(u)
    end function

    pure subroutine initialize_d(dual, val, idiff)
        type(dual_uvw_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%x = val
        dual%dx = 0
        dual%dx(idiff) = 1

    end subroutine
    elemental subroutine assign_d_i(u, i)
        type(dual_uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_d_r(u, r)
        type(dual_uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_d(i, v)
        type(dual_uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_d(r, v)
        type(dual_uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental function unary_add_d(u) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t) :: res
        
        res%x = u%x
        res%dx = u%dx
    end function
    elemental function add_d_d(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
    end function
    elemental function add_d_r(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental function add_d_i(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
    end function
    elemental function add_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
    end function
    elemental  function acos_d(u) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t) :: res

        res%x = acos(u%x)
    
        if (u%x == 1.0_dp .or. u%x == -1.0_dp) then
            res%dx = set_Nan()  ! Undefined derivative
        else
            res%dx = -u%dx / sqrt(1.0_dp - u%x**2)
        end if

    end function
    
    ! Workaround to make pure functions produce NaN's deliberately:
    elemental function set_NaN() result(res)
        real(dp) :: res
        res = sqrt(negative_one)
    end function set_NaN
    
end module