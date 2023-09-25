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
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize_d_scalar
        module procedure initialize_d_vector
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
        print *, fhd%f

        print *, "Gradient:"
        print *, fhd%g

    end subroutine

    elemental function test_function(u, v, w) result(f)
        type(dual_uvw_t), intent(in) :: u, v, w
        type(dual_uvw_t) :: f
        f = acos(u)
    end function

    pure subroutine initialize_d_scalar(dual, val, idiff)
        !! Initialize a single dual number, whose derivative with respect to design variable 'idiff' is 1
        type(dual_uvw_t), intent(out) :: dual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        dual%f = val
        dual%g = 0
        dual%g(idiff) = 1

    end subroutine
    pure subroutine initialize_d_vector(dual, val)
        !! Initialize a vector of dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(dual_uvw_t), intent(out) :: dual(:)
        real(dp), intent(in) :: val(:)

        integer :: i

        do i = 1, size(dual)
            dual(i)%f = val(i)
            dual(i)%g = 0
            dual(i)%g(i) = 1
        end do

    end subroutine
    elemental subroutine assign_d_i(u, i)
        type(dual_uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign_d_r(u, r)
        type(dual_uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign_i_d(i, v)
        type(dual_uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign_r_d(r, v)
        type(dual_uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental function unary_add_d(u) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add_d_d(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add_d_r(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add_r_d(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add_d_i(u, v) result(res)
        type(dual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add_i_d(u, v) result(res)
        integer, intent(in) :: u
        type(dual_uvw_t), intent(in) :: v
        type(dual_uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental  function acos_d(u) result(res)
        type(dual_uvw_t), intent(in) :: u
        type(dual_uvw_t) :: res

        res%f = acos(u%f)
    
        if (u%f == 1.0_dp .or. u%f == -1.0_dp) then
            res%g = set_Nan()  ! Undefined derivative
        else
            res%g = -u%g / sqrt(1.0_dp - u%f**2)
        end if

    end function
    
    ! Workaround to make pure functions produce NaN's deliberately:
    elemental function set_NaN() result(res)
        real(dp) :: res
        res = sqrt(negative_one)
    end function set_NaN
    
end module