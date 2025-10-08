module example_dual_undef_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: example_dual_undef

    integer, parameter :: num_deriv = 3 ! Number of derivatives

    ! Workaround to make pure functions produce NaN's deliberately:
    real(dp) :: negative_one = -1.0_dp
    
    type :: dual__uvw_t
        !! dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type
    interface fvalue
        !! Extract function value from a real, dual or hyper-dual number
        module procedure fvalue__r
        module procedure fvalue__r1
        module procedure fvalue__r2
        module procedure fvalue__d_uvw
        module procedure fvalue__d_uvw_r1
        module procedure fvalue__d_uvw_r2
    end interface

    interface gradient
        !! Extract gradient from a dual or hyper-dual number
        module procedure gradient__d_uvw
    end interface

    interface gradient_d
        !! Extract gradient from a dual or hyper-dual number. In the hyper-dual case, the gradient is dual valued.
        module procedure gradient__d_uvw ! Same implementations as gradient()
    end interface

    interface hessian 
        !! Extract Hessian from a hyper-dual number
    end interface

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize__d_uvw_scalar
        module procedure initialize__d_uvw_vector
    end interface
    interface assignment (=)
        module procedure assign__duvw_i  ! dual=integer, elemental
        module procedure assign__duvw_r  ! dual=real, elemental
        module procedure assign__i_duvw  ! integer=dual, elemental
        module procedure assign__r_duvw  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure add__duvw   ! +dual number, elemental
        module procedure add__duvw_duvw       ! dual + dual, elemental
        module procedure add__duvw_i       ! dual + integer, elemental
        module procedure add__duvw_r       ! dual + real, elemental
        module procedure add__i_duvw       ! integer + dual, elemental
        module procedure add__r_duvw       ! real + dual, elemental
    end interface
    interface acos
        module procedure acos__duvw ! arccosine of a dual number, elemental
    end interface
    
contains

    subroutine example_dual_undef()

        type(dual__uvw_t) :: uhd, vhd, whd, fhd

        call initialize(uhd, 1.0_dp, 1)
        call initialize(vhd, 1.0_dp, 2)
        call initialize(whd, 1.0_dp, 3)

        ! Function to compute value, gradient and Hessian for:
        fhd = test_function(uhd, vhd, whd)


        print *, "Function value:"
        print *, fvalue(fhd)

        print *, "Gradient:"
        print *, gradient(fhd)

    end subroutine

    elemental function test_function(u, v, w) result(f)
        type(dual__uvw_t), intent(in) :: u, v, w
        type(dual__uvw_t) :: f
        f = acos(u)
    end function


    pure function fvalue__r(fi) result(f)
        real(dp), intent(in) :: fi
        real(dp) :: f
        f = fi
    end function
    pure function fvalue__r1(fi) result(f)
        real(dp), intent(in) :: fi(:)
        real(dp) :: f(size(fi))
        f = fi
    end function
    pure function fvalue__r2(fi) result(f)
        real(dp), intent(in) :: fi(:,:)
        real(dp) :: f(size(fi, 1), size(fi, 2))
        f = fi
    end function
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

    pure function gradient__d_uvw(d) result(g)
        type(dual__uvw_t), intent(in) :: d
        real(dp) :: g(size(d%g))
        g = d%g
    end function

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
    elemental subroutine assign__duvw_i(u, i)
        type(dual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__duvw_r(u, r)
        type(dual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp

    end subroutine
    elemental subroutine assign__i_duvw(i, v)
        type(dual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign__r_duvw(r, v)
        type(dual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental function add__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res
        
        res%f = u%f
        res%g = u%g
    end function
    elemental function add__duvw_duvw(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
    end function
    elemental function add__duvw_r(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__r_duvw(u, v) result(res)
        real(dp), intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental function add__duvw_i(u, v) result(res)
        type(dual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
    end function
    elemental function add__i_duvw(u, v) result(res)
        integer, intent(in) :: u
        type(dual__uvw_t), intent(in) :: v
        type(dual__uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
    end function
    elemental  function acos__duvw(u) result(res)
        type(dual__uvw_t), intent(in) :: u
        type(dual__uvw_t) :: res

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