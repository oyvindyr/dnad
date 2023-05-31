module example_hdual_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: example_hdual

    integer, parameter :: num_deriv = 3 ! Number of derivatives

    
    type :: hdual_uvw_t
        !! hyper-dual number type
        sequence
        real(dp) :: x = 0  ! functional value
        real(dp) :: dx(num_deriv) = 0  ! derivatives
        real(dp) :: ddx(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface hessian ! Extract Hessian from a hyper-dual number
        module procedure hessian_hd
    end interface

    interface initialize ! Initialize a dual or hyper dual number
        module procedure initialize_hd_scalar
        module procedure initialize_hd_vector
    end interface
  
    interface assignment (=)
        module procedure assign_hd_i  ! dual=integer, elemental
        module procedure assign_hd_r  ! dual=real, elemental
        module procedure assign_i_hd  ! integer=dual, elemental
        module procedure assign_r_hd  ! real=dual, elemental
    end interface
    interface operator (+)
        module procedure unary_add_hd  ! +dual number, elemental
        module procedure add_hd_hd     ! dual + dual, elemental
        module procedure add_hd_i      ! dual + integer, elemental
        module procedure add_hd_r      ! dual + real, elemental
        module procedure add_i_hd      ! integer + dual, elemental
        module procedure add_r_hd      ! real + dual, elemental
    end interface
    interface operator (*)
        module procedure mult_hd_hd  ! dual*dual, elemental
        module procedure mult_hd_i   ! dual*integer,elemental
        module procedure mult_hd_r   ! dual*real,elemental
        module procedure mult_i_hd   ! integer*dual,elemental
        module procedure mult_r_hd   ! real*dual,elemental
    end interface
    interface operator (**)
        module procedure pow_hd_i ! hdual number to an integer power,elemental
        module procedure pow_hd_r ! hdual number to a real power, elemental
        module procedure pow_hd_hd ! hdual number to a hdual power, elemental
    end interface
    interface log
        module procedure log_hd ! log of a dual number, elemental
    end interface
    interface sqrt
        module procedure sqrt_hd ! obtain the sqrt of a dual number, elemental
    end interface
    
contains

    subroutine example_hdual()

        ! Number of function evaluations to perform:
        integer, parameter :: nval = 2

        integer :: i
        real(dp) :: r

        type(hdual_uvw_t), dimension(nval) :: uhd, vhd, whd, fhd

        real(dp), dimension(num_deriv, num_deriv) :: hessian1, hessian2 

        ! Initialize dual inputs:
        do i = 1, nval

            call random_number(r)
            call initialize(uhd(i), r, 1)

            call random_number(r)
            call initialize(vhd(i), r, 2)

            call random_number(r)
            call initialize(whd(i), r, 3)

        end do

        ! Function to compute value, gradient and Hessian for:
        fhd = test_function_hdual(uhd, vhd, whd)

        ! Extract full hessian from lower triangular:
        hessian1 = hessian(fhd(1))
        hessian2 = hessian(fhd(2))


        print *, "Function value 1:"
        print *, fhd(1)%x
        print *, "Function value 2:"
        print *, fhd(2)%x
        print *, "Gradient 1:"
        print *, fhd(1)%dx
        print *, "Gradient 2:"
        print *, fhd(2)%dx
        print *, "Hessian 1:"
        do i = 1, num_deriv
            print *, hessian1(i, :)
        end do
        print *, "Hessian 2:"
        do i = 1, num_deriv
            print *, hessian2(i, :)
        end do

      
    end subroutine

    elemental function test_function_hdual(u, v, w) result(f)
        type(hdual_uvw_t), intent(in) :: u, v, w
        type(hdual_uvw_t) :: f
        f = sqrt(u)*log(v) + u**w
    end function

    pure subroutine initialize_hd_scalar(hdual, val, idiff)
        !! Initialize a single hyper-dual number, whose derivative with respect to design variable 'idiff' is 1
        type(hdual_uvw_t), intent(out) :: hdual
        real(dp), intent(in) :: val
        integer, intent(in) :: idiff
        
        hdual%x = val
        hdual%dx = 0
        hdual%ddx = 0
        hdual%dx(idiff) = 1

    end subroutine
    pure subroutine initialize_hd_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual_uvw_t), intent(out) :: hdual(:)
        real(dp), intent(in) :: val(:)

        integer :: i
        
        do i = 1, size(hdual)
            hdual(i)%x = val(i)
            hdual(i)%dx = 0
            hdual(i)%ddx = 0
            hdual(i)%dx(i) = 1
        end do

    end subroutine
    pure function hessian_hd(d) result(m)
        type(hdual_uvw_t), intent(in) :: d
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
    elemental subroutine assign_hd_i(u, i)
        type(hdual_uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%x = real(i, dp)  ! This is faster than direct assignment
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_hd_r(u, r)
        type(hdual_uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp
        u%ddx = 0.0_dp

    end subroutine
    elemental subroutine assign_i_hd(i, v)
        type(hdual_uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%x)

    end subroutine
    elemental subroutine assign_r_hd(r, v)
        type(hdual_uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%x

    end subroutine
    elemental function unary_add_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
        
        res%x = u%x
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u%x + v%x
        res%dx = u%dx + v%dx
        res%ddx = u%ddx + v%ddx
    end function
    elemental function add_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function add_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u%x + v
        res%dx = u%dx
        res%ddx = u%ddx
    end function
    elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u + v%x
        res%dx = v%dx
        res%ddx = v%ddx
    end function
    elemental function mult_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function mult_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function mult_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u%x*v
        res%dx = u%dx*v
        res%ddx = u%ddx*v
    end function
    elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%x = u*v%x
        res%dx = u*v%dx
        res%ddx = u*v%ddx
    end function
    elemental function pow_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function pow_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function pow_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function log_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
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
    end function
    elemental function sqrt_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
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
    end function
    
    
end module