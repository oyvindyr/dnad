module example_hdual_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: example_hdual

    integer, parameter :: num_deriv = 3 ! Number of derivatives

    
    type :: hdual_uvw_t
        !! hyper-dual number type
        sequence
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
        real(dp) :: h(num_deriv*(num_deriv + 1)/2) = 0  ! Lower triangular of Hessian
    end type

    interface initialize 
        !! Initialize a dual or hyper dual number
        module procedure initialize_hd_scalar
        module procedure initialize_hd_vector
    end interface
    interface hessian 
        !! Extract Hessian from a hyper-dual number
        module procedure hessian_hd
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
        print *, fhd(1)%f
        print *, "Function value 2:"
        print *, fhd(2)%f
        print *, "Gradient 1:"
        print *, fhd(1)%g
        print *, "Gradient 2:"
        print *, fhd(2)%g
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
        
        hdual%f = val
        hdual%g = 0
        hdual%h = 0
        hdual%g(idiff) = 1

    end subroutine
    pure subroutine initialize_hd_vector(hdual, val)
        !! Initialize a vector of hyper-dual numbers, where the derivative of 
        !! number i with respect to design variable i is 1
        type(hdual_uvw_t), intent(out) :: hdual(:)
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
        type(hdual_uvw_t), intent(in) :: d
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
    elemental subroutine assign_hd_i(u, i)
        type(hdual_uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%f = real(i, dp)  ! This is faster than direct assignment
        u%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign_hd_r(u, r)
        type(hdual_uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%f = r
        u%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign_i_hd(i, v)
        type(hdual_uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%f)

    end subroutine
    elemental subroutine assign_r_hd(r, v)
        type(hdual_uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%f

    end subroutine
    elemental function unary_add_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
        
        res%f = u%f
        res%g = u%g
        res%h = u%h
    end function
    elemental function add_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u%f + v%f
        res%g = u%g + v%g
        res%h = u%h + v%h
    end function
    elemental function add_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        res%h = u%h
    end function
    elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        res%h = v%h
    end function
    elemental function add_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u%f + v
        res%g = u%g
        res%h = u%h
    end function
    elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u + v%f
        res%g = v%g
        res%h = v%h
    end function
    elemental function mult_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function mult_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        res%h = u%h*v
    end function
    elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        res%h = u*v%h
    end function
    elemental function mult_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u%f*v
        res%g = u%g*v
        res%h = u%h*v
    end function
    elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
        
        res%f = u*v%f
        res%g = u*v%g
        res%h = u*v%h
    end function
    elemental function pow_hd_i(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function pow_hd_r(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function pow_hd_hd(u, v) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t), intent(in) :: v
        type(hdual_uvw_t) :: res
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
    end function
    elemental function log_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
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
    end function
    elemental function sqrt_hd(u) result(res)
        type(hdual_uvw_t), intent(in) :: u
        type(hdual_uvw_t) :: res
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
    end function
    
    
end module