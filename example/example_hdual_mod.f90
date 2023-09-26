module example_hdual_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: example_hdual

    integer, parameter :: num_deriv = 3 ! Number of derivatives

    
    type :: dual__uvw_t
        !! dual number type
        
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(num_deriv) = 0  ! derivatives
    end type
    type :: hdual__uvw_t
        !! hyper-dual number type
        
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

        type(hdual__uvw_t), dimension(nval) :: uhd, vhd, whd, fhd

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
        print *, fvalue(fhd(1))
        print *, "Function value 2:"
        print *, fvalue(fhd(2))
        print *, "Gradient 1:"
        print *, gradient(fhd(1))
        print *, "Gradient 2:"
        print *, gradient(fhd(2))
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
        type(hdual__uvw_t), intent(in) :: u, v, w
        type(hdual__uvw_t) :: f
        f = sqrt(u)*log(v) + u**w
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
    elemental subroutine assign_hd_i(u, i)
        type(hdual__uvw_t), intent(out) :: u
        integer, intent(in) :: i

        u%d%f = real(i, dp)  ! This is faster than direct assignment
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign_hd_r(u, r)
        type(hdual__uvw_t), intent(out) :: u
        real(dp), intent(in) :: r

        u%d%f = r
        u%d%g = 0.0_dp
        u%h = 0.0_dp

    end subroutine
    elemental subroutine assign_i_hd(i, v)
        type(hdual__uvw_t), intent(in) :: v
        integer, intent(out) :: i

        i = int(v%d%f)

    end subroutine
    elemental subroutine assign_r_hd(r, v)
        type(hdual__uvw_t), intent(in) :: v
        real(dp), intent(out) :: r

        r = v%d%f

    end subroutine
    elemental function unary_add_hd(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add_hd_hd(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v%d%f
        res%d%g = u%d%g + v%d%g
        res%h = u%h + v%h
    end function
    elemental function add_hd_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function add_hd_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f + v
        res%d%g = u%d%g
        res%h = u%h
    end function
    elemental function add_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u + v%d%f
        res%d%g = v%d%g
        res%h = v%h
    end function
    elemental function mult_hd_hd(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f*v%d%f
        res%d%g = u%d%g*v%d%f + u%d%f*v%d%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = u%h(k)*v%d%f + u%d%g(i)*v%d%g(j) + u%d%g(j)*v%d%g(i) + u%d%f*v%h(k)
            end do
        end do
    end function
    elemental function mult_hd_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult_r_hd(u, v) result(res)
        real(dp), intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function mult_hd_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u%d%f*v
        res%d%g = u%d%g*v
        res%h = u%h*v
    end function
    elemental function mult_i_hd(u, v) result(res)
        integer, intent(in) :: u
        type(hdual__uvw_t), intent(in) :: v
        type(hdual__uvw_t) :: res
        
        res%d%f = u*v%d%f
        res%d%g = u*v%d%g
        res%h = u*v%h
    end function
    elemental function pow_hd_i(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        integer, intent(in) :: v
        type(hdual__uvw_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow_hd_r(u, v) result(res)
        type(hdual__uvw_t), intent(in) :: u
        real(dp), intent(in) :: v
        type(hdual__uvw_t) :: res
        integer :: i, j, k
        
        res%d%f = u%d%f**v
        res%d%g = u%d%f**(v - 1)*v*u%d%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = v*(u%d%g(i)*u%d%g(j)*(v - 1)*u%d%f**(v-2) + u%h(k)*u%d%f**(v-1))
            end do
        end do
    end function
    elemental function pow_hd_hd(u, v) result(res)
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
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t0*(t1*v%h(k) + t2*u%h(k)*v%d%f + t2*u%d%g(j)*(t2*u%d%g(i)*v%d%f*(v%d%f - &
      1) + t3*v%d%g(i)) + v%d%g(j)*(t1**2*v%d%g(i) + t2*t3*u%d%g(i)))
            end do
        end do
    end function
    elemental function log_hd(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        integer :: i, j, k
        real(dp) :: t0
        
        t0 = 1.0_dp/u%d%f
        res%d%f = log(u%d%f)
        res%d%g = t0*u%d%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = t0*(-t0*u%d%g(i)*u%d%g(j) + u%h(k))
            end do
        end do
    end function
    elemental function sqrt_hd(u) result(res)
        type(hdual__uvw_t), intent(in) :: u
        type(hdual__uvw_t) :: res
        integer :: i, j, k
        real(dp) :: t0,t1
        
        t0 = sqrt(u%d%f)
        t1 = 1.0_dp/t0
        res%d%f = t0
        res%d%g = 0.5_dp*t1*u%d%g
        k = 0
        do j = 1, num_deriv
            do i = j, num_deriv
                k = k + 1
                res%h(k) = (0.25_dp)*t1*(2*u%h(k) - u%d%g(i)*u%d%g(j)/u%d%f)
            end do
        end do
    end function
    
    
end module