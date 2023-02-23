



abs,+,log


    No

!#:if "=" in lst_interfaces:
    elemental subroutine assign_di(u, i)
         type(dual), intent(out) :: u
         integer, intent(in) :: i

         u%x = real(i, dp)  ! This is faster than direct assignment
         u%dx = 0.0_dp

    end subroutine assign_di
    elemental subroutine assign_dr(u, r)
        type(dual), intent(out) :: u
        real(dp), intent(in) :: r

        u%x = r
        u%dx = 0.0_dp

    end subroutine assign_dr
    elemental subroutine assign_id(i, v)
         type(dual), intent(in) :: v
         integer, intent(out) :: i

         i = int(v%x)

    end subroutine assign_id
    elemental subroutine assign_rd(r, v)
         type(dual), intent(in) :: v
         real(dp), intent(out) :: r

         r = v%x

    end subroutine assign_rd
!#:endif
    elemental function add_d(u) result(res)
         type(dual), intent(in) :: u
         type(dual) :: res

         res = u  ! Faster than assigning component wise

    end function add_d
    elemental function add_dd(u, v) result(res)
         type(dual), intent(in) :: u, v
         type(dual) :: res

         res%x = u%x + v%x
         res%dx = u%dx + v%dx

    end function add_dd
    elemental function add_di(u, i) result(res)
         type(dual), intent(in) :: u
         integer, intent(in) :: i
         type(dual) :: res

         res%x = real(i, dp) + u%x
         res%dx = u%dx

    end function add_di
    elemental function add_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        res%x = r + u%x
        res%dx = u%dx

    end function add_dr
    elemental function add_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: v
        type(dual) :: res

        res%x = real(i, dp) + v%x
        res%dx = v%dx

    end function add_id
    elemental function add_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual), intent(in) :: v
        type(dual) :: res

        res%x = r + v%x
        res%dx = v%dx

    end function add_rd
    elemental function minus_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = -u%x
        res%dx = -u%dx

    end function minus_d
    elemental function minus_dd(u, v) result(res)
        type(dual), intent(in) :: u, v
        type(dual) :: res

        res%x = u%x - v%x
        res%dx = u%dx - v%dx

    end function minus_dd
    elemental function minus_di(u, i) result(res)
        type(dual), intent(in) :: u
        integer, intent(in) :: i
        type(dual) :: res

        res%x = u%x - real(i, dp)
        res%dx = u%dx

    end function minus_di
    elemental function minus_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp),intent(in) :: r
        type(dual) :: res

        res%x = u%x - r
        res%dx = u%dx

    end function minus_dr
    elemental function minus_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: v
        type(dual) :: res

        res%x = real(i, dp) - v%x
        res%dx = -v%dx

    end function minus_id
    elemental function minus_rd(r, v) result(res)
         real(dp), intent(in) :: r
         type(dual), intent(in) :: v
         type(dual) :: res

        res%x = r - v%x
        res%dx = -v%dx

    end function minus_rd
    elemental function mult_dd(u, v) result(res)
        type(dual), intent(in) :: u, v
        type(dual) :: res

        res%x = u%x * v%x
        res%dx = u%x * v%dx + v%x * u%dx

    end function mult_dd
    elemental function mult_di(u, i) result(res)
        type(dual), intent(in) :: u
        integer, intent(in) :: i
        type(dual) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * u%x
        res%dx = r * u%dx

    end function mult_di
    elemental function mult_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        res%x = u%x * r
        res%dx = u%dx * r

    end function mult_dr
    elemental function mult_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: v
        type(dual) :: res

        real(dp) :: r

        r = real(i, dp)
        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_id
    elemental function mult_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual), intent(in) :: v
        type(dual) :: res

        res%x = r * v%x
        res%dx = r * v%dx

    end function mult_rd
    elemental function div_dd(u, v) result(res)
        type(dual), intent(in) :: u, v
        type(dual) :: res

        res%x = u%x / v%x
        res%dx = (u%dx - res%x * v%dx) / v%x

    end function div_dd
    elemental function div_di(u, i) result(res)
        type(dual), intent(in) :: u
        integer, intent(in) :: i
        type(dual) :: res

        res%x = u%x / i
        res%dx = u%dx / i

    end function div_di
    elemental function div_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        res%x = u%x / r
        res%dx = u%dx / r

    end function div_dr
    elemental function div_id(i, v) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: v
        type(dual) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = real(i, dp) * inv
        res%dx = -res%x * inv * v%dx

    end function div_id
    elemental function div_rd(r, v) result(res)
        real(dp), intent(in) :: r
        type(dual), intent(in) :: v
        type(dual) :: res

        real(dp) :: inv

        inv = 1.0_dp / v%x
        res%x = r * inv
        res%dx = -res%x * inv * v%dx

    end function div_rd
    elemental function pow_i(u, i) result(res)
        type(dual), intent(in) :: u
        integer, intent(in) :: i
        type(dual) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (i - 1)
        res%x = u%x * pow_x
        res%dx = real(i, dp) * pow_x * u%dx

    end function pow_i
    elemental function pow_r(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        real(dp) :: pow_x

        pow_x = u%x ** (r - 1.0_dp)
        res%x = u%x * pow_x
        res%dx = r * pow_x * u%dx

    end function pow_r
    elemental function pow_d(u, v) result(res)
        type(dual), intent(in)::u, v
        type(dual) :: res

        res%x = u%x ** v%x
        res%dx = res%x * (v%x / u%x * u%dx + log(u%x) * v%dx)

    end function pow_d
    elemental function eq_dd(lhs, rhs) result(res)
         type(dual), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x == rhs%x)

    end function eq_dd
    elemental function eq_di(lhs, rhs) result(res)
         type(dual), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x == real(rhs, dp))

    end function eq_di
    elemental function eq_dr(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical::res

        res = (lhs%x == rhs)

    end function eq_dr
    elemental function eq_id(lhs, rhs) result(res)
         integer, intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_id
    elemental function eq_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs == rhs%x)

    end function eq_rd
    elemental function le_dd(lhs, rhs) result(res)
         type(dual), intent(in) :: lhs, rhs
         logical :: res

         res = (lhs%x <= rhs%x)

    end function le_dd
    elemental function le_di(lhs, rhs) result(res)
         type(dual), intent(in) :: lhs
         integer, intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_di
    elemental function le_dr(lhs, rhs) result(res)
         type(dual), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         logical :: res

         res = (lhs%x <= rhs)

    end function le_dr
    elemental function le_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual), intent(in) :: rhs
         logical :: res

         res = (i <= rhs%x)

    end function le_id
    elemental function le_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs <= rhs%x)

    end function le_rd
    elemental function lt_dd(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x < rhs%x)

    end function lt_dd
    elemental function lt_di(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_di
    elemental function lt_dr(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x < rhs)

    end function lt_dr
    elemental function lt_id(i, rhs) result(res)
         integer, intent(in) :: i
         type(dual), intent(in) :: rhs
         logical :: res

         res = (i < rhs%x)

    end function lt_id
    elemental function lt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs < rhs%x)

    end function lt_rd
    elemental function ge_dd(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x >= rhs%x)

    end function ge_dd
    elemental function ge_di(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_di
    elemental function ge_dr(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x >= rhs)

    end function ge_dr
    elemental function ge_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: rhs
        logical :: res

        res = (i >= rhs%x)

    end function ge_id
    elemental function ge_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs >= rhs%x)

    end function ge_rd
    elemental function gt_dd(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x > rhs%x)

    end function gt_dd
    elemental function gt_di(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_di
    elemental function gt_dr(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x > rhs)

    end function gt_dr
    elemental function gt_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: rhs
        logical :: res

        res = (i > rhs%x)

    end function gt_id
    elemental function gt_rd(lhs, rhs) result(res)
         real(dp), intent(in) :: lhs
         type(dual), intent(in) :: rhs
         logical :: res

         res = (lhs > rhs%x)

    end function gt_rd
    elemental function ne_dd(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs, rhs
        logical :: res

        res = (lhs%x /= rhs%x)

    end function ne_dd
    elemental function ne_di(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        integer, intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_di
    elemental function ne_dr(lhs, rhs) result(res)
        type(dual), intent(in) :: lhs
        real(dp), intent(in) :: rhs
        logical :: res

        res = (lhs%x /= rhs)

    end function ne_dr
    elemental function ne_id(i, rhs) result(res)
        integer, intent(in) :: i
        type(dual), intent(in) :: rhs
        logical :: res

        res = (i /= rhs%x)

    end function ne_id
    elemental function ne_rd(lhs, rhs) result(res)
        real(dp), intent(in) :: lhs
        type(dual), intent(in) :: rhs
        logical :: res

        res = (lhs /= rhs%x)

    end function ne_rd
    elemental function abs_d(u) result(res)
         type(dual), intent(in) :: u
         type(dual) :: res
         integer :: i

         if(u%x > 0) then
            res%x = u%x
            res%dx = u%dx
         else if (u%x < 0) then
            res%x = -u%x
            res%dx = -u%dx
         else
            res%x = 0.0_dp
            do i = 1, 2
                if (u%dx(i) .eq. 0.0_dp) then
                    res%dx(i) = 0.0_dp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
         endif

    end function abs_d
    elemental function abs_d(u) result(res)
         type(dual), intent(in) :: u
         type(dual) :: res
         integer :: i

         if(u%x > 0) then
            res%x = u%x
            res%dx = u%dx
         else if (u%x < 0) then
            res%x = -u%x
            res%dx = -u%dx
         else
            res%x = 0.0_dp
            do i = 1, 2
                if (u%dx(i) .eq. 0.0_dp) then
                    res%dx(i) = 0.0_dp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
         endif

    end function abs_d
    elemental function acos_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = acos(u%x)
        if (u%x == 1.0_dp .or. u%x == -1.0_dp) then
            res%dx = set_Nan()  ! Undefined derivative
        else
            res%dx = -u%dx / sqrt(1.0_dp - u%x**2)
        end if

    end function acos_d
    elemental function asin_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = asin(u%x)
        if (u%x == 1.0_dp .or. u%x == -1.0_dp) then
            res%dx = set_NaN()  ! Undefined derivative
        else
            res%dx = u%dx / sqrt(1.0_dp - u%x**2)
        end if

    end function asin_d
    elemental function atan_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = atan(u%x)
        res%dx = u%dx / (1.0_dp + u%x**2)

    end function atan_d
    elemental function atan2_d(u, v) result(res)
        type(dual), intent(in) :: u, v
        type(dual) :: res

        real(dp) :: usq_plus_vsq

        res%x = atan2(u%x, v%x)

        usq_plus_vsq = u%x**2 + v%x**2
        res%dx = v%x / usq_plus_vsq * u%dx - u%x / usq_plus_vsq * v%dx

    end function atan2_d
    elemental function cos_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d
    elemental function cos_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = cos(u%x)
        res%dx = -sin(u%x) * u%dx

    end function cos_d
    pure function dot_product_dd(u, v) result(res)
        type(dual), intent(in) :: u(:), v(:)
        type(dual) :: res

        integer :: i

        res%x = dot_product(u%x, v%x)
        do i = 1, 2
            res%dx(i) = dot_product(u%x, v%dx(i)) + dot_product(v%x, u%dx(i))
        end do

    end function dot_product_dd
    elemental function exp_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        real(dp) :: exp_x

        exp_x = exp(u%x)
        res%x = exp_x
        res%dx = u%dx * exp_x

    end function exp_d
    elemental function int_d(u) result(res)
         type(dual), intent(in) :: u
         integer :: res

         res = int(u%x)

    end function int_d
    elemental function log_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = log(u%x)
        res%dx = u%dx / u%x

    end function log_d
    elemental function log10_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        real(dp) :: inv

        inv = 1.0_dp / (u%x * log(10.0_dp))
        res%x = log10(u%x)
        res%dx = u%dx * inv

    end function log10_d
    pure function matmul_dd(u,v) result(res)
        type(dual), intent(in) :: u(:,:), v(:,:)
        type(dual) :: res(size(u,1), size(v,2))

        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, 2
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dd
    pure function matmul_dv(u, v) result(res)
        type(dual), intent(in) :: u(:,:), v(:)
        type(dual) :: res(size(u,1))
        integer :: i

        res%x = matmul(u%x, v%x)
        do i = 1, 2
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_dv
    pure function matmul_vd(u, v) result(res)
        type(dual), intent(in) :: u(:), v(:,:)
        type(dual) :: res(size(v, 2))
        integer::i

        res%x = matmul(u%x, v%x)
        do i = 1, 2
            res%dx(i) = matmul(u%dx(i), v%x) + matmul(u%x, v%dx(i))
        end do

    end function matmul_vd
    elemental function max_dd(val1, val2, val3, val4,val5) result(res)
        type(dual), intent(in) :: val1, val2
        type(dual), intent(in), optional :: val3, val4,val5
        type(dual) :: res

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
    elemental function max_di(u, i) result(res)
        type(dual), intent(in) :: u
        integer, intent(in) :: i
        type(dual) :: res

        if (u%x > i) then
            res = u
        else
            res = i
        endif

    end function max_di
    elemental function max_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        if (u%x > r) then
            res = u
        else
            res = r
        endif

    end function max_dr
     elemental function max_rd(n, u) result(res)
        real(dp), intent(in) :: n
        type(dual), intent(in) :: u
        type(dual) :: res

        if (u%x > n) then
            res = u
        else
            res = n
        endif

    end function max_rd
    elemental function max_dd(val1, val2, val3, val4,val5) result(res)
        type(dual), intent(in) :: val1, val2
        type(dual), intent(in), optional :: val3, val4,val5
        type(dual) :: res

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
    pure function maxval_d(u) result(res)
        type(dual), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual) :: res

        iloc=maxloc(u%x)
        res=u(iloc(1))

    end function maxval_d
    elemental function min_dd(val1, val2, val3, val4) result(res)
        type(dual), intent(in) :: val1, val2
        type(dual), intent(in), optional :: val3, val4
        type(dual) :: res

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
    elemental function min_dr(u, r) result(res)
        type(dual), intent(in) :: u
        real(dp), intent(in) :: r
        type(dual) :: res

        if (u%x < r) then
            res = u
        else
            res = r
        endif

    end function min_dr
    elemental function min_dd(val1, val2, val3, val4) result(res)
        type(dual), intent(in) :: val1, val2
        type(dual), intent(in), optional :: val3, val4
        type(dual) :: res

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
    pure function minval_d(u) result(res)
        type(dual), intent(in) :: u(:)
        integer :: iloc(1)
        type(dual) :: res

        iloc=minloc(u%x)
        res=u(iloc(1))

    end function minval_d
    elemental function nint_d(u) result(res)
        type(dual), intent(in) :: u
        integer :: res

        res=nint(u%x)

    end function nint_d
    elemental function sign_dd(val1, val2) result(res)
        type(dual), intent(in) :: val1, val2
        type(dual) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res =  abs(val1)
        endif

     end function sign_dd
    elemental function sign_rd(val1, val2) result(res)
        real(dp), intent(in) :: val1
        type(dual), intent(in) :: val2
        type(dual) :: res

        if (val2%x < 0.0_dp) then
            res = -abs(val1)
        else
            res = abs(val1)
        endif

     end function sign_rd
    elemental function sin_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d
    elemental function sin_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = sin(u%x)
        res%dx = cos(u%x) * u%dx

    end function sin_d
    elemental function tan_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d
    elemental function tan_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = tan(u%x)
        res%dx = u%dx / cos(u%x)**2

    end function tan_d
    elemental function sqrt_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res
        integer :: i

        res%x = sqrt(u%x)

        if (res%x .ne. 0.0_dp) then
            res%dx = 0.5_dp * u%dx / res%x
        else
            do i = 1, 2
                if (u%dx(i) .eq. 0.0_dp) then
                    res%dx(i) = 0.0_dp
                else
                    res%dx(i) = set_NaN()
                end if
            end do
        end if

    end function sqrt_d
    elemental function fastsqrt_r(r) result(res)
        real(dp), intent(in) :: r
        real(dp) :: res

        res = sqrt(r)

        
    end function fastsqrt_r
    elemental function fastsqrt_d(u) result(res)
        type(dual), intent(in) :: u
        type(dual) :: res

        res%x = sqrt(u%x)
        res%dx = 0.5_dp * u%dx / res%x

    end function fastsqrt_d
    pure function sum_d(u) result(res)
        type(dual), intent(in) :: u(:)
        type(dual) :: res
        integer :: i

        res%x = sum(u%x)
        do i = 1, 2
            res%dx(i) = sum(u%dx(i))
        end do

    end function sum_d
    pure function maxloc_d(array) result(ind)
        type(dual), intent(in) :: array(:)
        integer :: ind(1)

        ind = maxloc(array%x)

    end function maxloc_d
