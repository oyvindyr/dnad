from sympy.matrices import Matrix
from sympy import Symbol
import sympy as sp
from sympy.printing.fortran import fcode
import sympy.printing as printing
from jinja2 import Template


def replacements(c, u_type, v_type=None):
    if u_type == 'dual':
        c = c.replace('u_x', f'u%x')
        c = c.replace('u_dx1', f'u%dx')
    else:
        c = c.replace('u_x', 'u')

    if v_type is not None:
        if v_type == 'dual':
            c = c.replace('v_x', f'v%x')
            c = c.replace('v_dx1', f'v%dx')
        else:
            c = c.replace('v_x', 'v')

    c = c.replace('u_dx2', f'u%dx(j)')
    c = c.replace('u_ddx', f'u%ddx(:, j)')
    if v_type is not None:
        c = c.replace('v_dx2', f'v%dx(j)')
        c = c.replace('v_ddx', f'v%ddx(:, j)')
    return c

def uv_types(num_types, is_hyper_dual):
    if num_types[0] == 'dual':
        ut = "type(${dual_type}$)" 
        if is_hyper_dual:
            uts = 'hd'
        else:
            uts = 'd'
    elif num_types[0] == 'real':
        ut = "real(${real_kind}$)"
        uts = 'r'
    elif num_types[0] == 'integer':
        ut = "integer"
        uts = 'i'

    if num_types[1] == 'dual':
        vt = "type(${dual_type}$)" 
        if is_hyper_dual:
            vts = 'hd'
        else:
            vts = 'd'
    elif num_types[1] == 'real':
        vt = "real(${real_kind}$)"
        vts = 'r'
    elif num_types[1] == 'integer':
        vt = "integer"
        vts = 'i'
    return ut, uts, vt, vts

def binary_overload(fun, is_hyper_dual=True, skip_cse=False, num_types=('dual','dual'), code=[]):
    """Generate Fortran code for hyper-dual-number overloads of binary functions

    Parameters
    ------------
        fun: Function
            The binary function to overload
        is_hyper_dual: Logical
            If True, the function is overloaded for hyper-dual numbers instead of dual numbers
        skip_cse: Logical
            Skip common subexpression elimination if True
        num_types: tuple of strings (size 2)
            The type of the argument. If not 'dual', *_dx1, *_dx2, and *_ddx are assumed to be zero.
    Returns
    ------------
        code: String
            The generated Fortran code
        expr: Tuple
            The sympy expressions that were generated

    """

    # Create type string and type suffix for the u and v arguments
    ut, uts, vt, vts = uv_types(num_types, is_hyper_dual)

    elemental_purity = "${elemental_purity}$"
    code1 = [f'    {elemental_purity} function {fun.__name__}_{uts}_{vts}(u, v) result(res)']
    code1.append(f"        {ut}, intent(in) :: u")
    code1.append(f"        {vt}, intent(in) :: v")
    code1.append( "        type(${dual_type}$) :: res")

    u_type, v_type = num_types

    u = Symbol('u_x')
    v = Symbol('v_x')

    f = fun(u, v)

    if u_type == 'dual':
        du1 = Symbol('u_dx1')
    else:
        du1 = 0
    
    if v_type == 'dual':
        dv1 = Symbol('v_dx1')
    else:
        dv1 = 0

    if is_hyper_dual:
        if u_type == 'dual':
            du2 = Symbol('u_dx2')
        else:
            du2 = 0
        if v_type == 'dual':
            dv2 = Symbol('v_dx2')
        else:
            dv2 = 0

        if u_type == 'dual':
            ddu = Symbol('u_ddx')
        else:
            ddu = 0
        if v_type == 'dual':
            ddv = Symbol('v_ddx')
        else:
            ddv = 0

    x = Matrix([u, v])
    dx1 = Matrix([du1, dv1])
    if is_hyper_dual:
        dx2 = Matrix([du2, dv2])
        ddx = Matrix([ddu, ddv])

    df_dx = sp.diff(f, x)
    df1 = (df_dx.T*dx1)[0]
    if is_hyper_dual:
        # df2 = (df_dx.T*dx2)[0]
        ddf = (dx1.T*sp.hessian(f, x)*dx2 + sp.diff(f, x).T*ddx)[0]

    if is_hyper_dual:
        expr = (f, df1, ddf)
        assign_to = (f'res%x', f'res%dx', f'res%ddx(:, j)')
    else:
        expr = (f, df1)
        assign_to = (f'res%x', f'res%dx')

    code1, code2 = apply_cse_and_return_fcode(expr, assign_to, source_format='free',standard=95, skip_cse=skip_cse, code1=code1)

    if is_hyper_dual:
        code1.append(8*' '+ 'integer :: j')

    code22 = []
    for il in range(2):
        c = code2[il]
        c = 8*' ' + c
        c = replacements(c, u_type, v_type)
        code22.append(c)

    if is_hyper_dual:
        code22.append(8*' '+'do j = 1, ${number_of_derivatives}$')
        c = 12*' ' + code2[2]
        c = replacements(c, u_type, v_type)
        code22.append(c)
        code22.append( 8*' '+'end do')
    code22.append(6*' '+'#:if test_coverage == True')
    code22.append(8*' '+f'{fun.__name__}_{uts}_{vts}_counter = {fun.__name__}_{uts}_{vts}_counter + 1')
    code22.append(6*' '+'#:endif')
    code22.append('    end function')

    code = code + code1 + code22

    return code, expr

def binary_overload2(fun, tm, is_hyper_dual=True, arg_class=('dual','dual')):
    """Generate Fortran code for hyper-dual-number overloads of binary functions

    Parameters
    ------------
        fun: Function
            The binary function to overload
        is_hyper_dual: Logical
            If True, the function is overloaded for hyper-dual numbers instead of dual numbers
        arg_class: tuple of strings (size 2)
            The "class" of the argument. If not 'dual', *_dx1, *_dx2, and *_ddx are assumed to be zero.
    Returns
    ------------
        code: String
            The generated Fortran code
        expr: Tuple
            The sympy expressions that were generated

    """

    # Jinja2 template for binary function for dual numbers inside a fypp macro
    binary_fun_d = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}_{{vsn}}(u, v) result(res)
        {{u_type}}, intent(in) :: u
        {{v_type}}, intent(in) :: v
        {{res_type}} :: res
        {{temp_decl}}
        {{temp_assign}}
        {{x_assign}}
        {{dx_assign}}
      #:if test_coverage == True
        {{interface_name}}_{{usn}}_{{vsn}}_counter = {{interface_name}}_{{usn}}_{{vsn}}_counter + 1
      #:endif
    end function'''

    # Jinja2 template for a binary function for hyper-dual numbers inside a fypp macro
    binary_fun_hd = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}_{{vsn}}(u, v) result(res)
        {{u_type}}, intent(in) :: u
        {{v_type}}, intent(in) :: v
        {{res_type}} :: res
        integer :: j
        {{temp_decl}}
        {{temp_assign}}
        {{x_assign}}
        {{dx_assign}}
        do j = 1, ${number_of_derivatives}$
            {{ddx_assign}}
        end do
      #:if test_coverage == True
        {{interface_name}}_{{usn}}_{{vsn}}_counter = {{interface_name}}_{{usn}}_{{vsn}}_counter + 1
      #:endif
    end function'''

    u_class, v_class = arg_class

    # Create type string and type suffix for the u and v arguments
    u_type, usn, v_type, vsn = uv_types(arg_class, is_hyper_dual)
    res_type = 'type(${dual_type}$)'


    u = Symbol('u_x')
    v = Symbol('v_x')

    f = fun(u, v)

    if u_class == 'dual':
        du1 = Symbol('u_dx1')
    else:
        du1 = 0
    
    if v_class == 'dual':
        dv1 = Symbol('v_dx1')
    else:
        dv1 = 0

    if is_hyper_dual:
        if u_class == 'dual':
            du2 = Symbol('u_dx2')
        else:
            du2 = 0
        if v_class == 'dual':
            dv2 = Symbol('v_dx2')
        else:
            dv2 = 0

        if u_class == 'dual':
            ddu = Symbol('u_ddx')
        else:
            ddu = 0
        if v_class == 'dual':
            ddv = Symbol('v_ddx')
        else:
            ddv = 0

    x = Matrix([u, v])
    dx1 = Matrix([du1, dv1])
    if is_hyper_dual:
        dx2 = Matrix([du2, dv2])
        ddx = Matrix([ddu, ddv])

    df_dx = sp.diff(f, x)
    df1 = (df_dx.T*dx1)[0]
    if is_hyper_dual:
        # df2 = (df_dx.T*dx2)[0]
        ddf = (dx1.T*sp.hessian(f, x)*dx2 + sp.diff(f, x).T*ddx)[0]

    if is_hyper_dual:
        expr = (f, df1, ddf)
        assign_to = (f'res%x', f'res%dx', f'res%ddx(:, j)')
    else:
        expr = (f, df1)
        assign_to = (f'res%x', f'res%dx')

    # code1, code2 = apply_cse_and_return_fcode(expr, assign_to, source_format='free',standard=95, skip_cse=skip_cse, code1=code1)

    rvar, rexpr = sp.cse(sp.simplify(expr),
        order='none', list=False, symbols=sp.utilities.iterables.numbered_symbols(prefix='t', start=0), 
        ignore=(du1, du2, ddu, dv1, dv2, ddv), optimizations='basic')
    
    # Decalaration of temporary variables
    temp_decl = 'real(dp) :: ' + ','.join(str(var) for (var, _) in rvar)

    # Set temporary variables
    temp_set = ''
    for var, var_expr in rvar:
        temp_set = temp_set + replacements(fcode(var_expr,assign_to=var,source_format='free',standard=95) + '\n', u_class, v_class)

    x_assign = replacements(fcode(rexpr[0], assign_to=assign_to[0], source_format='free',standard=95), u_class, v_class)
    dx_assign = replacements(fcode(rexpr[1], assign_to=assign_to[1], source_format='free',standard=95), u_class, v_class)
    if is_hyper_dual:
        ddx_assign = replacements(fcode(rexpr[2], assign_to=assign_to[2], source_format='free',standard=95), u_class, v_class)

    if is_hyper_dual:
        code = Template(binary_fun_hd).render(
            interface_name = fun.__name__,
            usn = usn,
            vsn = vsn,
            u_type = u_type,
            v_type = v_type,
            res_type = res_type,
            temp_decl = temp_decl,
            temp_assign = temp_set,
            x_assign = x_assign,
            dx_assign = dx_assign,
            ddx_assign = ddx_assign)
    else:
        code = Template(binary_fun_d).render(
            interface_name = fun.__name__,
            usn = usn,
            vsn = vsn,
            u_type = u_type,
            v_type = v_type,
            res_type = res_type,
            temp_decl = temp_decl,
            temp_assign = temp_set,
            x_assign = x_assign,
            dx_assign = dx_assign)


    return code

def unary_overload(fun, is_hyper_dual=True, skip_cse=False, u_type='dual', code=[]):
    """Generate Fortran code for hyper-dual-number overloads of unary functions

    Parameters
    ------------
        fun: Function
            The unary function to overload
        is_hyper_dual: Logical
            If True, the function is overloaded for hyper-dual numbers instead of dual numbers
        skip_cse: Logical
            Skip common subexpression elimination if True
        u_type: String
            The type of the argument. If not 'dual', *_dx1, *_dx2, and *_ddx are assumed to be zero.
    Returns
    ------------
        code: String
            The generated Fortran code
        expr: Tuple
            The sympy expressions that were generated

    """

    # Create type string and type suffix for the u argument
    if u_type == 'dual':
        ut = "type(${dual_type}$)" 
        if is_hyper_dual:
            uts = 'hd'
        else:
            uts = 'd'
    elif u_type == 'real':
        ut = "real(${real_kind}$)"
        uts = 'r'
    elif u_type == 'integer':
        ut = "integer"
        uts = 'i'

    elemental_purity = "${elemental_purity}$"
    code1 = [f'    {elemental_purity} function {fun.__name__}_{uts}(u) result(res)']
    code1.append(f"        {ut}, intent(in) :: u")
    code1.append( "        type(${dual_type}$) :: res")

    u = Symbol('u_x')

    f = fun(u)

    if u_type == 'dual':
        du1 = Symbol('u_dx1')
    else:
        du1 = 0

    if is_hyper_dual:
        if u_type == 'dual':
            du2 = Symbol('u_dx2')
        else:
            du2 = 0

        if u_type == 'dual':
            ddu = Symbol('u_ddx')
        else:
            ddu = 0

    df_dx = sp.diff(f, u)
    df1 = df_dx*du1
    if is_hyper_dual:
        # df2 = (df_dx.T*dx2)[0]
        ddf = du1*sp.diff(sp.diff(f, u), u)*du2 + sp.diff(f, u)*ddu

    if is_hyper_dual:
        expr = (f, df1, ddf)
        assign_to = (f'res%x', f'res%dx', f'res%ddx(:, j)')
    else:
        expr = (f, df1)
        assign_to = (f'res%x', f'res%dx')

    code1, code2 = apply_cse_and_return_fcode(expr, assign_to, source_format='free',standard=95, skip_cse=skip_cse, code1=code1)

    if is_hyper_dual:
        code1.append(8*' '+ 'integer :: j')

    code22 = []
    for il in range(2):
        c = code2[il]
        c = 8*' ' + c
        c = replacements(c, u_type)
        code22.append(c)

    if is_hyper_dual:
        code22.append(8*' '+'do j = 1, ${number_of_derivatives}$')
        c = 12*' ' + code2[2]
        c = replacements(c, u_type)
        code22.append(c)
        code22.append( 8*' '+'end do')
    code22.append(6*' '+'#:if test_coverage == True')
    code22.append(8*' '+f'{fun.__name__}_{uts}_counter = {fun.__name__}_{uts}_counter + 1')
    code22.append(6*' '+'#:endif')
    code22.append('    end function')

    code = code + code1 + code22

    return code, expr

def apply_cse_and_return_fcode(expr,assign_to,source_format='free',standard=95, skip_cse=False, code1=[]):
    """Apply common subexpression simplification and return Fortran code
    
    Input:
        expr - a list of expressions we would like to evaluate
        assign_to - the symbols we would like to assign them to (either a tuple of strings or sp.symbols)
    
    Output:
        String containing subroutine or function body
    """


    code2 = [] # Dual type assignments

    if skip_cse:
        rexpr = sp.simplify(expr)
    else:
        rvar, rexpr = sp.cse(sp.simplify(expr),
            order='none',
            list=False)

        if len(rvar) > 0: 
            code1.append(8*' ' + 'real(dp) :: ' + ','.join(str(var) for (var, _) in rvar))

        for var, var_expr in rvar:
            code1.append(8*' ' + fcode(var_expr,assign_to=var,source_format=source_format,standard=standard))
    

    for var, var_expr in zip(assign_to,rexpr):
        code2.append(fcode(var_expr,assign_to=var,source_format=source_format,standard=standard))

    return code1, code2