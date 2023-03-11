from sympy.matrices import Matrix
from sympy import Symbol
import sympy as sp
from sympy.printing.fortran import fcode
import sympy.printing as printing
from jinja2 import Template
import os

_literal_replacements = {
    '(1.0d0/2.0d0)' : '0.5_${real_kind}$',
    '1.0d0/4.0d0'   : '0.25_${real_kind}$',
    '(3.0d0/2.0d0)' : '1.5_${real_kind}$',
    '1d0'           : '1.0_${real_kind}$',
    '0d0'           : '0.0_${real_kind}$'
}

# Jinja2 template for a fypp macro
_macro_template = '''
#:def {{interface_name}}_{{sn}}(dual_type, real_kind, size_dx, is_pure, test_coverage)
    #:if is_pure and not test_coverage
        #:set elemental_purity = "elemental"
    #:else
        #:set elemental_purity = "impure elemental"
    #:endif
    {%- for fun in funs -%}
        {{fun}}
    {%- endfor %}
#:enddef'''

# Jinja2 template for binary function for dual numbers inside a fypp macro
_binary_fun_d_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}_{{vsn}}(u, v) result(res)
        {{u_type}}, intent(in) :: u
        {{v_type}}, intent(in) :: v
        {{res_type}} :: res
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
      #:if test_coverage
        {{interface_name}}_{{usn}}_{{vsn}}_counter = {{interface_name}}_{{usn}}_{{vsn}}_counter + 1
      #:endif
    end function'''

# Jinja2 template for binary function for dual numbers inside a fypp macro
_unary_fun_d_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}(u) result(res)
        {{u_type}}, intent(in) :: u
        {{res_type}} :: res
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
      #:if test_coverage
        {{interface_name}}_{{usn}}_counter = {{interface_name}}_{{usn}}_counter + 1
      #:endif
    end function'''

# Jinja2 template for a binary function for hyper-dual numbers inside a fypp macro
_binary_fun_hd_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}_{{vsn}}(u, v) result(res)
        {{u_type}}, intent(in) :: u
        {{v_type}}, intent(in) :: v
        {{res_type}} :: res
        integer :: i, j, k
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
        k = 0
        do j = 1, ${size_dx}$
            do i = j, ${size_dx}$
                k = k + 1
                {{ddx_assign}}
            end do
        end do
      #:if test_coverage
        {{interface_name}}_{{usn}}_{{vsn}}_counter = {{interface_name}}_{{usn}}_{{vsn}}_counter + 1
      #:endif
    end function'''

# Jinja2 template for a unary function for hyper-dual numbers inside a fypp macro
_unary_fun_hd_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}(u) result(res)
        {{u_type}}, intent(in) :: u
        {{res_type}} :: res
        integer :: i, j, k
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
        k = 0
        do j = 1, ${size_dx}$
            do i = j, ${size_dx}$
                k = k + 1
                {{ddx_assign}}
            end do
        end do
      #:if test_coverage
        {{interface_name}}_{{usn}}_counter = {{interface_name}}_{{usn}}_counter + 1
      #:endif
    end function'''

# Jinja2 template for a binary function for hyper-dual numbers inside a fypp macro
#  Version without loop. Applies when ddx is not dependent on dx(i) and dx(j)
_binary_fun_hd_noloop_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}_{{vsn}}(u, v) result(res)
        {{u_type}}, intent(in) :: u
        {{v_type}}, intent(in) :: v
        {{res_type}} :: res
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
        {{ddx_assign}}
      #:if test_coverage
        {{interface_name}}_{{usn}}_{{vsn}}_counter = {{interface_name}}_{{usn}}_{{vsn}}_counter + 1
      #:endif
    end function'''

# Jinja2 template for a unary function for hyper-dual numbers inside a fypp macro
#  Version without loop. Applies when ddx is not dependent on dx(i) and dx(j)
_unary_fun_hd_noloop_template = '''
    ${elemental_purity}$ function {{interface_name}}_{{usn}}(u) result(res)
        {{u_type}}, intent(in) :: u
        {{res_type}} :: res
        {% if temp_assigns|length > 0 -%}
        {{temp_decl}}
        {% for temp_assign in temp_assigns %}
        {{temp_assign}}
        {%- endfor -%}
        {%- endif %}
        {{x_assign}}
        {{dx_assign}}
        {{ddx_assign}}
      #:if test_coverage
        {{interface_name}}_{{usn}}_counter = {{interface_name}}_{{usn}}_counter + 1
      #:endif
    end function'''

def generate_macros():

    def unary_add(u):
        return u
    def unary_minus(u):
        return -u
    def add(u, v):
        return u + v
    def minus(u, v):
        return u - v
    def mult(u, v):
        return u*v
    def div(u, v):
        return u/v
    def pow(u, v):
        return u**v
    
    is_hyper_dual_v = [False, True]
    dual_type_short_name_v = ['d', 'hd']

    funs = add, minus, mult, div

    for fun in funs:
        macro_code = ''
        for is_hyper_dual, sn in zip(is_hyper_dual_v, dual_type_short_name_v):

            if fun.__name__ == 'add':
                funs_code = 6*[None]
                funs_code[0] = unary_overload(unary_add, is_hyper_dual=is_hyper_dual, u_class='dual')
                i0 = 0
            elif fun.__name__ == 'minus':
                funs_code = 6*[None]
                funs_code[0] = unary_overload(unary_minus, is_hyper_dual=is_hyper_dual, u_class='dual')
                i0 = 0
            else:
                funs_code = 5*[None]
                i0 = -1
            funs_code[i0+1] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','dual'))
            funs_code[i0+2] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','real'))
            funs_code[i0+3] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('real','dual'))
            funs_code[i0+4] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','integer'))
            funs_code[i0+5] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('integer','dual'))

            macro_code += Template(_macro_template).render(
                interface_name=fun.__name__,
                sn=sn,
                funs=funs_code) + '\n'

        finish_and_write_macro(fun.__name__, macro_code)

    fun = pow
    macro_code = ''
    for is_hyper_dual, sn in zip(is_hyper_dual_v, dual_type_short_name_v):
        funs_code = 3*[None]
        funs_code[0] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','integer'))
        funs_code[1] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','real'))
        funs_code[2] = binary_overload(fun, is_hyper_dual=is_hyper_dual, arg_class=('dual','dual'))

        macro_code += Template(_macro_template).render(
            interface_name=fun.__name__,
            sn=sn,
            funs=funs_code) + '\n'

    finish_and_write_macro(fun.__name__, macro_code)

    
    unary_overload_and_write(sp.sqrt, is_hyper_dual_v, dual_type_short_name_v)
    unary_overload_and_write(sp.log , is_hyper_dual_v, dual_type_short_name_v)

def unary_overload_and_write(fun, is_hyper_dual_v, dual_type_short_name_v):
    macro_code = ''
    for is_hyper_dual, sn in zip(is_hyper_dual_v, dual_type_short_name_v):
        funs_code = 1*[None]
        funs_code[0] = unary_overload(fun, is_hyper_dual=is_hyper_dual, u_class='dual')

        macro_code += Template(_macro_template).render(
            interface_name=fun.__name__,
            sn=sn,
            funs=funs_code) + '\n'

    finish_and_write_macro(fun.__name__, macro_code)

def finish_and_write_macro(name, macro_code):
    
    macro_code = replace_literals(macro_code)

    fid = open(os.path.dirname(os.path.abspath(__file__)) + f'\\macros\\implementations\\{name}_gen.fypp', 'w')
    fid.write(macro_code + '\n')
    fid.close()

def replace_literals(code):
    for k, v in _literal_replacements.items():
        code = code.replace(k, v)
    return code

def replacements_x(c, u_type, v_type=None):
    if u_type == 'dual':
        c = c.replace('u_x', f'u%x')
    else:
        c = c.replace('u_x', 'u')

    if v_type is not None:
        if v_type == 'dual':
            c = c.replace('v_x', f'v%x')
        else:
            c = c.replace('v_x', 'v')
    return c

def replacements_dx(c, u_type, v_type=None):
    if u_type == 'dual':
        c = c.replace('u_x', f'u%x')
        c = c.replace('u_dx1', f'u%dx') # Vector
    else:
        c = c.replace('u_x', 'u')

    if v_type is not None:
        if v_type == 'dual':
            c = c.replace('v_x', f'v%x')
            c = c.replace('v_dx1', f'v%dx') # Vector
        else:
            c = c.replace('v_x', 'v')
    return c

def replacements_ddx(c, u_type, v_type=None):
    if "u_dx1" in c or "v_dx1" in c or "u_dx2" in c or "v_dx2" in c:
        has_loop = True
        return has_loop, replacements_ddx_with_loop(c, u_type, v_type)
    else:
        has_loop = False
        return has_loop, replacements_ddx_no_loop(c, u_type, v_type)

def replacements_ddx_with_loop(c, u_type, v_type):
    if u_type == 'dual':
        c = c.replace('u_x', f'u%x')
        c = c.replace('u_dx1', f'u%dx(i)')
    else:
        c = c.replace('u_x', 'u')

    if v_type is not None:
        if v_type == 'dual':
            c = c.replace('v_x', f'v%x')
            c = c.replace('v_dx1', f'v%dx(i)')
        else:
            c = c.replace('v_x', 'v')

    c = c.replace('u_dx2', f'u%dx(j)')
    c = c.replace('u_ddx', f'u%ddx(k)')
    if v_type is not None:
        c = c.replace('v_dx2', f'v%dx(j)')
        c = c.replace('v_ddx', f'v%ddx(k)')
    return c

def replacements_ddx_no_loop(c, u_type, v_type):
    if u_type == 'dual':
        c = c.replace('u_x', f'u%x')
    else:
        c = c.replace('u_x', 'u')

    if v_type is not None:
        if v_type == 'dual':
            c = c.replace('v_x', f'v%x')
        else:
            c = c.replace('v_x', 'v')

    c = c.replace('u_ddx', f'u%ddx')
    if v_type is not None:
        c = c.replace('v_ddx', f'v%ddx')
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

def binary_overload(fun, is_hyper_dual=True, arg_class=('dual','dual')):
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

    u_class, v_class = arg_class

    # Create type string and type suffix for the u and v arguments
    u_type, usn, v_type, vsn = uv_types(arg_class, is_hyper_dual)
    res_type = 'type(${dual_type}$)'

    if u_class == "integer":
        u_assumption = {'integer':True}
    else:
        u_assumption = {'real':True}

    if v_class == "integer":
        v_assumption = {'integer':True}
    else:
        v_assumption = {'real':True}


    u = Symbol('u_x', **u_assumption)
    v = Symbol('v_x', **v_assumption)

    f = fun(u, v)

    ignore = []
    if u_class == 'dual':
        du1 = Symbol('u_dx1', **u_assumption)
        ignore.append(du1)
    else:
        du1 = 0
    
    if v_class == 'dual':
        dv1 = Symbol('v_dx1', **v_assumption)
        ignore.append(dv1)
    else:
        dv1 = 0

    if is_hyper_dual:
        if u_class == 'dual':
            du2 = Symbol('u_dx2', **u_assumption)
            ignore.append(du2)
        else:
            du2 = 0

        if v_class == 'dual':
            dv2 = Symbol('v_dx2', **v_assumption)
            ignore.append(dv2)
        else:
            dv2 = 0

        if u_class == 'dual':
            ddu = Symbol('u_ddx', **u_assumption)
            ignore.append(ddu)
        else:
            ddu = 0
        if v_class == 'dual':
            ddv = Symbol('v_ddx', **v_assumption)
            ignore.append(ddv)
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
        assign_to = (f'res%x', f'res%dx', f'res%ddx(k)')
    else:
        expr = (f, df1)
        assign_to = (f'res%x', f'res%dx')

    # code1, code2 = apply_cse_and_return_fcode(expr, assign_to, source_format='free',standard=95, skip_cse=skip_cse, code1=code1)

    rvar, rexpr = sp.cse(sp.simplify(expr),
        order='none', list=False, symbols=sp.utilities.iterables.numbered_symbols(prefix='t', start=0), 
        ignore=ignore, optimizations='basic')
    
    # Decalaration of temporary variables
    if len(rvar) > 0:
        temp_decl = 'real(${real_kind}$) :: ' + ','.join(str(var) for (var, _) in rvar)
    else:
        temp_decl = ''

    # Set temporary variables
    temp_assigns = []
    for var, var_expr in rvar:
        temp_assigns.append(replacements_x(fcode(var_expr,assign_to=var,source_format='free',standard=95), u_class, v_class))

    x_assign = replacements_x(fcode(rexpr[0], assign_to=assign_to[0], source_format='free',standard=95), u_class, v_class)
    dx_assign = replacements_dx(fcode(rexpr[1], assign_to=assign_to[1], source_format='free',standard=95), u_class, v_class)
    if is_hyper_dual:
        has_loop, ddx_assign = replacements_ddx(fcode(rexpr[2], assign_to=assign_to[2], source_format='free',standard=95), u_class, v_class)
        if not has_loop:
            ddx_assign = ddx_assign.replace(f'res%ddx(k)', f'res%ddx')

    if is_hyper_dual:
        if has_loop:
            code = Template(_binary_fun_hd_template).render(
                interface_name = fun.__name__,
                usn = usn,
                vsn = vsn,
                u_type = u_type,
                v_type = v_type,
                res_type = res_type,
                temp_decl = temp_decl,
                temp_assigns = temp_assigns,
                x_assign = x_assign,
                dx_assign = dx_assign,
                ddx_assign = ddx_assign)
        else:
            code = Template(_binary_fun_hd_noloop_template).render(
                interface_name = fun.__name__,
                usn = usn,
                vsn = vsn,
                u_type = u_type,
                v_type = v_type,
                res_type = res_type,
                temp_decl = temp_decl,
                temp_assigns = temp_assigns,
                x_assign = x_assign,
                dx_assign = dx_assign,
                ddx_assign = ddx_assign)
    else:
        code = Template(_binary_fun_d_template).render(
            interface_name = fun.__name__,
            usn = usn,
            vsn = vsn,
            u_type = u_type,
            v_type = v_type,
            res_type = res_type,
            temp_decl = temp_decl,
            temp_assigns = temp_assigns,
            x_assign = x_assign,
            dx_assign = dx_assign)


    return code

def unary_overload(fun, is_hyper_dual=True, u_class='dual'):
    """Generate Fortran code for hyper-dual-number overloads of unary functions

    Parameters
    ------------
        fun: Function
            The unary function to overload
        is_hyper_dual: Logical
            If True, the function is overloaded for hyper-dual numbers instead of dual numbers
        arg_class: str
            The "class" of the argument. If not 'dual', *_dx1, *_dx2, and *_ddx are assumed to be zero.
    Returns
    ------------
        code: String
            The generated Fortran code
    """

    if u_class == "integer":
        u_assumption = {'integer':True}
    else:
        u_assumption = {'real':True}

    # Create type string and type suffix for the u argument
    if u_class == 'dual':
        u_type = "type(${dual_type}$)" 
        if is_hyper_dual:
            usn = 'hd'
        else:
            usn = 'd'
    elif u_class == 'real':
        u_type = "real(${real_kind}$)"
        usn = 'r'
    elif u_class == 'integer':
        u_type = "integer"
        usn = 'i'

    res_type = 'type(${dual_type}$)'

    u = Symbol('u_x', **u_assumption)

    f = fun(u)

    ignore = []
    if u_class == 'dual':
        du1 = Symbol('u_dx1', **u_assumption)
        ignore.append(du1)
    else:
        du1 = 0

    if is_hyper_dual:
        if u_class == 'dual':
            du2 = Symbol('u_dx2', **u_assumption)
            ignore.append(du2)
        else:
            du2 = 0

        if u_class == 'dual':
            ddu = Symbol('u_ddx', **u_assumption)
            ignore.append(ddu)
        else:
            ddu = 0

    df_dx = sp.diff(f, u)
    df1 = df_dx*du1
    if is_hyper_dual:
        ddf = du1*sp.diff(sp.diff(f, u), u)*du2 + sp.diff(f, u)*ddu

    if is_hyper_dual:
        expr = (f, df1, ddf)
        assign_to = (f'res%x', f'res%dx', f'res%ddx(k)')
    else:
        expr = (f, df1)
        assign_to = (f'res%x', f'res%dx')

    rvar, rexpr = sp.cse(sp.simplify(expr),
        order='none', list=False, symbols=sp.utilities.iterables.numbered_symbols(prefix='t', start=0), 
        ignore=ignore, optimizations='basic')
    
    # Decalaration of temporary variables
    if len(rvar) > 0:
        temp_decl = 'real(${real_kind}$) :: ' + ','.join(str(var) for (var, _) in rvar)
    else:
        temp_decl = ''

    # Set temporary variables
    temp_assigns = []
    for var, var_expr in rvar:
        temp_assigns.append(replacements_x(fcode(var_expr,assign_to=var,source_format='free',standard=95), u_class))

    x_assign = replacements_x(fcode(rexpr[0], assign_to=assign_to[0], source_format='free',standard=95), u_class)
    dx_assign = replacements_dx(fcode(rexpr[1], assign_to=assign_to[1], source_format='free',standard=95), u_class)
    if is_hyper_dual:
        has_loop, ddx_assign = replacements_ddx(fcode(rexpr[2], assign_to=assign_to[2], source_format='free',standard=95), u_class)
        if not has_loop:
            ddx_assign = ddx_assign.replace(f'res%ddx(k)', f'res%ddx')

    if is_hyper_dual:
        if has_loop:
            code = Template(_unary_fun_hd_template).render(
                interface_name = fun.__name__,
                usn = usn,
                u_type = u_type,
                res_type = res_type,
                temp_decl = temp_decl,
                temp_assigns = temp_assigns,
                x_assign = x_assign,
                dx_assign = dx_assign,
                ddx_assign = ddx_assign)
        else:
            code = Template(_unary_fun_hd_noloop_template).render(
                interface_name = fun.__name__,
                usn = usn,
                u_type = u_type,
                res_type = res_type,
                temp_decl = temp_decl,
                temp_assigns = temp_assigns,
                x_assign = x_assign,
                dx_assign = dx_assign,
                ddx_assign = ddx_assign)
    else:
        code = Template(_unary_fun_d_template).render(
            interface_name = fun.__name__,
            usn = usn,
            u_type = u_type,
            res_type = res_type,
            temp_decl = temp_decl,
            temp_assigns = temp_assigns,
            x_assign = x_assign,
            dx_assign = dx_assign)


    return code

if __name__ == "__main__":
    generate_macros()