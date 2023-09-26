all_interfaces =   ['=', '+', '-', '*', '/', '**', '==', '<=', '<', '>=', '>', '/=', 'abs', 'dabs', 'acos', 'asin', 'atan', 'atan2', 'cos', 'dcos',
       'dot_product', 'exp', 'int', 'log', 'log10', 'matmul', 'max', 'dmax1', 'maxval', 'min', 'dmin1', 'minval', 'nint', 'sign',
       'sin', 'dsin', 'tan', 'dtan', 'sqrt', 'sum', 'maxloc']

# All implementations
all_implementations = [
    'assign_d_i', 'assign_d_r', 'assign_i_d', 'assign_r_d', 'assign_hd_i', 'assign_hd_r', 'assign_i_hd', 'assign_r_hd', 
    'unary_add_d', 'add_d_d', 'add_d_i', 'add_d_r', 'add_i_d', 'add_r_d', 'unary_add_hd', 'add_hd_hd', 'add_hd_i', 'add_hd_r', 'add_i_hd', 'add_r_hd', 
    'unary_minus_d', 'minus_d_d', 'minus_d_i', 'minus_d_r', 'minus_i_d', 'minus_r_d', 'unary_minus_hd', 'minus_hd_hd', 'minus_hd_i', 'minus_hd_r', 'minus_i_hd', 'minus_r_hd', 
    'mult_d_d', 'mult_d_i', 'mult_d_r', 'mult_i_d', 'mult_r_d', 'mult_hd_hd', 'mult_hd_i', 'mult_hd_r', 'mult_i_hd', 'mult_r_hd', 
    'div_d_d', 'div_d_r', 'div_r_d', 'div_d_i', 'div_i_d', 'div_hd_hd', 'div_hd_r', 'div_r_hd', 'div_hd_i', 'div_i_hd', 
    'pow_d_i', 'pow_d_r', 'pow_d_d', 'pow_hd_i', 'pow_hd_r', 'pow_hd_hd', 
    'eq_dd', 'eq_di', 'eq_dr', 'eq_id', 'eq_rd', 
    'le_dd', 'le_di', 'le_dr', 'le_id', 'le_rd', 
    'lt_dd', 'lt_di', 'lt_dr', 'lt_id', 'lt_rd', 
    'ge_dd', 'ge_di', 'ge_dr', 'ge_id', 'ge_rd', 
    'gt_dd', 'gt_di', 'gt_dr', 'gt_id', 'gt_rd', 
    'ne_dd', 'ne_di', 'ne_dr', 'ne_id', 'ne_rd', 
    'abs_d', 
    'acos_d', 
    'asin_d', 
    'atan_d', 
    'atan2_d', 
    'cos_d', 
    'dot_product_dd', 
    'exp_d', 
    'int_d', 
    'log_d', 'log_hd',
    'log10_d', 
    'matmul_dd', 'matmul_dv', 'matmul_vd', 
    'max_dd', 'max_di', 'max_dr', 'max_rd', 
    'dmax1_dd', 
    'maxval_d', 
    'min_dd', 'min_dr', 
    'dmin1_dd', 
    'minval_d', 
    'nint_d', 
    'sin_d', 
    'tan_d', 
    'sqrt_d', 'sqrt_hd',
    'sum_d', 
    'maxloc_d']

def select_interfaces(selected_interfaces):
    if selected_interfaces == "--all":
        return all_interfaces
    else:
        # Convert comma-separated string to list
        return selected_interfaces.replace(" ","").split(",")
    
def decode_type_spec(type_spec):
    if len(type_spec) > 2 and type_spec[-2:] == ":h":
        type_suffix = type_spec[0:-2]
        has_hdual = True
    else:
        type_suffix = type_spec
        has_hdual = False
    return type_suffix, has_hdual

def decode_type_specs(type_specs):
    dual_type = []
    dual_sn = []
    hdual_type = []
    hdual_sn = []
    for type_spec in type_specs:
        type_suffix, has_hdual = decode_type_spec(type_spec)
        dual_sn.append("d" + type_suffix)
        dual_type.append("dual__" + type_suffix + "_t")
        if has_hdual:
            hdual_sn.append("hd" + type_suffix)
            hdual_type.append("hdual__" + type_suffix + "_t")
    return dual_type, dual_sn, hdual_type, hdual_sn

def decode_chain_types_spec(str):
    s, has_hdual = decode_type_spec(str)
    if "->" not in s:
        raise ValueError("Missing '->' in chain_types string representation")
    lst = s.split("->")
    if len(lst) != 2:
        raise ValueError("Too many '->' in chain_types string representation")
    return lst[0], lst[1], has_hdual
