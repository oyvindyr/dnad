all_interfaces =   ['=', '+', '-', '*', '/', '**', '==', '<=', '<', '>=', '>', '/=', 'abs', 'dabs', 'acos', 'asin', 'atan', 'atan2', 'cos', 'dcos',
       'dot_product', 'exp', 'int', 'log', 'log10', 'matmul', 'max', 'dmax1', 'maxval', 'min', 'dmin1', 'minval', 'nint', 'sign',
       'sin', 'dsin', 'tan', 'dtan', 'sqrt', 'sum', 'maxloc']

all_implementations = [
    'assign__d_i', 'assign__d_r', 'assign__i_d', 'assign__r_d', 'assign__hd_i', 'assign__hd_r', 'assign__i_hd', 'assign__r_hd', 
    'add__d', 'add__d_d', 'add__d_i', 'add__d_r', 'add__i_d', 'add__r_d', 'add__hd', 'add__hd_hd', 'add__hd_i', 'add__hd_r', 'add__i_hd', 'add__r_hd', 
    'minus__d', 'minus__d_d', 'minus__d_i', 'minus__d_r', 'minus__i_d', 'minus__r_d', 'minus__hd', 'minus__hd_hd', 'minus__hd_i', 'minus__hd_r', 'minus__i_hd', 'minus__r_hd', 
    'mult__d_d', 'mult__d_i', 'mult__d_r', 'mult__i_d', 'mult__r_d', 'mult__hd_hd', 'mult__hd_i', 'mult__hd_r', 'mult__i_hd', 'mult__r_hd', 
    'div__d_d', 'div__d_r', 'div__r_d', 'div__d_i', 'div__i_d', 'div__hd_hd', 'div__hd_r', 'div__r_hd', 'div__hd_i', 'div__i_hd', 
    'pow__d_i', 'pow__d_r', 'pow__d_d', 'pow__hd_i', 'pow__hd_r', 'pow__hd_hd', 
    'eq__d_d', 'eq__d_i', 'eq__d_r', 'eq__i_d', 'eq__r_d', 
    'le__d_d', 'le__d_i', 'le__d_r', 'le__i_d', 'le__r_d', 
    'lt__d_d', 'lt__d_i', 'lt__d_r', 'lt__i_d', 'lt__r_d', 
    'ge__d_d', 'ge__d_i', 'ge__d_r', 'ge__i_d', 'ge__r_d', 
    'gt__d_d', 'gt__d_i', 'gt__d_r', 'gt__i_d', 'gt__r_d', 
    'ne__d_d', 'ne__d_i', 'ne__d_r', 'ne__i_d', 'ne__r_d', 
    'eq__hd_hd', 'eq__hd_i', 'eq__hd_r', 'eq__i_hd', 'eq__r_hd', 
    'le__hd_hd', 'le__hd_i', 'le__hd_r', 'le__i_hd', 'le__r_hd', 
    'lt__hd_hd', 'lt__hd_i', 'lt__hd_r', 'lt__i_hd', 'lt__r_hd', 
    'ge__hd_hd', 'ge__hd_i', 'ge__hd_r', 'ge__i_hd', 'ge__r_hd', 
    'gt__hd_hd', 'gt__hd_i', 'gt__hd_r', 'gt__i_hd', 'gt__r_hd', 
    'ne__hd_hd', 'ne__hd_i', 'ne__hd_r', 'ne__i_hd', 'ne__r_hd', 
    'abs__d', 
    'acos__d', 
    'asin__d', 
    'atan__d', 
    'atan2__d', 
    'cos__d', 
    'dot_product__dd', 
    'exp__d', 
    'int__d', 
    'log__d', 'log__hd',
    'log10__d', 
    'matmul__dd', 'matmul__dv', 'matmul__vd', 
    'max__dd', 'max__di', 'max__dr', 'max__rd', 
    'dmax1__dd', 
    'maxval__d', 
    'min__dd', 'min__dr', 
    'dmin1__dd', 
    'minval__d', 
    'nint__d', 
    'sin__d', 
    'tan__d', 
    'sqrt__d', 'sqrt__hd',
    'sum__d', 
    'maxloc__d']

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
    dual_types = []
    dual_shortnames = []
    hdual_types = []
    hdual_shortnames = []
    for type_spec in type_specs:
        type_suffix, has_hdual = decode_type_spec(type_spec)
        dual_shortnames.append("d" + type_suffix)
        dual_types.append("dual__" + type_suffix + "_t")
        if has_hdual:
            hdual_shortnames.append("hd" + type_suffix)
            hdual_types.append("hdual__" + type_suffix + "_t")
    return dual_types, dual_shortnames, hdual_types, hdual_shortnames

def decode_chain_types_spec(str):
    s, has_hdual = decode_type_spec(str)
    if "->" not in s:
        raise ValueError("Missing '->' in chain_types string representation")
    lst = s.split("->")
    if len(lst) != 2:
        raise ValueError("Too many '->' in chain_types string representation")
    return lst[0], lst[1], has_hdual
