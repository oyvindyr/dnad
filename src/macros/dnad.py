all_interfaces =   ['=', '+', '-', '*', '/', '**', '==', '<=', '<', '>=', '>', '/=', 'abs', 'dabs', 'acos', 'asin', 'atan', 'atan2', 'cos', 'dcos',
       'dot_product', 'exp', 'int', 'log', 'log10', 'matmul', 'max', 'dmax1', 'maxval', 'min', 'dmin1', 'minval', 'nint', 'sign',
       'sin', 'dsin', 'tan', 'dtan', 'sqrt', 'fastsqrt', 'sum', 'maxloc']

def select_interfaces(selected_interfaces):
    if selected_interfaces == "--all":
        return all_interfaces
    else:
        # Convert comma-separated string to list
        return selected_interfaces.replace(" ","").split(",")

