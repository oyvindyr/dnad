
# Module variables
_Type_def_code = None
_Interface_code = None
_Interface_routines = None
_Routine_code = None

_dual_type_name = "dual"
_real_kind_name = "real_kind"
_number_of_derivatives_name = "number_of_derivatives"

def read_file(fname):

    global _Type_def_code
    global _Interface_code
    global _Interface_routines
    global _Routine_code

    Lines = open(fname, 'r').readlines()
    _Type_def_code, _Interface_routines, _Interface_code, _Routine_code = read_blocks(Lines)
    
    return "dummy string"

def write_type_def_to_str(dual_type_name, real_kind_name, number_of_derivatives_name):

    Type_def_code = _Type_def_code
    Type_def_code[0] = Type_def_code[0].rstrip() + " ! Auto-generated interface. Do not edit. \n"
    s = "".join(Type_def_code)
    s = s.replace(_dual_type_name, dual_type_name)
    s = s.replace(_real_kind_name, real_kind_name)
    s = s.replace(_number_of_derivatives_name, number_of_derivatives_name)
    return s


def write_interfaces_to_str(selected_interfaces):

    Interface_code = _Interface_code

    if type(selected_interfaces) == str:
        if selected_interfaces == '--all':
            selected_interfaces = list(Interface_code.keys())
        else:
            # Split into list with comma as separator
            selected_interfaces = selected_interfaces.replace(" ","").split(',')
        
    print(selected_interfaces)
    # Write selected interfaces
    s = ""
    for interface in selected_interfaces: # Loop over selected interfaces
        Interface_code[interface][0] = Interface_code[interface][0].rstrip() + " ! Auto-generated interface. Do not edit. \n"
        s += "".join(Interface_code[interface])
    return s


def write_interface_routines_to_str(selected_interfaces, dual_type_name, real_kind_name, number_of_derivatives_name):

    Interface_routines = _Interface_routines
    Routine_code = _Routine_code

    if type(selected_interfaces) == str:
        if selected_interfaces == '--all':
            selected_interfaces = list(Interface_routines.keys())
        else:
            # Split into list with comma as separator
            selected_interfaces = selected_interfaces.replace(" ","").split(',')


    s = ""
    for interface in selected_interfaces: # Loop over selected interfaces
        s += "    ! ------------------------------------------------------------------------------------------------------------------------------------ \n"
        s += "    ! Members of "+interface+ " interface. (Auto-generated routines. Do not edit.) \n"
        s += "    ! ------------------------------------------------------------------------------------------------------------------------------------ \n"
        for routine in Interface_routines[interface]: # Loop over routines in current interface
            Routine_code[routine][0] = Routine_code[routine][0].rstrip() + " ! Auto-generated routine. Do not edit. \n"
            s += "".join(Routine_code[routine])
    
    # Replace names:
    s = s.replace("type("+_dual_type_name+")", "type("+dual_type_name+")")
    s = s.replace(_real_kind_name, real_kind_name)
    s = s.replace(_number_of_derivatives_name, number_of_derivatives_name)
    return s




def read_blocks(Lines):

    #filename_fpp = "dnad_mod.fpp"
    #file_fpp = open(filename_fpp, 'r')
    #Lines = file_fpp.readlines()

    Type_def_code = []

    Interface_code = {}
    Interface_routines = {}
    
    Routine_code = {}

    found_type_def = False

    j = -1
    while j < len(Lines) - 1:

        j = j + 1

        line = Lines[j]
        words = line.split()

        # Look for beginning of type definition
        if not found_type_def and len(words)>0 and words[0] == "type":
            found_type_def = True
            while not line.replace(" ","").startswith("endtype"):
                Type_def_code += [line]
                j = j + 1
                line = Lines[j]
            Type_def_code += [line]

        # Look for beginning of interface
        if len(words)>0 and words[0] == "interface":

            if words[1] == "assignment" or words[1] == "operator":
                dkey = words[2].replace("(","").replace(")","") # Remove paranthesis and spaces, leave operator (assuming no spaces inside paranthesis)
            else:
                dkey = words[1]
            
            
            Interface_code[dkey] = list()
            Interface_routines[dkey] = list()
            while not line.replace(" ","").startswith("endinterface"):
                Interface_code[dkey] += [line]#[lstrip_max(line, nindent)]
                #print("nindent: "+str(nindent))
                if len(words)>0 and words[0] == "module" and words[1] == "procedure":
                    Interface_routines[dkey] += [words[2]]
                j = j + 1
                line = Lines[j]
                words = line.split()
            Interface_code[dkey] += [line]#[lstrip_max(line, nindent)]


        # Look for beginning of routine
        match = False
        if len(words) > 1:
            if words[0:2] == ['elemental','subroutine']:
                match = True
                routine_type = "subroutine"
            elif words[0:2] == ['pure','function']:
                match = True
                routine_type = "function"
            elif words[0:2] == ['elemental','function']:
                match = True
                routine_type = "function"
        if match:
            ind = words[2].find("(")
            if ind>0:
                dkey = words[2][0:ind]
            else:
                dkey = words

            Routine_code[dkey] = list()
            while not line.replace(" ","").startswith("end"+routine_type):
                Routine_code[dkey] += [line]#[lstrip_max(line, nindent)]
                j = j + 1
                line = Lines[j]
            Routine_code[dkey] += [line]#[lstrip_max(line, nindent)]

    return Type_def_code, Interface_routines, Interface_code, Routine_code
