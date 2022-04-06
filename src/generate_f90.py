import os

# Read .fpp source file
base_path = os.path.dirname(os.path.abspath(__file__))
filename_fpp = os.path.normpath(base_path + "/dnad_mod.fpp") #Remove /../
file_fpp = open(filename_fpp, 'r')
Lines_fpp = file_fpp.read()
 
# Generate one .f90 file for each value of ndv, each with its own dual type with name dual{ndv}
for ndv in range(1, 3):
    Lines_f90 = Lines_fpp.replace("dual", f"dual{ndv}") # Replace type name
    Lines_f90 = Lines_f90.replace("ndv_literal", f"{ndv}", 1) # Replace first occurence of ndv_literal with integer ndv
    Lines_f90 = Lines_f90.replace("dnad_mod", f"dnad{ndv}_mod") # Replace module name

    # Write f90 file
    filename_f90 = os.path.normpath(base_path + f"/dnad{ndv}_mod.f90") #Remove /../
    file_f90 = open(filename_f90, 'w')
    file_f90.write(Lines_f90)
