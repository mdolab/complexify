# ----------------------------------------------------------------------
# Config file for Intel fortran compiler
# ----------------------------------------------------------------------

# ------- Define install dir -------------------------------------------
INSTALL_DIR = $(COMPLEXIFY_DIR)

# ------- Define the compiler ------------------------------------------
FF90 = ifort

# ------- Define compiler flags ----------------------------------------
FF90_FLAGS = -fPIC -O2 -r8 -stand f08

# ------- Define linker flags ------------------------------------------
LINKER_FLAGS = -fPIC
