# ----------------------------------------------------------------------
# Config file for Intel fortran compiler
# ----------------------------------------------------------------------

# ------- Define install dir -------------------------------------------
INSTALL_DIR = ./opt-intel

# ------- Define the compiler ------------------------------------------
FF90 = ifort

# ------- Define compiler flags ----------------------------------------
FF90_FLAGS = -fPIC -O2 -fdefault-real-8 -std=f2008

# ------- Define linker flags ------------------------------------------
LINKER_FLAGS = -fPIC