# ----------------------------------------------------------------------
# Config file for Gfortran
# ----------------------------------------------------------------------

# ------- Define install dir -------------------------------------------
INSTALL_DIR = ./opt-gnu

# ------- Define the compilers -----------------------------------------
FF90 = gfortran

# ------- Define compiler flags ----------------------------------------
FF90_FLAGS = -fPIC -O2 -fdefault-real-8 -std=f2008

# ------- Define linker flags ------------------------------------------
LINKER_FLAGS = -fPIC # If on a OSX add -undefined dynamic_lookup