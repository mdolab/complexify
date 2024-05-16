# ----------------------------------------------------------------------
# Config file for Intel fortran compiler
# ----------------------------------------------------------------------

# ------- Define install dir -------------------------------------------
INSTALL_DIR = $(COMPLEXIFY_DIR)

# ------- Define the compiler ------------------------------------------
# Note that ";" is there to avoid make shell optimization, otherwise the shell command may fail
ICC_EXISTS := $(shell command -v icc;)

ifdef ICC_EXISTS
  # icc only exists on older Intel versions
  # Assume that we want to use the old compilers
  FF90 = ifort
else
  # Use the new compilers
  FF90 = ifx
endif

# ------- Define compiler flags ----------------------------------------
FF90_FLAGS = -fPIC -O2 -r8 -stand f08

# ------- Define linker flags ------------------------------------------
LINKER_FLAGS = -fPIC
