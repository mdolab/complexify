include ../config.mk

default:

# Compile the source files:
	$(FF90) -c $(FF90_FLAGS) complexify.f90

# Create a library:
	$(FF90) -shared complexify.o -o libcomplexify.so


