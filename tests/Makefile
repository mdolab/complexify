
# This makefile is intended to build and run tests

F90FLAGS = -Og -fdefault-real-8 -fdefault-double-8 

all: complex tests

complex:
	cd ../src; gfortran $(F90FLAGS) -c complexify.f90
	mv ../src/*.o .
	mv ../src/*.mod .    
    
tests: complex
	gfortran $(F90FLAGS) -I../src -c test_complexify.f90
	gfortran $(F90FLAGS) -o test_complexify *.o 

clean_obj:
	rm -fr *.o
	rm -fr *.mod
    
clean: clean_obj
	rm -f testcomplex
