include ../config.mk
# This makefile builds the tests

COMPLEXIFY_INCLUDE_FLAGS=-I$(COMPLEXIFY_DIR)/include
COMPLEXIFY_LINKER_FLAGS=-L$(COMPLEXIFY_DIR)/lib -lcomplexify

default:
	$(FF90) -o test_complexify test_complexify.f90 $(FF90_FLAGS) $(COMPLEXIFY_INCLUDE_FLAGS) $(COMPLEXIFY_LINKER_FLAGS)


clean_obj:
	rm -rf *.o
	rm -rf *.mod

clean: clean_obj
	rm -rf test_complexify
