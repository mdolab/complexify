-include config.mk

.PHONY: default clean install complexify_build

default:
# Check if the config.mk file is in the config dir.
# Check also if env variable is set.
	@if [ ! -f "config/config.mk" ]; then \
	echo "Before compiling, copy an existing config file from the "; \
	echo "config/defaults/ directory to the config/ directory and  "; \
	echo "rename to config.mk. For example:"; \
	echo " ";\
	echo "  cp config/defaults/config.LINUX_GFORTRAN.mk config/config.mk"; \
	echo " ";\
	echo "Then modify this config file as required. "; \
	echo "Rerun 'make' and the build will start"; \
	elif [ -z "${COMPLEXIFY_DIR}" ]; then \
	echo "Please set the COMPLEXIFY_DIR environmental variable before continuing"; \
	else make complexify_build;\
	fi;

clean:
	@echo "Making clean ... "
	rm -rf config.mk
	rm -rf src/*.mod
	rm -rf src/*.o
	rm -rf src/*.so

install:
	mkdir -p ${INSTALL_DIR}/include && cp ./src/*.mod ${INSTALL_DIR}/include
	mkdir -p ${INSTALL_DIR}/lib && cp ./src/*.so ${INSTALL_DIR}/lib

complexify_build:
	ln -sf config/config.mk config.mk;
	(cd src && make)
