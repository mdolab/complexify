.. _complexify_library:

Fortran library and scripts
===========================


Installation
------------

Before complexifying a Fortran code one needs to build the `complexify` library that contains the overloaded operations, and install the necessary ``complexify`` parsing script.

First, define the ``COMPLEXIFY_DIR`` environmental variable in your ``.bashrc``.
This variable defines where the complexify library and headers will be installed, and is useful when defining include and linking flags in your project.

.. note::

    The ``COMPLEXIFY_DIR`` environmental variable can point to any location on the disk and does not have to be confined to the ``complexify`` directory.

In addition, for the library to be loaded properly, ``LD_LIBRARY_PATH`` also needs to be updated.
For example, to install the library in a folder called ``opt-gnu`` in the ``complexify`` repository, add the following to your ``.bashrc``::

    export COMPLEXIFY_DIR=$MDOLAB_REPO_DIR/complexify/opt-gnu
    export LD_LIBRARY_PATH=$COMPLEXIFY_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

To build the fortran module, an input configuration file is needed.
Default configuration files are found in ``config/defaults``.
Copy one that suits your needs to ``config/config.mk``.
For example,

.. prompt:: bash

    cp config/defaults/config.LINUX_GFORTRAN.mk config/config.mk

To build and install the library type,

.. prompt:: bash

    make
    make install

Once the library is built and installed, run the following to install the parsing script,

.. prompt:: bash

    pip install .


Usage
-----

To complexify your Fortran code, one needs first needs to run the ``complexify`` parser.
By default, ``complexify`` accepts one or multiple files (glob pattern), adds a ``c_`` prefix to the complexified file, and writes it to the same directory as the original source file.
For example, to complexify one file, run

.. prompt:: bash

    complexify <file_name>.f90

Please check ``complexify --help`` for more information on the available options.

To use the module in your Fortran project, first make sure the environmental variables are defined as discussed above.
Next, in the case of a Makefile, one can define the following variables that can then be included in the include and linking process::

    # ------- Define complexify inlcude and linker flags -------------------------
    COMPLEXIFY_INCLUDE_FLAGS=-I$(COMPLEXIFY_DIR)/include
    COMPLEXIFY_LINKER_FLAGS=-L$(COMPLEXIFY_DIR)/lib -lcomplexify



