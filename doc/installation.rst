.. _complexify_installation:

Installation
============

If one wants to complexify a Fortran code the complexify library needs to be build, and the necessary scripts installed.
If one is not interested in complexifying a fortran code, but only the complex-safe python function, the building and installing of the library can be omitted.


Build and installing the complexify Fortran library and scripts
---------------------------------------------------------------

Before complexifying a Fortran code one needs to build the `complexify` library that contains the overloaded operations.
First, define the `COMPLEXIFY_DIR` environmental variable in your `.bashrc`.
This variable defines where the complexify library will be installed.
In addition, for the library to be loaded properly `LD_LIBRARY_PATH` also needs to be updated.
Example::

    export COMPLEXIFY_DIR=$MDOLAB_REPO_DIR/complexify/opt-gnu
    export LD_LIBRARY_PATH=$COMPLEXIFY_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

To build a input configuration file needed.
Default configuration files are found in

.. prompt:: bash

    config/defaults

Copy one that suits your needs to ''config/config.mk''.
For example::

.. prompt:: bash

    cp config/defaults/config.LINUX_GFORTRAN.mk config/config.mk

To then build and install the library type

.. prompt:: bash

    make
    make install

Once the library is built and installed the parsing script installed by.

.. prompt:: bash

  pip install .


Install complex-step safe python functions only
------------------------------------------------

If the complex-step safe functions are only desired then the above library does not have to be built.

To install run

.. prompt:: bash

  pip install .










