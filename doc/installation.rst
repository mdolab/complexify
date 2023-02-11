.. _complexify_installation:

Installation
============

If one wants to complexify a Fortran code the complexify library needs to be built, and the necessary scripts installed.
If one is not interested in complexifying a Fortran code, but only the :ref:`complex-safe python module<cs_safe_python_module_install>` , then building and installing the library can be omitted.


Fortran library and scripts
---------------------------

Before complexifying a Fortran code one needs to build the `complexify` library that contains the overloaded operations.

First, define the ``COMPLEXIFY_DIR`` environmental variable in your ``.bashrc``.
This variable defines where the complexify library will be installed.

.. note::

    The ``COMPLEXIFY_DIR`` environmental variable can point to any location on the disk

In addition, for the library to be loaded properly ``LD_LIBRARY_PATH`` also needs to be updated.
For example, to install the library in a folder ``opt-gnu`` in the ``complexify`` repository add the following to your ``.bashrc``::

    export COMPLEXIFY_DIR=$MDOLAB_REPO_DIR/complexify/opt-gnu
    export LD_LIBRARY_PATH=$COMPLEXIFY_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

To build a input configuration file needed.
Default configuration files are found in

.. prompt:: bash

    config/defaults

Copy one that suits your needs to ``config/config.mk``.
For example,

.. prompt:: bash

    cp config/defaults/config.LINUX_GFORTRAN.mk config/config.mk

To build and install the library type,

.. prompt:: bash

    make
    make install

Once the library is built and installed the parsing script installed by.

.. prompt:: bash

  pip install .

.. _cs_safe_python_module_install:

Complex-step safe python module
-------------------------------

If the complex-step safe functions are only desired then the above library does not have to be compiled or installed.
To install run,

.. prompt:: bash

  pip install .

Please refer to the :ref:`complexify_api` and  :ref:`complexify_examples` on how to use the module.







