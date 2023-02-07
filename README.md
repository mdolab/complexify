# README

The complex-step is a method for estimating derivatives.

This repository provides a:
1. Fortran library and scripts to covert Fortran code to complex-step compatible code
2. Python module redefining numpy functions that are not complex safe

## Documentation
Please see the documentation for installation details and API documentation.

To locally build the documentation, enter the `doc` folder and enter `make html` in terminal. You can then view the built documentation in the `_build` folder.

## Citation
Please cite the complex step method in any publication for which you find it useful. For more background, theory, and figures, see the this paper.

The Complex-Step Derivative Approximation

```
@Article{Martins2003a,
    author      = {Joaquim R. R. A. Martins and Peter Sturdza and Juan J. Alonso},
    title       = {The Complex-Step Derivative Approximation},
    doi         = {10.1145/838250.838251},
    journal     = {ACM Transactions on Mathematical Software},
    month       = {September},
    number      = {3},
    pages       = {245--262},
    pdfurl      = {https://www.researchgate.net/profile/Joaquim_Martins/publication/222112601_The_Complex-Step_Derivative_Approximation/links/0912f50c7474bb861d000000/The-Complex-Step-Derivative-Approximation.pdf},
    volume      = {29},
    year        = {2003}
}

Maintainer: Eirikur Jonsson

Copyright MDO Lab.
