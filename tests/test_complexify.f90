
module precision
  ! A module to handle setting the data type for the real/complex analysis
  integer, parameter :: dtype = selected_real_kind(12)
end module precision

module constants
  ! A module that defines constants for later useage
  use precision
  real(kind=dtype), parameter :: PI = 3.1415926535897931_dtype
  real(kind=dtype), parameter :: zero = 0.0_dtype
  real(kind=dtype), parameter :: one = 1.0_dtype
  real(kind=dtype), parameter :: two = 2.0_dtype
  real(kind=dtype), parameter :: three = 3.0_dtype
  real(kind=dtype), parameter :: four  = 4.0_dtype
  real(kind=dtype), parameter :: half = one/two
  real(kind=dtype), parameter :: quarter = one/four
  real(kind=dtype), parameter :: meps = 1.11e-16_dtype
  real(kind=dtype), parameter :: h = 1.e-40_dtype
  real(kind=dtype), parameter :: eps = 1.e-15_dtype
end module constants


subroutine assert(condition, func)
    ! If condition == .false., it aborts the program.
    !
    ! Arguments
    ! ---------
    !
    logical, intent(in) :: condition
    character(len=*), intent(in) :: func
    !
    ! Example
    ! -------
    !
    ! call assert(a == 5)

    if (.not. condition) then
        print *, "FAILED  - Function: ",  func,  " Assert failed...stopping!"
        !stop 1
    else
        print *, "SUCCESS - Function: ", func
    end if
end subroutine assert


program test_complex
    ! DESCRIPTION
    ! This file will test all complex derivatives

    use constants
    use complexify
    implicit none

    ! Define variables used
    integer, parameter :: n = 5
    logical, parameter :: debug  = .false.

    real(kind=dtype) :: x, y, yDot
    real(kind=dtype) :: yc, ycDot
    complex(kind=dtype) :: xc

    real(kind=dtype) :: A(n)
    complex(kind=dtype) :: Ac(n)
    integer :: i

    ! ------------------------------------------------------------------
    ! y = asin(x)
    ! ------------------------------------------------------------------
    x = 0.5
    y = asin(x)
    yDot = one/sqrt(one-x*x)

    xc = cmplx(x,h)
    yc = real(asin(xc))
    ycDot = aimag(ASIN(xc))/h


    call assert(y == yc, "asin value")
    if (debug) print *, y, yc
    call assert(abs(yDot-ycDot) < eps, "asin derivative")
    if (debug) print *, yDot, ycDot


    ! ------------------------------------------------------------------
    ! y = maxval(x)
    ! ------------------------------------------------------------------
    A = (/ (i, i = 1, 5) /)
    y = maxval(A)
    yDot = zero

    Ac = cmplx(A,zero)
    Ac(1) = cmplx(A(1),h)
    yc = real(maxval(Ac))
    ycDot = aimag(maxval(Ac))/h

    call assert(y == yc, "maxval value")
    if (debug) print *, y, yc
    call assert(abs(yDot-ycDot) < eps, "maxval derivative")
    if (debug) print *, yDot, ycDot


end program test_complex
