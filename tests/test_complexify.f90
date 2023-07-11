module precision
    ! A module to handle setting the data type for the real/complex analysis
    integer, parameter :: dtype = selected_real_kind(p=12, r=200)
end module precision

module constants
    ! A module that defines constants for later useage
    use precision
    real(kind=dtype), parameter :: PI = 3.1415926535897931_dtype
    real(kind=dtype), parameter :: zero = 0.0_dtype
    real(kind=dtype), parameter :: one = 1.0_dtype
    real(kind=dtype), parameter :: two = 2.0_dtype
    real(kind=dtype), parameter :: three = 3.0_dtype
    real(kind=dtype), parameter :: four = 4.0_dtype
    real(kind=dtype), parameter :: ten = 10.0_dtype
    real(kind=dtype), parameter :: half = one / two
    real(kind=dtype), parameter :: quarter = one / four
    real(kind=dtype), parameter :: meps = 1.11e-16_dtype
    real(kind=dtype), parameter :: h = 1.e-6_dtype
    real(kind=dtype), parameter :: hc = 1.e-40_dtype
    real(kind=dtype), parameter :: eps = 1.e-15_dtype
end module constants

module assert
    use, intrinsic :: ieee_arithmetic
    use constants

    implicit none
contains
    impure elemental logical function assert_isclose(actual, desired, rtol, atol, equal_nan, msg)
        ! inputs
        ! ------
        ! actual: value "measured"
        ! desired: value "wanted"
        ! rtol: relative tolerance
        ! atol: absolute tolerance
        ! equal_nan: consider NaN to be equal?
        !
        !  rtol overrides atol when both are specified
        !
        ! https://www.python.org/dev/peps/pep-0485/#proposed-implementation
        ! https://github.com/PythonCHB/close_pep/blob/master/is_close.py

        real(kind=dtype), intent(in) :: actual, desired
        real(kind=dtype), intent(in), optional :: rtol, atol
        logical, intent(in), optional :: equal_nan
        character(*), intent(in), optional :: msg

        real(kind=dtype) :: r, a
        logical :: n

        r = 1e-5_dtype
        a = 0.0_dtype
        n = .false.

        if (present(rtol)) r = rtol
        if (present(atol)) a = atol
        if (present(equal_nan)) n = equal_nan

        !--- sanity check
        assert_isclose = .false.
        if ((r < zero) .or. (a < zero)) then
            print *, "Relative or absolute value are below zero"
            return
        end if

        !--- simplest case
        assert_isclose = (actual == desired)
        if (assert_isclose) return

        !--- equal nan
        assert_isclose = n .and. (ieee_is_nan(actual) .and. ieee_is_nan(desired))
        if (assert_isclose) return

        !--- Inf /= -Inf, unequal NaN
        if (.not. ieee_is_finite(actual) .or. .not. ieee_is_finite(desired)) return

        !--- floating point closeness check
        assert_isclose = abs(actual - desired) <= max(r * max(abs(actual), abs(desired)), a)
        if (.not. assert_isclose) then
            print *, "FAILED - Not equal: actual", actual, "desired", desired
        end if

    end function assert_isclose

    impure logical function assert_allclose(actual, desired, rtol, atol, equal_nan, msg)
        ! inputs
        ! ------
        ! actual: value "measured"
        ! desired: value "wanted"
        ! rtol: relative tolerance
        ! atol: absolute tolerance
        ! equal_nan: consider NaN to be equal?
        !
        !  rtol overrides atol when both are specified
        real(kind=dtype), intent(in) :: actual(:), desired(:)
        real(kind=dtype), intent(in), optional :: rtol, atol
        logical, intent(in), optional :: equal_nan
        character(*), intent(in), optional :: msg

        integer :: i
        assert_allclose = .true.
        do i = 1, size(actual)
            assert_allclose = assert_isclose(actual(i), desired(i), rtol, atol, equal_nan, msg)
            if (.not. assert_allclose) return
        end do
    end function assert_allclose

    impure elemental logical function assert_true(actual)
        logical, intent(in) :: actual
        assert_true = .false.
        if (actual) then
            assert_true = .true.
        else
            print *, "FAILED - False is not true : Actual value is not true."
        end if
        return
    end function assert_true

    impure elemental logical function assert_false(actual)
        logical, intent(in) :: actual
        assert_false = .false.
        if (.not. actual) then
            assert_false = .true.
        else
            print *, "FAILED - True is not false : Actual value is not false."
        end if
        return
    end function assert_false
end module assert

module testing

    use constants
    use complexify

    implicit none

    ! Shared testing data
    integer, parameter :: n = 5
    real(kind=dtype) :: A(n)
    complex(kind=dtype) :: Ac(n)
    logical :: test_result

    ! Test statistics
    integer :: failed_tests = 0
    integer :: ran_tests = 0

contains

    subroutine print_announce(test_name)
        implicit none
        character(*), intent(in) :: test_name
        character(len=100) :: fmt1 = '(1X,I3," Running: ",A)'
        write (*, fmt1, advance="no") ran_tests + 1, test_name
    end subroutine print_announce

    subroutine print_result(test_status)
        implicit none
        logical, intent(in) :: test_status
        character(len=100) :: fmt1 = '(1X,A)'
        if (test_status) then
            write (*, fmt1, advance="yes") "... OK"
        end if
    end subroutine print_result

    subroutine print_summary()
        implicit none
        character(len=100) :: fmt1 = '(1X,A,1X,I3)'
        print *, "------------------------------------------------"
        print *, "Test summary:"
        print *, "------------------------------------------------"
        print fmt1, "Tests success:", ran_tests - failed_tests
        print fmt1, "Tests failed:", failed_tests
    end subroutine print_summary

    logical function assert_isclose_wrap(actual, desired, rtol, atol, equal_nan, msg)
        use assert, only: assert_isclose
        implicit none

        real(kind=dtype), intent(in) :: actual, desired
        real(kind=dtype), intent(in), optional :: rtol, atol
        logical, intent(in), optional :: equal_nan
        character(*), intent(in), optional :: msg

        call print_announce(msg)
        assert_isclose_wrap = assert_isclose(actual, desired, rtol, atol, equal_nan, msg)
        call increment(assert_isclose_wrap)
        call print_result(assert_isclose_wrap)
    end function assert_isclose_wrap

    logical function assert_allclose_wrap(actual, desired, rtol, atol, equal_nan, msg)
        use assert, only: assert_allclose
        implicit none

        real(kind=dtype), intent(in) :: actual(:), desired(:)
        real(kind=dtype), intent(in), optional :: rtol, atol
        logical, intent(in), optional :: equal_nan
        character(*), intent(in), optional :: msg

        call print_announce(msg)
        assert_allclose_wrap = assert_allclose(actual, desired, rtol, atol, equal_nan, msg)
        call increment(assert_allclose_wrap)
        call print_result(assert_allclose_wrap)
    end function assert_allclose_wrap

    logical function assert_true_wrap(actual, msg)
        use assert, only: assert_true
        implicit none

        logical, intent(in) :: actual
        character(*), intent(in), optional :: msg

        call print_announce(msg)
        assert_true_wrap = assert_true(actual)
        call increment(assert_true_wrap)
        call print_result(assert_true_wrap)
    end function assert_true_wrap

    logical function assert_false_wrap(actual, msg)
        use assert, only: assert_false
        implicit none

        logical, intent(in) :: actual
        character(*), intent(in), optional :: msg

        call print_announce(msg)
        assert_false_wrap = assert_false(actual)
        call increment(assert_false_wrap)
        call print_result(assert_false_wrap)
    end function assert_false_wrap

    ! real(kind=dtype) function fd(func, x)
    !   implicit none
    !   real(kind=dtype), external :: func
    !   real(kind=dtype), intent(in) :: x

    !   fd = (func(x+h) - func(x)) / h
    !   return
    ! end function fd

    subroutine initArray()
        implicit none
        integer :: i

        ! Initialize shared data
        do i = 1, n
            A(i) = i
            Ac(i) = cmplx(A(i), A(i) + 10)
        end do
    end subroutine initArray

    subroutine increment(result)
        implicit none
        logical, intent(in) :: result
        ran_tests = ran_tests + 1
        if (.not. result) failed_tests = failed_tests + 1
    end subroutine increment

    subroutine test_abs()
        implicit none

        ! abs_c
        ! Test value
        test_result = assert_isclose_wrap(real(abs(cmplx(three, two))), three, msg="func abs_c(+r,+i)")
        test_result = assert_isclose_wrap(real(abs(cmplx(-three, two))), three, msg="func abs_c(-r,+i)")
        test_result = assert_isclose_wrap(real(abs(cmplx(three, -two))), three, msg="func abs_c(+r,-i)")
        test_result = assert_isclose_wrap(real(abs(cmplx(-three, -two))), three, msg="func abs_c(-r,-i)")

        ! Test derivative
        test_result = assert_isclose_wrap(aimag(abs(cmplx(three, two))), two, msg="sens abs_c(+r,+i)")
        test_result = assert_isclose_wrap(aimag(abs(cmplx(-three, two))), -two, msg="sens abs_c(-r,+i)")
        test_result = assert_isclose_wrap(aimag(abs(cmplx(three, -two))), -two, msg="sens abs_c(+r,-i)")
        test_result = assert_isclose_wrap(aimag(abs(cmplx(-three, -two))), two, msg="sens abs_c(-r,-i)")

    end subroutine test_abs

    subroutine test_acos()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = acos(x)
        fDot = -one / sqrt(one - x * x)

        xc = cmplx(x, hc)
        fc = real(acos(xc))
        fcDot = aimag(acos(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func acos")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens acos")

        ! Compare FD also
        fd = (acos(x + h) - acos(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd acos")

    end subroutine test_acos

    subroutine test_asin()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = asin(x)
        fDot = one / sqrt(one - x * x)

        xc = cmplx(x, hc)
        fc = real(asin(xc))
        fcDot = aimag(asin(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func asin")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens asin")

        ! Compare FD also
        fd = (asin(x + h) - asin(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd asin")

    end subroutine test_asin

    subroutine test_atan()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = atan(x)
        fDot = one / (one + x * x)

        xc = cmplx(x, hc)
        fc = real(atan(xc))
        fcDot = aimag(atan(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func atan")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens atan")

        ! Compare FD also
        fd = (atan(x + h) - atan(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd atan")

    end subroutine test_atan

    subroutine test_atan2()
        implicit none

        real(kind=dtype) :: x, y, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc, yc

        x = half
        y = quarter
        f = atan2(y, x)
        ! TODO: Add analytic derivative
        !fDot = one/(one+x*x)

        xc = cmplx(half, hc)
        yc = cmplx(quarter, zero)
        fc = real(atan2(yc, xc))
        fcDot = aimag(atan2(yc, xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func atan2")

        ! Test sens
        !test_result = assert_isclose_wrap(fcDot, fDot, msg="sens atan2")

        ! Compare FD also
        fd = (atan2(y, x + h) - atan2(y, x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd atan2")

    end subroutine test_atan2

    subroutine test_cosh()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = cosh(x)
        fDot = sinh(x)

        xc = cmplx(x, hc)
        fc = real(cosh(xc))
        fcDot = aimag(cosh(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func cosh")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens cosh")

        ! Compare FD also
        fd = (cosh(x + h) - cosh(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd cosh")

    end subroutine test_cosh

    subroutine test_sinh()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = sinh(x)
        fDot = cosh(x)

        xc = cmplx(x, hc)
        fc = real(sinh(xc))
        fcDot = aimag(sinh(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func sinh")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens sinh")

        ! Compare FD also
        fd = (sinh(x + h) - sinh(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd sinh")

    end subroutine test_sinh

    subroutine test_tan()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = tan(x)
        fDot = one / (cos(x) * cos(x))

        xc = cmplx(x, hc)
        fc = real(tan(xc))
        fcDot = aimag(tan(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func tan")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens tan")

        ! Compare FD also
        fd = (tan(x + h) - tan(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd tan")

    end subroutine test_tan

    subroutine test_tanh()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = tanh(x)
        fDot = one - tanh(x) * tanh(x)

        xc = cmplx(x, hc)
        fc = real(tanh(xc))
        fcDot = aimag(tanh(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func tanh")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens tanh")

        ! Compare FD also
        fd = (tanh(x + h) - tanh(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd tanh")

    end subroutine test_tanh

    subroutine test_max()
        implicit none

        ! --- max_cc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(max(cmplx(zero, four), cmplx(three, two))), &
                      three, msg="func max_cc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(max(cmplx(zero, four), cmplx(three, one))), &
                      one, msg="sens max_cc")

        ! --- max_cr ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(max(cmplx(one, four), three)), &
                      three, msg="func max_cr")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(max(cmplx(one, four), cmplx(three, one))), &
                      one, msg="sens max_cr")

        ! --- max_ccc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(max(cmplx(zero, four), cmplx(two, two), cmplx(three, one))), &
                      three, msg="func max_ccc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(max(cmplx(zero, four), cmplx(two, two), cmplx(three, one))), &
                      one, msg="sens max_ccc")

        ! --- max_cccc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(max(cmplx(zero, four), cmplx(two, two), cmplx(one, one), cmplx(three, one))), &
                      three, msg="func max_cccc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(max(cmplx(zero, four), cmplx(two, two), cmplx(one, one), cmplx(three, one))), &
                      one, msg="sens max_cccc")
    end subroutine test_max

    subroutine test_min()
        implicit none

        ! --- min_cc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(min(cmplx(zero, four), cmplx(three, two))), &
                      zero, msg="func min_cc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(min(cmplx(zero, four), cmplx(three, one))), &
                      four, msg="sens min_cc")

        ! --- min_cr ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(min(cmplx(one, four), three)), &
                      one, msg="func min_cr")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(min(cmplx(one, four), cmplx(three, one))), &
                      four, msg="sens min_cr")

        ! --- min_ccc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(min(cmplx(zero, four), cmplx(two, two), cmplx(three, one))), &
                      zero, msg="func min_ccc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(min(cmplx(zero, four), cmplx(two, two), cmplx(three, one))), &
                      four, msg="sens min_ccc")

        ! --- min_cccc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(min(cmplx(zero, four), cmplx(two, two), cmplx(one, one), cmplx(three, one))), &
                      zero, msg="func min_cccc")
        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(min(cmplx(zero, four), cmplx(two, two), cmplx(one, one), cmplx(three, one))), &
                      four, msg="sens min_cccc")

    end subroutine test_min

    subroutine test_minval()
        implicit none
        call initArray()

        ! Check function value
        test_result = assert_isclose_wrap(real(minval(Ac)), minval(A), msg="func minval")

        ! Check derivative value
        test_result = assert_isclose_wrap(aimag(minval(Ac)), minval(aimag(Ac)), msg="sens minval")

    end subroutine test_minval

    subroutine test_maxval()
        implicit none
        call initArray()

        ! Check function value
        test_result = assert_isclose_wrap(real(maxval(Ac)), maxval(A), msg="func maxval")

        ! Check derivative value
        test_result = assert_isclose_wrap(aimag(maxval(Ac)), maxval(aimag(Ac)), msg="sens maxval")

    end subroutine test_maxval

    subroutine test_sign()
        implicit none

        ! --- sign_cc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(two, four), cmplx(one, one))), &
                      sign(two, one), msg="func sign_cc(++,++)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(two, four), cmplx(-one, one))), &
                      sign(two, -one), msg="func sign_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(two, four), cmplx(one, -one))), &
                      sign(two, one), msg="func sign_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(two, four), cmplx(-one, -one))), &
                      sign(two, -one), msg="func sign_cc(++,--)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, four), cmplx(one, one))), &
                      sign(-two, one), msg="func sign_cc(-+,++)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, four), cmplx(-one, one))), &
                      sign(-two, -one), msg="func sign_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, four), cmplx(one, -one))), &
                      sign(-two, one), msg="func sign_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, four), cmplx(-one, -one))), &
                      sign(-two, -one), msg="func sign_cc(-+,--)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, -four), cmplx(one, one))), &
                      sign(-two, one), msg="func sign_cc(--,++)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, -four), cmplx(-one, one))), &
                      sign(-two, -one), msg="func sign_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, -four), cmplx(one, -one))), &
                      sign(-two, one), msg="func sign_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      real(sign(cmplx(-two, -four), cmplx(-one, -one))), &
                      sign(-two, -one), msg="func sign_cc(--,--)")

        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(two, four), cmplx(one, one))), &
                      four * (two * one) / abs(two * one), msg="sens sign_cc(++,++)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(two, four), cmplx(-one, one))), &
                      four * (two * (-one)) / abs(two * (-one)), msg="sens sign_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(two, four), cmplx(one, -one))), &
                      four * (two * one) / abs(two * one), msg="sens sign_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(two, four), cmplx(-one, -one))), &
                      four * (two * (-one)) / abs(two * (-one)), msg="sens sign_cc(++,--)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, four), cmplx(one, one))), &
                      four * (-two * one) / abs(-two * one), msg="sens sign_cc(-+,++)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, four), cmplx(-one, one))), &
                      four * (-two * (-one)) / abs(-two * (-one)), msg="sens sign_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, four), cmplx(one, -one))), &
                      four * (-two * one) / abs(-two * one), msg="sens sign_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, four), cmplx(-one, -one))), &
                      four * (-two * (-one)) / abs(-two * (-one)), msg="sens sign_cc(-+,--)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, -four), cmplx(one, one))), &
                      -four * (-two * one) / abs(-two * one), msg="sens sign_cc(--,++)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, -four), cmplx(-one, one))), &
                      -four * (-two * (-one)) / abs(-two * (-one)), msg="sens sign_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, -four), cmplx(one, -one))), &
                      -four * (-two * one) / abs(-two * one), msg="sens sign_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(sign(cmplx(-two, -four), cmplx(-one, -one))), &
                      -four * (-two * (-one)) / abs(-two * (-one)), msg="sens sign_cc(--,--)")

        ! TODO --- sign_cca ---
        ! TODO --- sign_cr ---

    end subroutine test_sign

    subroutine test_dim()
        implicit none
        ! --- dim_cc ---
        ! Test value
        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(two, four), cmplx(one, one))), &
                      dim(two, one), msg="func dim_cc(++,++)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(two, four), cmplx(-one, one))), &
                      dim(two, -one), msg="func dim_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(two, four), cmplx(one, -one))), &
                      dim(two, one), msg="func dim_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(two, four), cmplx(-one, -one))), &
                      dim(two, -one), msg="func dim_cc(++,--)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, four), cmplx(one, one))), &
                      dim(-two, one), msg="func dim_cc(-+,++)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, four), cmplx(-one, one))), &
                      dim(-two, -one), msg="func dim_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, four), cmplx(one, -one))), &
                      dim(-two, one), msg="func dim_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, four), cmplx(-one, -one))), &
                      dim(-two, -one), msg="func dim_cc(-+,--)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, -four), cmplx(one, one))), &
                      dim(-two, one), msg="func dim_cc(--,++)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, -four), cmplx(-one, one))), &
                      dim(-two, -one), msg="func dim_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, -four), cmplx(one, -one))), &
                      dim(-two, one), msg="func dim_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      real(dim(cmplx(-two, -four), cmplx(-one, -one))), &
                      dim(-two, -one), msg="func dim_cc(--,--)")

        ! Test derivative
        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(two, four), cmplx(one, one))), &
                      four - one, msg="sens dim_cc(++,++)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(two, four), cmplx(-one, one))), &
                      four - one, msg="sens dim_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(two, four), cmplx(one, -one))), &
                      four - (-one), msg="sens dim_cc(++,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(two, four), cmplx(-one, -one))), &
                      four - (-one), msg="sens dim_cc(++,--)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, four), cmplx(one, one))), &
                      zero, msg="sens dim_cc(-+,++)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, four), cmplx(-one, one))), &
                      zero, msg="sens dim_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, four), cmplx(one, -one))), &
                      zero, msg="sens dim_cc(-+,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, four), cmplx(-one, -one))), &
                      zero, msg="sens dim_cc(-+,--)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, -four), cmplx(one, one))), &
                      zero, msg="sens dim_cc(--,++)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, -four), cmplx(-one, one))), &
                      zero, msg="sens dim_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, -four), cmplx(one, -one))), &
                      zero, msg="sens dim_cc(--,-+)")

        test_result = assert_isclose_wrap( &
                      aimag(dim(cmplx(-two, -four), cmplx(-one, -one))), &
                      zero, msg="sens dim_cc(--,--)")

        ! TODO --- dim_cr ---
        ! TODO --- dim_rc ---

    end subroutine test_dim

    subroutine test_log10()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot, fd
        complex(kind=dtype) :: xc

        x = half
        f = log10(x)
        fDot = one / (x * log(ten))

        xc = cmplx(x, hc)
        fc = real(log10(xc))
        fcDot = aimag(log10(xc)) / hc

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func log10")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens log10")

        ! Compare FD also
        fd = (log10(x + h) - log10(x)) / h
        test_result = assert_isclose_wrap(fcDot, fd, msg="sens fd log10")

    end subroutine test_log10

    subroutine test_nint()
        implicit none

        real(kind=dtype) :: x, f, fc ! Just use real instead of int for now
        complex(kind=dtype) :: xc

        x = half
        f = nint(x)

        xc = cmplx(x, hc)
        fc = nint(xc)

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func nint")

    end subroutine test_nint

    subroutine test_epsilon()
        implicit none

        real(kind=dtype) :: x, f, fDot, fc, fcDot
        complex(kind=dtype) :: xc

        x = half
        f = epsilon(x)
        fDot = zero

        xc = cmplx(x, hc)
        fc = real(epsilon(xc))
        fcDot = aimag(epsilon(xc))

        ! Test value
        test_result = assert_isclose_wrap(fc, f, msg="func epsilon")

        ! Test sens
        test_result = assert_isclose_wrap(fcDot, fDot, msg="sens epsilon")

    end subroutine test_epsilon

    subroutine test_lt()
        implicit none

        test_result = assert_true_wrap(cmplx(one, one) .lt. cmplx(two, two), msg="func lt_cc(++,++)")
        test_result = assert_true_wrap(cmplx(one, one) .lt. cmplx(two, -two), msg="func lt_cc(++,+-)")
        test_result = assert_false_wrap(cmplx(one, one) .lt. cmplx(-two, two), msg="func lt_cc(++,-+)")
        test_result = assert_false_wrap(cmplx(one, one) .lt. cmplx(-two, -two), msg="func lt_cc(++,--)")

        test_result = assert_true_wrap(cmplx(one, -one) .lt. cmplx(two, two), msg="func lt_cc(+-,++)")
        test_result = assert_true_wrap(cmplx(one, -one) .lt. cmplx(two, -two), msg="func lt_cc(+-,+-)")
        test_result = assert_false_wrap(cmplx(one, -one) .lt. cmplx(-two, two), msg="func lt_cc(+-,-+)")
        test_result = assert_false_wrap(cmplx(one, -one) .lt. cmplx(-two, -two), msg="func lt_cc(+-,--)")

        test_result = assert_true_wrap(cmplx(-one, one) .lt. cmplx(two, two), msg="func lt_cc(-+,++)")
        test_result = assert_true_wrap(cmplx(-one, one) .lt. cmplx(two, -two), msg="func lt_cc(-+,+-)")
        test_result = assert_false_wrap(cmplx(-one, one) .lt. cmplx(-two, two), msg="func lt_cc(-+,-+)")
        test_result = assert_false_wrap(cmplx(-one, one) .lt. cmplx(-two, -two), msg="func lt_cc(-+,--)")

        test_result = assert_true_wrap(cmplx(-one, -one) .lt. cmplx(two, two), msg="func lt_cc(--,++)")
        test_result = assert_true_wrap(cmplx(-one, -one) .lt. cmplx(two, -two), msg="func lt_cc(--,+-)")
        test_result = assert_false_wrap(cmplx(-one, -one) .lt. cmplx(-two, two), msg="func lt_cc(--,-+)")
        test_result = assert_false_wrap(cmplx(-one, -one) .lt. cmplx(-two, -two), msg="func lt_cc(--,--)")

        ! TODO Add other variations
    end subroutine test_lt

    ! TODO Add other operators

    subroutine test_floor()
        implicit none

        real(kind=dtype) :: f(n), fc(n), fcDot(n)

        call initArray()

        f = floor(A + half)
        fc = real(floor(Ac + cmplx(half, half)))
        test_result = assert_allclose_wrap(fc, f, msg="func floor")

        ! TODO Add sens
        !fcDot = aimag(floor(Ac + cmplx(half,half)))
        !test_result = assert_allclose_wrap(fcDot, zero, msg="sens floor")

    end subroutine test_floor

end module testing

program test_complexify
    ! DESCRIPTION
    ! Program to run tests for complexify

    use testing
    implicit none

    ! Define variables used
    logical, parameter :: debug = .false.

    call test_acos()
    call test_asin()
    call test_atan()
    call test_atan2()
    call test_cosh()
    call test_sinh()
    call test_tan()
    call test_tanh()

    call test_abs()
    call test_max()
    call test_min()
    call test_minval()
    call test_maxval()
    call test_sign()
    call test_dim()
    call test_log10()
    call test_nint()
    call test_epsilon()

    call test_lt()
    ! call test_le()
    ! call test_gt()
    call test_floor()

    call print_summary()

    if (failed_tests > 0) stop 1
end program test_complexify
