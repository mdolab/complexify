!-----------------------------------------------------------------------
! Complex arithmetics using real numbers only
!-----------------------------------------------------------------------

! Limited to scalars operations
! Here we refer a,b,c,d the components of the following complex variables
!   z = a+ib
!   w = c+id

!-----------------------------------------------------------------------
! Scalar routines
!-----------------------------------------------------------------------
subroutine cmplxMult(a,b,c,d,re,im)
    ! Eq. 5.4.2
    ! z*w = (a+ib)(c+id) = (ac - bd) + i(bc + ad) 
    !                    = (ac - bd) + i[(a + b)(c + d) - ac - bd]
    use precision
	use complexify 
    implicit none
    
    ! Input/Output
    complex(kind=dtype), intent(in) :: a,b,c,d
    complex(kind=dtype), intent(out) :: re, im
        
    re = (a*c - b*d)
    im = (a + b)*(c + d) - a*c - b*d
    
end subroutine cmplxMult

subroutine cmplxDiv(a,b,c,d,re,im)
    ! Eq. 5.4.5
    use precision
	use complexify 
    implicit none
    
    ! Input/Output
    complex(kind=dtype), intent(in) :: a,b,c,d
    complex(kind=dtype), intent(out) :: re, im
    
    ! Working
    complex(kind=dtype) :: absc, absd, tmp1, tmp2
    
    absc = abs(c)
    absd = abs(d)
    
    if ( absc >= absd ) then
        tmp1 = d/c
        tmp2 = (c + d*tmp1)
        re = (a + b*tmp1) / tmp2
        im = (b - a*tmp1) / tmp2
    else
        tmp1 = c/d
        tmp2 = (c*tmp1 + d)
        re = (a*tmp1 + b) / tmp2
        im = (b*tmp1 - a) / tmp2
    end if
        
end subroutine cmplxDiv

subroutine cmplxAbs(a,b,res)
    ! Eq. 5.4.4
    use precision
	use complexify 
    implicit none
    
    ! Input/Output
    complex(kind=dtype), intent(in) :: a,b
    complex(kind=dtype), intent(out) :: res
    
    ! Working
    complex(kind=dtype) :: absa, absb
    
    absa = abs(a)
    absb = abs(b)
    res = 0.0
    if ( absa >= absb ) then
        res = absa * sqrt(1.0 + (b/a)**2.0)
    else
        res = absb * sqrt(1.0 + (a/b)**2.0)
    end if
end subroutine cmplxAbs


subroutine cmplxSqrt(c,d,re,im)
    !Eq. 5.4.6 - 5.4.7
    use precision
    use constants, only : meps
	use complexify 
    implicit none
    
    ! Input/Output
    complex(kind=dtype), intent(in) :: c,d
    complex(kind=dtype), intent(out) :: re, im
    
    ! Working
    complex(kind=dtype) :: absc, absd, w
        
    absc = abs(c)
    absd = abs(d)
    
    ! Calculate intermediate variable w
    !if ( c == 0.0 .AND. d == 0.0 ) then
    !if ( absc <= tiny(c) .AND. absd <= tiny(d) ) then
    if ( absc <= meps .AND. absd <= meps ) then
        w = 0.0
    else if ( absc >= absd ) then
        w = sqrt(absc) * sqrt( ( 1.0 + sqrt(1.0 + (d/c)**2.0) ) / 2.0 )
    else
        w = sqrt(absd) * sqrt( (absc/absd + sqrt(1.0 + (c/d)**2.0) ) / 2.0 )
    end if
    
    re = 0.0
    im = 0.0
    if ( w .ceq. 0.0 ) then
        ! Do nothing, already set to 0
    else if ( c >= 0.0 ) then
        re = w
        im = d / (2.0 * w)
    else if ( c < 0.0 .AND. d >= 0.0 ) then
        re = absd / (2.0 * w)
        im = w
    else ! c < 0.0 .AND. d < 0.0
        re = absd / (2.0 * w)
        im = -w
    end if
    
end subroutine cmplxSqrt

!-----------------------------------------------------------------------
! Vector routines
!-----------------------------------------------------------------------

subroutine cmplxL2Norm(x_r, x_i, n, l2Norm)

    use constants
	use complexify 
    implicit none

    ! Input/Output
    complex(kind=dtype), intent(in) :: x_r(n), x_i(n)
    integer, intent(in) :: n
    complex(kind=dtype), intent(out) ::l2Norm

    ! Working
    integer :: i

    ! Initialize
    l2Norm = zero

    do i=1,n
        l2Norm = l2Norm + x_r(i)**2 + x_i(i)**2
    end do
    l2Norm = sqrt(l2Norm)

end subroutine cmplxL2Norm

