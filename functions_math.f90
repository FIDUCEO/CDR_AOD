MODULE functions_math
!=====#################################################################
! NAME:
!		functions_math.f90
! PURPOSE:
! 		Module with several function to handle several Mathproblems
! CALLING SEQUENCE:
!		-
! NEEDS:
!		kinds.f90
!		LAPACK subs DGETRF and DGETRI (polyfit)
! INCLUDES:
!		q2abc
!		polyfit
!		d2_interpol
!		d3_interpol
!		d4_interpol
!		FindMinimum,Swap,Sort,Median
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
USE kinds
IMPLICIT NONE 
CONTAINS
  
!=====#################################################################
SUBROUTINE q2abc(x,y,a,b,c)
! PURPOSE:
!		returns the parameters of a,b,c
!		for a quadratic equation y=ax^2+bx+c
!		3 values for x,y must be provided (3 equations)
! INPUT:
!		x		::	the x-values
!		y		::	the y-values, where the quadratic should be fitted
! OUTPUT:
!		a,b,c	::	the parameters fullfilling the equation y=ax^2+bx+c
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
	REAL(KIND=p_double),intent(in)			:: x(3),y(3)
	REAL(KIND=p_double),intent(out)			:: a,b,c
	
	a = ( ( (y(1)-y(2))*(x(1)-x(3) )) - ( (y(1)-y(3))*(x(1)-x(2)) ) )  /  &
		( ( (x(1)**2-x(2)**2)*(x(1)-x(3)) ) - ( (x(1)**2-x(3)**2)*(x(1)-x(2)) ) )
	b = ((y(1)-y(2))-(a*(x(1)**2-x(2)**2))) / &
		(x(1)-x(2))
	c = y(3)-a*x(3)**2-b*x(3)
	  
	RETURN

END SUBROUTINE q2abc
!=====#################################################################
FUNCTION polyfit(vx, vy, d)
! PURPOSE:
!		returns the parameters of a,b,c
!		for a quadratic equation y=ax^2+bx+c
!		3 values for x,y must be provided (3 equations)
! NEEDS:
!		calls to LAPACK subs DGETRF and DGETRI
! INPUT:
!		x		::	the x-values
!		y		::	the y-values, where the quadratic should be fitted
! OUTPUT:
!		a,b,c	::	the parameters fullfilling the equation y=ax^2+bx+c
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
	INTEGER,INTENT(in)					:: d
    REAL(dp), dimension(d+1)			:: polyfit
    REAL(dp), dimension(:), intent(in)	:: vx, vy
 
    REAL(dp), dimension(:,:), allocatable	:: X
    REAL(dp), dimension(:,:), allocatable	:: XT
    REAL(dp), dimension(:,:), allocatable	:: XTX
	
	INTEGER									:: i,j
    INTEGER									:: n, lda, lwork
    INTEGER									:: info
    INTEGER, dimension(:), allocatable 		:: ipiv
    REAL(dp), dimension(:), allocatable		:: work
	
	
    n = d+1
    lda = n
    lwork = n
 
    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(XT(n, size(vx)))
    allocate(X(size(vx), n))
    allocate(XTX(n, n))
 
    ! prepare the matrix
    do i = 0, d
       do j = 1, size(vx)
          X(j, i+1) = vx(j)**i
       end do
    end do
 
    XT  = transpose(X)
    XTX = matmul(XT, X)
 
    ! calls to LAPACK subs DGETRF and DGETRI
    call DGETRF(n, n, XTX, lda, ipiv, info)
    if ( info /= 0 ) then
       print *, "problem"
	   ! print*,'vx:',vx
	   ! print*,'vy:',vy
	   ! print*,'info:',info
	   ! print*,'d:',d
	   ! stop
       return
    end if
    call DGETRI(n, XTX, lda, ipiv, work, lwork, info)
    if ( info /= 0 ) then
       print *, "problem"
	   ! print*,'vx:',vx
	   ! print*,'vy:',vy
	   ! print*,'info:',info
	   ! print*,'d:',d
	   ! stop
       return
    end if
 
    polyfit = matmul( matmul(XTX, XT), vy)
 
    deallocate(ipiv)
    deallocate(work)
    deallocate(X)
    deallocate(XT)
    deallocate(XTX)
 
END FUNCTION
!=====#################################################################
SUBROUTINE interpol_factor(x,x0,w,i0,i1)
! PURPOSE:
!		returns the interpolation factors
! INPUT:
!		x			::	the array, where x0 should be
!		x0			::	the value, which should be interpolated
! OUTPUT:
!		w			::	the factor for the upcoming interpolation
!		i0,i1		::	the indices around the value x0 in x
! REVISION HISTORY:
! 		Miriam Kosmale 2018
!----------------------------------------------------------------------
	REAL (KIND=p_real),INTENT(in)			::  x(:)
	REAL (KIND=p_real),INTENT(in)			::  x0
	REAL (KIND=p_real),INTENT(out)			::  w
	INTEGER,INTENT(out)						::  i0,i1
	INTEGER									::  idx(1)
	REAL (KIND=p_real)						::  delta1,delta2

	IF (x0 .LT. MINVAL(x)) THEN
		i0	= 1
		i1	= 2
		w	= 1.
	ENDIF
	IF (x0 .GT. MAXVAL(x)) THEN
		i0	= size(x)-1
		i1	= size(x)
		w	= 0.
	ENDIF
	IF (x0 .GE. MINVAL(x) .AND. x0 .LE. MAXVAL(x)) THEN
	
		idx		= minloc(ABS(x-x0))
		delta1	= x0 - x(idx(1))
		
			IF (delta1 .LT. 0.) THEN
				i0		= idx(1)-1
				i1		= idx(1)
				delta2	= x(i1) - x(i0)				
				w		= ABS(delta1)/delta2			
			ELSE 
				i0		= idx(1)
				i1		= idx(1)+1
				delta2	= x(i1) - x(i0)				
				w		= 1.-ABS(delta1)/delta2			
			ENDIF
	ENDIF
END SUBROUTINE interpol_factor
!----------------------------------------------------------------------
SUBROUTINE d2_interpol(t,u,yi,d2val)
! PURPOSE:
!		makes interpolation over a 2/3/4-D matrix
! INPUT:
!		t,u,v,w		::	the relative distance to the first corner of the matrix
!						for each index (1,2,3,4=t,u,v,w)
!						has to be between [0.,1.]
!		yi			::	the matrix values on each corner (left and right)
! OUTPUT:
!		d2val		::	the interpolated value
! REVISION HISTORY:
! 		based on code from Pekka Kolmonen 2014
! 		cleaned and adopted by Miriam Kosmale 2015
!----------------------------------------------------------------------
	!									index 1,2=t,u
	REAL (KIND=p_double),INTENT(in)			::  t,u
	!										t,u,v
	REAL (KIND=p_double),INTENT(in)			::  yi(2,2)
	REAL (KIND=p_double),INTENT(out)		::  d2val
      
      d2val =	&
	  &	t		*u		*yi(1,1)	+	&
	  &	(1.-t)	*u		*yi(2,1)	+	&
	  &	(1.-t)	*(1.-u)	*yi(2,2)	+	&
	  &	t		*(1.-u)	*yi(1,2)
END SUBROUTINE d2_interpol
!----------------------------------------------------------------------
SUBROUTINE d3_interpol(t,u,v,yi,d3val)
	!									index 1,2,3=t,u,v
	REAL (KIND=p_double),INTENT(in)			::  t,u,v
	!										t,u,v
	REAL (KIND=p_double),INTENT(in)			::  yi(2,2,2)
	REAL (KIND=p_double),INTENT(out)		::  d3val
      
      d3val =	&
	  &	t		*u		*v		*yi(1,1,1)	+	&
	  &	(1.-t)	*u		*v		*yi(2,1,1)	+	&
	  &	(1.-t)	*u		*(1.-v)	*yi(2,1,2)	+	&
	  &	(1.-t)	*(1.-u)	*v		*yi(2,2,1)	+	&
      &	(1.-t)	*(1.-u)	*(1.-v)	*yi(2,2,2)	+	&
	  &	t		*(1.-u)	*v		*yi(1,2,1)	+	&
	  &	t		*(1.-u)	*(1.-v)	*yi(1,2,2)	+	&
	  &	t		*u		*(1.-v)	*yi(1,1,2)
END SUBROUTINE d3_interpol
!----------------------------------------------------------------------
SUBROUTINE d4_interpol(t,u,v,w,yi,d4val)
	!									index 1,2,3,4=t,u,v,w
	REAL (KIND=p_double),INTENT(in)			::  t,u,v,w
	!										t,u,v
	REAL (KIND=p_double),INTENT(in)			::  yi(2,2,2,2)
	REAL (KIND=p_double),INTENT(out)		::  d4val
      
      d4val =	&
	  &	t		*u		*v		*w		*yi(1,1,1,1)	+	&
	  &	(1.-t)	*u		*v		*w		*yi(2,1,1,1)	+	&
	  &	(1.-t)	*u		*(1.-v)	*w		*yi(2,1,2,1)	+	&
	  &	(1.-t)	*(1.-u)	*v		*w		*yi(2,2,1,1)	+	&
      &	(1.-t)	*(1.-u)	*(1.-v)	*w		*yi(2,2,2,1)	+	&
	  &	t		*(1.-u)	*v		*w		*yi(1,2,1,1)	+	&
	  &	t		*(1.-u)	*(1.-v)	*w		*yi(1,2,2,1)	+	&
	  &	t		*u		*(1.-v)	*w		*yi(1,1,2,1)	+	&
	  &	t		*u		*v		*(1.-w)	*yi(1,1,1,2)	+	&
	  &	(1.-t)	*u		*v		*(1.-w)	*yi(2,1,1,2)	+	&
	  &	(1.-t)	*u		*(1.-v)	*(1.-w)	*yi(2,1,2,2)	+	&
	  &	(1.-t)	*(1.-u)	*v		*(1.-w)	*yi(2,2,1,2)	+	&
      &	(1.-t)	*(1.-u)	*(1.-v)	*(1.-w)	*yi(2,2,2,2)	+	&
	  &	t		*(1.-u)	*v		*(1.-w)	*yi(1,2,1,2)	+	&
	  &	t		*(1.-u)	*(1.-v)	*(1.-w)	*yi(1,2,2,2)	+	&
	  &	t		*u		*(1.-v)	*(1.-w)	*yi(1,1,2,2)
END SUBROUTINE d4_interpol

!=====#################################################################
! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

   INTEGER FUNCTION  FindMinimum(x, Start, End)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)                :: Start, End
      INTEGER                            :: Minimum
      INTEGER                            :: Location
      INTEGER                            :: i

      Minimum  = x(Start)          ! assume the first is the min
      Location = Start             ! record its position
      DO i = Start+1, End          ! start with next elements
         IF (x(i) < Minimum) THEN  !   if x(i) less than the min?
            Minimum  = x(i)        !      Yes, a new minimum found
            Location = i                !      record its position
         END IF
      END DO
      FindMinimum = Location            ! return the position
   END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      INTEGER, INTENT(INOUT) :: a, b
      INTEGER                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

   SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER, INTENT(IN)                   :: Size
      INTEGER                               :: i
      INTEGER                               :: Location

      DO i = 1, Size-1             ! except for the last
         Location = FindMinimum(x, i, Size)  ! find min from this to last
         CALL  Swap(x(i), x(Location))  ! swap this and the minimum
      END DO
   END SUBROUTINE  Sort

!############################################################################### 
! --------------------------------------------------------------------
! REAL FUNCTION  Median() :
!    This function receives an array X of N entries, copies its value
! to a local array Temp(), sorts Temp() and computes the median.
!    The returned value is of REAL type.
! example: value=Median(DataArray, ActualSize)
! --------------------------------------------------------------------

   REAL FUNCTION  Median(X, N)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(IN) :: X
      INTEGER, INTENT(IN)                :: N
      INTEGER, DIMENSION(1:N)            :: Temp
      INTEGER                            :: i

      DO i = 1, N                       ! make a copy
         Temp(i) = X(i)
      END DO
      CALL  Sort(Temp, N)               ! sort the copy
      IF (MOD(N,2) == 0) THEN           ! compute the median
         Median = (Temp(N/2) + Temp(N/2+1)) / 2.0
      ELSE
         Median = Temp(N/2+1)
      END IF
   END FUNCTION  Median
!=====#################################################################
END MODULE functions_math
