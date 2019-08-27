MODULE functions_array
!=====#################################################################
! NAME:
!		functions_array.f90
! PURPOSE:
! 		Module with several function for array manipulation
! CALLING SEQUENCE:
!		-
! NEEDS:
!		kinds.f90
! INCLUDES:
!		whereidxi
!		whereidxf
!		whereidxfge
!		arrayselect
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
USE kinds
IMPLICIT NONE 
CONTAINS
  
!=====#################################################################
SUBROUTINE whereidxi(v,vidx,idx)
! PURPOSE:
! 		returns the first index, where the array has a selected value
! INPUT:
!		v			::	the array
!		vidx		::	the value to be searched
! OUTPUT:
!		idx			::	the index
! CALLING SEQUENCE:
!		CALL whereidxi(v,vidx,idx)
!		CALL whereidxf(v,vidx,idx)
!		CALL whereidxfge(v,vidx,idx)
!		print*,idx
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
	INTEGER(KIND=p_long),intent(in)				:: v(:)
	INTEGER(KIND=p_long),intent(in)				:: vidx
	INTEGER(KIND=p_long), intent(out)			:: idx
	INTEGER(KIND=p_long)						:: l,i
	  
	l	= size(v)
	idx	= -1
	do i=1,l
		if( v(i) == vidx ) then
			idx=i
		exit
		endif
	enddo

END SUBROUTINE whereidxi
!----------------------------------------------------------------------
SUBROUTINE whereidxf(v,vidx,idx)
	REAL(KIND=p_real),intent(in)				:: v(:)
	REAL(KIND=p_real),intent(in)				:: vidx
	INTEGER, intent(out)			:: idx
	INTEGER							:: l,i
	  
	l	= size(v)
	idx	= -1
	do i=1,l
		if( v(i) == vidx ) then
			idx=i
		exit
		endif
	enddo

END SUBROUTINE whereidxf
!----------------------------------------------------------------------
SUBROUTINE whereidxfge(v,vidx,idx)
	REAL(KIND=p_real),intent(in)				:: v(:)
	REAL(KIND=p_real),intent(in)				:: vidx
	INTEGER, intent(out)			:: idx
	INTEGER							:: l,i,c
	  
	l	= size(v)
	idx	= -1
	c	= 0
	do i=1,l
	if( v(i) .ge. vidx .and. c .eq. 0) then
		idx=i
		c=1
		exit
	endif
	enddo

END SUBROUTINE whereidxfge
!=====#################################################################
SUBROUTINE arrayselect(x,y,ny,flag,xnew,ynew)
! PURPOSE:
! 		selects only values, where certain value is true/not true
! INPUT:
!		x,y			::	the arrays
!		ny			::	the value in y, which should be true/not be true
!		flag		::	a flag, either 1 or 0
!							1 means check if ny is true
!							0 means check if ny is not true
! OUTPUT:
!		xnew,ynew	::	the new arrays
! CALLING SEQUENCE:
!		CALL arrayselect_ne(x,y,ny,xnew,ynew)
!		CALL arrayselect_eq(x,y,ny,xnew,ynew)
!		print*,xnew
!		print*,ynew
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
	REAL (KIND=p_real),intent(in)				:: x(:)
	REAL (KIND=p_real),intent(in)				:: y(:)
	REAL (KIND=p_real),intent(in)				:: ny
	INTEGER,intent(in)							:: flag
	REAL (KIND=p_real),intent(out), allocatable	:: ynew(:)
	REAL (KIND=p_real),intent(out), allocatable	:: xnew(:)
	integer(KIND=p_long), allocatable			:: idx(:)
	integer(KIND=p_long)						:: l,i,j,n
	  
	l 	= size(y)
	allocate(idx(l))
	idx	= 0
	IF (flag .EQ. 1) THEN
	WHERE (y .EQ. ny) idx=1
	ENDIF
	IF (flag .EQ. 0) THEN
	WHERE (y .NE. ny) idx=1
	ENDIF
	IF (flag .EQ. 2) THEN
	WHERE (y .GE. ny) idx=1
	ENDIF
	n	= SUM(idx)
	allocate(xnew(n))
	allocate(ynew(n))
	  
	j=1
	DO i=1,l
	IF (flag .EQ. 1) THEN
		IF( y(i) .EQ. ny ) THEN
			xnew(j)=x(i)
			ynew(j)=y(i)
			j=j+1
		ENDIF
	ENDIF
	IF (flag .EQ. 0) THEN
		IF( y(i) .NE. ny ) THEN
			xnew(j)=x(i)
			ynew(j)=y(i)
			j=j+1
		ENDIF
	ENDIF
	IF (flag .EQ. 2) THEN
		IF( y(i) .GE. ny ) THEN
			xnew(j)=x(i)
			ynew(j)=y(i)
			j=j+1
		ENDIF
	ENDIF
	ENDDO
END SUBROUTINE arrayselect

!=====#################################################################
END MODULE functions_array
