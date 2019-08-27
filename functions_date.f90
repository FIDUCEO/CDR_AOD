MODULE functions_date
!=====#################################################################
! NAME:
!		functions_date.f90
! PURPOSE:
! 		Module with several function to handle Date-Formats
! CALLING SEQUENCE:
!		-
! NEEDS:
!		kinds.f90
! INCLUDES:
!		get_days_of_month
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
USE kinds
IMPLICIT NONE 
CONTAINS
  
!=====#################################################################
SUBROUTINE get_days_of_month(year,month,ndays)
! PURPOSE:
! 		get the numbers of days of a month
!		taking leap years into account
! CALLING SEQUENCE:
!		year=2008
!		month=2
!		CALL get_days_of_month(year,month,ndays)
!		print*,ndays
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------
	INTEGER(KIND=p_short),intent(in)		:: year,month
	INTEGER(KIND=p_short),intent(out)		:: ndays
	
    INTEGER(KIND=p_short)					:: mod_4,mod_100,mod_400
    INTEGER(KIND=p_short)					:: schaltjahr
	
		IF (month .EQ. 1)	ndays=31
		IF (month .EQ. 3)	ndays=31
		IF (month .EQ. 5)	ndays=31
		IF (month .EQ. 7)	ndays=31
		IF (month .EQ. 8)	ndays=31
		IF (month .EQ. 10)	ndays=31
		IF (month .EQ. 12)	ndays=31
		IF (month .EQ. 4)	ndays=30
		IF (month .EQ. 6)	ndays=30
		IF (month .EQ. 9)	ndays=30
		IF (month .EQ. 11)	ndays=30
		schaltjahr	=0
		mod_4 		= mod ( year, 4 )
		mod_100 	= mod ( year, 100 )
		mod_400 	= mod ( year, 400 )
		IF (mod_100 .NE. 0 .AND. mod_4 .EQ. 0) &
			schaltjahr=1
		IF (mod_100 .EQ. 0 .AND. mod_4 .EQ. 0 &
			.AND. mod_400 .EQ. 0) &
			schaltjahr=1
		
		IF (month .EQ. 2 .AND. schaltjahr .EQ. 1) ndays	=29
		IF (month .EQ. 2 .AND. schaltjahr .EQ. 0) ndays	=28


END SUBROUTINE get_days_of_month
!=====#################################################################

!=====#################################################################
END MODULE functions_date
