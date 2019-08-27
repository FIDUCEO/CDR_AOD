MODULE functions_runtime
!=====#################################################################
! NAME:
!		functions_runtime.f90
! PURPOSE:
! 		Module with several functions to handle processing issues
! CALLING SEQUENCE:
!		-
! NEEDS:
!		kinds.f90
! INCLUDES:
!		istatus
!		loopstatus
!		processing_time
!		get_lu
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
USE kinds
USE functions_date
IMPLICIT NONE 

! Global variable from SUBROUTINE loopstatus
	INTEGER(KIND=p_long)			:: statuscheck_v(11)
	INTEGER(KIND=p_long)			:: statuscheck_i
	
! Global variable from SUBROUTINE processing_time
	INTEGER(KIND=p_short)			:: processing_start(8)
	INTEGER(KIND=p_short)			:: processing_stop(8)
	
CONTAINS
  
!=====#################################################################
SUBROUTINE istatus (status, action)
! PURPOSE:
! 		returns the status of an action
! CALLING SEQUENCE:
! REVISION HISTORY:
! 		written by Lars Klueser 2014
!----------------------------------------------------------------------
    INTEGER           :: status
    CHARACTER (LEN=*) :: action

    if (status /= 0) then
		write (*, *) 'Error while reading netCDF input file during performing ', action
		write (*, *) 'NetCDF error code: ', status
		stop
    endif

END SUBROUTINE istatus
!=====#################################################################
! SUBROUTINE loopstatus(i_inner,j_outer,statuscheck_v,statuscheck_i)
SUBROUTINE loopstatus(i_inner,j_outer)
! PURPOSE:
! 		prints a status on how much a loop is already finished
! CALLING SEQUENCE:
!		CALL loopstatus((/i,i0,ni/),(/j,j0,nj/),statuscheck_v,statuscheck_i)
! INPUT:
!		i_inner :: array with (/i,i0,ni/)
!		j_outer :: array with (/j,j0,nj/)
!		i		:: index of the inner loop
!		j		:: index of the outer loop
!		i0		:: number of first iteration of the inner loop
!		j0		:: number of first iteration of the outer loop
!		ni		:: number of iterations of the inner loop
!		nj		:: number of iterations of the outer loop
! REVISION HISTORY:
! 		written by Miriam Kosmale 2014
!----------------------------------------------------------------------
  INTEGER(KIND=p_long),INTENT(in)			:: i_inner(3)
  INTEGER(KIND=p_long),INTENT(in)			:: j_outer(3)
  
  INTEGER(KIND=p_long)						:: i,j
  INTEGER(KIND=p_long)						:: i0,j0
  INTEGER(KIND=p_long)						:: ni,nj 
  INTEGER(KIND=p_long)						:: n
  INTEGER(KIND=p_long)						:: ij,ij0
  
  REAL(KIND=p_real)							:: pproc
  
  REAL(KIND=p_real),parameter				:: procstat(11)= &
			(/1.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100./)
  
  i		= i_inner(1)
  i0	= i_inner(2)
  ni	= i_inner(3)
  j		= j_outer(1)
  j0	= j_outer(2)
  nj	= j_outer(3)
  n		= ni*nj
  
  ij	= (j-1)*ni+i
  ij0	= (j0-1)*ni+i0
  pproc	= FLOAT(ij)/n*100.
  
  IF (i .EQ. i0 .AND. j .EQ. j0) &
	statuscheck_v=(/0,0,0,0,0,0,0,0,0,0,0/)
  IF (i .EQ. i0 .AND. j .EQ. j0) statuscheck_i=1

  IF (ij .EQ. ij0) print*,'relax and take a break'
  IF (ij .EQ. ij0) print*,'there is enough time for a coffee!'
  
  IF (statuscheck_v(statuscheck_i) .EQ. 0 &
	.AND. pproc .gt. procstat(statuscheck_i)) THEN
     print*,'that many percent processed [%]:',procstat(statuscheck_i)
     IF (statuscheck_i .lt. 10) print*,'relax and take a break'
     IF (statuscheck_i .lt. 5) &
		print*,'there is still time for a coffee'
     IF (statuscheck_i .ge. 5 .AND. statuscheck_i .lt. 7) &
		print*,'coffee time limited '
     IF (statuscheck_i .ge. 7) &
		print*,'maybe not enough time for coffee'
     IF (statuscheck_i .ge. 9) &
		print*,'processor soon finished'
		
	 statuscheck_v(statuscheck_i)	=	1
	 statuscheck_i					=	statuscheck_i+1
  ENDIF
  
END SUBROUTINE loopstatus
!=====#################################################################
SUBROUTINE processing_time(step)
! PURPOSE:
! 		prints a status on the processing time needed
! INPUT:
!		step =  1 then the first call to initialize the values
!		step /= 1 the last call to print the processing time needed
! REVISION HISTORY:
! 		written by Miriam Kosmale 2014
!----------------------------------------------------------------------
    INTEGER,INTENT(in)		:: step
    INTEGER(KIND=p_short)	:: d_month,d_day
    INTEGER(KIND=p_short)	:: d_hh,d_min,d_sec,max_days
	CHARACTER (LEN=10)		:: d_month_str,d_day_str
	CHARACTER (LEN=10)		:: d_hh_str,d_min_str,d_sec_str
	INTEGER(KIND=p_short)	:: diffvalues(8)
	INTEGER(KIND=p_short)	:: diffvalues2(8)
	INTEGER(KIND=p_short)	:: processing_start2(8)
	
	! beginning of processing
	IF (step .EQ. 1) THEN
		CALL date_and_time(VALUES=processing_start)
	ELSE
	! end of processing
		CALL date_and_time(VALUES=processing_stop)
		
		processing_start2=processing_start
		diffvalues=processing_stop-processing_start
		diffvalues2=diffvalues
		
		CALL get_days_of_month(processing_start(1),processing_start(2),max_days)
		
		! get the difference second
		IF (diffvalues(7) .LT. 0) THEN 
			! d_sec=diffvalues(7)+60 
			d_sec=60-processing_start(7)+processing_stop(7) 
			processing_start2(6)=processing_start2(6)+1
			diffvalues=processing_stop-processing_start2
		ELSE
			d_sec=diffvalues(7)
		ENDIF
		! get the difference minute
		IF (diffvalues(6) .LT. 0) THEN 
			! d_min=diffvalues(6)+60 
			d_min=60-processing_start(6)+processing_stop(6) 
			processing_start2(5)=processing_start2(5)+1
			diffvalues=processing_stop-processing_start2
		ELSE
			d_min=diffvalues(6)
		ENDIF
		! get the difference hour
		IF (diffvalues(5) .LT. 0) THEN 
			! d_hh=diffvalues(5)+24 
			d_hh=24-processing_start(5)+processing_stop(5) 
			processing_start2(4)=processing_start2(4)+1
			diffvalues=processing_stop-processing_start2
		ELSE
			d_hh=diffvalues(5)
		ENDIF
		! get the difference day
		IF (diffvalues(3) .LT. 0) THEN 
			! d_day=diffvalues(3)+max_days 
			d_day=max_days-processing_start(3)+processing_stop(3) 
			processing_start2(2)=processing_start2(2)+1
			diffvalues=processing_stop-processing_start2
		ELSE
			d_day=diffvalues(3)
		ENDIF
		! get the difference day
		IF (diffvalues(2) .LT. 0) THEN 
			! d_month=diffvalues(2)+12 
			d_month=12-processing_start(2)+processing_stop(2) 
			processing_start2(1)=processing_start2(1)+1
			diffvalues=processing_stop-processing_start2
		ELSE
			d_month=diffvalues(2)
		ENDIF
		
		Write( d_hh_str, '(i2)' ) d_hh
		Write( d_min_str, '(i2)' ) d_min
		Write( d_sec_str, '(i2)' ) d_sec
		Write( d_day_str, '(i2)' ) d_day
		Write( d_month_str, '(i2)' ) d_month
		
		print*,'Processing time was: ',processing_stop
		print*,'Processing time was: ',processing_start
		print*,'Processing time was: ',diffvalues
		print*,'Processing time was: '//trim(d_month_str)//' months'
		print*,'                     '//trim(d_day_str)//' days'
		print*,'                     '//trim(d_hh_str)//' hours'
		print*,'                     '//trim(d_min_str)//' minutes'
		print*,'                     '//trim(d_sec_str)//' seconds'
	ENDIF

END SUBROUTINE processing_time
!=====#################################################################
SUBROUTINE get_lu (lu, istat)
!
      IMPLICIT NONE                ! Keine implizite Deklaration !!
!  ===DOC-Anfang================================================================
!  Name         : get_lu.for
!  Art          : Fortran SUBROUTINE
!  Funktion     : Freie LOGICAL UNIT (LU) suchen.
!  Ersteller/in : Helmut Reutter
!  Datum        : 4 Juni 1992
!  Aufruf       : CALL get_lu (lu, istat)
!  Library      : isyslib.a
!  Erklärung    : Eine freie LOGICAL UNIT suchen oder überprüfen ob sie frei ist
!                                Für LOGICAL UNITs (LU) kleiner als 0, wird überprüft, ob
!                                die LOGICAL UNIT mit diesem positiven Zahlenwert frei ist.
!                                Ansonsten wir eine freie LOGICAL UNIT gesucht.
!                                Es wird mit 10 begonnen und aufsteigend bis 100 gesucht.
!                                Die 1. frei LOGICAL UNIT wird ins aufrufende Programm
!                                übergeben.
!  Parameter    : (in der Reihenfolge der Parameterliste)
!     Typ           Name         Erklärung
!     EIN:
!     AUS:                      !
      INTEGER        lu                            ! 10-100 freie LOGICAL UNIT
!                                                             ! lu < 0 wird auf FREI geprüft.
      INTEGER        istat                       ! 0  = ok,
                                                               ! -1 = keine freie LOGICAL UNIT vorhanden
!                               !
!  Compiler     : f77
!
!  Common       : ---
!
!  Include      : ---
!
!  Änderungen   :
!                 Datum     Erklärung (mit Name)
!                                21.10.92  lu < 0 wird auf FREI geprüft (Ol).
!  ===DOC-Ende==================================================================
      INTEGER        i
      LOGICAL        besetzt_

      istat = 0
      IF (lu .LT. 0) THEN
         i = ABS (lu)
         INQUIRE (UNIT       = i,OPENED = besetzt_,IOSTAT = istat,ERR = 999)
         IF (.NOT. besetzt_) THEN
            lu = i
            RETURN
         END IF
      ELSE      
         DO i = 10, 100
            INQUIRE (UNIT = i,OPENED = besetzt_, IOSTAT = istat,ERR = 999)
            IF (.NOT. besetzt_) THEN
               lu = i
               RETURN
            END IF
         END DO
      ENDIF
      istat = -1
999   RETURN
END SUBROUTINE get_lu
!=====#################################################################
END MODULE functions_runtime
