MODULE fiduceo_config
!=====#################################################################
! NAME:
!		fiduceo_config.f90
! PURPOSE:
!		setting the configuration for Fiduceo
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Miriam Kosmale, 20.11.2018
! PROJECT:
!		FIDUCEO
!=====#################################################################
USE kinds
USE functions_runtime

IMPLICIT NONE




	INTEGER, parameter :: c_len_short		= 20
	CHARACTER (len=c_len)				:: lv1file
	CHARACTER (len=c_len)				:: lv1path
	CHARACTER (len=c_len_short)			:: project
	CHARACTER (len=c_len_short)			:: datatype
	CHARACTER (len=c_len_short)			:: level
	CHARACTER (len=c_len_short)			:: sensor
	CHARACTER (len=c_len_short)			:: satellite
	CHARACTER (len=c_len_short)			:: satellite_number
	CHARACTER (len=c_len_short)			:: orbit_starttime
	CHARACTER (len=c_len_short)			:: orbit_stoptime
	CHARACTER (len=c_len_short)			:: date_str
  
	
!=====#################################################################

CONTAINS
!=====#################################################################
SUBROUTINE read_config_file()
! PURPOSE:
! 		reads the configuration file
!		here all the settings for the retrieval are written
!		the file is taken from environment variable configfile
!		this file is produced by a shell script surrounding
! INPUTS:
!		configfile	:: UNIX environment variable
! CALLING SEQUENCE:
!		CALL read_config_file
! REVISION HISTORY:
! 		written by Miriam Kosmale 2015
!----------------------------------------------------------------------

    IMPLICIT NONE
    INTEGER					:: unit,istat,k
	CHARACTER(len=c_len)	:: string,string2
	CHARACTER(len=c_len)	:: configfile
	LOGICAL					:: dir_e
	
    !CALL get_environment_variable("configfile", configfile)
	CALL getenv("configfile", configfile)
	print*,'configfile:',configfile
	CALL get_lu (unit,istat)

    open(unit,FILE=trim(configfile),status="old",action="read")
!----------------------------------------------------------------------
!-------- skip comment lines
    do k=1,60
        read(unit,'(a)') string
        if (INDEX(trim(string),'>>>') .ne. 0) exit
    enddo
    do
 

    read(unit,'(a)',end=1) string

    select case (string(1:22))
!----------------------------------------------------------------------
!-------- read parameter
    case ("path                :")
	  string2=string(24:200)
      ! read(string2,*,iostat=istat)  lv1path
      lv1path=string2

	  case ("filename            :")
	  string2=string(24:200)
      read(string2,*,iostat=istat)  lv1file
	  print*,istat
	  
    case ("project             :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  project
	  
    case ("datatype            :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  datatype
	  
    case ("level               :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  level
	  
    case ("sensor              :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  sensor
	  
    case ("satellite           :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  satellite
	  
    case ("number              :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  satellite_number

    case ("starttime           :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  orbit_starttime

    case ("stoptime            :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  orbit_stoptime

    case ("date                :")
	  string2=string(24:44)
      read(string2,*,iostat=istat)  date_str


    end select

    end do
    1 close(unit)

  
  print*,lv1file
  print*,lv1path
  print*,project

END SUBROUTINE read_config_file
!=====#################################################################
END MODULE fiduceo_config