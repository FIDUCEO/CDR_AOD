SUBROUTINE fiduceo_writeforapollo()
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		writing AVHRR lv1 data for APOLLO (binary)
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Thomas Popp, 03.01.2019
! PROJECT:
!		FIDUCEO
!=====#################################################################
 USE kinds
 USE functions_ascii
 USE functions_runtime
 USE fiduceo_config
 USE fiduceo_data

IMPLICIT NONE

!	INTEGER				:: dimx,dimy
	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------


        CHARACTER(LEN=200)        :: outfile
        INTEGER                   :: irec
        INTEGER                   :: recl2
        INTEGER                   :: ind3a,ind3b
	
  print*,'write binary files for APOLLO_NG...'

!----------------------------------------------------
! write layers needed for apollo
!----------------------------------------------------

        ind3a=0
        ind3b=0
        if (MAXVAL(reflectance(3,:,:)).gt.0.) ind3a=1
        if (MAXVAL(brightness_temperature(1,:,:)).gt.0.) ind3b=1

        outfile = TRIM(adjustl(lv1path))//'/size'
        OPEN(102,FILE=TRIM(outfile))
          WRITE (102,'(i3,x,i5)') dimy,dimx
          WRITE (102,'(2i2)') ind3a,ind3b
        CLOSE(192)

        irec = 1
        recl2 = 4*(dimx*dimy)

        outfile = TRIM(adjustl(lv1path))//'/AREA31.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) reflectance(1,:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/AREA32.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) reflectance(2,:,:)
        CLOSE(101)

        if (MAXVAL(brightness_temperature(1,:,:)).gt.0.) then
        print*,'writing channel 3B'
        outfile = TRIM(adjustl(lv1path))//'/AREA37.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) brightness_temperature(1,:,:)
        CLOSE(101)
        endif

        outfile = TRIM(adjustl(lv1path))//'/AREA34.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) brightness_temperature(2,:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/AREA35.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) brightness_temperature(3,:,:)
        CLOSE(101)

        if (MAXVAL(reflectance(3,:,:)).gt.0.) then
        print*,'writing channel 3A'
        outfile = TRIM(adjustl(lv1path))//'/AREA37.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) reflectance(3,:,:)
        CLOSE(101)
        endif

        outfile = TRIM(adjustl(lv1path))//'/AREA38.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) solzen(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/AREA39.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) satzen(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/AREA40.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) relazi(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/latitude.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) latitude(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/longitude.dta'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='WRITE')
          WRITE (101,REC=irec) longitude(:,:)
        CLOSE(101)

  RETURN
 END SUBROUTINE fiduceo_writeforapollo

