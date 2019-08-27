SUBROUTINE fiduceo_readfromapollo()
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		reading AVHRR lv1 data from APOLLO (binary)
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
	INTEGER(KIND=p_long)	  :: status
	
  print*,'read binary files from APOLLO_NG...'

!----------------------------------------------------
! read layers from apollo
!----------------------------------------------------

        allocate(CLM(dimx2,dimy), stat=status)
        allocate(LSGM(dimx2,dimy), stat=status)
        allocate(PCLD(dimx2,dimy), stat=status)
        allocate(REF3(dimx2,dimy), stat=status)
        allocate(REF3_U_I(dimx2,dimy), stat=status)
        allocate(REF3_U_C(dimx2,dimy), stat=status)
        allocate(REF3_U_S(dimx2,dimy), stat=status)

        irec = 1
        recl2 = 2*(dimx2*dimy)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_CLOUDMASK.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) CLM(:,:)
        CLOSE(101)


        irec = 1
        recl2 = 2*(dimx2*dimy)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_LSGM.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) LSGM(:,:)
        CLOSE(101)

        irec = 1
        recl2 = 2*(dimx2*dimy)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_PCLD.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) PCLD(:,:)
        CLOSE(101)


        irec = 1
        recl2 = 4*(dimx2*dimy)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) REF3(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_UI.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) REF3_U_I(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_UC.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) REF3_U_C(:,:)
        CLOSE(101)

        outfile = TRIM(adjustl(lv1path))//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_US.DAT'
        OPEN(101,FILE=TRIM(outfile),ACCESS='DIRECT',RECL=recl2,ACTION='READ')
          READ (101,REC=irec) REF3_U_S(:,:)
        CLOSE(101)

  print*,'ref3_u_i: ',minval(ref3_u_i),maxval(ref3_u_i)
  print*,'ref3_u_c: ',minval(ref3_u_c),maxval(ref3_u_c)
  print*,'ref3_u_s: ',minval(ref3_u_s),maxval(ref3_u_s)


  RETURN
 END SUBROUTINE fiduceo_readfromapollo

