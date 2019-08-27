PROGRAM fiduceo_main
!=====#################################################################
! NAME:
!		fid_avhrr_aod.f90
! PURPOSE:
!		retrieve AOD from AVHRR data
! INCLUDES:
!		---
! NEEDS:
!		---
! AUTHOR:
!		written by Thomas Popp, 03.01.2019
!               with contributions by Miriam Kosmale, 20.11.2018
! PROJECT:
!		FIDUCEO
!=====#################################################################
USE kinds
USE fiduceo_config
USE fiduceo_data
USE fiduceo_avhrr_read
USE fiduceo_coeffs

IMPLICIT NONE

        CHARACTER(LEN=200)        :: outfile,inputpath,filex1,filex2
        CHARACTER(LEN=1)          :: arginput
        INTEGER                   :: irec,nx1,nx2
        INTEGER                   :: recl2
        INTEGER                   :: ind3a,ind3b
        INTEGER                   :: stat,system,n1
        INTEGER                   :: ixmon,ixlat,ixlon

  write(*,*) '#########################################################'
  write(*,*) '##                                                     ##'
  write(*,*) '##                     FIDUCEO                         ##'
  write(*,*) '##                  AOD retrieval                      ##'
  write(*,*) '##                                                     ##'
  write(*,*) '##   dark field inversion + uncertainty propagation    ##'
  write(*,*) '##             written by Thomas Popp                  ##'
  write(*,*) '##      with major contributions by Miriam Kosmale     ##'
  write(*,*) '##                    DLR DFD-ATM                      ##'
  write(*,*) '##                                                     ##'
  write(*,*) '#########################################################'
  write(*,*)
  
  CALL getarg (1,arginput)

  CALL read_config_file

  CALL read_orbit

  
  write(*,*) '#########################################################'
  print*,'                     Reading data finished'
  write(*,*) '#########################################################'

        print*,trim(lv1path)


!-------------------------------
! write layers needed for apollo 
!-------------------------------


  if (arginput.eq.'1') CALL fiduceo_writeforapollo


!-----------------------------------------------
!  start APOLLO_NG cloud probability calculation
!-----------------------------------------------

  if (arginput.eq.'1') then
  inputpath=trim(adjustl(lv1path))
  n1=lnblnk(inputpath)
  print*,inputpath(1:n1)//'process_apollo.csh'
  open (unit=34,file=inputpath(1:n1)//'process_apollo.csh')
  write (34,*) '#!/bin/csh -f'
  write (34,*) '#'
  write (34,*) 'cd '//inputpath
  write (34,*) 'setenv APOLORB '//inputpath
  write (34,*) 'bash '//'/users/hpopp/sysneu/src/APOLLO_NG_Release1/APOLLO13_SRC/run_apollo_ng_fid.sh'
  close (34)

  stat=system('chmod 777 '//inputpath(1:n1)//'process_apollo.csh')
  stat=system('/bin/csh '//inputpath(1:n1)//'process_apollo.csh'//' > '//inputpath(1:n1)//'apollo.log')

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_CLOUDMASK.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_CLOUDMASK.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_LSGM.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_LSGM.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_PCLD.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_PCLD.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_REF3.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_REF3_UI.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_UI.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_REF3_UC.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_UC.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_REF3_US.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_REF3_US.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  filex1=inputpath(1:n1)//'/test_AVHRR_APOLLONG_T3.7.DAT'
  nx1=lnblnk(filex1)
  filex2=inputpath(1:n1)//'/'//lv1file(31:44)//'_test_AVHRR_APOLLONG_T3.7.DAT'
  nx2=lnblnk(filex2)
  stat=system('mv '//filex1(1:nx1)//' '//filex2(1:nx2))

  endif

!------------------------
! read layers from apollo 
!------------------------

  CALL fiduceo_readfromapollo

!-----------------------
! read coefficient files 
!-----------------------


  CALL fiduceo_readcoeffs

!-------------------------------------------------------------
! select + prepare dark fields (estimate their surface albedo) 
!-------------------------------------------------------------

  CALL fiduceo_prepare_darkfields



!--------------
! aod retrieval 
!--------------

  CALL fiduceo_aod_retrieval



!------------------------
! write 4km output layers 
!------------------------

  CALL fiduceo_write_netcdf


!----------------------------------------------------------
! calculate 3x3 layers and cloud masked-induced uncertainty 
!----------------------------------------------------------


  CALL fiduceo_average


!------------------------
! write 12km output layers 
!------------------------

  CALL fiduceo_write_netcdf2


!=====#################################################################
END PROGRAM fiduceo_main
