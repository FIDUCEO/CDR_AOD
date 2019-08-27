SUBROUTINE fiduceo_readcoeffs()
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		reading coefficient tables for aod retrieval
!                      trcoeff (inversion AOD = f(RTOA, ALB, angles)
!                      aerosol mixing climatology (Kinne 2006)
!                      bfactor for conversion of TOA REF3 (3.7micron) into surface albedo at 0.63 micron
!                      angle fields
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Thomas Popp, 04.01.2019
! PROJECT:
!		FIDUCEO
!=====#################################################################
 USE kinds
 USE functions_ascii
 USE functions_runtime
 USE fiduceo_config
 USE fiduceo_coeffs
 USE netcdf


IMPLICIT NONE

include '/usr/include/netcdf.inc'

      character (LEN=200)     :: file0
      character (LEN=28)      :: mixname
      integer*4               :: l,ib,it,is,ia,jm,k,j,nrec,jk
      real*4                  :: val
      integer*4               :: naed,ixm,ixlat,ixlon
      character (LEN=10)      :: aetext
      character (LEN=80)      :: aefname,aedirname
      character (LEN=120)     :: aename
      real*4                  :: inmon,inlat,inlon,infrac
      integer*4               :: jx,jy

      integer                 :: ncid, varid_B, retval, kl, ndims
      character(LEN=100)      :: FILE_NAME, dim_name

      integer                 :: nazid

      real*4                  :: albisi061,albisi086,albisi166

      integer                 :: itamue,itamus,itaphi
      character (LEN=8)       :: text

      namelist/list5h/hmess
      namelist /list7/nazid,azidg
          
	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------

	
 

!----------------------------------------------------
! read trcoef, breakrtoa
!----------------------------------------------------

 print*,'read retrieval coefficient tables...'

     do 70 jk=1,36

           if (jk.eq. 1) j=1
           if (jk.eq. 2) j=4
           if (jk.eq. 3) j=7
           if (jk.eq. 4) j=10
           if (jk.eq. 5) j=41
           if (jk.eq. 6) j=44
           if (jk.eq. 7) j=47
           if (jk.eq. 8) j=50
           if (jk.eq. 9) j=61
           if (jk.eq.10) j=64
           if (jk.eq.11) j=67
           if (jk.eq.12) j=70
           if (jk.eq.13) j=101
           if (jk.eq.14) j=104
           if (jk.eq.15) j=107
           if (jk.eq.16) j=110
           if (jk.eq.17) j=121
           if (jk.eq.18) j=124
           if (jk.eq.19) j=127
           if (jk.eq.20) j=130
           if (jk.eq.21) j=11
           if (jk.eq.22) j=14
           if (jk.eq.23) j=17
           if (jk.eq.24) j=20
           if (jk.eq.25) j=31
           if (jk.eq.26) j=34
           if (jk.eq.27) j=37
           if (jk.eq.28) j=40
           if (jk.eq.29) j=71
           if (jk.eq.30) j=74
           if (jk.eq.31) j=77
           if (jk.eq.32) j=80
           if (jk.eq.33) j=91
           if (jk.eq.34) j=94
           if (jk.eq.35) j=97
           if (jk.eq.36) j=100

           mixnum(j)=jk
           mixnum2(jk)=j

       if (j.eq.  1) mixname='mlsapp000000000000000000rh00'
       if (j.eq.  4) mixname='mlsa67330000000000000000rh00'
       if (j.eq.  7) mixname='mlsa34660000000000000000rh00'
       if (j.eq. 10) mixname='mlsa00pp0000000000000000rh00'
       if (j.eq. 11) mixname='mlsa81000019000000000000rh00'
       if (j.eq. 14) mixname='mlsa55260019000000000000rh00'
       if (j.eq. 17) mixname='mlsa28530019000000000000rh00'
       if (j.eq. 20) mixname='mlsa02790019000000000000rh00'
       if (j.eq. 31) mixname='mlsa81001900000000000000rh00'
       if (j.eq. 34) mixname='mlsa55261900000000000000rh00'
       if (j.eq. 37) mixname='mlsa28531900000000000000rh00'
       if (j.eq. 40) mixname='mlsa02791900000000000000rh00'
       if (j.eq. 41) mixname='mlsa61000039000000000000rh00'
       if (j.eq. 44) mixname='mlsa42190039000000000000rh00'
       if (j.eq. 47) mixname='mlsa22390039000000000000rh00'
       if (j.eq. 50) mixname='mlsa02590039000000000000rh00'
       if (j.eq. 61) mixname='mlsa61003900000000000000rh00'
       if (j.eq. 64) mixname='mlsa42193900000000000000rh00'
       if (j.eq. 67) mixname='mlsa22393900000000000000rh00'
       if (j.eq. 70) mixname='mlsa02593900000000000000rh00'
       if (j.eq. 71) mixname='don140000060000000000000rh00'
       if (j.eq. 74) mixname='don127130060000000000000rh00'
       if (j.eq. 77) mixname='don114260060000000000000rh00'
       if (j.eq. 80) mixname='don101390060000000000000rh00'
       if (j.eq. 91) mixname='mls240006000000000000000rh00'
       if (j.eq. 94) mixname='mls227136000000000000000rh00'
       if (j.eq. 97) mixname='mls214266000000000000000rh00'
       if (j.eq.100) mixname='mls201396000000000000000rh00'
       if (j.eq.101) mixname='don120000080000000000000rh00'
       if (j.eq.104) mixname='don114060080000000000000rh00'
       if (j.eq.107) mixname='don107130080000000000000rh00'
       if (j.eq.110) mixname='don101190080000000000000rh00'
       if (j.eq.121) mixname='mls220008000000000000000rh00'
       if (j.eq.124) mixname='mls214068000000000000000rh00'
       if (j.eq.127) mixname='mls207138000000000000000rh00'
       if (j.eq.130) mixname='mls201198000000000000000rh00'

       file0='/users/hpopp/syslin/dat/tab/avhrr/isot/acoef4_'//mixname

       open (unit = 19, file=file0, access='DIRECT', recl=4)
       nrec=1

       do 34 l=1,maxlamatsr
         read (19,rec=nrec) dmtyp(l,j)
         nrec=nrec+1
         read (19,rec=nrec) d0(l,j)
         nrec=nrec+1
         read (19,rec=nrec) dfak(l,j)
         nrec=nrec+1
! *** patch
         if (l.eq.1) dfak(l,j)=11.53
   34  continue


       l=2

        do 39 ib=1,ind5
         do 38 it=nm2,nmm
          do 37 is=nm0,nmm
           do 36 jm=1,2*nlut
            do 35 ia=1,3
             read (19,rec=nrec) val
             trcoef(l,ib,it,is,jm,j,ia)=val
             nrec=nrec+1
   35       continue
   36      continue
   37     continue
   38    continue
   39   continue

       do 60 l=1,maxlamatsr
        do 59 ib=1,ind5
         do 58 it=nm2,nmm
          do 57 is=nm0,nmm
           do 56 jm=1,6
            do 55 k=1,3
              read (19,rec=nrec) acoef(l,ib,it,is,jm,k,j)
              nrec=nrec+1
   55       continue
   56      continue
   57     continue
   58    continue
   59   continue
   60  continue

        do 64 ib=1,ind5
         do 63 it=nm2,nmm
          do 62 is=nm0,nmm
            do 61 k=1,nlut
             read (19,rec=nrec) breakrtoa(j,ib,it,is,k)
             nrec=nrec+1
   61	    continue
   62	   continue
   63	  continue
   64	continue

       close (19)

   70 continue

!----------------------------------------------------
! read AEROCOM mixing climatology
!----------------------------------------------------

  print*,'read AEROCOM median aerosol type mixing climatology...'

      aedirname = '/users/hpopp/sysneu/dat/aerocom/'
      
      aefname = 'aod550_aer_dust_coarse_ratio_mo.txt'
      naed=lnblnk(aedirname)
      aename=aedirname(1:naed)//aefname
      open (unit=40, file=aename)
      read (40,*) aetext
      
      do 763 ixm=1,12
      
         do 762 ixlat=1,180
	 
	    do 761 ixlon=1,360
	    
	       read (40,*) inmon,inlat,inlon,infrac
	       dustfrac(ixm,ixlat,ixlon)=infrac
	       
  761       continue
   
  762    continue
      
  763 continue

      close (40)

      aefname = 'aod550_aer_fine_total_ratio_mo.txt'

      naed=lnblnk(aedirname)
      aename=aedirname(1:naed)//aefname
      open (unit=40, file=aename)
      read (40,*) aetext
      
      do 766 ixm=1,12
      
         do 765 ixlat=1,180
	 
	    do 764 ixlon=1,360
	    
	       read (40,*) inmon,inlat,inlon,infrac
	       finefrac(ixm,ixlat,ixlon)=infrac
	       
  764       continue
   
  765    continue
      
  766 continue

      close (40)

      aefname = 'aod550_aer_fine_less_abs_type_fraction_mo.txt'
      naed=lnblnk(aedirname)
      aename=aedirname(1:naed)//aefname
      open (unit=40, file=aename)
      read (40,*) aetext
      
      do 769 ixm=1,12
      
         do 768 ixlat=1,180
	 
	    do 767 ixlon=1,360
	    
	       read (40,*) inmon,inlat,inlon,infrac
	       absfrac(ixm,ixlat,ixlon)=infrac
	       
  767       continue
   
  768    continue
      
  769 continue

      close (40)

!----------------------------------------------------
! define mixing variables for 130 aerosol types
!----------------------------------------------------

      do 863 j=1,maxmixatsr
            if (j.le.10) xfmf(j)=1.0
            if (j.ge.11.and.j.lt.41) xfmf(j)=0.8
            if (j.ge.41.and.j.lt.71) xfmf(j)=0.6
            if (j.ge.71.and.j.lt.101) xfmf(j)=0.4
            if (j.ge.101) xfmf(j)=0.2
            jx=j-((j-1)/10)*10
            xfmaf(j)=(jx-1)*0.111
            jy=(j-1)/10+1
            if (jy.eq.1) xcmsf(j)=0.0
            if (jy.eq.2.or.jy.eq.5.or.jy.eq.8.or.jy.eq.11) xcmsf(j)=0.0
            if (jy.eq.3.or.jy.eq.6.or.jy.eq.9.or.jy.eq.12) xcmsf(j)=0.5
            if (jy.eq.4.or.jy.eq.7.or.jy.eq.10.or.jy.eq.13) xcmsf(j)=1.0
  863 continue

!------------------------------------------
! read B factor table for albedo estimation
!------------------------------------------

 print*,'read B factor tables...'

      FILE_NAME='/users/hpopp/syslin/dat/tab/B_AVHRR.nc'
      retval = nf90_open(trim(FILE_NAME), NF_NOWRITE, ncid)                                   
      retval = nf90_inq_varid(ncid, 'B_nogaps', varid_B)
      retval = nf90_get_var(ncid, varid_B, data_in)          
      retval = nf90_close(ncid)

  print*,'B factor: ',minval(data_in),maxval(data_in)
;  print*,'B factor (100,100): ',data_in(100,100)

!-----------------
! read angle fields
!-----------------

      open (unit=11, file='/users/hpopp/syslin/dat/angl/NMU26')
      read (11,list5h)
      close(unit=11)

      open (unit=12, file='/users/hpopp/syslin/dat/angl/NAZ19')
      read(12,list7)
      close(unit=12)

!-----------------------------------------
! read brdf field and calcualte indicatrix
!-----------------------------------------

! *** read + normalize WALD indicatrices for 3 wavelengths

! *** WIES
      albisi061 = 0.027
      albisi086 = 0.405
      albisi166 = 0.346

! *** WALD
      albisi061 = 0.029
      albisi061 = 0.019
      albisi086 = 0.146
      albisi166 = 0.081

! *** SAVA
      albisi061 = 0.098
!      albisi061 = 0.047
      albisi086 = 0.184
      albisi166 = 0.384

      open(44,file='/users/hpopp/syslin/dat/kriebel/sava061')
!      open(44,file='/users/hpopp/syslin/dat/kriebel/wald061')
!      open(44,file='/users/hpopp/syslin/dat/kriebel/wies061')


      read (44,'(a8)') text
  720 format (a8)
      read (44,list62)
      read (44,list63)
      read (44,list64)
      do 605 itamus=1,ntamus
        do 604 itaphi=1,ntaphi
          read (44,'(10F8.5)') (tabalb061(itamue,itaphi,itamus),itamue=1,ntamue) 
          do 607 itamue=1,ntamue
            tabalb061(itamue,itaphi,itamus)=tabalb061(itamue,itaphi,itamus)/albisi061
  607     continue    
  604   continue
  605 continue
      close(44)

      print*,'tabalb: ',maxval(tabalb061),minval(tabalb061)

      print*,tabmue
      print*,tabmus
      print*,tabphi


  RETURN
 END SUBROUTINE fiduceo_readcoeffs

