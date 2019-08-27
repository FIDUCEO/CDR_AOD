SUBROUTINE fiduceo_prepare_darkfields()
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		prepare dark fields for aod retrieval
!                      select dark fields from vegetation index and MIR channel
!                      select AEROCOM climatology mix
!                      select angle pointers ib, is, iz
!                      calculate ndvi, ndii fields
!                      estimate dark field albedo from vegetation index and MIR channel
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
 USE fiduceo_data
 USE fiduceo_coeffs

IMPLICIT NONE


	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------

  INTEGER                 :: ixmon,ixlat,ixlon,jmix,imix
  real*4                  :: latval,lonval,ensdist
  real*4                  :: edist(36)
  INTEGER(KIND=p_long)	  :: status
  integer                 :: lin,col,jk,ndiival,num
  real*4                  :: xabs,xfinefrac,xdust
  real*4                  :: xabsm,xfinefracm,xdustm
  integer                 :: lin1,lin2,col1,col2,ipcld
  integer*2               :: iptp,jsel,imask,icloud,ix1,ix2,ix3
  real*4                  :: ref37,ref16
  integer                 :: clthresh1,clthresh2
  real*4                  :: r3thresh,vithresh,sunmin
  real*4                  :: np1,np2,nc0,nc1,nc2,nall,nl,nl2

  integer*2               :: isun,ibmue,ibphi
  real*4                  :: sonhoe,thetb,phib

  real*4                  :: xndvi,xndii
  real*4, parameter       :: xndvi_min = -0.6
  real*4, parameter       :: xndii_min = -0.6
  real*4                  :: delta_ndvi, delta_ndii, diff_albedo
  real*4                  :: dB_ndvi, dB_ndii
  integer                 :: ndvi_index, ndii_index, B_dimids(2)
  integer                 :: ndvi_index_1, ndii_index_1
  real*4                  :: albfak,offset,albval,albfak_b,albfak_c,albfak2,albval2

  real*4                  :: db,pi
 
  integer                 :: dimxh,dimyh,nwrong

  CHARACTER(LEN=200)      :: outfile
  INTEGER                 :: irec
  INTEGER                 :: recl2
  INTEGER                 :: count

  real*4                  :: minB,maxB,minr37,maxr37,minndvi,maxndvi,minalb,maxalb

  real*4                  :: ref1,ref2,rad_err1,rad_err2,rad_err37,xndvidash1,xndvidash2,deltandvi

  real*4                  :: xoff,testfak
  real*4                  :: delta_ndvi2_i,delta_ndvi2_c,delta_ndvi2_s,delta_ndii2_i,delta_ndii2_c,delta_ndii2_s
  real*4                  :: albfak_u1_i,albfak_u1_c,albfak_u1_s,albfak_u2_i,albfak_u2_c,albfak_u2_s
  real*4                  :: albfak_u_i,albfak_u_c,albfak_u_s
  real*4                  :: albval_u1_i,albval_u1_c,albval_u1_s,albval_u2_i,albval_u2_c,albval_u2_s,albval_u3_c

  real*4                  :: aod_rayl_550, aod_rayl_red, aod_rayl_nir, aod_rayl_swir, angstrom_rayl
  real*4                  :: xmuenul,xmue,scat_function_rayl
  real*4                  :: devide_rayl_red,devide_rayl_nir,minus_rayl_red,minus_rayl_nir
  real*4                  :: albedo_red,albedo_nir,albedo_swir,darkalbmax,albedo_red0,corrval

  integer                 :: ityp,ialb
  real*4                  :: iopt,c1,c2,c3,albedo_red2,albedo_nir2,albedo_swir2

  integer                 :: itamue,itamues,itaphi,itamue2,itamues2,itaphi2,indang
  real*4                  :: tabval1,tabval2,xfak1,xfak2,xfak3

  real*4,allocatable      :: xndvi_u_i(:,:)
  real*4,allocatable      :: xndvi_u_c(:,:)
  real*4,allocatable      :: xndvi_u_s(:,:)
  real*4,allocatable      :: xndii_u_i(:,:)
  real*4,allocatable      :: xndii_u_c(:,:)
  real*4,allocatable      :: xndii_u_s(:,:)

!----------------------------------------------------
! select dark fields
!----------------------------------------------------
! conditions to be met:
!     must be land, no coast (next pixels also land)
!     must be cloudfree (with weakest cloud threshold
!     must be darker than threshold in MIR channel and larger vegetation index than threshold
! set to ptp if not land or not cloud free
! set to ptp = 1 if below strongest cloud threshold
! set to ptp = 2 if between wekest and strongeest cloud threshold

! also calculate ndvi, ndii and assign angle pointers (ib, is, iz)

      pi=4.*atan(1.)
      db=pi/180.

 print*,'select dark fields...'

       sunmin=15.

! *** set cloud thresholds

      clthresh1=5
      clthresh2=50



! *** set ptp thresholds
       r3thresh=0.14
       vithresh=0.2

  aod_rayl_550 = 0.0988
  angstrom_rayl = 4.
  aod_rayl_red = aod_rayl_550*(550./621.)**angstrom_rayl
  aod_rayl_nir = aod_rayl_550*(550./797.)**angstrom_rayl
  aod_rayl_swir = aod_rayl_550*(550./3700.)**angstrom_rayl

  allocate(PTP(dimx2,dimy), stat=status)
  allocate(ib(dimx2,dimy), stat=status)
  allocate(is(dimx2,dimy), stat=status)
  allocate(iz(dimx2,dimy), stat=status)
  allocate(ndvi(dimx2,dimy), stat=status)
  allocate(ndii(dimx2,dimy), stat=status)
  allocate(MIXSEL(dimx2,dimy), stat=status)
  allocate(ALB(dimx2,dimy), stat=status)
  allocate(ALB2(dimx2,dimy), stat=status)
  allocate(BFak(dimx2,dimy), stat=status)

  allocate(BFak2(dimx2,dimy), stat=status)
  allocate(BFak3(dimx2,dimy), stat=status)
  allocate(BFakratio(dimx2,dimy), stat=status)
  allocate(BFakratio2(dimx2,dimy), stat=status)

  allocate(ndvip(dimx2,dimy), stat=status)
  allocate(ndiip(dimx2,dimy), stat=status)
  allocate(ndvikorr(dimx2,dimy), stat=status)
  allocate(ndiikorr(dimx2,dimy), stat=status)
  allocate(ndvidiff(dimx2,dimy), stat=status)
  allocate(ndiidiff(dimx2,dimy), stat=status)
  allocate(ref1p(dimx2,dimy), stat=status)
  allocate(ref2p(dimx2,dimy), stat=status)
  allocate(ref37p(dimx2,dimy), stat=status)
  allocate(ref1korr(dimx2,dimy), stat=status)
  allocate(ref2korr(dimx2,dimy), stat=status)

  allocate(ref1korr2(dimx2,dimy), stat=status)
  allocate(ref2korr2(dimx2,dimy), stat=status)
  allocate(ndvikorr2(dimx2,dimy), stat=status)
  allocate(ndiikorr2(dimx2,dimy), stat=status)

  allocate(xndvi_u_i(dimx2,dimy), stat=status)
  allocate(xndvi_u_c(dimx2,dimy), stat=status)
  allocate(xndvi_u_s(dimx2,dimy), stat=status)
  allocate(xndii_u_i(dimx2,dimy), stat=status)
  allocate(xndii_u_c(dimx2,dimy), stat=status)
  allocate(xndii_u_s(dimx2,dimy), stat=status)

  allocate(albval_u_i(dimx2,dimy), stat=status)
  allocate(albval_u_c(dimx2,dimy), stat=status)
  allocate(albval_u_s(dimx2,dimy), stat=status)

  allocate(w(180,360,36), stat=status)

       dimxh=dimx2/2
       dimyh=dimy/2

       nall=0
       nl=0
       np1=0
       np2=0
       nc0=0
       nc1=0
       nc2=0
       nl2=0

       do 13 lin=1,dimy
        do 12 col=1,dimx2

             thetb=satzen(col,lin)
             sonhoe=90.-solzen(col,lin)
             phib=relazi(col,lin)
             if (abs(phib).gt.180.) phib=abs(360.-abs(phib))
             call ngeo (thetb,phib,sonhoe,isun,ibmue,ibphi)
             if (ibmue.lt.nm2) ibmue=nm2
             if (isun.lt.nm0) isun=nm0
             ib(col,lin)=ibphi
             is(col,lin)=isun
             iz(col,lin)=ibmue

             nall=nall+1

             col1=MAXVAL((/1,col-1/))
             col2=MINVAL((/dimx2,col+1/))
             lin1=MAXVAL((/1,lin-1/))
             lin2=MINVAL((/dimy,lin+1/))

           imask=0
 if (LSGM(col,lin).eq.1.and.LSGM(col1,lin1).eq.1.and.LSGM(col1,lin2).eq.1.and.LSGM(col2,lin1).eq.1.and.LSGM(col2,lin2).eq.1) imask=1


           if (imask.eq.1) nl=nl+1
           
           icloud=1
           if (PCLD(col,lin).gt.clthresh2) icloud=0

           if (icloud.eq.0) nc0=nc0+1
           if (icloud*imask.eq.1) nl2=nl2+1

           ipcld=0
           if (PCLD(col,lin).le.clthresh1) ipcld=1
           if (PCLD(col,lin).gt.clthresh1.and.PCLD(col,lin).le.clthresh2) ipcld=2

           if (ipcld.eq.1) nc1=nc1+1
           if (ipcld.eq.2) nc2=nc2+1

           ref1=reflectance(1,col,lin)
           ref2=reflectance(2,col,lin)
           ref37=REF3(col,lin)

! *** Rayleigh correction (single scattering approximation)

             xmuenul=cos(db*solzen(col,lin))
             xmue=cos(db*satzen(col,lin))
             scat_function_rayl = 3./4.*(1+(cos(db*scatangle(col,lin)))**2)
             devide_rayl_red = (exp(-aod_rayl_red/xmuenul)*exp(-aod_rayl_red/xmue))
             minus_rayl_red = aod_rayl_red*scat_function_rayl/(4*xmuenul*xmue)
             albedo_red = (reflectance(1,col,lin) - minus_rayl_red) / devide_rayl_red
             albedo_red0 = albedo_red
             if (albedo_red.le.0.) albedo_red = 0.005
             devide_rayl_nir = (exp(-aod_rayl_nir/xmuenul)*exp(-aod_rayl_nir/xmue))
             minus_rayl_nir = aod_rayl_nir*scat_function_rayl/(4*xmuenul*xmue)
             albedo_nir = (reflectance(2,col,lin) - minus_rayl_nir) / devide_rayl_nir
             if (albedo_nir.le.0.) albedo_nir = reflectance(2,col,lin)
             albedo_swir=REF3(col,lin)
             ref1korr(col,lin)=albedo_red
             ref2korr(col,lin)=albedo_nir
             ndvikorr(col,lin)=(albedo_nir-albedo_red)/(albedo_nir+albedo_red)
             ndiikorr(col,lin)=(albedo_nir-albedo_swir)/(albedo_nir+albedo_swir)


! *** Rayleigh correction (full RTE based atmospheric correction)

            ityp = 1
            iopt = 0.
            ialb = 10000. * reflectance(1,col,lin)  
        c1=acoef(2,ibphi,ibmue,isun,1,1,ityp)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,1,ityp)*iopt+acoef(2,ibphi,ibmue,isun,3,1,ityp)
        c2=acoef(2,ibphi,ibmue,isun,1,2,ityp)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,2,ityp)*iopt+acoef(2,ibphi,ibmue,isun,3,2,ityp)
        c3=acoef(2,ibphi,ibmue,isun,1,3,ityp)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,3,ityp)*iopt+acoef(2,ibphi,ibmue,isun,3,3,ityp)
            albedo_red2=(c1*ialb*ialb+c2*ialb+c3)/100.

            iopt = 0.
            ialb = 10000. * reflectance(2,col,lin)
        c1=acoef(3,ibphi,ibmue,isun,1,1,ityp)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,1,ityp)*iopt+acoef(3,ibphi,ibmue,isun,3,1,ityp)
        c2=acoef(3,ibphi,ibmue,isun,1,2,ityp)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,2,ityp)*iopt+acoef(3,ibphi,ibmue,isun,3,2,ityp)
        c3=acoef(3,ibphi,ibmue,isun,1,3,ityp)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,3,ityp)*iopt+acoef(3,ibphi,ibmue,isun,3,3,ityp)
            albedo_nir2=(c1*ialb*ialb+c2*ialb+c3)/100.

             albedo_swir2=REF3(col,lin)

             ref1korr2(col,lin)=albedo_red2
             ref2korr2(col,lin)=albedo_nir2

             ndvikorr2(col,lin)=(albedo_nir2-albedo_red2)/(albedo_nir2+albedo_red2)
             ndiikorr2(col,lin)=(albedo_nir2-albedo_swir2)/(albedo_nir2+albedo_swir2)

! *** calculate ndvi, ndii

             if (ref1+ref2.ne.0.) ndvi(col,lin)=(ref2-ref1)/(ref2+ref1)
             if (ref2+ref37.ne.0.) ndii(col,lin)=(ref2-ref37)/(ref2+ref37)
             if (ref1+ref2.eq.0.) ndvi(col,lin)=-999.
             if (ref2+ref37.eq.0.) ndii(col,lin)=-999.

             ndvidiff(col,lin)=ndvikorr(col,lin)-ndvi(col,lin)
             ndiidiff(col,lin)=ndiikorr(col,lin)-ndii(col,lin)

! *** use single scattering Rayleigh corrected reflectances 1, 2

             ref1=albedo_red
             ref2=albedo_nir
             ref37=albedo_swir

! *** use multiple scattering Rayleigh corrected reflectances 1, 2

             ref1=albedo_red2
             ref2=albedo_nir2
             ref37=albedo_swir2

           xndvi_u_i(col,lin) = 0.
           xndvi_u_c(col,lin) = 0.
           xndvi_u_s(col,lin) = 0.

           if (ref1+ref2.ne.0.) then
              ndvi(col,lin)=(ref2-ref1)/(ref2+ref1)

              rad_err1 = 0.
              rad_err2 = 0.
              if (ref1.ne.0.) rad_err1 = uncertainty_r_independent(1,col,lin)/ref1
              if (ref2.ne.0.) rad_err2 = uncertainty_r_independent(2,col,lin)/ref2
              xndvidash1 = ((1.-rad_err2)*ref2-ref1)/((1.-rad_err2)*ref2+ref1) - ndvi(col,lin)
              xndvidash2 = (ref2-(1.-rad_err1)*ref1)/(ref2+(1.-rad_err1)*ref1) - ndvi(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndvi_u_i(col,lin) = ndvi(col,lin) + deltandvi

              rad_err1 = 0.
              rad_err2 = 0.
              if (ref1.ne.0.) rad_err1 = uncertainty_r_common(1,col,lin)
              if (ref2.ne.0.) rad_err2 = uncertainty_r_common(2,col,lin)
              xndvidash1 = ((1.-rad_err2)*ref2-ref1)/((1.-rad_err2)*ref2+ref1) - ndvi(col,lin)
              xndvidash2 = (ref2-(1.-rad_err1)*ref1)/(ref2+(1.-rad_err1)*ref1) - ndvi(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndvi_u_c(col,lin) = ndvi(col,lin) + deltandvi

              rad_err1 = 0.
              rad_err2 = 0.
              if (ref1.ne.0.) rad_err1 = uncertainty_r_structured(1,col,lin)/ref1
              if (ref2.ne.0.) rad_err2 = uncertainty_r_structured(2,col,lin)/ref2
              xndvidash1 = ((1.-rad_err2)*ref2-ref1)/((1.-rad_err2)*ref2+ref1) - ndvi(col,lin)
              xndvidash2 = (ref2-(1.-rad_err1)*ref1)/(ref2+(1.-rad_err1)*ref1) - ndvi(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndvi_u_s(col,lin) = ndvi(col,lin) + deltandvi

           endif

           xndii_u_i(col,lin) = 0.
           xndii_u_c(col,lin) = 0.
           xndii_u_s(col,lin) = 0.

           if (ref2+ref37.ne.0.) then

              ndii(col,lin)= (ref2-ref37)/(ref2+ref37)

              rad_err2 = 0.
              rad_err37 = 0.
              if (ref2.ne.0.) rad_err2 = uncertainty_r_independent(2,col,lin)/ref2
              if (ref37.ne.0.) rad_err37 = REF3_U_I(col,lin)/ref37
              xndvidash1 = ((1.-rad_err2)*ref2-ref37)/((1.-rad_err2)*ref2+ref37) - ndii(col,lin)
              xndvidash2 = (ref2-(1.-rad_err37)*ref37)/(ref2+(1.-rad_err37)*ref37) - ndii(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndii_u_i(col,lin) = ndii(col,lin) + deltandvi

              rad_err2 = 0.
              rad_err37 = 0.
              if (ref2.ne.0.) rad_err2 = uncertainty_r_common(2,col,lin)
              if (ref37.ne.0.) rad_err37 = REF3_U_C(col,lin)/ref37
              xndvidash1 = ((1.-rad_err2)*ref2-ref37)/((1.-rad_err2)*ref2+ref37) - ndii(col,lin)
              xndvidash2 = (ref2-(1.-rad_err37)*ref37)/(ref2+(1.-rad_err37)*ref37) - ndii(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndii_u_c(col,lin) = ndii(col,lin) + deltandvi

              rad_err2 = 0.
              rad_err37 = 0.
              if (ref2.ne.0.) rad_err2 = uncertainty_r_structured(2,col,lin)/ref2
              if (ref37.ne.0.) rad_err37 = REF3_U_S(col,lin)/ref37
              xndvidash1 = ((1.-rad_err2)*ref2-ref37)/((1.-rad_err2)*ref2+ref37) - ndii(col,lin)
              xndvidash2 = (ref2-(1.-rad_err37)*ref37)/(ref2+(1.-rad_err37)*ref37) - ndii(col,lin)
              deltandvi=sqrt(xndvidash1*xndvidash1+xndvidash2*xndvidash2)
              xndii_u_s(col,lin) = ndii(col,lin) + deltandvi

           endif

! *** set ndvi to Rayleigh-corrected value for ptp selection

           ndvi(col,lin)=ndvikorr(col,lin)

           iptp=0
           if ((ref37.lt.r3thresh.and.ref37.gt.0.005).and.(ndvi(col,lin).gt.vithresh)) iptp=1
           iptp = iptp * imask * icloud
           if (iptp.eq.1.and.ipcld.eq.2) iptp=2

           if (icloud*imask.eq.0) iptp=-999

           if (sonhoe.lt.sunmin) iptp=-999

           PTP(col,lin) = iptp

           if (iptp.ge.0) then
             ref1p(col,lin)=reflectance(1,col,lin)
             ref2p(col,lin)=reflectance(2,col,lin)
             ref37p(col,lin)=REF3(col,lin)
             ndvip(col,lin)=ndvi(col,lin)
             ndiip(col,lin)=ndii(col,lin)
           endif

           if (iptp.lt.0) then
             ref1p(col,lin)=-999.
             ref2p(col,lin)=-999.
             ref37p(col,lin)=-999.
             ndvip(col,lin)=-999.
             ndiip(col,lin)=-999.
             ref1korr(col,lin)=-999.
             ref2korr(col,lin)=-999.
             ref1korr2(col,lin)=-999.
             ref2korr2(col,lin)=-999.
             ndvikorr(col,lin)=-999.
             ndiikorr(col,lin)=-999.
             ndvikorr2(col,lin)=-999.
             ndiikorr2(col,lin)=-999.
             ndvidiff(col,lin)=-999.
             ndiidiff(col,lin)=-999.
           endif

           
           if (iptp.eq.1) np1=np1+1
           if (iptp.eq.2) np2=np2+1

   12   continue
   13 continue

      print*,'all/land/land-cloudfree: ',nall,nl,nl2
      print*,'clouds: ',nc0,nc1,nc2,nc0+nc1+nc2
      print*,'dark fields: ',np1,np2

!----------------------------------------------------
! select AEROCOM aerosol mix 
!----------------------------------------------------

  print*,'select AEROCOM aerosol mix...'

  print*,'DATE: ',date_str
  ixmon=1
  if (date_str(5:6).eq.'02') ixmon=2
  if (date_str(5:6).eq.'03') ixmon=3
  if (date_str(5:6).eq.'04') ixmon=4
  if (date_str(5:6).eq.'05') ixmon=5
  if (date_str(5:6).eq.'06') ixmon=6
  if (date_str(5:6).eq.'07') ixmon=7
  if (date_str(5:6).eq.'08') ixmon=8
  if (date_str(5:6).eq.'09') ixmon=9
  if (date_str(5:6).eq.'10') ixmon=10
  if (date_str(5:6).eq.'11') ixmon=11
  if (date_str(5:6).eq.'02') ixmon=12
  
  print*,'month: ',ixmon

      do 23 lin=1,dimy
        do 22 col=1,dimx2
           latval=latitude(col,lin)
           lonval=longitude(col,lin)
	   ixlat=nint(90.5 - latval)
	   ixlon=nint(180.5+lonval)
	   if (ixlon.lt.1) ixlon=360+ixlon
	   if (ixlon.gt.360) ixlon=ixlon-360

	   xabs=1. - absfrac(ixmon,ixlat,ixlon)
	   xfinefrac=finefrac(ixmon,ixlat,ixlon)
	   xdust=dustfrac(ixmon,ixlat,ixlon)

           mixsel(col,lin)=0

	   ix1=1
	   if (xfinefrac.lt..9.and.xfinefrac.ge..7) ix1=2
	   if (xfinefrac.lt..7.and.xfinefrac.ge..5) ix1=3
	   if (xfinefrac.lt..5.and.xfinefrac.ge..3) ix1=4
	   if (xfinefrac.lt..3) ix1=5
	   	   
	   ix2=1
	   if (xabs.ge.0.17.and.xabs.lt.0.50) ix2=4
	   if (xabs.ge.0.50.and.xabs.lt.0.83) ix2=7
	   if (xabs.ge.0.83) ix2=10

	   ix3=2
	   if (xdust.ge.0.50) ix3=0

	   if (ix1.eq.1) jsel=ix2
	   if (ix1.gt.1) jsel=10+10*((ix1-2)*3+ix3)+ix2 

           if (jsel.ge.1.and.jsel.le.130) MIXSEL(col,lin) = jsel

   22   continue
   23 continue
 
!------------------------------------------------------------------------------------------------------------------------
! define weights per AEROCOM aerosol mix in cliamtology grid (not yet normalized -> is done during ensemble calculations)
!------------------------------------------------------------------------------------------------------------------------

      do 53 ixlat=1,65
         do 52 ixlon=150,280

            xabs=1. - absfrac(ixmon,ixlat,ixlon)
	    xfinefrac=finefrac(ixmon,ixlat,ixlon)
	    xdust=dustfrac(ixmon,ixlat,ixlon)

            do 51 imix=1,36
               jmix=mixnum2(imix)
               xabsm=xfmaf(jmix)
               xfinefracm=xfmf(jmix)
               xdustm=1.-xcmsf(jmix)
               ensdist=(xabsm-xabs)*(xabsm-xabs)
               ensdist=ensdist+(xfinefracm-xfinefrac)*(xfinefracm-xfinefrac)
               ensdist=ensdist+(xdust-xdustm)*(xdust-xdustm)
               if(sqrt(ensdist).lt.0.2) ensdist=0.2*0.2
               edist(imix)=sqrt(ensdist)
               w(ixlat,ixlon,imix)=1./ensdist
   51       continue

   52    continue
   53 continue

!----------------------------------------------------
! estimate dark field albedo
!----------------------------------------------------

! dark pixel selection
  darkalbmax=74.9

! *** set offset / factor for NDVI
              xoff=0.0

! set multiplicative factor
              testfak=1.

  print*,'estimate dark field albedo...'

  print*,'B factor: ',minval(data_in),maxval(data_in)

       nwrong=0

       count=0

       albval_u3_c=10.
       albval_u3_c=0.

       do 33 lin=1,dimy
        do 32 col=1,dimx2

           ALB(col,lin)=-999.
           ALB2(col,lin)=-999.

           BFAK(col,lin)=-999.
           BFAK2(col,lin)=-999.
           BFAK3(col,lin)=-999.
           BFAKratio(col,lin)=-999.
           BFAKratio2(col,lin)=-999.

           albval_u_i(col,lin) = -999.
           albval_u_c(col,lin) = -999.
           albval_u_s(col,lin) = -999.

           if (PTP(col,lin).gt.0) then

              isun=is(col,lin)
              ibmue=iz(col,lin)
              ibphi=ib(col,lin)

              thetb=satzen(col,lin)
              sonhoe=90.-solzen(col,lin)

              xmuenul=cos(db*(90.-sonhoe))
              xmue=cos(db*thetb)

              phib=relazi(col,lin)
              if (abs(phib).gt.180.) phib=abs(360.-abs(phib))

              itamue=1
              itamues=1
              do indang=1,inda1
                if (xmue.ge.tabmue(indang)) itamue=indang
                if (xmuenul.ge.tabmus(indang)) itamues=indang
              enddo
              itaphi=nint(phib/30.)+1

              itamue2=itamue+1
              if (itamue2.gt.inda1) itamue2=inda1
              itamues2=itamues+1
              if (itamues2.gt.inda3) itamues2=inda3
              itaphi2=itaphi+1
              if (itaphi2.gt.inda2) itaphi2=inda2

              ref37=1000.*REF3(col,lin)
              ref16=1000.*reflectance(3,col,lin)
              xndvi=ndvi(col,lin)

              xndvi=xndvi + xoff
              xndvi_u_i(col,lin)=xndvi_u_i(col,lin)+xoff
              xndvi_u_c(col,lin)=xndvi_u_c(col,lin)+xoff
              xndvi_u_s(col,lin)=xndvi_u_s(col,lin)+xoff

              xndii=ndii(col,lin)

              ndvi_index = (xndvi-xndvi_min)/0.01+1
              ndii_index = (xndii-xndii_min)/0.01+1

              if (ndvi_index.lt.1) ndvi_index=1
              if (ndii_index.lt.1) ndii_index=1
              if (ndvi_index.gt.160) ndvi_index=160
              if (ndii_index.gt.160) ndii_index=160

              ndvi_index_1 = ndvi_index + 1
              ndii_index_1 = ndii_index + 1

              if (ndvi_index_1.gt.160) ndvi_index_1=160
              if (ndii_index_1.gt.160) ndii_index_1=160

              if (data_in(ndvi_index, ndii_index).eq.0) then
                 ndii_index=ndii_index-10
                 ndii_index=ndii_index-5
              endif
             
              delta_ndvi = 100.*xndvi - int(100.*xndvi)
              delta_ndii = 100.*xndii - int(100.*xndii) 

              delta_ndvi2_i = 100.*xndvi_u_i(col,lin) - int(100.*xndvi)
              delta_ndii2_i = 100.*xndii_u_i(col,lin) - int(100.*xndii)
              delta_ndvi2_c = 100.*xndvi_u_c(col,lin) - int(100.*xndvi)
              delta_ndii2_c = 100.*xndii_u_c(col,lin) - int(100.*xndii)
              delta_ndvi2_s = 100.*xndvi_u_s(col,lin) - int(100.*xndvi)
              delta_ndii2_s = 100.*xndii_u_s(col,lin) - int(100.*xndii)            
             
              if (ndii_index.le.160.and.ndvi_index.le.160.and.ndii_index.ge.1.and.ndvi_index.ge.1) then             
                 dB_ndvi = testfak * (data_in(ndvi_index_1, ndii_index)-data_in(ndvi_index, ndii_index))
                 dB_ndii = testfak * (data_in(ndvi_index, ndii_index_1)-data_in(ndvi_index, ndii_index))
                 if (data_in(ndvi_index_1, ndii_index).eq.0.) dB_ndvi = 0.
                 if (data_in(ndvi_index, ndii_index_1).eq.0.) dB_ndii = 0.
             
                 albfak = testfak * data_in(ndvi_index, ndii_index)+dB_ndvi*delta_ndvi+dB_ndii*delta_ndii

                 if (data_in(ndvi_index, ndii_index).eq.0.) then
                    albfak_u_i = 0.
                    albfak_u_c = 0.
                    albfak_u_s = 0.
                    nwrong=nwrong+1
                 endif
                 
              endif

              albfak_b = albfak

! ***use theoretical B factor

              albfak=testfak*((xndvi-1)/(xndvi+1))*((xndii+1)/(xndii-1))

! *** set NDVI to Rayleigh-corrected for new BFAK

              xndvi=ndvikorr(col,lin)

              xndvi=xndvi + xoff
              if (xndvi.gt.1.) xndvi=1.

              ndvi_index = (xndvi-xndvi_min)/0.01+1
              ndii_index = (xndii-xndii_min)/0.01+1

              if (ndvi_index.lt.1) ndvi_index=1
              if (ndii_index.lt.1) ndii_index=1
              if (ndvi_index.gt.160) ndvi_index=160
              if (ndii_index.gt.160) ndii_index=160

              ndvi_index_1 = ndvi_index + 1
              ndii_index_1 = ndii_index + 1

              if (ndvi_index_1.gt.160) ndvi_index_1=160
              if (ndii_index_1.gt.160) ndii_index_1=160

              if (data_in(ndvi_index, ndii_index).eq.0) then
                 ndii_index=ndii_index-10
                 ndii_index=ndii_index-5
              endif
             
              delta_ndvi = 100.*xndvi - int(100.*xndvi)
              delta_ndii = 100.*xndii - int(100.*xndii)     
             
              if (ndii_index.le.160.and.ndvi_index.le.160.and.ndii_index.ge.1.and.ndvi_index.ge.1) then             
                 dB_ndvi = testfak * (data_in(ndvi_index_1, ndii_index)-data_in(ndvi_index, ndii_index))
                 dB_ndii = testfak * (data_in(ndvi_index, ndii_index_1)-data_in(ndvi_index, ndii_index))
                 if (data_in(ndvi_index_1, ndii_index).eq.0.) dB_ndvi = 0.
                 if (data_in(ndvi_index, ndii_index_1).eq.0.) dB_ndii = 0.
             
                 albfak_c = testfak * data_in(ndvi_index, ndii_index)+dB_ndvi*delta_ndvi+dB_ndii*delta_ndii
                 
              endif

              BFAK(col,lin)=albfak_c

              BFAK2(col,lin)=testfak*((xndvi-1)/(xndvi+1))*((xndii+1)/(xndii-1))
              BFAKratio(col,lin)=BFAK2(col,lin)/BFAK(col,lin)
              BFAK3(col,lin)=albfak*cos(db*solzen(col,lin))
              BFAKratio2(col,lin)=albfak_c/albfak

              offset = 0.0

              corrval=1

              bfakratio(col,lin)=corrval

               BFAK(col,lin)=BFAK2(col,lin)*corrval

               if (BFAK(col,lin).lt.0.01) BFAK(col,lin)=0.01
               albfak_c=BFAK(col,lin)

              albval=albfak_c*ref37
              albval2=BFAK2(col,lin)*ref37

              albval2=albval

! *** estimate uncertainties with theoretical B factor

              albfak=albfak_c
              delta_ndvi2_i = xndvi_u_i(col,lin) - xndvi
              delta_ndii2_i = xndii_u_i(col,lin) - xndii
              delta_ndvi2_c = xndvi_u_c(col,lin) - xndvi
              delta_ndii2_c = xndii_u_c(col,lin) - xndii
              delta_ndvi2_s = xndvi_u_s(col,lin) - xndvi
              delta_ndii2_s = xndii_u_s(col,lin) - xndii

              albfak_u1_i=testfak*((xndvi+delta_ndvi2_i-1)/(xndvi+delta_ndvi2_i+1))*((xndii+1)/(xndii-1))*corrval-albfak
              albfak_u2_i=testfak*((xndvi-1)/(xndvi+1))*((xndii+delta_ndii2_i+1)/(xndii+delta_ndii2_i-1))*corrval-albfak
              albfak_u1_c=testfak*((xndvi+delta_ndvi2_c-1)/(xndvi+delta_ndvi2_c+1))*((xndii+1)/(xndii-1))*corrval-albfak
              albfak_u2_c=testfak*((xndvi-1)/(xndvi+1))*((xndii+delta_ndii2_c+1)/(xndii+delta_ndii2_c-1))*corrval-albfak
              albfak_u1_s=testfak*((xndvi+delta_ndvi2_s-1)/(xndvi+delta_ndvi2_s+1))*((xndii+1)/(xndii-1))*corrval-albfak
              albfak_u2_s=testfak*((xndvi-1)/(xndvi+1))*((xndii+delta_ndii2_s+1)/(xndii+delta_ndii2_s-1))*corrval-albfak
              albfak_u_i = albfak + sqrt(albfak_u1_i*albfak_u1_i+albfak_u2_i*albfak_u2_i)
              albfak_u_c = albfak + sqrt(albfak_u1_c*albfak_u1_c+albfak_u2_c*albfak_u2_c)
              albfak_u_s = albfak + sqrt(albfak_u1_s*albfak_u1_s+albfak_u2_s*albfak_u2_s)

              albval_u1_i=1000.*albfak*REF3_U_I(col,lin)
              albval_u1_c=1000.*albfak*REF3_U_C(col,lin)
              albval_u1_s=1000.*albfak*REF3_U_S(col,lin)

              albval_u2_i = (albfak_u_i-albfak)*ref37
              albval_u2_c = (albfak_u_c-albfak)*ref37
              albval_u2_s = (albfak_u_s-albfak)*ref37

              albval_u_i(col,lin) = sqrt(albval_u1_i*albval_u1_i+albval_u2_i*albval_u2_i)
              albval_u_c(col,lin) = sqrt(albval_u1_c*albval_u1_c+albval_u2_c*albval_u2_c+albval_u3_c*albval_u3_c)
              albval_u_s(col,lin) = sqrt(albval_u1_s*albval_u1_s+albval_u2_s*albval_u2_s)

             if (albval.lt.1) albval=1

             if (albval.lt.darkalbmax) then
                ALB(col,lin)=albval
             endif

           endif

   32   continue
   33 continue

  print*,'albval_u_i: ',minval(albval_u_i),maxval(albval_u_i)
  print*,'albval_u_c: ',minval(albval_u_c),maxval(albval_u_c)
  print*,'albval_u_s: ',minval(albval_u_s),maxval(albval_u_s)

      print*,'nwrong: ',nwrong

      do 77 jk=-10,10

      minndvi=2.
      maxndvi=-2.
      minr37=2000.
      maxr37=-2000.
      minB=1000.
      maxB=-1000.
      minalb=2000.
      maxalb=-2000.
      num=0

      do 43 lin=1,dimy
        do 42 col=1,dimx2

           if (PTP(col,lin).gt.0) then
              xndii=ndii(col,lin)
              ndiival=nint(10.*xndii)

              if (ndiival.eq.jk) then

              num=num+1

              ref37=1000.*REF3(col,lin)
              ref16=1000.*reflectance(3,col,lin)
              xndvi=ndvi(col,lin)
              albfak=BFAK(col,lin)
              albval=ALB(col,lin)


              if (xndvi.lt.minndvi) minndvi=xndvi
              if (xndvi.gt.maxndvi) maxndvi=xndvi
              if (ref37.lt.minr37) minr37=ref37
              if (ref37.gt.maxr37) maxr37=ref37
              if (albfak.lt.minB.and.albval.gt.0.) minB=albfak
              if (albfak.gt.maxB) maxB=albfak
              if (albval.lt.minalb.and.albval.gt.0.) minalb=albval
              if (albval.gt.maxalb) maxalb=albval

           endif


           endif

   42   continue
   43 continue

      if (jk.eq.-10) print*,'ndii - min/max: ndvi,r37,BFak,alb: '
      if (num.gt.0) print*,float(jk)/10.,num,minndvi,maxndvi,minr37,maxr37,minB,maxB,minalb,maxalb

  77  continue


  deallocate(xndvi_u_i)
  deallocate(xndvi_u_c)
  deallocate(xndvi_u_s)
  deallocate(xndii_u_i)
  deallocate(xndii_u_c)
  deallocate(xndii_u_s)


  RETURN
 END SUBROUTINE fiduceo_prepare_darkfields

