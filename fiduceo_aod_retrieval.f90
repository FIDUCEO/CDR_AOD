SUBROUTINE fiduceo_aod_retrieval()
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		aod retrieval

! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Thomas Popp, 11.01.2019
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

  INTEGER(KIND=p_long)	  :: status
  integer                 :: lin,col,jmix,j,imix,jmix2,maxmixloop
  integer*2               :: ptpval,ptpval2,kprint

  integer*2               :: isun,ibmue,ibphi
  real*4                  :: sonhoe,thetb,phib

  real*4                  :: xndvi,xndii,turbx
  real*4                  :: xgrau1,albval,turb

  INTEGER                 :: albindex,albindex2,ax1,ax2,noselect
  INTEGER                 :: albsign1,albsign2
  real*4                  :: albval0,rtoaval1,rtoaval2
  real*4                  :: t1,t2,t11,t12,t13,t21,t22,t23
  real*4                  :: nall,nptp,nptp1,nptp2,nx1,nx2
  real*4                  :: nlow(36),nhigh(36),nsmall(36),nnotvalid(36)
  real*4                  :: meanaod(36),meanaod1(36),meanaod2(36)
  real*4                  :: minaod(36),minaod1(36),minaod2(36)
  real*4                  :: maxaod(36),maxaod1(36),maxaod2(36)
  real*4                  :: minclaod,maxclaod,aodclimmean

  real*4                  :: t1d,t2d,turb_d1i,turb_d1s,turb_d1c,rad_erri,rad_errs,rad_errc,tdiff
  real*4                  :: turb_d2i,turb_d2s,turb_d2c
 
  CHARACTER(LEN=200)      :: outfile
  INTEGER                 :: irec
  INTEGER                 :: recl2

  INTEGER                 :: sigmanum,aodensnum
  real*4                  :: sigmasum,aoddiff,sigmasum2,aoddiff2,aodensmean,aodensmean2,aodensnorm,aodensnorm2
  real*4                  :: sq1,sq2,sq3,sq4

  INTEGER                 :: ixmon,ixlat,ixlon
  real*4                  :: latval,lonval,val

  real*4                  :: aod1val,aod2val,aod3val
  real*4                  :: albedo_red2,albedo_nir2,albedo_swir2
  real*4                  :: db,pi
  real*4                  :: ref37,albfak,albvalnew,epsilon
  integer                 :: nitermax,nturbiter
  integer                 :: ialb
  real*4                  :: iopt,c1,c2,c3,maxd1s,maxd2s

  real*4                  :: aodu1s,aodu1c,aodu1i,aodu2s,aodu2c,aodu2i,aod1in,aod2in,aod2norm

  real*4                  :: xmuenul,xmue
  integer                 :: itamue,itamues,itaphi,itamue2,itamues2,itaphi2,indang
  real*4                  :: tabval1,tabval2,xfak1,xfak2,xfak3


  allocate(PTP2(36,dimx2,dimy), stat=status)
  allocate(AOD(36,dimx2,dimy), stat=status)
  allocate(AOD_U1I(dimx2,dimy), stat=status)
  allocate(AOD_U1S(dimx2,dimy), stat=status)
  allocate(AOD_U1C(dimx2,dimy), stat=status)
  allocate(AOD_U2I(dimx2,dimy), stat=status)
  allocate(AOD_U2S(dimx2,dimy), stat=status)
  allocate(AOD_U2C(dimx2,dimy), stat=status)
  allocate(AOD_U1(dimx2,dimy), stat=status)
  allocate(AOD_U2(dimx2,dimy), stat=status)
  allocate(AOD_UI(dimx2,dimy), stat=status)
  allocate(AOD_US(dimx2,dimy), stat=status)
  allocate(AOD_UC(dimx2,dimy), stat=status)
  allocate(AOD_U3(dimx2,dimy), stat=status)
  allocate(AOD_U3B(dimx2,dimy), stat=status)
  allocate(AOD_U(dimx2,dimy), stat=status)
  allocate(AOD_U_SEL(dimx2,dimy), stat=status)
  allocate(AODCLIM(dimx2,dimy), stat=status)
  allocate(AODENS(dimx2,dimy), stat=status)
  allocate(AODENSB(dimx2,dimy), stat=status)
  allocate(AODENSBSEL(dimx2,dimy), stat=status)
  allocate(AOD2(dimx2,dimy), stat=status)
  allocate(AOD1(dimx2,dimy), stat=status)

      pi=4.*atan(1.)
      db=pi/180.

!----------------------------------------------------
! aod retrieval
!----------------------------------------------------

      nitermax=1

      epsilon=2.

      maxmixloop=36

 print*,'aod retrieval...'

     maxd1s=0.05
     maxd2s=0.04

      nall=0
      nptp=0
      nptp1=0
      nptp2=0
      aodclimmean=0.

      do 10 imix=1,maxmixloop
         nlow(imix)=0
         nhigh(imix)=0
         nsmall(imix)=0
         nnotvalid(imix)=0
         meanaod(imix)=0.
         meanaod1(imix)=0.
         meanaod2(imix)=0.
         minaod1(imix)=10.
         maxaod1(imix)=-10.
         minaod2(imix)=10.
         maxaod2(imix)=-10.
   10 continue


       do 13 lin=1,dimy
        do 12 col=1,dimx2

              nall=nall+1

              ptpval=PTP(col,lin)

              do imix=1,maxmixloop
                 aod(imix,col,lin) = -999.
              enddo

              aod_u3(col,lin) = -999.
              aod_u3b(col,lin) = -999.
              aod1(col,lin) = -999.
              aod2(col,lin) = -999.
              aodclim(col,lin) = -999.
              aodens(col,lin) = -999.        
              aodensb(col,lin) = -999.
              aodensbsel(col,lin) = -999.        			  
 
              aod_u1i(col,lin) = -999.
              aod_u1s(col,lin) = -999.
              aod_u1c(col,lin) = -999.
              aod_u2i(col,lin) = -999.
              aod_u2s(col,lin) = -999.
              aod_u2c(col,lin) = -999.
              aod_u1(col,lin) = -999.
              aod_u2(col,lin) = -999.
              aod_ui(col,lin) = -999.
              aod_us(col,lin) = -999.
              aod_uc(col,lin) = -999.
              aod_u(col,lin) = -999.
			  aod_u_sel(col,lin) = -999.

              if ((ptpval.eq.1.or.ptpval.eq.2).and.(ALB(col,lin).ge.1.and.ALB(col,lin).lt.135.)) then

                 nptp=nptp+1
                 if (ptpval.eq.1) nptp1=nptp1+1
                 if (ptpval.eq.2) nptp2=nptp2+1

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


                 xgrau1=reflectance(1,col,lin)

                 ref37=1000.*REF3(col,lin)

                 jmix2=MIXSEL(col,lin)

                 rad_erri=uncertainty_r_independent(1,col,lin)
                 rad_errs=uncertainty_r_structured(1,col,lin)
                 rad_errc=uncertainty_r_common(1,col,lin)

                 aodensnum=0
                 aodensmean=0.
                 aodensnorm=0.
                 aodensnorm2=0.

                 aodu1s=0.
                 aodu1c=0.
                 aodu1i=0.
                 aodu2s=0.
                 aodu2c=0.
                 aodu2i=0.

                 aod1in=0.
                 aod2in=0.
                 aod2norm=.0

                 latval=latitude(col,lin)
                 lonval=longitude(col,lin)
	         ixlat=nint(90.5 - latval)
	         ixlon=nint(180.5+lonval)
	         if (ixlon.lt.1) ixlon=360+ixlon
	         if (ixlon.gt.360) ixlon=ixlon-360

                 do 11 imix=1,maxmixloop

                   jmix=mixnum2(imix)

                   albval=ALB(col,lin)

                   nturbiter=0

! *** iteration of albedo
  753              continue

! aod inversion start

                   albindex = aint(albval/5.)
                   if (albindex.lt.1) albindex=1
                   if (albindex.gt.nlut-1) albindex=nlut-1
                   albindex2=albindex+1
                   albval0=albindex*5.
                   rtoaval1=breakrtoa(jmix,ibphi,ibmue,isun,albindex)
                   rtoaval2=breakrtoa(jmix,ibphi,ibmue,isun,albindex+1)
                   albsign1=1.
                   if (rtoaval1.lt.0.) albsign1=-1.
                   albsign2=1.
                   if (rtoaval2.lt.0.) albsign2=-1.
                   if (xgrau1*albsign1.gt.rtoaval1) albindex=albindex+nlut
                   if (xgrau1*albsign2.gt.rtoaval2) albindex2=albindex2+nlut
                   ax1=albindex
                   ax2=albindex2

                  t11=trcoef(2,ibphi,ibmue,isun,ax1,jmix,1)
                  t12=trcoef(2,ibphi,ibmue,isun,ax1,jmix,2)
                  t13=trcoef(2,ibphi,ibmue,isun,ax1,jmix,3)
                  t21=trcoef(2,ibphi,ibmue,isun,ax2,jmix,1)
                  t22=trcoef(2,ibphi,ibmue,isun,ax2,jmix,2)
                  t23=trcoef(2,ibphi,ibmue,isun,ax2,jmix,3)
                  t1=t11*xgrau1*xgrau1+t12*xgrau1+t13
                  t2=t21*xgrau1*xgrau1+t22*xgrau1+t23

                  turb=999.

                  if (t1.ne.0..and.t2.ne.0.) turb=t1+(t2-t1)*(albval-albval0)/5.
                  turbx=turb

                  if ((t1.ne.0..and.t2.ne.0.).and.nturbiter.lt.nitermax) then

! *** iterative albval(aod) correction per aerosol mix
            aod1val=63.5
            aod2val=63.5*dfak(1,jmix)/dfak(2,jmix)
            aod3val=63.5*dfak(1,jmix)/dfak(3,jmix)

            iopt = 100.*turb
            if (iopt.lt.0.) iopt=0.
            ialb = 10000. * reflectance(1,col,lin)  

            if (iopt.le.aod2val) then
        c1=acoef(2,ibphi,ibmue,isun,1,1,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,1,jmix)*iopt+acoef(2,ibphi,ibmue,isun,3,1,jmix)
        c2=acoef(2,ibphi,ibmue,isun,1,2,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,2,jmix)*iopt+acoef(2,ibphi,ibmue,isun,3,2,jmix)
        c3=acoef(2,ibphi,ibmue,isun,1,3,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,2,3,jmix)*iopt+acoef(2,ibphi,ibmue,isun,3,3,jmix)
            else
        c1=acoef(2,ibphi,ibmue,isun,4,1,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,5,1,jmix)*iopt+acoef(2,ibphi,ibmue,isun,6,1,jmix)
        c2=acoef(2,ibphi,ibmue,isun,4,2,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,5,2,jmix)*iopt+acoef(2,ibphi,ibmue,isun,6,2,jmix)
        c3=acoef(2,ibphi,ibmue,isun,4,3,jmix)*iopt*iopt+acoef(2,ibphi,ibmue,isun,5,3,jmix)*iopt+acoef(2,ibphi,ibmue,isun,6,3,jmix)
            endif

            albedo_red2=(c1*ialb*ialb+c2*ialb+c3)/100.
            if (albedo_red2.le.0.) albedo_red2=0.005

            iopt = 100.*turb*dfak(2,jmix)/dfak(3,jmix)
            if (iopt.lt.0.) iopt=0.
            ialb = 10000. * reflectance(2,col,lin)

            if (iopt.le.aod3val) then
        c1=acoef(3,ibphi,ibmue,isun,1,1,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,1,jmix)*iopt+acoef(3,ibphi,ibmue,isun,3,1,jmix)
        c2=acoef(3,ibphi,ibmue,isun,1,2,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,2,jmix)*iopt+acoef(3,ibphi,ibmue,isun,3,2,jmix)
        c3=acoef(3,ibphi,ibmue,isun,1,3,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,2,3,jmix)*iopt+acoef(3,ibphi,ibmue,isun,3,3,jmix)
            else
        c1=acoef(3,ibphi,ibmue,isun,4,1,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,5,1,jmix)*iopt+acoef(3,ibphi,ibmue,isun,6,1,jmix)
        c2=acoef(3,ibphi,ibmue,isun,4,2,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,5,2,jmix)*iopt+acoef(3,ibphi,ibmue,isun,6,2,jmix)
        c3=acoef(3,ibphi,ibmue,isun,4,3,jmix)*iopt*iopt+acoef(3,ibphi,ibmue,isun,5,3,jmix)*iopt+acoef(3,ibphi,ibmue,isun,6,3,jmix)
            endif

            albedo_nir2=(c1*ialb*ialb+c2*ialb+c3)/100.
            if (albedo_nir2.le.0.) albedo_nir2 = reflectance(2,col,lin)

            albedo_swir2=REF3(col,lin)

            xndvi = -1.
            if (albedo_nir2+albedo_red2.ne.0.) xndvi=(albedo_nir2-albedo_red2)/(albedo_red2+albedo_nir2)

            xndii=-1.
            if (albedo_swir2+albedo_nir2.ne.0.) xndii=(albedo_nir2-albedo_swir2)/(albedo_swir2+albedo_nir2)

            albfak=((xndvi-1)/(xndvi+1))*((xndii+1)/(xndii-1))

            albfak=albfak*cos(db*solzen(col,lin))
            if (relazi(col,lin).lt.90.) albfak=albfak/cos(db*satzen(col,lin))

            albvalnew=albfak*ref37

            if (albvalnew.lt.1) albvalnew=1

           endif

           nturbiter=nturbiter+1

           if (abs(albvalnew-albval).gt.epsilon.and.(nturbiter.le.nitermax)) then
                 albval=albvalnew
                 goto 753
           endif

           ALB2(col,lin)=albval

                  if (t1.ne.0..and.t2.ne.0.) then

                  turb_d1i=0.
                  turb_d1s=0.
                  turb_d1c=0.
                  turb_d2i=0.
                  turb_d2s=0.
                  turb_d2c=0.

! conversion to 550 nm
                     turb=turb*dfak(2,jmix)/dfak(1,jmix)

                        t1d=2.*t11*xgrau1+t12
                        t2d=2.*t21*xgrau1+t22

                           tdiff=(t2d-t1d)*(albval-albval0)/5.
                           turb_d1i=abs(t1d+tdiff)*rad_erri
                           turb_d1i=turb_d1i*dfak(2,jmix)/dfak(1,jmix)
                           turb_d1s=abs(t1d+tdiff)*rad_errs
                           turb_d1s=turb_d1s*dfak(2,jmix)/dfak(1,jmix)
                           turb_d1c=abs(t1d+tdiff)*rad_errc*xgrau1
                           turb_d1c=turb_d1c*dfak(2,jmix)/dfak(1,jmix)
                        turb_d2i=albval_u_i(col,lin)*abs(t2-t1)/5.
                        turb_d2i=turb_d2i*dfak(2,jmix)/dfak(1,jmix)
                        turb_d2c=albval_u_c(col,lin)*abs(t2-t1)/5.
                        turb_d2c=turb_d2c*dfak(2,jmix)/dfak(1,jmix)
                        turb_d2s=albval_u_s(col,lin)*abs(t2-t1)/5.
                        turb_d2s=turb_d2s*dfak(2,jmix)/dfak(1,jmix)

                  else
                     turb=-999.
                  endif

                  if (turb_d1s.gt.maxd1s) turb=999.

                  if (turb.eq.999.) nnotvalid(imix)=nnotvalid(imix)+1

                  if (turb.ne.-999..and.turb.ne.999.) then
                     if (ptpval.eq.1) then
                        aodensnum=aodensnum+1
                        aodensnorm=aodensnorm+w(ixlat,ixlon,imix)
                        aodensnorm2=aodensnorm2+w(ixlat,ixlon,imix)*w(ixlat,ixlon,imix)
                        aodensmean=aodensmean+turb
                        aodensmean2=aodensmean2+w(ixlat,ixlon,imix)*turb

                        aod1in=aod1in+w(ixlat,ixlon,imix)*turb

                        aodu1i=aodu1i+w(ixlat,ixlon,imix)*turb_d1i
                        aodu1s=aodu1s+w(ixlat,ixlon,imix)*turb_d1s
                        aodu1c=aodu1c+w(ixlat,ixlon,imix)*turb_d1c
                        aodu2i=aodu2i+w(ixlat,ixlon,imix)*turb_d2i
                        aodu2s=aodu2s+w(ixlat,ixlon,imix)*turb_d2s
                        aodu2c=aodu2c+w(ixlat,ixlon,imix)*turb_d2c
                     endif

                     if (ptpval.gt.0) then
                       aod2norm=aod2norm+w(ixlat,ixlon,imix)
                       aod2in=aod2in+w(ixlat,ixlon,imix)*turb
                     endif

                     if (turb.gt.5.) nhigh(imix)=nhigh(imix)+1
                     if (turb.lt.0.) nlow(imix)=nlow(imix)+1
                     if (turb.le.0..and.turb.gt.-0.05) nsmall(imix)=nsmall(imix)+1

                     if (turb.lt.minaod(imix)) minaod(imix)=turb
                     if (turb.gt.maxaod(imix)) maxaod(imix)=turb
                     meanaod(imix)=meanaod(imix)+turb

                     if (ptpval.eq.1) then
                        meanaod1(imix)=meanaod1(imix)+turb
                        if (turb.lt.minaod1(imix)) minaod1(imix)=turb
                        if (turb.gt.maxaod1(imix)) maxaod1(imix)=turb
                     endif
                     if (ptpval.eq.2) then
                        meanaod2(imix)=meanaod2(imix)+turb
                        if (turb.lt.minaod2(imix)) minaod2(imix)=turb
                        if (turb.gt.maxaod2(imix)) maxaod2(imix)=turb
                      endif

                      if (ptpval.eq.1) then
                         if (jmix.eq.jmix2) then
                            aodclimmean=aodclimmean+turb
                            if (turb.lt.minclaod) minclaod=turb
                            if (turb.gt.maxclaod) maxclaod=turb
                         endif
                      endif
                   endif

                   if (ptpval.eq.1) then
                      AOD(imix,col,lin)=turb
                   endif

! assign values to aod for climatology mix for ptp = 1 only / ptpt = 1 or 2
                   if (jmix.eq.jmix2) then
                      if (ptpval.eq.1) AODCLIM(col,lin)=turb
                   endif

! define ptp2 values
                   ptpval2=ptpval+250
                   if (turb.gt.5..and.turb.ne.999.) ptpval2=ptpval+150
                   if (turb.le.-0.05) ptpval2=ptpval+120
                   if (turb.gt.-0.05.and.turb.le.0.) ptpval2=ptpval+130
                   if (turb.eq.999.) ptpval2=ptpval+170
                   PTP2(imix,col,lin)=ptpval2

! aod inversion end

   11          continue

               if (ptpval.gt.0.and.aod2norm.ne.0.) AOD2(col,lin)=aod2in/aod2norm


               if (ptpval.eq.1) then
                  if (aodensnum.ne.0) aodensmean=aodensmean/aodensnum
                  if (aodensnorm.ne.0.) then
                    aodensmean2=aodensmean2/aodensnorm
                    AOD1(col,lin)=aod1in/aodensnorm
                    AOD_U1I(col,lin)=aodu1i/aodensnorm
                    AOD_U1S(col,lin)=aodu1s/aodensnorm
                    AOD_U1C(col,lin)=aodu1c/aodensnorm
                    AOD_U2I(col,lin)=aodu2i/aodensnorm
                    AOD_U2S(col,lin)=aodu2s/aodensnorm
                    AOD_U2C(col,lin)=aodu2c/aodensnorm
                  endif
                  aodens(col,lin)=aodensmean
                  aodensb(col,lin)=aodensmean2
				  aodensbsel(col,lin)=aodensb(col,lin)

                  sigmasum=0.
                  sigmasum2=0.
                  sigmanum=0

                  do imix=1,maxmixloop
                     if (AOD(imix,col,lin).ne.999..and.AOD(imix,col,lin).ne.-999.) then
                        sigmanum=sigmanum+1
                        aoddiff=AOD(imix,col,lin)-aodensmean
                        aoddiff2=AOD(imix,col,lin)-aodensmean2
                        sigmasum=sigmasum+aoddiff*aoddiff
                        sigmasum2=sigmasum2+aoddiff*aoddiff
                     endif
                  enddo

                 if (sigmanum.gt.1) sigmasum=sqrt(sigmasum)/sqrt(float(sigmanum-1))
                 if (sigmanum.le.1) sigmasum=0.
                 if (sigmanum.gt.1) sigmasum2=sqrt(sigmasum2)/sqrt(float(sigmanum-1))
                 if (sigmanum.le.1) sigmasum2=0.

                 aod_u3(col,lin) = sigmasum
                 if (aodensnorm.ne.0.) aod_u3b(col,lin) = sigmasum * sqrt(aodensnorm2)/aodensnorm

                 aod_ui(col,lin) = sqrt(aod_u1i(col,lin)*aod_u1i(col,lin)+aod_u2i(col,lin)*aod_u2i(col,lin)+0.01*0.01)
                 aod_us(col,lin) = sqrt(aod_u1s(col,lin)*aod_u1s(col,lin)+aod_u2s(col,lin)*aod_u2s(col,lin))
                 aod_uc(col,lin) = sqrt(aod_u1c(col,lin)*aod_u1c(col,lin)+aod_u2c(col,lin)*aod_u2c(col,lin))

                 val=aod_u1c(col,lin)*aod_u1c(col,lin)+aod_u1i(col,lin)*aod_u1i(col,lin)
                 aod_u1(col,lin) = sqrt(val+aod_u1s(col,lin)*aod_u1s(col,lin))
                 val=aod_u2c(col,lin)*aod_u2c(col,lin)+aod_u2i(col,lin)*aod_u2i(col,lin)
                 aod_u2(col,lin) = sqrt(val+aod_u2s(col,lin)*aod_u2s(col,lin))

! *** add noise term sigma(AOD) := 0.01 to independent component
                 sq1=aod_ui(col,lin)*aod_ui(col,lin)
                 sq2=aod_us(col,lin)*aod_us(col,lin)
                 sq3=aod_uc(col,lin)*aod_uc(col,lin)
                 sq4=aod_u3b(col,lin)*aod_u3b(col,lin)
                 aod_u(col,lin)=sqrt(sq1+sq2+sq3+sq4)
				 aod_u_sel(col,lin)=aod_u(col,lin)

               endif

           endif

           if (aodensb(col,lin).eq.-999.) alb2(col,lin)=-999.

   12   continue
   13 continue

      print*,'numbers: ',nall,nptp,nptp1,nptp2

   do 14 imix=1,maxmixloop
      if (nptp.ne.0) meanaod(imix)=meanaod(imix)/nptp
      if (nptp1.ne.0) meanaod1(imix)=meanaod1(imix)/nptp1
      if (nptp2.ne.0) meanaod2(imix)=meanaod2(imix)/nptp2

      nx1=nptp-nlow(imix)-nhigh(imix)
      nx2=nptp-nlow(imix)+nsmall(imix)-nhigh(imix)
      if (imix.eq.1.or.imix.eq.4.or.imix.eq.16.or.imix.eq.20) then
         print*,'numbers: ',imix,nlow(imix),nsmall(imix),nhigh(imix),nx1,nx2,nnotvalid(imix)
         print*,'aod:           ',minaod(imix),maxaod(imix),meanaod(imix)
         print*,'aod(ptpt=1): ',minaod1(imix),maxaod1(imix),meanaod1(imix)
         print*,'aod(ptpt=1/2):   ',minaod2(imix),maxaod2(imix),meanaod2(imix)
      endif

   14 continue

      if (nptp.ne.0) aodclimmean=aodclimmean/nptp
      print*,'aodclim:     ',minclaod,maxclaod,aodclimmean

      print*,minval(aod),maxval(aod)


  RETURN
 END SUBROUTINE fiduceo_aod_retrieval

