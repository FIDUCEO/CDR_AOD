SUBROUTINE fiduceo_average()
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
 
IMPLICIT NONE


	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------

  INTEGER*2, parameter    :: n1 = 1
  integer                 :: i,j,imix,lin,col,maxmixloop
  integer                 :: ig,jg,ik,jk,colb,linb
  INTEGER                 :: status

  INTEGER                 :: aoddiffnum1,aoddiffnum2,minnumber,minnumber2
  REAL                    :: aodval1,aodval2,aodval
  real*4                  :: sq1,sq2,sq3,sq4,sq5
  real*4                  :: weight

  CHARACTER(LEN=200)      :: outfile
  INTEGER                 :: irec
  INTEGER                 :: recl2

  gridval=3

  gridval2=2*gridval

  minnumber=3
  minnumber2=minnumber

  dimx_g=aint(float(dimx2)/float(gridval))
  if (dimx_g*gridval.ne.dimx2) dimx_g=dimx_g+1
  dimy_g=aint(float(dimy)/float(gridval))
  if (dimy_g*gridval.ne.dimy) dimy_g=dimy_g+1

  allocate(latitude_g(dimx_g,dimy_g), stat=status)
  allocate(longitude_g(dimx_g,dimy_g), stat=status)

  allocate(AOD_G(36,dimx_g,dimy_g), stat=status)
  allocate(ALB_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_UI_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_US_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_UC_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U3_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U3B_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U4_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U_G(dimx_g,dimy_g), stat=status)
  allocate(AODCLIM_G(dimx_g,dimy_g), stat=status)
  allocate(AODENS_G(dimx_g,dimy_g), stat=status)
  allocate(AODENSB_G(dimx_g,dimy_g), stat=status)
  allocate(AOD1_G(dimx_g,dimy_g), stat=status)
  allocate(AOD2_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U_RAN_G(dimx_g,dimy_g), stat=status)
  allocate(AOD_U_CORR_G(dimx_g,dimy_g), stat=status)

  allocate(N_G(dimx_g,dimy_g), stat=status)
  allocate(N2_G(dimx_g,dimy_g), stat=status)
  allocate(NENS_G(dimx_g,dimy_g), stat=status)
  allocate(NENSB_G(dimx_g,dimy_g), stat=status)
  allocate(NCLIM_G(dimx_g,dimy_g), stat=status)
  allocate(NMIX_G(36,dimx_g,dimy_g), stat=status)
  allocate(nlat(dimx_g,dimy_g), stat=status)
  allocate(N3_G(dimx_g,dimy_g), stat=status)
  allocate(XCLIM_G(dimx_g,dimy_g), stat=status)

  allocate(TIME_G(dimx_g,dimy_g), stat=status)

!----------------------------------------------------
! aod averaging
!----------------------------------------------------


      maxmixloop=36

 print*,'aod averaging...'

       do  lin=1,dimy_g
           do  col=1,dimx_g
                 n_g(col,lin)=0
                 n2_g(col,lin)=0
                 nclim_g(col,lin)=0
                 xclim_g(col,lin)=0.
                 nens_g(col,lin)=0
                 nensb_g(col,lin)=0
                 nlat(col,lin)=0
                 alb_g(col,lin)=0.
                 aod1_g(col,lin)=0.
                 aod2_g(col,lin)=0.
                 aodens_g(col,lin)=0.
                 aodensb_g(col,lin)=0.
                 aodclim_g(col,lin)=0.
                 latitude_g(col,lin)=0.
                 longitude_g(col,lin)=0.
                 aod_u3_g(col,lin)=0.
                 aod_u3b_g(col,lin)=0.
                 aod_u4_g(col,lin)=0.
                 aod_ui_g(col,lin)=0.
                 aod_us_g(col,lin)=0.
                 aod_us_g(col,lin)=0.
                 aod_uc_g(col,lin)=0.
                 aod_u_ran_g(col,lin)=0.
                 aod_u_corr_g(col,lin)=0.
                 do  imix=1,maxmixloop
                     aod_g(imix,col,lin)=0.
                     nmix_g(imix,col,lin)=0
                 enddo
           enddo
      enddo

       do  lin=1,dimy
           jg=(lin-1)/gridval+1

           do  col=1,dimx2
               ig=(col-1)/gridval+1

                nlat(ig,jg)=nlat(ig,jg)+n1
                latitude_g(ig,jg)=latitude_g(ig,jg)+latitude(col,lin)
                longitude_g(ig,jg)=longitude_g(ig,jg)+longitude(col,lin)
                time_g(ig,jg)=time(col,lin)

                if (ptp(col,lin).eq.1) then

                  if (aod1(col,lin).gt.-999.and.aod1(col,lin).lt.999) then
                    n_g(ig,jg)=n_g(ig,jg)+n1
                    alb_g(ig,jg)=alb_g(ig,jg)+alb(col,lin)
                    aod1_g(ig,jg)=aod1_g(ig,jg)+aod1(col,lin)
                  endif

                  if (aodens(col,lin).gt.-999.and.aodens(col,lin).lt.999) then
                    nens_g(ig,jg)=nens_g(ig,jg)+n1
                    aodens_g(ig,jg)=aodens_g(ig,jg)+aodens(col,lin)
                  endif

                  if (aodensb(col,lin).gt.-999.and.aodensb(col,lin).lt.999) then
                    nensb_g(ig,jg)=nensb_g(ig,jg)+n1
                    aodensb_g(ig,jg)=aodensb_g(ig,jg)+aodensb(col,lin)
                  endif

                  if (aodclim(col,lin).gt.-999.and.aodclim(col,lin).lt.999) then
                    nclim_g(ig,jg)=nclim_g(ig,jg)+n1
                    weight=0.
                    if (aod_u1s(col,lin).ne.0.) weight=1./(aod_u1s(col,lin)*aod_u1s(col,lin))
                    xclim_g(ig,jg)=xclim_g(ig,jg)+weight
                    aodclim_g(ig,jg)=aodclim_g(ig,jg)+aodclim(col,lin)
                  endif

                    do  imix=1,maxmixloop
                      if (aod(imix,col,lin).gt.-999.and.aod(imix,col,lin).lt.999) then
                        nmix_g(imix,ig,jg)=nmix_g(imix,ig,jg)+n1
                        aod_g(imix,ig,jg)=aod_g(imix,ig,jg)+aod(imix,col,lin)
                      endif

                    enddo

                  if (aod1(col,lin).gt.-999.and.aod1(col,lin).lt.999) then
                    aod_ui_g(ig,jg)=aod_ui_g(ig,jg)+aod_ui(col,lin)*aod_ui(col,lin)
                    aod_uc_g(ig,jg)=aod_uc_g(ig,jg)+aod_uc(col,lin)
                    aod_us_g(ig,jg)=aod_us_g(ig,jg)+aod_us(col,lin)
                    aod_u3_g(ig,jg)=aod_u3_g(ig,jg)+aod_u3(col,lin)
                    aod_u3b_g(ig,jg)=aod_u3b_g(ig,jg)+aod_u3b(col,lin)
                    aod_u_ran_g(ig,jg)=aod_u_ran_g(ig,jg)+aod_u(col,lin)*aod_u(col,lin)
                    aod_u_corr_g(ig,jg)=aod_u_corr_g(ig,jg)+aod_u(col,lin)
                  endif

                 endif


                if (ptp(col,lin).eq.1.or.ptp(col,lin).eq.2) then
                  if (aod2(col,lin).gt.-999.and.aod2(col,lin).lt.999) then
                    n2_g(ig,jg)=n2_g(ig,jg)+n1
                    aod2_g(ig,jg)=aod2_g(ig,jg)+aod2(col,lin)
                  endif
                 endif

           enddo

      enddo

       do  lin=1,dimy_g
           do  col=1,dimx_g
               if (nlat(col,lin).gt.0) then
                  latitude_g(col,lin)=latitude_g(col,lin)/nlat(col,lin)
                  longitude_g(col,lin)=longitude_g(col,lin)/nlat(col,lin)
               else
                  latitude_g(col,lin)=-999.
                  longitude_g(col,lin)=-999.
               endif

               if (n_g(col,lin).gt.minnumber) then
                 alb_g(col,lin)=alb_g(col,lin) / n_g(col,lin)
                 aod1_g(col,lin)=aod1_g(col,lin) / n_g(col,lin)
                 aod_us_g(col,lin)=aod_us_g(col,lin) / n_g(col,lin)
                 aod_uc_g(col,lin)=aod_uc_g(col,lin) / n_g(col,lin)
                 aod_u3_g(col,lin)=aod_u3_g(col,lin) / n_g(col,lin)
                 aod_u3b_g(col,lin)=aod_u3b_g(col,lin) / n_g(col,lin)
                 aod_u_corr_g(col,lin)=aod_u_corr_g(col,lin) / n_g(col,lin)
               else
                 alb_g(col,lin)=-999.
                 aod_us_g(col,lin)=-999.
                 aod_uc_g(col,lin)=-999.
                 aod_u3_g(col,lin)=-999.
                 aod_u3b_g(col,lin)=-999.
                 aod_u_corr_g(col,lin)=-999.
               endif

               if (n_g(col,lin).gt.minnumber2) then
                 aod_ui_g(col,lin)=sqrt(aod_ui_g(col,lin))/float(n_g(col,lin))
                 aod_u_ran_g(col,lin)=sqrt(aod_u_ran_g(col,lin))/float(n_g(col,lin))
               else
                 aod_ui_g(col,lin)=-999.
                 aod_u_ran_g(col,lin)=-999.
               endif

               if (nens_g(col,lin).gt.minnumber) then
                 aodens_g(col,lin)=aodens_g(col,lin) / nens_g(col,lin)
               else
                 aodens_g(col,lin)=-999.
               endif
               if (aodens_g(col,lin).ge.-0.05.and.aodens_g(col,lin).lt.0.) aodens_g(col,lin)=0.0001
               if (aodens_g(col,lin).lt.-0.05) aodens_g(col,lin)=-999.

               if (nensb_g(col,lin).gt.minnumber) then
                 aodensb_g(col,lin)=aodensb_g(col,lin) / nensb_g(col,lin)
               else
                 aodensb_g(col,lin)=-999.
               endif
               if (aodensb_g(col,lin).ge.-0.05.and.aodensb_g(col,lin).lt.0.) aodensb_g(col,lin)=0.0001
               if (aodensb_g(col,lin).lt.-0.05) aodensb_g(col,lin)=-999.

               if (nclim_g(col,lin).gt.minnumber) then
                 aodclim_g(col,lin)=aodclim_g(col,lin) / nclim_g(col,lin)
               else
                 aodclim_g(col,lin)=-999.
               endif
               if (aodclim_g(col,lin).ge.-0.05.and.aodclim_g(col,lin).lt.0.) aodclim_g(col,lin)=0.0001
               if (aodclim_g(col,lin).lt.-0.05) aodclim_g(col,lin)=-999.

               do  imix=1,maxmixloop
                 if (nmix_g(imix,col,lin).gt.minnumber) then
                     aod_g(imix,col,lin)=aod_g(imix,col,lin) / nmix_g(imix,col,lin)
                 else
                     aod_g(imix,col,lin)=-999.
                 endif
                 if (aod_g(imix,col,lin).ge.-0.05.and.aod_g(imix,col,lin).lt.0.) aod_g(imix,col,lin)=0.0001
                 if (aod_g(imix,col,lin).lt.-0.05) aod_g(imix,col,lin)=-999.
               enddo

               if (n2_g(col,lin).gt.minnumber) then
                  aod2_g(col,lin)=aod2_g(col,lin) / n2_g(col,lin)
               endif

           enddo
      enddo



!----------------------------------------------------
! calculate cloud maks induced difference for aod_u4
!----------------------------------------------------

      do lin=1,dimy_g,4
         linb=MINVAL((/dimy_g,lin+3/))
         do col=1,dimx_g,4
            colb=MINVAL((/dimx_g,col+3/))
            aodval1=0.
            aodval2=0.
            aoddiffnum1=0
            aoddiffnum2=0
            do ik=col,colb
               do jk=lin,linb
                  if (n_g(ik,jk)+n2_g(ik,jk).gt.minnumber) then
                    aoddiffnum1=aoddiffnum1+n_g(ik,jk)
                    aoddiffnum2=aoddiffnum2+n2_g(ik,jk)
                    aodval1=aodval1+n_g(ik,jk)*aod1_g(ik,jk)
                    aodval2=aodval2+n2_g(ik,jk)*aod2_g(ik,jk)
                  endif
               enddo
            enddo
            if (aoddiffnum1.ne.0) aodval1=aodval1/aoddiffnum1
            if (aoddiffnum2.ne.0) aodval2=aodval2/aoddiffnum2
            aodval=abs(aodval2-aodval1)
            do ik=col,colb
               do jk=lin,linb
                  if (aodval1+aodval2.ne.0.) then
                     aod_u4_g(ik,jk)=aodval
                  else
                     aod_u4_g(ik,jk)=0.
                  endif
               enddo
            enddo
         enddo
      enddo 

       do  lin=1,dimy_g
           do col=1,dimx_g
               if (n_g(col,lin).gt.minnumber) then
                 sq1=aod_ui_g(col,lin)*aod_ui_g(col,lin)
                 sq2=aod_us_g(col,lin)*aod_us_g(col,lin)
                 sq3=aod_uc_g(col,lin)*aod_uc_g(col,lin)
                 sq4=aod_u3b_g(col,lin)*aod_u3b_g(col,lin)
                 sq5=aod_u4_g(col,lin)*aod_u4_g(col,lin)
                 aod_u_g(col,lin)=sqrt(sq1+sq2+sq3+sq4+sq5)
                 sq1=aod_u_ran_g(col,lin)*aod_u_ran_g(col,lin)
                 aod_u_ran_g(col,lin)=sqrt(sq1+sq5)
                 sq1=aod_u_corr_g(col,lin)*aod_u_corr_g(col,lin)
                 aod_u_corr_g(col,lin)=sqrt(sq1+sq5)
               else
                 aod_u_g(col,lin)=-999.
                 aod_u4_g(col,lin)=-999.
                 aod_u_ran_g(col,lin)=-999.
                 aod_u_corr_g(col,lin)=-999.
               endif

              if (aodensb_g(col,lin).eq.-999.) n_g(col,lin)=-999
              if (aodensb_g(col,lin).ne.-999.) n3_g(col,lin)=n_g(col,lin)
              if (aodensb_g(col,lin).eq.-999.) n3_g(col,lin)=-999
              if (n3_g(col,lin).eq.0) n3_g(col,lin)=-999
           enddo
       enddo 


  RETURN
 END SUBROUTINE fiduceo_average

