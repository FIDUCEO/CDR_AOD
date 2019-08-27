MODULE fiduceo_coeffs
!=====#################################################################
! NAME:
!		fid_avhrr_aod.f90
! PURPOSE:
!		define coefficient arrays for commmon use in main and subroutines
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Thomas Popp, 04.01.2018
! PROJECT:
!		FIDUCEO
!=====#################################################################
 USE kinds


IMPLICIT NONE

        INTEGER, parameter    ::  ind5        =  19
	INTEGER, parameter    ::  nmm         =  26
	INTEGER, parameter    ::  nm0         =  8
        INTEGER, parameter    ::  maxlamatsr  =  3
        INTEGER, parameter    ::  maxmixatsr  =  130
	INTEGER, parameter    ::  nlut        =  27
	INTEGER, parameter    ::  nm2         =  nmm-17
        INTEGER, parameter    ::  NX          =  160
        INTEGER, parameter    ::  NY          =  160

        INTEGER, parameter    ::  inda1       =  10
        INTEGER, parameter    ::  inda2       =   7
        INTEGER, parameter    ::  inda3       =  10
	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------
	
      real*4  tabmue(inda1),tabphi(inda2),tabmus(inda3)
      real*4  tabalb061(inda1,inda2,inda3)
      integer ntamue,ntamus,ntaphi

      namelist/list62/ntamue,tabmue
      namelist/list63/ntaphi,tabphi
      namelist/list64/ntamus,tabmus

      real*4           dmtyp(maxlamatsr,maxmixatsr)
      real*4           dfak(maxlamatsr,maxmixatsr)
      real*4           d0(maxlamatsr,maxmixatsr)

      real*4           trcoef(maxlamatsr,ind5,nm2:nmm,nm0:nmm,2*nlut,maxmixatsr,3)

      real*4           acoef(maxlamatsr,ind5,nm2:nmm,nm0:nmm,6,3,maxmixatsr)

      real*4           breakrtoa(maxmixatsr,ind5,nm2:nmm,nm0:nmm,nlut)

      real*4           dustfrac(12,180,360)
      real*4           finefrac(12,180,360)
      real*4           absfrac(12,180,360)

      real*4           xfmf(maxmixatsr)
      real*4           xfmaf(maxmixatsr)
      real*4           xcmsf(maxmixatsr)

      real*4           data_in(NY, NX)

      real*4           hmess(nmm),azidg(ind5)

      integer          mixnum(130),mixnum2(36)

!=====#################################################################
END MODULE fiduceo_coeffs
