MODULE fiduceo_data
!=====#################################################################
! NAME:
!		fid_avhrr_aod.f90
! PURPOSE:
!		define data arrays for commmon use in main and subroutines
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


IMPLICIT NONE

	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------
	
	INTEGER				:: dimx,dimy,dimx2,dimy2
        INTEGER                         :: dimx_g,dimy_g
        INTEGER                         :: gridval,gridval2


        real*4           radval(6,1500)
        real*4           linecorrel(6,201) 

	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: reflectance
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: brightness_temperature
	
 	REAL (KIND=p_real),dimension(:,:), allocatable	        :: latitude
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: longitude
 
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: solzen
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: satzen
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: relazi
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: scatangle
	
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_r_common
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_r_independent
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_r_structured
	
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_bt_common
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_bt_independent
	REAL (KIND=p_real),dimension(:,:,:), allocatable	:: uncertainty_bt_structured
  
	INTEGER,dimension(:,:), allocatable	                :: quality_pixel_bitmask
	REAL,dimension(:,:), allocatable	                :: time
	
        INTEGER*2, ALLOCATABLE                                  :: CLM(:,:)      ! APOLLO CLOUD MASK (COV + TYPE)
        INTEGER*2, ALLOCATABLE                                  :: LSGM(:,:)     ! APOLLO LAND-SEA-GLINT MASK 
        INTEGER*2, ALLOCATABLE                                  :: PCLD(:,:)     ! cloud probability
        REAL, ALLOCATABLE                                       :: REF3(:,:)     ! 3.7µm  reflectance
        REAL, ALLOCATABLE                                       :: REF3_U_I(:,:) ! 3.7µm  independent uncertainty
        REAL, ALLOCATABLE                                       :: REF3_U_C(:,:) ! 3.7µm  common  uncertainty
        REAL, ALLOCATABLE                                       :: REF3_U_S(:,:) ! 3.7µm  structured  uncertainty

        INTEGER*2, ALLOCATABLE                                  :: ib(:,:)
        INTEGER*2, ALLOCATABLE                                  :: is(:,:)
        INTEGER*2, ALLOCATABLE                                  :: iz(:,:)
        REAL, ALLOCATABLE                                       :: ndvi(:,:)
        REAL, ALLOCATABLE                                       :: ndii(:,:)


        INTEGER*2, ALLOCATABLE                                  :: PTP(:,:)      ! dark field selection mask
        INTEGER*2, ALLOCATABLE                                  :: PTP2(:,:,:)   ! dark field selection mask
        INTEGER*2, ALLOCATABLE                                  :: MIXSEL(:,:)   ! aerosol mix selection mask
        REAL, ALLOCATABLE                                       :: BFAK(:,:)     ! conversion factor
        REAL, ALLOCATABLE                                       :: ALB(:,:)      ! dark field estimated albedo mask
        REAL, ALLOCATABLE                                       :: ALB2(:,:)     ! dark field estimated albedo mask

        REAL, ALLOCATABLE                                       :: albval_u_i(:,:) 
        REAL, ALLOCATABLE                                       :: albval_u_c(:,:) 
        REAL, ALLOCATABLE                                       :: albval_u_s(:,:) 

        REAL, ALLOCATABLE                                       :: AOD(:,:,:)    ! retrieved AOD per mix
        REAL, ALLOCATABLE                                       :: AOD1(:,:)     ! retrieved AOD  mix1 for ptp2 only
        REAL, ALLOCATABLE                                       :: AOD2(:,:)     ! retrieved AOD  mix1 for ptp1 only
        REAL, ALLOCATABLE                                       :: AOD_U1I(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U1S(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U1C(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U2I(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U2S(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U2C(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_UI(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U1(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U2(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_US(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_UC(:,:)  ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U3(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U3B(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U(:,:)    ! propagated AOD uncertainty
		REAL, ALLOCATABLE                                       :: AOD_U_SEL(:,:)    ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AODCLIM(:,:)  ! retrieved AOD for climatology mix
        REAL, ALLOCATABLE                                       :: AODENS(:,:)   ! retrieved AOD for mix ensemble

        REAL, ALLOCATABLE                                       :: AODENSB(:,:)   ! retrieved AOD for mix ensemble
		REAL, ALLOCATABLE                                       :: AODENSBSEL(:,:)   ! retrieved AOD for mix ensemble

        REAL, ALLOCATABLE                                       :: w(:,:,:)   ! ensemble weights per mixture (not normalized)

! further layers for testing purposes
        REAL, ALLOCATABLE                                       :: BFAK2(:,:)
        REAL, ALLOCATABLE                                       :: BFAK3(:,:)
        REAL, ALLOCATABLE                                       :: BFAKratio(:,:)
        REAL, ALLOCATABLE                                       :: BFAKratio2(:,:)
        REAL, ALLOCATABLE                                       :: ndvikorr(:,:)
        REAL, ALLOCATABLE                                       :: ndiikorr(:,:)
        REAL, ALLOCATABLE                                       :: ref1korr(:,:)
        REAL, ALLOCATABLE                                       :: ref2korr(:,:)
        REAL, ALLOCATABLE                                       :: ref1p(:,:)
        REAL, ALLOCATABLE                                       :: ref2p(:,:)
        REAL, ALLOCATABLE                                       :: ndvip(:,:)
        REAL, ALLOCATABLE                                       :: ndiip(:,:)
        REAL, ALLOCATABLE                                       :: ref37p(:,:)
        REAL, ALLOCATABLE                                       :: ndvidiff(:,:)
        REAL, ALLOCATABLE                                       :: ndiidiff(:,:)

        REAL, ALLOCATABLE                                       :: ndvikorr2(:,:)
        REAL, ALLOCATABLE                                       :: ndiikorr2(:,:)
        REAL, ALLOCATABLE                                       :: ref1korr2(:,:)
        REAL, ALLOCATABLE                                       :: ref2korr2(:,:)

! averaged grid
 	REAL, allocatable	                                :: latitude_g(:,:)
	REAL, allocatable	                                :: longitude_g(:,:)
        REAL, ALLOCATABLE                                       :: ALB_G(:,:)      ! dark field estimated albedo mask
        REAL, ALLOCATABLE                                       :: AOD_G(:,:,:)    ! retrieved AOD per mix
        REAL, ALLOCATABLE                                       :: AOD1_G(:,:)     ! retrieved AOD  mix1 for ptp=1 only
        REAL, ALLOCATABLE                                       :: AOD2_G(:,:)     ! retrieved AOD  mix1 for ptp=1 and 2
        REAL, ALLOCATABLE                                       :: AOD_UI_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_US_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_UC_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U3_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U3B_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U4_G(:,:)   ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U_G(:,:)    ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U_RAN_G(:,:)    ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AOD_U_CORR_G(:,:)    ! propagated AOD uncertainty
        REAL, ALLOCATABLE                                       :: AODCLIM_G(:,:)  ! retrieved AOD for climatology mix
        REAL, ALLOCATABLE                                       :: AODENS_G(:,:)   ! retrieved AOD for mix ensemble
        REAL, ALLOCATABLE                                       :: AODENSB_G(:,:)   ! retrieved AOD for mix ensemble
        INTEGER*2, ALLOCATABLE                                  :: N_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: N3_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: N2_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: NENS_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: NENSB_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: NCLIM_G(:,:) 
        INTEGER*2, ALLOCATABLE                                  :: NMIX_G(:,:,:) 
        INTEGER*2, ALLOCATABLE                                  :: nlat(:,:)

        REAL, ALLOCATABLE                                       :: XCLIM_G(:,:) 

        REAL, ALLOCATABLE                                       :: TIME_G(:,:) 


!=====#################################################################
END MODULE fiduceo_data
