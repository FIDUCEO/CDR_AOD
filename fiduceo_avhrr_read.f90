MODULE fiduceo_avhrr_read
!=====#################################################################
! NAME:
!		avhrr_read.f90
! PURPOSE:
!		reading AVHRR data for SYNAER
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
 USE functions_ascii
 USE functions_runtime
 USE fiduceo_config
 USE fiduceo_data
 USE netcdf

IMPLICIT NONE

include '/usr/include/netcdf.inc'

	


	INTEGER, parameter		:: n_channels_r			 	= 3
	INTEGER, parameter		:: n_channels_bt 			= 3
	INTEGER, parameter		:: channel_no_r(n_channels_r)	= &
		(/1,2,3/)
	INTEGER, parameter		:: channel_no_bt(n_channels_bt)	= &
		(/3,4,5/)

	
!----------------------------------------------------
! variable declaration
!----------------------------------------------------
	REAL (KIND=p_real)		:: channel_wl_r(n_channels_r)
	REAL (KIND=p_real)		:: channel_wl_bt(n_channels_r)
	
	INTEGER(KIND=p_long), parameter						:: fill_valuei = -999
	REAL (KIND=p_real), parameter						:: fill_valuef = 0.
	

	
CONTAINS
!=====#################################################################
!*********************************************************
  SUBROUTINE read_orbit()
!*********************************************************
  IMPLICIT NONE
  
	CHARACTER(len=c_len)			:: reflectancefile

        REAL                                    :: latmax,latmin
        INTEGER(KIND=p_long)                    :: l0,lmax
	
	INTEGER(KIND=p_long)			:: i,j
	CHARACTER (LEN=10)				:: chnr_str,i_str
	CHARACTER (LEN=80)				:: dimname1,dimname2,varname
	CHARACTER (LEN=20)				:: start_date_tmp
	INTEGER							:: idx(1)
	INTEGER							:: dimids(2),ncid,varid
	INTEGER(KIND=p_long)			:: status
	REAL(KIND=p_real), PARAMETER	:: PI   = 2*ASIN(1.0)   ! [-], the constant pi
	REAL(KIND=p_real)				:: wldiff,fillval
  
	INTEGER							:: ndims,nvars,ngatts,unlimdimid
	REAL (KIND=p_real),dimension(:,:), allocatable	:: reflectance_x
	REAL (KIND=p_real),dimension(:,:), allocatable	:: var_x
	REAL (KIND=p_real),dimension(:,:), allocatable	:: brightness_x

 	REAL (KIND=p_real),dimension(:,:), allocatable	        :: latitude_x
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: longitude_x
 
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: solzen_x
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: satzen_x
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: relazi_x
	REAL (KIND=p_real),dimension(:,:), allocatable	        :: time_x
  
        character (len = 10) 	:: dim_name1,dim_name2
        character (len = 10) 	:: var_name
		integer					:: dim_len1,dim_len2
        integer					:: xtype
        integer					:: nAtts
        integer					:: sizex
        integer					:: nfields
	
  latmin=30.
  latmax=75.

	reflectancefile=trim(adjustl(lv1path))//trim(adjustl(lv1file))
	
  print*,'Open file...'
  print*,'#'//trim(reflectancefile)//'#'
  status = nf90_open(trim(reflectancefile), NF_NOWRITE, ncid)
  
  status = nf90_inq_varid(ncid, 'latitude',varid)
  status = nf90_inquire_variable(ncid, varid, var_name, xtype, ndims, dimids, nAtts)
  status = nf90_inq_type(ncid, xtype, var_name, sizex)
  status = nf90_inquire_dimension(ncid, dimids(1), dim_name1, dimx)
  status = nf90_inquire_dimension(ncid, dimids(2), dim_name2, dimy)

  print*,'input dimensions: ',dimx,dimy

  allocate(latitude_x(dimx,dimy), stat=status)
  allocate(longitude_x(dimx,dimy), stat=status)

  status 	= nf90_get_var(ncid,varid,latitude_x)
  
  status 	= nf90_inq_varid(ncid, 'longitude',varid)
  status 	= nf90_get_var(ncid,varid,longitude_x)
  print*,'coordinates read'

! identify lines range where latmin <= latitude <= latmax

  print*,'latitude range: ',latmin,latmax
  l0=dimx
  lmax=1

  do i=1,dimx
     do j=1,dimy
        if (latitude_x(i,j).ge.latmin.and.l0.eq.dimx) l0=i
        if (latitude_x(i,j).le.latmax) lmax=i
     enddo
  enddo
  dimx2=lmax-l0+1

  print*,'selected lines range: ',l0,lmax
  print*,'output dimensions: ',dimx2,dimy

! copy coordinates variables to selected latitude range
  allocate(latitude(dimx2,dimy), stat=status)
  allocate(longitude(dimx2,dimy), stat=status)

  allocate(reflectance(n_channels_r,dimx2,dimy), stat=status)
  allocate(brightness_temperature(n_channels_bt,dimx2,dimy), stat=status)
  allocate(solzen(dimx2,dimy), stat=status)
  allocate(satzen(dimx2,dimy), stat=status)
  allocate(relazi(dimx2,dimy), stat=status)
  allocate(scatangle(dimx2,dimy), stat=status)
  allocate(time(dimx2,dimy), stat=status)
				
  allocate(uncertainty_r_common(n_channels_r,dimx2,dimy), stat=status)
  allocate(uncertainty_r_independent(n_channels_r,dimx2,dimy), stat=status)
  allocate(uncertainty_r_structured(n_channels_r,dimx2,dimy), stat=status)

  allocate(uncertainty_bt_common(n_channels_bt,dimx2,dimy), stat=status)
  allocate(uncertainty_bt_independent(n_channels_bt,dimx2,dimy), stat=status)
  allocate(uncertainty_bt_structured(n_channels_bt,dimx2,dimy), stat=status)

  latitude = latitude_x(l0:lmax,:)
  longitude = longitude_x(l0:lmax,:)

!----------------------------------------------------
! read all channel reflectances
!----------------------------------------------------
  DO i=1,n_channels_r
    
  Write( i_str, '(i10)' ) i
  chnr_str='Ch'//trim(adjustl(i_str))
  IF (channel_no_r(i) .EQ. 3) chnr_str='Ch'//trim(adjustl(i_str))//'a'
  
  status = nf90_inq_varid(ncid, trim(adjustl(chnr_str)),varid)
  status = nf90_inquire_variable(ncid, varid, var_name, xtype, ndims, dimids, nAtts)
  status = nf90_inq_type(ncid, xtype, var_name, sizex)
  status = nf90_inquire_dimension(ncid, dimids(1), dim_name1, dimx)
  status = nf90_inquire_dimension(ncid, dimids(2), dim_name2, dimy)

  print*,'input dimensions: ',dimx,dimy
  
  IF (i .eq. 1) THEN
  print*,'allocating variables'
  allocate(reflectance_x(dimx,dimy), stat=status)
  allocate(var_x(dimx,dimy), stat=status)
  
  print*,'dimension of orbit: ',dimx,dimy
  ENDIF

  status 	= nf90_get_var(ncid,varid,reflectance_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(reflectance_x .EQ. fillval) reflectance_x=fill_valuef
  
  reflectance(i,:,:)=reflectance_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_independent_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_r_independent(i,:,:)=var_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_common_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_r_common(i,:,:)=var_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_structured_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_r_structured(i,:,:)=var_x(l0:lmax,:)
  
  ENDDO 

  print*,'reflectance read'

  DO i=1,n_channels_bt
    
  Write( i_str, '(i10)' ) i+2
  chnr_str='Ch'//trim(adjustl(i_str))
  IF (channel_no_bt(i) .EQ. 3) chnr_str='Ch'//trim(adjustl(i_str))//'b'
print*,chnr_str
  
  status 	= nf90_inq_varid(ncid, trim(adjustl(chnr_str)),varid)
  IF (i .eq. 1) THEN
  print*,'allocating variables'
                allocate(brightness_x(dimx,dimy), stat=status)
				
  ENDIF
  status 	= nf90_get_var(ncid,varid,brightness_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(brightness_x .EQ. fillval) brightness_x=fill_valuef
  brightness_temperature(i,:,:)=brightness_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_independent_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_bt_independent(i,:,:)=var_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_common_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_bt_common(i,:,:)=var_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'u_structured_'//trim(adjustl(chnr_str)),varid)
  status 	= nf90_get_var(ncid,varid,var_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(var_x .EQ. fillval) var_x=fill_valuef
  uncertainty_bt_structured(i,:,:)=var_x(l0:lmax,:)
  
  ENDDO 
  print*,'brightness temperature read'
  
  allocate(solzen_x(dimx,dimy), stat=status)
  allocate(satzen_x(dimx,dimy), stat=status)
  allocate(relazi_x(dimx,dimy), stat=status)
  allocate(time_x(dimx,dimy), stat=status)

  status 	= nf90_inq_varid(ncid, 'relative_azimuth_angle',varid)
  status 	= nf90_get_var(ncid,varid,relazi_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(relazi_x .EQ. fillval) relazi_x=fill_valuef
  relazi(:,:)   = relazi_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'satellite_zenith_angle',varid)
  status 	= nf90_get_var(ncid,varid,satzen_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(satzen_x .EQ. fillval) satzen_x=fill_valuef
  satzen(:,:)   = satzen_x(l0:lmax,:)
  
  status 	= nf90_inq_varid(ncid, 'solar_zenith_angle',varid)
  status 	= nf90_get_var(ncid,varid,solzen_x)
  status 	= nf90_get_att(ncid, varid, "_FillValue",fillval)
  WHERE(solzen_x .EQ. fillval) solzen_x=fill_valuef
  solzen(:,:)   = solzen_x(l0:lmax,:)
  
  
	scatangle(:,:)		= (ACOS((SIN((satzen(:,:)*PI)/180.) * &
	SIN((solzen(:,:)*PI)/180.)*COS((relazi(:,:)*PI)/180.) + &
	COS((satzen(:,:)*PI)/180.)*COS((solzen(:,:)*PI)/180.))) * &
	180.)/PI
	scatangle(:,:)		= 	180.-scatangle(:,:)  
  print*,'angles read'

  print*,'read time'
  status 	= nf90_inq_varid(ncid, 'Time',varid)
  status 	= nf90_get_var(ncid,varid,time_x)
  time(:,:)     = time_x(l0:lmax,:)

  print*,'read auxiliary variables'
  
  status 	= nf90_inq_varid(ncid, 'lookup_table_radiance',varid)
  status 	= nf90_get_var(ncid,varid,radval)
  print*,MINVAL(radval),maxval(radval)
  print*,radval(:,1)
  print*,radval(:,1500)

  status 	= nf90_inq_varid(ncid, 'cross_line_correlation_coefficients',varid)
  status 	= nf90_get_var(ncid,varid,linecorrel)
  print*,MINVAL(linecorrel),maxval(linecorrel)
  print*,linecorrel(:,1)
  print*,linecorrel(:,201)

!----------------------------------------------------  
  status 	= nf90_close(ncid)
  
  deallocate(var_x)
  deallocate(brightness_x)
  deallocate(reflectance_x)
  deallocate(solzen_x)
  deallocate(satzen_x)
  deallocate(relazi_x)
  deallocate(time_x)
  deallocate(latitude_x)
  deallocate(longitude_x)
  
  RETURN
 END SUBROUTINE read_orbit
  
END MODULE fiduceo_avhrr_read
