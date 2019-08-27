  SUBROUTINE fiduceo_write_netcdf()
!*********************************************************
!=====#################################################################
! NAME:
!		fiduceo_avhrr_aod.f90
! PURPOSE:
!		write aod output (1 km grid)
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		adapted by Thomas Popp, 14.01.2019
!               from code written by Miriam Kosmale, 20.11.2018 (SeaWIFS4MERIS)
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
  
  INTEGER, parameter	:: NDIMS1 	= 1
  INTEGER, parameter	:: NDIMS2 	= 2
  INTEGER, parameter	:: NDIMS3	= 3
  INTEGER, parameter	:: NDIMS4	= 4
  INTEGER, parameter	:: NDIMS5	= 4
  INTEGER 				:: ncid
  INTEGER 				:: dimids1(NDIMS1), dimids2(NDIMS2)
  INTEGER 				:: dimids3(NDIMS3),dimids3b(NDIMS3)
  INTEGER 				:: dimids5(NDIMS5),dimids4(NDIMS4)
  INTEGER 				:: x_dimid, y_dimid, ch_dimid
  INTEGER 				:: aod_dimid, mix_dimid

  INTEGER 				:: status
  INTEGER 				:: varid(100),i
  
  LOGICAL 				:: dir_e
  INTEGER				:: nxdims_pm,nydims_pm,vardim(2),vardim2(3),n_mix,norb,norb2,nsens

  CHARACTER (LEN=200)                   :: aodfile,orbstring,orbstring2,sensstring
  CHARACTER (LEN=68)                    :: desctext
  CHARACTER (LEN=100)                   :: litext1
  CHARACTER (LEN=87)                    :: litext2
  CHARACTER (LEN=25)                    :: litext3

  REAL                                  :: var_out(dimx2,dimy)


	INTEGER(KIND=p_long), parameter						:: fill_valuei = -999
	INTEGER(KIND=p_long), parameter						:: fill_valuei2 = 0
	REAL (KIND=p_real), parameter						:: fill_valuef = -999.
	
	vardim	= (/size(longitude,1),size(longitude,2)/)
	nxdims_pm		= vardim(1)
	nydims_pm		= vardim(2)

	vardim2	= (/size(aod,1),size(aod,2),size(aod,3)/)
	n_mix		        = vardim2(1)

  orbstring=lv1file(31:44)  
  norb=lnblnk(orbstring)

  orbstring2=lv1file(31:59)  
  norb2=lnblnk(orbstring2)

  sensstring=lv1file(18:29)  
  nsens=lnblnk(sensstring)

  aodfile=trim(adjustl(lv1path))//'FIDUCEO_CDR_AOD_'//sensstring(1:nsens)//'_'//orbstring2(1:norb2)//'_L2A_Europe_v1.0.0_fv1.0.0.nc'
  
  inquire(file=trim(aodfile),exist=dir_e)
  if (dir_e) then
    print*,'AOD exists already; removing it first'
    CALL system('rm '//trim(aodfile))
  endif
  
  print *,'Write file...'
  print*,'#'//trim(aodfile)//'#'
!----------------------------------------------------
! write data
!----------------------------------------------------
 
  status = nf_create(trim(aodfile), NF90_NETCDF4, ncid)

  status = nf_def_dim(ncid, "x", nxdims_pm, x_dimid)
  status = nf_def_dim(ncid, "y", nydims_pm, y_dimid)
  status = nf_def_dim(ncid, "mix", n_mix, mix_dimid)

  dimids2 =  (/ x_dimid, y_dimid /)
  dimids3 =  (/ mix_dimid, x_dimid, y_dimid /)

  status = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'institution', 'DLR-DFD')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'source', lv1file(1:80)//'.nc')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'title', 'AOD retrieval')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'history', '')
  litext1='This dataset is released for use under CC-BY licence (https://creativecommons.org/licenses/by/4.0/) and was developed '
  litext2='in the EC FIDUCEO project Fidelity and Uncertainty in Climate Data Records from Earth Observations. '
  litext3='Grant Agreement: 638822.'
  status = nf90_put_att(ncid, NF90_GLOBAL, 'license', litext1//litext2//litext3)
  status = nf90_put_att(ncid, NF90_GLOBAL, 'auxiliary_data', 'RTE look-up tables, MAC-v1 aerosol type climatology (Kinne 2019)')

  i=1
  status = nf_def_var(ncid, "latitude", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 15, 'degree north')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), '_CoordinateAxisType', 3, 'lat')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-90.,90.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',8 , 'latitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',8 , 'latitude')
  i=2
  status = nf_def_var(ncid, "longitude", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 15, 'degree east')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), '_CoordinateAxisType', 3, 'lon')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-180.,180.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',9 , 'longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',9 , 'longitude')
  i=3
  status = nf_def_var(ncid, "RTOA_Ch1", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',25 , 'TOA reflectance at 630 nm')
  i=4
  status = nf_def_var(ncid, "albedo", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',36 , 'surface directional albedo at 670 nm')
  i=5
  status = nf_def_var(ncid, "ndvi", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=6
  status = nf_def_var(ncid, "ndii", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=7
  status = nf_def_var(ncid, "AOD", NF_FLOAT, NDIMS3,dimids3, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',31 , 'aerosol optical depth at 550 nm')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=8
  status = nf_def_var(ncid, "AOD_UNCERTAINTY1_indep", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',56 , 'independent AOD550 uncertainty due to reflectance effect')
  i=9
  status = nf_def_var(ncid, "AOD_UNCERTAINTY1_struct", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',55 , 'structured AOD550 uncertainty due to reflectance effect')
  i=10
  status = nf_def_var(ncid, "AOD_UNCERTAINTY1_common", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',51 , 'common AOD550 uncertainty due to reflectance effect')
  i=11
  status = nf_def_var(ncid, "AOD_UNCERTAINTY2_indep", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',51 , 'independent AOD550 uncertainty due to albedo effect')
  i=12
  status = nf_def_var(ncid, "AOD_UNCERTAINTY2_struct", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',50 , 'structured AOD550 uncertainty due to albedo effect')
  i=13
  status = nf_def_var(ncid, "AOD_UNCERTAINTY2_comm", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',46 , 'common AOD550 uncertainty due to albedo effect')
  i=14
  status = nf_def_var(ncid, "AOD_UNCERTAINTY3", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',54 , 'common AOD550 uncertainty due to aerosol type ensemble')
  i=15
  status = nf_def_var(ncid, "AOD_CLIM", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',51 , 'aerosol optical depth at 550 nm for climatology mix')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=16
  status = nf_def_var(ncid, "AOD1", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',53 , 'AOD at 550 nm for clim mix / strict cloud filter (5%)')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=17
  status = nf_def_var(ncid, "AOD2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',52 , 'AOD at 550 nm for clim mix / weak cloud filter (50%)')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=18
  status = nf_def_var(ncid, "PCLD", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '%')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,100])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',17 , 'cloud probability')
  i=19
  status = nf_def_var(ncid, "solar_zenith_angle", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 6, 'degree')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',19 , 'solar zenith angle')
  i=20
  status = nf_def_var(ncid, "satellite_zenith_angle", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 6, 'degree')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',24 , 'satellite zenith angle')
  i=21
  status = nf_def_var(ncid, "relative_azimuth_angle", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 6, 'degree')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',22 , 'relative azimuth angle')
  i=22
  status = nf_def_var(ncid, "scattering_angle", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 6, 'degree')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',16 , 'scattering angle')
  i=23
  status = nf_def_var(ncid, "dark_field", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,3])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'description', 50,'0=no, 1=best, 2=uncertain cloudiness,3=no B-factor')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=24
  status = nf_def_var(ncid, "climatology_mix", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,130])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=25
  status = nf_def_var(ncid, "B-factor", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,10.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=26
  status = nf_def_var(ncid, "reflectance3.7", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=27
  status = nf_def_var(ncid, "alb_u_i", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=28
  status = nf_def_var(ncid, "alb_u_c", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=29
  status = nf_def_var(ncid, "alb_u_s", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=30
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_indep", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',63 , 'independent AOD550 uncertainty due to albedo+reflectance effect')
  i=31
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_common", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',58 , 'common AOD550 uncertainty due to albedo+reflectance effect')
  i=32
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_struct", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',62 , 'structured AOD550 uncertainty due to albedo+reflectance effect')
  i=33
  status = nf_def_var(ncid, "AOD_UNCERTAINTY", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',18 , 'AOD550 uncertainty')
  i=34
  status = nf_def_var(ncid, "RTOA_Ch2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',25 , 'TOA reflectance at 870 nm')  
  i=35
  status = nf_def_var(ncid, "ndvikorr", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=36
  status = nf_def_var(ncid, "ndiikorr", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=37
  status = nf_def_var(ncid, "B-factor2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,10.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=38
  status = nf_def_var(ncid, "B-factor3", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,10.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=39
  status = nf_def_var(ncid, "reflectance3.7ptp", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=40
  status = nf_def_var(ncid, "ref1korr", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=41
  status = nf_def_var(ncid, "ref2korr", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=42
  status = nf_def_var(ncid, "B-factor-ratio", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,10.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=43
  status = nf_def_var(ncid, "ref1ptp", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')  
  i=44
  status = nf_def_var(ncid, "ref2ptp", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')  
  i=45
  status = nf_def_var(ncid, "ndviptp", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=46
  status = nf_def_var(ncid, "ndiiptp", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=47
  status = nf_def_var(ncid, "ndvidiff", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=48
  status = nf_def_var(ncid, "ndiidiff", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=49
  status = nf_def_var(ncid, "B-factor-ratio2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,10.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=50
  desctext='0=no, 121/2: low, 131/2: small, 151/2: high, 171/2: n/a, 251/2: good'
  status = nf_def_var(ncid, "dark_field_used1", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,255])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'description', 68,desctext)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=51
  status = nf_def_var(ncid, "dark_field_used4", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,255])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'description', 68,desctext)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=52
  status = nf_def_var(ncid, "dark_field_used16", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,255])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'description', 68,desctext)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=53
  status = nf_def_var(ncid, "dark_field_used20", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,255])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'description', 68,desctext)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')

  i=54
  status = nf_def_var(ncid, "AOD_ENS", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',49 , 'aerosol optical depth at 550 nm for ensemble mean')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=55
  status = nf_def_var(ncid, "AOD_ENSB", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',58 , 'aerosol optical depth at 550 nm for ensemble weighted mean')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=56
  status = nf_def_var(ncid, "AOD_UNCERTAINTY3B", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',70 , 'common AOD550 uncertainty due to type ensemble weightsum normalized')

  i=57
  status = nf_def_var(ncid, "TIME", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, 's')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',53 , 'Acquisition time in seconds since 1970-01-01 00:00:00')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',4 , 'time')

  i=58
  status = nf_def_var(ncid, "ref3_u_indep", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',30 , 'independent ref3.7 uncertainty')
  i=59
  status = nf_def_var(ncid, "ref3_u_struct", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',29 , 'structured ref3.7 uncertainty')
  i=60
  status = nf_def_var(ncid, "ref3_u_common", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',25 , 'common ref3.7 uncertainty')

  i=61
  status = nf_def_var(ncid, "AOD_UNCERTAINTY1", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',44 , 'AOD550 uncertainty due to reflectance effect')
  i=62
  status = nf_def_var(ncid, "AOD_UNCERTAINTY2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',39 , 'AOD550 uncertainty due to albedo effect')

  i=63
  status = nf_def_var(ncid, "ndvikorr2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=64
  status = nf_def_var(ncid, "ndiikorr2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=65
  status = nf_def_var(ncid, "ref1korr2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=66
  status = nf_def_var(ncid, "ref2korr2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=67
  status = nf_def_var(ncid, "albedo2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',52 , 'surface directional albedo at 670nm after iteration')
  
  i=68
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_SEL", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',27 , 'AOD550 uncertainty selected')
  i=69
  status = nf_def_var(ncid, "AOD_ENSB_SEL", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',67 , 'aerosol optical depth at 550 nm for ensemble weighted mean selected')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')

! End define mode. This tells netCDF we are done defining metadata.
  status = nf_enddef(ncid) 

! write the varaibles to file
  
  i=1
  status = nf_put_var(ncid, varid(i), latitude) 
  i=2
  status = nf_put_var(ncid, varid(i), longitude) 
  i=3
  var_out=reflectance(1,:,:)
  status = nf_put_var(ncid, varid(i),  var_out)
  i=4
  status = nf_put_var(ncid, varid(i), alb) 
  i=5
  status = nf_put_var(ncid, varid(i), ndvi) 
  i=6
  status = nf_put_var(ncid, varid(i), ndii) 
  i=7
  status = nf_put_var(ncid, varid(i), aod(:,:,:)) 
  i=8
  status = nf_put_var(ncid, varid(i), aod_u1i(:,:)) 
  i=9
  status = nf_put_var(ncid, varid(i), aod_u1s(:,:)) 
  i=10
  status = nf_put_var(ncid, varid(i), aod_u1c(:,:)) 
  i=11
  status = nf_put_var(ncid, varid(i), aod_u2i(:,:)) 
  i=12
  status = nf_put_var(ncid, varid(i), aod_u2s(:,:)) 
  i=13
  status = nf_put_var(ncid, varid(i), aod_u2c(:,:)) 
  i=14
  status = nf_put_var(ncid, varid(i), aod_u3(:,:)) 
  i=15
  status = nf_put_var(ncid, varid(i), aodclim(:,:)) 
  i=16
  status = nf_put_var(ncid, varid(i), aod1(:,:)) 
  i=17
  status = nf_put_var(ncid, varid(i), aod2(:,:)) 
  i=18
  status = nf_put_var(ncid, varid(i), pcld(:,:)) 
  i=19
  status = nf_put_var(ncid, varid(i), solzen(:,:)) 
  i=20
  status = nf_put_var(ncid, varid(i), satzen(:,:)) 
  i=21
  status = nf_put_var(ncid, varid(i), relazi(:,:)) 
  i=22
  status = nf_put_var(ncid, varid(i), scatangle(:,:)) 
  i=23
  status = nf_put_var(ncid, varid(i), ptp(:,:)) 
  i=24
  status = nf_put_var(ncid, varid(i), mixsel(:,:)) 
  i=25
  status = nf_put_var(ncid, varid(i), bfak(:,:)) 
  i=26
  status = nf_put_var(ncid, varid(i), ref3(:,:)) 
  i=27
  status = nf_put_var(ncid, varid(i), albval_u_i)   
  i=28
  status = nf_put_var(ncid, varid(i), albval_u_c)   
  i=29
  status = nf_put_var(ncid, varid(i), albval_u_s)
  i=30
  status = nf_put_var(ncid, varid(i), aod_ui(:,:))    
  i=31
  status = nf_put_var(ncid, varid(i), aod_uc(:,:))    
  i=32
  status = nf_put_var(ncid, varid(i), aod_us(:,:))    
  i=33
  status = nf_put_var(ncid, varid(i), aod_u(:,:))    

  i=34
  var_out=reflectance(2,:,:)
  status = nf_put_var(ncid, varid(i),  var_out)
  i=35
  status = nf_put_var(ncid, varid(i), ndvikorr) 
  i=36
  status = nf_put_var(ncid, varid(i), ndiikorr) 
  i=37
  status = nf_put_var(ncid, varid(i), bfak2(:,:)) 
  i=38
  status = nf_put_var(ncid, varid(i), bfak3(:,:)) 
  i=39
  status = nf_put_var(ncid, varid(i), ref37p(:,:)) 
  i=40
  status = nf_put_var(ncid, varid(i), ref1korr(:,:)) 
  i=41
  status = nf_put_var(ncid, varid(i), ref2korr(:,:)) 
  i=42
  status = nf_put_var(ncid, varid(i), bfakratio(:,:)) 
  i=43
  status = nf_put_var(ncid, varid(i),  ref1p(:,:))
  i=44
  status = nf_put_var(ncid, varid(i),  ref2p(:,:))
  i=45
  status = nf_put_var(ncid, varid(i), ndvip) 
  i=46
  status = nf_put_var(ncid, varid(i), ndiip) 
  i=47
  status = nf_put_var(ncid, varid(i), ndvidiff) 
  i=48
  status = nf_put_var(ncid, varid(i), ndiidiff) 
  i=49
  status = nf_put_var(ncid, varid(i), bfakratio2(:,:)) 
  i=50
  status = nf_put_var(ncid, varid(i), ptp2(1,:,:)) 
  i=51
  status = nf_put_var(ncid, varid(i), ptp2(4,:,:)) 
  i=52
  status = nf_put_var(ncid, varid(i), ptp2(16,:,:)) 
  i=53
  status = nf_put_var(ncid, varid(i), ptp2(20,:,:)) 

  i=54
  status = nf_put_var(ncid, varid(i), aodens(:,:)) 
  i=55
  status = nf_put_var(ncid, varid(i), aodensb(:,:)) 
  i=56
  status = nf_put_var(ncid, varid(i), aod_u3b(:,:)) 

  i=57
  status = nf_put_var(ncid, varid(i), time(:,:)) 

  i=58
  status = nf_put_var(ncid, varid(i), ref3_u_i(:,:)) 
  i=59
  status = nf_put_var(ncid, varid(i), ref3_u_s(:,:)) 
  i=60
  status = nf_put_var(ncid, varid(i), ref3_u_c(:,:)) 

  i=61
  status = nf_put_var(ncid, varid(i), aod_u1(:,:))    
  i=62
  status = nf_put_var(ncid, varid(i), aod_u2(:,:))   

  i=63
  status = nf_put_var(ncid, varid(i), ndvikorr2) 
  i=64
  status = nf_put_var(ncid, varid(i), ndiikorr2)  
  i=65
  status = nf_put_var(ncid, varid(i), ref1korr2(:,:)) 
  i=66
  status = nf_put_var(ncid, varid(i), ref2korr2(:,:)) 
  i=67
  status = nf_put_var(ncid, varid(i), alb2) 
  
  i=68
  status = nf_put_var(ncid, varid(i), aod_u_sel(:,:))    
  i=69
  status = nf_put_var(ncid, varid(i), aodensbsel(:,:)) 
  
  print*,size(latitude,1),size(latitude,2)
 
! Close the file. This frees up any internal netCDF resources
! associated with the file, and flushes any buffers.
  status = nf_close(ncid) 
  IF (status .EQ. 0 ) print *, "*** SUCCESS writing file! "
  IF (status .NE. 0 ) print *, "*** Problem with writing file! ",status

  return	
 END SUBROUTINE fiduceo_write_netcdf
