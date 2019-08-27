  SUBROUTINE fiduceo_write_netcdf2()
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
  INTEGER 				:: x_dimid, y_dimid,ch_dimid
  INTEGER 				:: aod_dimid, mix_dimid

  INTEGER 				:: status
  INTEGER 				:: varid(100),i
  
  LOGICAL 				:: dir_e
  INTEGER				:: nxdims_pm,nydims_pm,vardim(2),vardim2(3),n_mix,norb,norb2,nsens

  CHARACTER (LEN=200)                   :: aodfile,orbstring,orbstring2,sensstring
  CHARACTER (LEN=100)                   :: litext1
  CHARACTER (LEN=87)                    :: litext2
  CHARACTER (LEN=25)                    :: litext3

  REAL                                  :: var_out_g(dimx_g,dimy_g)
  INTEGER                               :: var_g(dimx_g,dimy_g)

	INTEGER(KIND=p_long), parameter						:: fill_valuei = -999
	REAL (KIND=p_real), parameter						:: fill_valuef = -999.
	
	vardim	= (/size(longitude_g,1),size(longitude_g,2)/)
	nxdims_pm		= vardim(1)
	nydims_pm		= vardim(2)

	vardim2	= (/size(aod_g,1),size(aod_g,2),size(aod_g,3)/)
	n_mix		        = vardim2(1)

  orbstring=lv1file(31:44)
  norb=lnblnk(orbstring)

  orbstring2=lv1file(31:59)  
  norb2=lnblnk(orbstring2)

  sensstring=lv1file(18:29)  
  nsens=lnblnk(sensstring)

  aodfile=trim(adjustl(lv1path))//'FIDUCEO_CDR_AOD_'//sensstring(1:nsens)//'_'//orbstring2(1:norb2)//'_L2B_Europe_v1.0.0_fv1.0.0.nc'
  
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
  ! Always check the return code of every netCDF function call. In
  ! this example program, wrapping netCDF calls with "call check()"
  ! makes sure that any return which is not equal to nf90_noerr (0)
  ! will print a netCDF error message and exit.

  status = nf_create(trim(aodfile), NF90_NETCDF4, ncid)

  status = nf_def_dim(ncid, "x", nxdims_pm, x_dimid)
  status = nf_def_dim(ncid, "y", nydims_pm, y_dimid)
  status = nf_def_dim(ncid, "mix", n_mix, mix_dimid)

  dimids2 =  (/ x_dimid, y_dimid /)
  dimids3 =  (/ mix_dimid, x_dimid, y_dimid /)

  status = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'institution', 'DLR-DFD')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'source', lv1file(1:80)//'.nc')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'title', 'AOD retrieval 3x3 pixel average')
  status = nf90_put_att(ncid, NF90_GLOBAL, 'history', '')
  litext1='This dataset is released for use under CC-BY licence (https://creativecommons.org/licenses/by/4.0/) and was developed '
  litext2='in the EC FIDUCEO project Fidelity and Uncertainty in Climate Data Records from Earth Observations. '
  litext3='Grant Agreement: 638822.'
  status = nf90_put_att(ncid, NF90_GLOBAL, 'license', litext1//litext2)
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
  status = nf_def_var(ncid, "albedo", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [0.,1000.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',36 , 'surface directional albedo at 670 nm')
  i=4
  status = nf_def_var(ncid, "AOD", NF_FLOAT, NDIMS3,dimids3, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',31 , 'aerosol optical depth at 550 nm')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=5
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_indep", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',63 , 'independent AOD550 uncertainty due to albedo+reflectance effect')
   i=6
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_struct", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',62 , 'structured AOD550 uncertainty due to albedo+reflectance effect')
   i=7
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_common", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',58 , 'common AOD550 uncertainty due to albedo+reflectance effect')
  i=8
  status = nf_def_var(ncid, "AOD_UNCERTAINTY3", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',54 , 'common AOD550 uncertainty due to aerosol type ensemble')
  i=9
  status = nf_def_var(ncid, "AOD_UNCERTAINTY4", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',64 , 'indep AOD550 uncertainty due to cloud mask (probability 5% -50%)')
  i=10
  status = nf_def_var(ncid, "AOD_UNCERTAINTY", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',18 , 'AOD550 uncertainty')
  i=11
  status = nf_def_var(ncid, "AOD_CLIM", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',51 , 'aerosol optical depth at 550 nm for climatology mix')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=12
  status = nf_def_var(ncid, "AOD1", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',53 , 'AOD at 550 nm for clim mix / strict cloud filter (5%)')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=13
  status = nf_def_var(ncid, "AOD2", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',52 , 'AOD at 550 nm for clim mix / weak cloud filter (50%)')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=14
  status = nf_def_var(ncid, "NUMBER", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=15
  status = nf_def_var(ncid, "NUMBER0", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=16
  status = nf_def_var(ncid, "NUMBER2", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=17
  status = nf_def_var(ncid, "NUMBERc", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=18
  status = nf_def_var(ncid, "NUMBERl", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  i=19
  status = nf_def_var(ncid, "NUMBERm", NF_INT2, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_INT (ncid, varid(i), '_FillValue', NF_INT2,1, fill_valuei)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '#')
  status = NF_PUT_ATT_INT (ncid, varid(i), 'valid_range', NF_INT2,2, [0,9])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')

  i=20
  status = nf_def_var(ncid, "AOD_ENS", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',49 , 'aerosol optical depth at 550 nm for ensemble mean')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=21
  status = nf_def_var(ncid, "AOD_ENSB", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',58 , 'aerosol optical depth at 550 nm for ensemble weighted mean')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',51 , 'atmosphere_optical_thickness_due_to_ambient_aerosol')
  i=22
  status = nf_def_var(ncid, "AOD_UNCERTAINTY3B", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',65 , 'common AOD550 uncertainty due to type ensemble weightsum norm.')

  i=23
  status = nf_def_var(ncid, "TIME", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, 's')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',53 , 'Acquisition time in seconds since 1970-01-01 00:00:00')

  i=24
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_random", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',49 , 'AOD550 uncertainty assuming all components random')
  i=25
  status = nf_def_var(ncid, "AOD_UNCERTAINTY_correlated", NF_FLOAT, NDIMS2,dimids2, varid(i)) 
  status = NF_PUT_ATT_REAL (ncid, varid(i), '_FillValue', NF_FLOAT,1, fill_valuef)
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'Unit', 1, '1')
  status = NF_PUT_ATT_REAL (ncid, varid(i), 'valid_range', NF_FLOAT,2, [-1.,4.])
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'coordinates',18 , 'latitude longitude')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'long_name',59 , 'AOD550 uncertainty assuming all components fully correlated')
  status = NF_PUT_ATT_TEXT (ncid, varid(i), 'standard_name',4 , 'time')
  
! End define mode. This tells netCDF we are done defining metadata.
  status = nf_enddef(ncid) 

! Write the variables to file

  i=1
  status = nf_put_var(ncid, varid(i), latitude_g) 
  i=2
  status = nf_put_var(ncid, varid(i), longitude_g) 
  i=3
  status = nf_put_var(ncid, varid(i), alb_g) 
  i=4
  status = nf_put_var(ncid, varid(i), aod_g(:,:,:)) 
  i=5
  status = nf_put_var(ncid, varid(i), aod_ui_g(:,:)) 
  i=6
  status = nf_put_var(ncid, varid(i), aod_us_g(:,:)) 
  i=7
  status = nf_put_var(ncid, varid(i), aod_uc_g(:,:)) 
  i=8
  status = nf_put_var(ncid, varid(i), aod_u3_g(:,:)) 
  i=9
  status = nf_put_var(ncid, varid(i), aod_u4_g(:,:)) 
  i=10
  status = nf_put_var(ncid, varid(i), aod_u_g(:,:)) 
  i=11
  status = nf_put_var(ncid, varid(i), aodclim_g(:,:)) 
  i=12
  status = nf_put_var(ncid, varid(i), aod1_g(:,:)) 
  i=13
  status = nf_put_var(ncid, varid(i), aod2_g(:,:)) 
  i=14
  status = nf_put_var(ncid, varid(i), nensb_g(:,:)) 
  i=15
  status = nf_put_var(ncid, varid(i), n_g(:,:)) 
  i=16
  status = nf_put_var(ncid, varid(i), n2_g(:,:)) 
  i=17
  status = nf_put_var(ncid, varid(i), nclim_g(:,:)) 
  i=18
  status = nf_put_var(ncid, varid(i), nlat(:,:)) 
  i=19
  status = nf_put_var(ncid, varid(i), nmix_g(1,:,:)) 

  i=20
  status = nf_put_var(ncid, varid(i), aodens_g(:,:)) 
  i=21
  status = nf_put_var(ncid, varid(i), aodensb_g(:,:)) 
  i=22
  status = nf_put_var(ncid, varid(i), aod_u3b_g(:,:)) 

  i=23
  status = nf_put_var(ncid, varid(i), time_g(:,:)) 

  i=24
  status = nf_put_var(ncid, varid(i), aod_u_ran_g(:,:)) 
  i=25
  status = nf_put_var(ncid, varid(i), aod_u_corr_g(:,:)) 
  
  print*,size(latitude_g,1),size(latitude_g,2)
 
! Close the file. This frees up any internal netCDF resources
! associated with the file, and flushes any buffers.
!  call check( nf90_close(ncid) )
  status = nf_close(ncid) 
  IF (status .EQ. 0 ) print *, "*** SUCCESS writing file! "
  IF (status .NE. 0 ) print *, "*** Problem with writing file! ",status

  return	
 END SUBROUTINE fiduceo_write_netcdf2
