
ncdir='/home/scheiner1/hpopp/fiduceo/CDR/lv3/daily/AVHRR18_G/'
ncdir='/home/scheiner1/hpopp/fiduceo/CDR/lv3/daily/AVHRR16_G/'

outdir='/home/scheiner1/hpopp/fiduceo/CDR/lv3/monthly/'

nclen=strlen(ncdir)

platform='AVHRR18_G/
year0=2005
numyear=8
if ncdir eq '/home/scheiner1/hpopp/fiduceo/CDR/lv3/daily/AVHRR16_G/' then begin
  year0=2003
  numyear=3
  platform='AVHRR16_G/
end

for nyear=0,numyear-1 do begin
year=strmid(string(year0+nyear),4,4)

mon0=0
if ncdir eq '/home/scheiner1/hpopp/fiduceo/CDR/lv3/daily/AVHRR18_G/' and nyear eq 0 then mon0=5

for nmonth=mon0,11 do begin
month='0'+strmid(string(nmonth+1),7,1)
if nmonth ge 9 then month=strmid(string(nmonth+1),6,2)

print,year+month

weekcount=[8,8,8,7]
if month eq '02' then weekcount=[7,7,7,7]
if month eq '02' and year eq '2004' then weekcount=[8,7,7,7]
if month eq '02' and year eq '2008' then weekcount=[8,7,7,7]
if month eq '02' and year eq '2012' then weekcount=[8,7,7,7]
if month eq '04' or month eq '06' or month eq '09' or month eq '11' then weekcount=[8,7,8,7]

nummonth=31
if month eq '02' then nummonth=28
if month eq '02' and year eq '2004' then nummonth=29
if month eq '02' and year eq '2008' then nummonth=29
if month eq '02' and year eq '2012' then nummonth=29
if month eq '04' or month eq '06' or month eq '09' or month eq '11' then nummonth=30

print,'weekcount',weekcount

text=''
db=!pi/180.
grid=1
nmix=36

uthresh=0.15
uthresh2=0.10
fthresh=0.2

minpixnum=2

print,'minpixnum: ',minpixnum

onefile=0

lon0=-10.
lon1=70
lat0=30.
lat1=75

nlat=fix(lat1-lat0)+1
nlon=fix(lon1-lon0)+1

latitude_g=fltarr(nlat)
longitude_g=fltarr(nlon)
lat_bnds=fltarr(nlat,2)
lon_bnds=fltarr(nlon,2)

for i=0,nlat-1 do latitude_g(i) = lat0 + 0.5 + i
for i=0,nlat-1 do lat_bnds(i,0) = lat0 + i
for i=0,nlat-1 do lat_bnds(i,1) = lat0 + 1. + i
for i=0,nlon-1 do longitude_g(i) = lon0 + 0.5 + i
for i=0,nlon-1 do lon_bnds(i,0) = lon0 + i
for i=0,nlon-1 do lon_bnds(i,1) = lon0 + 1. + i

num0_g=intarr(nlon,nlat)
num1_g=intarr(nlon,nlat)
num_g=intarr(nlon,nlat)
num2_g=intarr(nlon,nlat,nmix)
num3_g=intarr(nlon,nlat)
num4_g=intarr(nlon,nlat)
numday_g=intarr(nlon,nlat)

timemin_g=fltarr(nlon,nlat)
for i=0,nlon-1 do for j=0,nlat-1 do timemin_g(i,j)=2000000000.
timemax_g=fltarr(nlon,nlat)

aodclim_g=fltarr(nlon,nlat)
aodensb_g=fltarr(nlon,nlat)
aodensbday_g=fltarr(31,nlon,nlat)
aod_g=fltarr(nlon,nlat,nmix)

albedo_g=fltarr(nlon,nlat)

aod_uc_g=fltarr(nlon,nlat)
aod_uc12_g=fltarr(nlon,nlat)
aod_uc3b_g=fltarr(nlon,nlat)
aod_uc3b_gw=fltarr(4,nlon,nlat)
aod_ui_g=fltarr(nlon,nlat)
aod_us_g=fltarr(nlon,nlat)
aod_u_g=fltarr(nlon,nlat)

aod_u_random_g=fltarr(nlon,nlat)
aod_u_correl_g=fltarr(nlon,nlat)
aod_u_sampling_g=fltarr(nlon,nlat)
aod_u_samplingday_g=fltarr(nlon,nlat)
aod_min_g=fltarr(nlon,nlat)
aod_max_g=fltarr(nlon,nlat)
aod_var_g=fltarr(nlon,nlat)

for i=0,nlon-1 do for j=0,nlat-1 do aod_min_g(i,j)=999.

aodbest_g=fltarr(nlon,nlat)
aodbest2_g=fltarr(nlon,nlat)

spawn,'ls '+ncdir+year+'/'+month+'/*L3*.nc > '+ncdir+'lists/list'+year+month+'.txt'
print,'ls '+ncdir+year+'/'+month+'/*L3*.nc > '+ncdir+'lists/list'+year+month+'.txt'

on_ioerror,endlist

openr,1,ncdir+'lists/list'+year+month+'.txt'

kfile=0

for nfile = 1, 31 do begin

  week=0
  if nfile gt weekcount(0) then week=1
  if nfile gt weekcount(1) then week=2
  if nfile gt weekcount(2) then week=3

  readf,1,text

  if nfile ne 16 and onefile eq 1 then goto,nextfile

  kfile=kfile+1

  tlen=strlen(text)
  file1=strmid(text,nclen,tlen-nclen+1)
  print,file1,week
  if nfile eq 1 then sourcestring=file1
  if nfile eq 16 and onefile eq 1 then sourcestring=file1
  if nfile gt 1 then sourcestring=sourcestring+','+file1

  zeilen=0
  FileID=0
  refnum=0

  netcdf_name=ncdir+file1
  print,netcdf_name
  FileID = ncdf_OPEN(netcdf_name)
  file_handle=FileID
  netcdf_handle=FileID
  fileinq_struct = ncdf_inquire(FileID)
  nvars = fileinq_struct.nvars
  print, 'nvars:', nvars
  variable_name = "AODCLIM"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aodclim
  variable_name = "AODENSB"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aodensb
  dims = size(aodclim,/dimensions)
  zeilen = dims
  print, dims
  
  variable_name = "albedo"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, albedo

  variable_name = "AOD_UNCERTAINTY"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u

  variable_name = "AOD_UNCERTAINTY_random"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u_ran
  variable_name = "AOD_UNCERTAINTY_correlated"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u_corr

  variable_name = "AOD_UNCERTAINTY_struct"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_us
  variable_name = "AOD_UNCERTAINTY_common"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_uc
  variable_name = "AOD_UNCERTAINTY_indep"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_ui

  variable_name = "AOD_UNCERTAINTY_common_12"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_uc_12
  variable_name = "AOD_UNCERTAINTY_common_3b"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_uc_3b
  variable_name = "AOD_UNCERTAINTY_sampling"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u_s

  variable_name = "AODBEST"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_best
  
  print,'aodbest',max(aod_best)
  variable_name = "AODBEST2"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_best2

  variable_name = "NUMBER"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, number
  variable_name = "NUMBER0"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, number0
  variable_name = "NUMBER1"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, number1

  variable_name = "AOD"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod
  
  number=transpose(number)
  number0=transpose(number0)
  number1=transpose(number1)
  aodensb=transpose(aodensb)
  aodclim=transpose(aodclim)
  aod_best=transpose(aod_best)
  aod_best2=transpose(aod_best2)
  albedo=transpose(albedo)
  aod=transpose(aod)
  aod_u=transpose(aod_u)
  aod_us=transpose(aod_us)
  aod_uc=transpose(aod_uc)
  aod_ui=transpose(aod_ui)
  aod_uc_12=transpose(aod_uc_12)
  aod_uc_3b=transpose(aod_uc_3b)
  aod_u_s=transpose(aod_u_s)
  aod_u_ran=transpose(aod_u_ran)
  aod_u_corr=transpose(aod_u_corr)
  
  ncdf_CLOSE,FileID

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    
	aodensbday_g(nfile-1,indlon,indlat) = aodensb(indlon,indlat)
	
    if abs(aodensb(indlon,indlat)) ne 999 then begin
	  numday_g(indlon,indlat)=numday_g(indlon,indlat)+1
	  num_g(indlon,indlat)=num_g(indlon,indlat)+number(indlon,indlat)
	  num0_g(indlon,indlat)=num0_g(indlon,indlat)+number0(indlon,indlat)
	  num1_g(indlon,indlat)=num1_g(indlon,indlat)+number1(indlon,indlat)
	  
      aodensb_g(indlon,indlat)=aodensb_g(indlon,indlat)+aodensb(indlon,indlat)
      albedo_g(indlon,indlat)=albedo_g(indlon,indlat)+albedo(indlon,indlat)
      aodclim_g(indlon,indlat)=aodclim_g(indlon,indlat)+aodclim(indlon,indlat)

      aod_uc_g(indlon,indlat)=aod_uc_g(indlon,indlat)+aod_uc(indlon,indlat)
      aod_uc12_g(indlon,indlat)=aod_uc12_g(indlon,indlat)+aod_uc_12(indlon,indlat)
      aod_u_correl_g(indlon,indlat)=aod_u_correl_g(indlon,indlat)+aod_u_corr(indlon,indlat)
	  
	  aod_uc3b_gw(week,indlon,indlat)=aod_uc3b_gw(week,indlon,indlat)+aod_uc_3b(indlon,indlat)

	  aod_ui_g(indlon,indlat)=aod_ui_g(indlon,indlat)+aod_ui(indlon,indlat)*aod_ui(indlon,indlat)
	  aod_us_g(indlon,indlat)=aod_us_g(indlon,indlat)+aod_us(indlon,indlat)*aod_us(indlon,indlat)
	  aod_u_random_g(indlon,indlat)=aod_u_random_g(indlon,indlat)+aod_u_ran(indlon,indlat)*aod_u_ran(indlon,indlat)
	  aod_u_sampling_g(indlon,indlat)=aod_u_sampling_g(indlon,indlat)+aod_u_s(indlon,indlat)*aod_u_s(indlon,indlat)
	  
	  if aodensb(indlon,indlat) lt aod_min_g(indlon,indlat) then aod_min_g(indlon,indlat)=aodensb(indlon,indlat)
      if aodensb(indlon,indlat) gt aod_max_g(indlon,indlat) then aod_max_g(indlon,indlat)=aodensb(indlon,indlat)

    end 

    if (abs(aod_best(indlon,indlat)) ne 999.) then begin
	  num3_g(indlon,indlat)=num3_g(indlon,indlat)+1
	  aodbest_g(indlon,indlat) = aodbest_g(indlon,indlat) + aod_best(indlon,indlat)
	endif
	
    if (abs(aod_best2(indlon,indlat)) ne 999.) then begin	
   	  num4_g(indlon,indlat)=num4_g(indlon,indlat)+1
	  aodbest2_g(indlon,indlat) = aodbest2_g(indlon,indlat) + aod_best2(indlon,indlat)
	endif

    for k=1,nmix-1 do begin
	  if (abs(aod_g(indlon,indlat,k)) ne 999.) then begin
	    num2_g(indlon,indlat,k)=num2_g(indlon,indlat,k)+1
        aod_g(indlon,indlat,k)=aod_g(indlon,indlat,k)+aod(indlon,indlat,k)
      end
    end

  end
end

nextfile:

end

endlist:

close,1

print,'num3',max(num3_g)
print,'aodbest',max(aodbest_g)

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    
    if numday_g(indlon,indlat) gt 0 then begin
      aodensb_g(indlon,indlat)=aodensb_g(indlon,indlat)/numday_g(indlon,indlat)
      albedo_g(indlon,indlat)=albedo_g(indlon,indlat)/numday_g(indlon,indlat)
      aodclim_g(indlon,indlat)=aodclim_g(indlon,indlat)/numday_g(indlon,indlat)

      aod_uc_g(indlon,indlat)=aod_uc_g(indlon,indlat)/num_g(indlon,indlat)
      aod_uc12_g(indlon,indlat)=aod_uc12_g(indlon,indlat)/num_g(indlon,indlat)

      aod_u_correl_g(indlon,indlat)=aod_u_correl_g(indlon,indlat)/num_g(indlon,indlat)
	  
	  scount=0
	  if aod_uc3b_gw(0,indlon,indlat) ne 0. then scount=scount+1
	  if aod_uc3b_gw(1,indlon,indlat) ne 0. then scount=scount+1
      if aod_uc3b_gw(2,indlon,indlat) ne 0. then scount=scount+1
	  if aod_uc3b_gw(3,indlon,indlat) ne 0. then scount=scount+1
	  val1=aod_uc3b_gw(0,indlon,indlat)*aod_uc3b_gw(0,indlon,indlat)
	  val2=aod_uc3b_gw(1,indlon,indlat)*aod_uc3b_gw(1,indlon,indlat)
	  val3=aod_uc3b_gw(2,indlon,indlat)*aod_uc3b_gw(2,indlon,indlat)
	  val4=aod_uc3b_gw(3,indlon,indlat)*aod_uc3b_gw(3,indlon,indlat)
	  if scount gt 0 then aod_uc3b_g(indlon,indlat)=sqrt(val1+val2+val3+val4)/scount
	  if scount eq 0 then aod_uc3b_g(indlon,indlat)=-999.
    end else begin
      aodensb_g(indlon,indlat)=-999.
      albedo_g(indlon,indlat)=-999.
      aodclim_g(indlon,indlat)=-999.

      aod_uc_g(indlon,indlat)=-999.
      aod_uc12_g(indlon,indlat)=-999.
      aod_uc3b_g(indlon,indlat)=-999.
      aod_u_correl_g(indlon,indlat)=-999.
    endelse

    if numday_g(indlon,indlat) gt 0 then begin
      scount=numday_g(indlon,indlat)
      aod_ui_g(indlon,indlat)=sqrt(aod_ui_g(indlon,indlat))/scount
      aod_us_g(indlon,indlat)=sqrt(aod_us_g(indlon,indlat))/scount
      aod_u_random_g(indlon,indlat)=sqrt(aod_u_random_g(indlon,indlat))/scount
    end else begin
      aod_ui_g(indlon,indlat)=-999.
      aod_us_g(indlon,indlat)=-999.
      aod_u_random_g(indlon,indlat)=-999.
    endelse

    if num3_g(indlon,indlat) gt 0 then begin
      aodbest_g(indlon,indlat) = aodbest_g(indlon,indlat) / num3_g(indlon,indlat)
    end else begin
      aodbest_g(indlon,indlat) = -999.
    endelse

    if num4_g(indlon,indlat) gt 0 then begin
      aodbest2_g(indlon,indlat) = aodbest2_g(indlon,indlat) / num4_g(indlon,indlat)
    end else begin
      aodbest2_g(indlon,indlat) = -999.
    endelse

    for k=1,nmix-1 do begin
      if num2_g(indlon,indlat,k) gt 0 then begin
        aod_g(indlon,indlat,k)=aod_g(indlon,indlat,k)/num2_g(indlon,indlat,k)
      end else begin
        aod_g(indlon,indlat,k) = -999.
      endelse
    end

    sq1=aod_ui_g(indlon,indlat)*aod_ui_g(indlon,indlat)
    sq2=aod_us_g(indlon,indlat)*aod_us_g(indlon,indlat)
    sq3=aod_uc12_g(indlon,indlat)*aod_uc12_g(indlon,indlat)
	sq4=aod_uc3b_g(indlon,indlat)*aod_uc3b_g(indlon,indlat)
    aod_u_g(indlon,indlat)=sqrt(sq1+sq2+sq3+sq4)

  end
end

sigaod=fltarr(nlon,nlat)
numsig=intarr(nlon,nlat)

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    for kday=0,30 do begin
	  if aodensb_g(indlon,indlat) ne -999. then begin
        if aodensbday_g(kday,indlon,indlat) ne -999 then begin
		  numsig(indlon,indlat)=numsig(indlon,indlat)+1
          diff=aodensbday_g(kday,indlon,indlat)-aodensb_g(indlon,indlat)
          sigaod(indlon,indlat)=sigaod(indlon,indlat)+diff*diff
        endif
      endif
	end
  end
end

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    if numsig(indlon,indlat) gt 1 then begin
      sigaod(indlon,indlat)=sqrt(sigaod(indlon,indlat))/sqrt(float(numsig(indlon,indlat))*float((numsig(indlon,indlat)-1)))
    end else begin
      if numsig(indlon,indlat) eq 1 then sigaod(indlon,indlat)=sqrt(sigaod(indlon,indlat))
    endelse
  end
end

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    if numday_g(indlon,indlat) gt 0 then begin
       pixfrac=numday_g(indlon,indlat)/nummonth
       aod_u_samplingday_g(indlon,indlat)=0.5*(aod_max_g(indlon,indlat)-aod_min_g(indlon,indlat)) * (1. - pixfrac)
    end else begin
       aod_u_samplingday_g(indlon,indlat)=-999.
    endelse

    if aodensb_g(indlon,indlat) eq -999. then num_g(indlon,indlat)=-999
    if aodensb_g(indlon,indlat) eq -999. then num0_g(indlon,indlat)=-999
    if aodensb_g(indlon,indlat) eq -999. then num1_g(indlon,indlat)=-999

    if numday_g(indlon,indlat) lt minpixnum then begin
      aodensb_g(indlon,indlat)=-999.
      aodclim_g(indlon,indlat)=-999.
      albedo_g(indlon,indlat)=-999.
      aodbest_g(indlon,indlat)=-999.
      aodbest2_g(indlon,indlat)=-999.
      for k=0,nmix-1 do aod_g(indlon,indlat,k)=-999.
      numday_g(indlon,indlat)=-999
	  num_g(indlon,indlat)=-999
      num0_g(indlon,indlat)=-999
      num1_g(indlon,indlat)=-999
      aod_u_g(indlon,indlat)=-999.
      aod_uc_g(indlon,indlat)=-999.
      aod_uc12_g(indlon,indlat)=-999.
      aod_uc3b_g(indlon,indlat)=-999.
      aod_us_g(indlon,indlat)=-999.
      aod_ui_g(indlon,indlat)=-999.
      aod_u_correl_g(indlon,indlat)=-999.
      aod_u_random_g(indlon,indlat)=-999.
      aod_u_sampling_g(indlon,indlat)=-999.
    end
  end
end

nmix= indgen(36)

nerr=0

print,'writing L3 file'
on_ioerror,endncf

filename=outdir+platform+year+'/'+'FIDUCEO_CDR_AOD_AVHRR_N18ALL_'+year+month+'_L3_monthly_Europe_v1.0.0_fv1.0.0.nc'
print,filename

spawn,'rm '+filename

id   = NCDF_CREATE(filename, /netcdf4)
print, 'id:', id

NCDF_ATTPUT, id, /GLOBAL, 'Conventions', 'CF-1.6'
NCDF_ATTPUT, id, /GLOBAL, 'title', 'AVHRR Aerosol Retrieval'
NCDF_ATTPUT, id, /GLOBAL, 'institution', 'DLR-DFD'
NCDF_ATTPUT, id, /GLOBAL, 'source', sourcestring
NCDF_ATTPUT, id, /GLOBAL, 'version', '1.0'
NCDF_ATTPUT, id, /GLOBAL, 'auxiliary_data', 'none'
NCDF_ATTPUT, id, /GLOBAL, 'history', 'FIDUCEO AVHRR L1C ver.: v0.2Bet_fv2.0.0, FIDUCEO AVHRR AOD L2A/B ver.: 1.0.0_fv1.0.0, FIDUCEO AVHRR AOD L3_daily ver.: 1.0.0_fv1.0.0'

nerr=1

ny   = nlat
nx   = nlon
nv   = 2
nmix = 36

print,'vardef + attributes'

xid  = NCDF_DIMDEF(id, 'lon', nx)
yid  = NCDF_DIMDEF(id, 'lat', ny)
xvid  = NCDF_DIMDEF(id, 'lon_bnds', nv)
yvid  = NCDF_DIMDEF(id, 'lat_bnds', nv)
nmixid = NCDF_DIMDEF(id, 'nmix', nmix)

nerr=2

vid_aodensb  = NCDF_VARDEF(id, 'AODENSB', [yid, xid], /FLOAT)
vid_aodclim  = NCDF_VARDEF(id, 'AODCLIM', [yid, xid], /FLOAT)
vid_albedo  = NCDF_VARDEF(id, 'albedo', [yid, xid], /FLOAT)
vid_aodbest  = NCDF_VARDEF(id, 'AODBEST', [yid, xid], /FLOAT)
vid_aodbest2  = NCDF_VARDEF(id, 'AODBEST2', [yid, xid], /FLOAT)
vid_aod  = NCDF_VARDEF(id, 'AOD', [nmixid,yid, xid], /FLOAT)

nerr=3

vid_num  = NCDF_VARDEF(id, 'NUMBER', [yid, xid], /LONG)
vid_numday  = NCDF_VARDEF(id, 'NUMBERDAYs', [yid, xid], /LONG)
vid_num0  = NCDF_VARDEF(id, 'NUMBER0', [yid, xid], /LONG)
vid_num1  = NCDF_VARDEF(id, 'NUMBER1', [yid, xid], /LONG)

nerr=4

vid_aoduc  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_common', [yid, xid], /FLOAT)
vid_aoduc12  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_common_12', [yid, xid], /FLOAT)
vid_aoduc3b  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_common_3b', [yid, xid], /FLOAT)
vid_aodus  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_struct', [yid, xid], /FLOAT)
vid_aodui  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_indep', [yid, xid], /FLOAT)
vid_aoducorr  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_correlated', [yid, xid], /FLOAT)
vid_aoduran  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_random', [yid, xid], /FLOAT)
vid_aodu  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY', [yid, xid], /FLOAT)
vid_aodusam  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_sampling', [yid, xid], /FLOAT)
vid_aodusamday  = NCDF_VARDEF(id, 'AOD_UNCERTAINTY_sampling_days', [yid, xid], /FLOAT)

nerr=5

vid_lat = NCDF_VARDEF(id, 'lat', yid, /FLOAT)
vid_lon = NCDF_VARDEF(id, 'lon', xid, /FLOAT)
vid_latb = NCDF_VARDEF(id, 'lat_bnds', [yid,yvid], /FLOAT)
vid_lonb = NCDF_VARDEF(id, 'lon_bnds', [xid,xvid], /FLOAT)

NCDF_ATTPUT, id, vid_lat,   'units', 'degree north'
NCDF_ATTPUT, id, vid_lat,   '_FillValue', -999.
NCDF_ATTPUT, id, vid_lat,   'long_name', 'latitude'
NCDF_ATTPUT, id, vid_lat,   'standard_name', 'latitude'
NCDF_ATTPUT, id, vid_lat,   '_CoordinateAxisType', 'lat'
NCDF_ATTPUT, id, vid_lat,   'valid_range', [-90.,90.]
NCDF_ATTPUT, id, vid_lat,   'bounds', 'lat_bnds'

NCDF_ATTPUT, id, vid_lon,   'units', 'degree east'
NCDF_ATTPUT, id, vid_lon,   '_FillValue', -999.
NCDF_ATTPUT, id, vid_lon,   'standard_name', 'longitude'
NCDF_ATTPUT, id, vid_lon,   'long_name', 'longitude'
NCDF_ATTPUT, id, vid_lon,   '_CoordinateAxisType', 'lon'
NCDF_ATTPUT, id, vid_lon,   'valid_range', [-180.,180.]
NCDF_ATTPUT, id, vid_lon,   'bounds', 'lon_bnds'

nerr=6

NCDF_ATTPUT, id, vid_aodensb,   'units', '1'
NCDF_ATTPUT, id, vid_aodensb,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodensb,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodensb,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodensb,   'long_name', 'gridded aerosol optical depth at 550 nm for ensemble weighted mean'
NCDF_ATTPUT, id, vid_aodensb,   'standard_name', 'atmosphere_optical_thickness_due_to_ambient_aerosol'

NCDF_ATTPUT, id, vid_aodclim,   'units', '1'
NCDF_ATTPUT, id, vid_aodclim,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodclim,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodclim,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodclim,   'long_name', 'gridded aerosol optical depth at 550 nm for climatology mix'
NCDF_ATTPUT, id, vid_aodclim,   'standard_name', 'atmosphere_optical_thickness_due_to_ambient_aerosol'

NCDF_ATTPUT, id, vid_albedo,   'units', '1'
NCDF_ATTPUT, id, vid_albedo,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_albedo,   'valid_range', [0.,1000.]
NCDF_ATTPUT, id, vid_albedo,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_albedo,   'long_name', 'gridded surface directional albedo at 670 nm'

NCDF_ATTPUT, id, vid_aodbest,   'units', '1'
NCDF_ATTPUT, id, vid_aodbest,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodbest,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodbest,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodbest,   'long_name', 'gridded aerosol optical depth for AOD550_uncertainty < 0.15'
NCDF_ATTPUT, id, vid_aodbest,   'standard_name', 'atmosphere_optical_thickness_due_to_ambient_aerosol'

NCDF_ATTPUT, id, vid_aodbest2,   'units', '1'
NCDF_ATTPUT, id, vid_aodbest2,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodbest2,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodbest2,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodbest2,   'long_name', 'gridded AOD550 for uncertainty < 0.10 OR 20% of AOD550'
NCDF_ATTPUT, id, vid_aodbest2,   'standard_name', 'atmosphere_optical_thickness_due_to_ambient_aerosol'

NCDF_ATTPUT, id, vid_aod,   'units', '1'
NCDF_ATTPUT, id, vid_aod,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aod,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aod,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aod,   'long_name', 'aerosol optical depth at 550 nm'
NCDF_ATTPUT, id, vid_aod,   'standard_name', 'atmosphere_optical_thickness_due_to_ambient_aerosol'

nerr=7

NCDF_ATTPUT, id, vid_num,   'units', '#'
NCDF_ATTPUT, id, vid_num,   'valid_range', [0,15]
NCDF_ATTPUT, id, vid_num,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_num,   'long_name', 'orbit number in grid cell'

NCDF_ATTPUT, id, vid_num0,   'units', '#'
NCDF_ATTPUT, id, vid_num0,   'valid_range', [0,1000]
NCDF_ATTPUT, id, vid_num0,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_num0,   'long_name', 'available pixel number in grid cell'

NCDF_ATTPUT, id, vid_num1,   'units', '#'
NCDF_ATTPUT, id, vid_num1,   'valid_range', [0,1000]
NCDF_ATTPUT, id, vid_num1,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_num1,   'long_name', 'valid pixel number in grid cell'

NCDF_ATTPUT, id, vid_numday,   'units', '#'
NCDF_ATTPUT, id, vid_numday,   'valid_range', [0,31]
NCDF_ATTPUT, id, vid_numday,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_numday,   'long_name', 'number of days with valid daily AOD in grid cell'

nerr=8

NCDF_ATTPUT, id, vid_aodu,   'units', '1'
NCDF_ATTPUT, id, vid_aodu,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodu,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodu,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodu,   'long_name', 'gridded AOD550 uncertainty'

NCDF_ATTPUT, id, vid_aoduc,   'units', '1'
NCDF_ATTPUT, id, vid_aoduc,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aoduc,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aoduc,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aoduc,   'long_name', 'gridded common AOD550 uncertainty'

NCDF_ATTPUT, id, vid_aoduc12,   'units', '1'
NCDF_ATTPUT, id, vid_aoduc12,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aoduc12,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aoduc12,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aoduc12,   'long_name', 'gridded common AOD550 uncertainty due to reflectance and albedo effect'

NCDF_ATTPUT, id, vid_aoduc3b,   'units', '1'
NCDF_ATTPUT, id, vid_aoduc3b,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aoduc3b,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aoduc3b,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aoduc3b,   'long_name', 'gridded common AOD550 uncertainty due to aerosol type ensemble'

NCDF_ATTPUT, id, vid_aodus,   'units', '1'
NCDF_ATTPUT, id, vid_aodus,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodus,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodus,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodus,   'long_name', 'gridded structured AOD550 uncertainty'

NCDF_ATTPUT, id, vid_aodui,   'units', '1'
NCDF_ATTPUT, id, vid_aodui,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodui,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodui,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodui,   'long_name', 'gridded independent AOD550 uncertainty'

NCDF_ATTPUT, id, vid_aoducorr,   'units', '1'
NCDF_ATTPUT, id, vid_aoducorr,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aoducorr,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aoducorr,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aoducorr,   'long_name', 'gridded AOD550 uncertainty assuming all components fully correlated'

NCDF_ATTPUT, id, vid_aoduran,   'units', '1'
NCDF_ATTPUT, id, vid_aoduran,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aoduran,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aoduran,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aoduran,   'long_name', 'gridded AOD550 uncertainty assuming all components fully random'

NCDF_ATTPUT, id, vid_aodusam,   'units', '1'
NCDF_ATTPUT, id, vid_aodusam,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodusam,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodusam,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodusam,   'long_name', 'gridded AOD550 uncertainty due to propagated daily sampling gaps'

NCDF_ATTPUT, id, vid_aodusamday,   'units', '1'
NCDF_ATTPUT, id, vid_aodusamday,   '_FillValue', -999.0
NCDF_ATTPUT, id, vid_aodusamday,   'valid_range', [-1.,4.]
NCDF_ATTPUT, id, vid_aodusamday,   'coordinates', 'latitude longitude'
NCDF_ATTPUT, id, vid_aodusamday,   'long_name', 'gridded AOD550 uncertainty due to monthly sampling gaps'

nerr=9

NCDF_CONTROL, id, /ENDEF 

print,'test'

print,'variables'

NCDF_VARPUT, id, vid_aodensb, transpose(aodensb_g)

NCDF_VARPUT, id, vid_aodclim, transpose(aodclim_g)
NCDF_VARPUT, id, vid_albedo, transpose(albedo_g)
NCDF_VARPUT, id, vid_aodbest, transpose(aodbest_g)
NCDF_VARPUT, id, vid_aodbest2, transpose(aodbest2_g)
NCDF_VARPUT, id, vid_aod, transpose(aod_g)

nerr=10

NCDF_VARPUT, id, vid_num, transpose(num_g)
NCDF_VARPUT, id, vid_num0, transpose(num0_g)
NCDF_VARPUT, id, vid_num1, transpose(num1_g)
NCDF_VARPUT, id, vid_numday, transpose(numday_g)

nerr=11

NCDF_VARPUT, id, vid_aodu, transpose(aod_u_g)
NCDF_VARPUT, id, vid_aoduc, transpose(aod_uc_g)
NCDF_VARPUT, id, vid_aoduc12, transpose(aod_uc12_g)
NCDF_VARPUT, id, vid_aoduc3b, transpose(aod_uc3b_g)
NCDF_VARPUT, id, vid_aodus, transpose(aod_us_g)
NCDF_VARPUT, id, vid_aodui, transpose(aod_ui_g)
NCDF_VARPUT, id, vid_aoducorr, transpose(aod_u_correl_g)
NCDF_VARPUT, id, vid_aoduran, transpose(aod_u_random_g)
NCDF_VARPUT, id, vid_aodusamday, transpose(aod_u_samplingday_g)

nerr=12

NCDF_VARPUT, id, vid_lat, latitude_g
NCDF_VARPUT, id, vid_lon, longitude_g

NCDF_VARPUT, id, vid_latb, lat_bnds
NCDF_VARPUT, id, vid_lonb, lon_bnds

NCDF_CLOSE, id

goto, endread

endncf: print,'error',nerr

endread:

nextday:

end
end

end