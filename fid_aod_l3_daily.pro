

ncdir='/home/scheiner1/hpopp/fiduceo/CDR/AVHRR18_G/'

outdir='/home/scheiner1/hpopp/fiduceo/CDR/lv3/daily/'

nclen=strlen(ncdir)

platform='AVHRR18_G/
year0=2005
numyear=8
if ncdir eq '/home/scheiner1/hpopp/fiduceo/CDR/AVHRR16_G/' then begin
  year0=2003
  numyear=3
  platform='AVHRR16_G/
end

for nyear=0,numyear-1 do begin

year=strmid(string(year0+nyear),4,4)
print,year

mon0=0
if ncdir eq '/home/scheiner1/hpopp/fiduceo/CDR/AVHRR18_G/' and nyear eq 0 then mon0=5


for nmonth=mon0,11 do begin
month='0'+strmid(string(nmonth+1),7,1)
if nmonth ge 9 then month=strmid(string(nmonth+1),6,2)

day0=0
if ncdir eq '/home/scheiner1/hpopp/fiduceo/CDR/AVHRR18_G/' and nyear eq 0 and nmonth eq 5 then day0=5
day1=31
if nmonth eq 3 or nmonth eq 5 or nmonth eq 8 or nmonth eq 10 then day1=30
if nmonth eq 1 then day1=28
if nmonth eq 1 and year eq '2004' then day1=29 
if nmonth eq 1 and year eq '2008' then day1=29 
if nmonth eq 1 and year eq '2012' then day1=29 

for nday=day0,day1 do begin
if nday le 8 then day='0'+strmid(string(nday+1),7,1)
if nday gt 8 then day=strmid(string(nday+1),6,2)

print,year+month+day

text=''
db=!pi/180.
grid=1
nmix=36

uthresh=0.15
uthresh2=0.10
fthresh=0.2

minpixnum=5

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

timemin_g=fltarr(nlon,nlat)
for i=0,nlon-1 do for j=0,nlat-1 do timemin_g(i,j)=2000000000.
timemax_g=fltarr(nlon,nlat)

aodclim_g=fltarr(nlon,nlat)
aodensb_g=fltarr(nlon,nlat)
aod_g=fltarr(nlon,nlat,nmix)

albedo_g=fltarr(nlon,nlat)

aod_uc_g=fltarr(nlon,nlat)
aod_uc12_g=fltarr(nlon,nlat)
aod_uc3b_g=fltarr(nlon,nlat)
aod_ui_g=fltarr(nlon,nlat)
aod_us_g=fltarr(nlon,nlat)
aod_u_g=fltarr(nlon,nlat)

aod_u_random_g=fltarr(nlon,nlat)
aod_u_correl_g=fltarr(nlon,nlat)
aod_u_sampling_g=fltarr(nlon,nlat)
aod_min_g=fltarr(nlon,nlat)
aod_max_g=fltarr(nlon,nlat)
aod_var_g=fltarr(nlon,nlat)

for i=0,nlon-1 do for j=0,nlat-1 do aod_min_g(i,j)=999.

aodbest_g=fltarr(nlon,nlat)
aodbest2_g=fltarr(nlon,nlat)

spawn,'ls '+ncdir+year+'/'+month+'/'+day+'/*L2B*.nc > '+ncdir+'lists/list'+year+month+day+'.txt'
print,'ls '+ncdir+year+'/'+month+'/'+day+'/*L2B*.nc > '+ncdir+'lists/list'+year+month+day+'.txt'
spawn,'more '+ncdir+'lists/list'+year+month+day+'.txt'

cij=fltarr(50,50)
for i=0,12 do for j=0,12 do cij(i,j)=1.-float(abs(i-j))/12.
for i=0,49 do for j=0,49 do if cij(i,j) lt 0. then cij(i,j)=0.

on_ioerror,endlist

openr,1,ncdir+'lists/list'+year+month+day+'.txt'

kfile=0

for nfile = 1, 15 do begin

  readf,1,text

  if nfile ne 4 and onefile eq 1 then goto,nextfile

  kfile=kfile+1

  tlen=strlen(text)
  file1=strmid(text,nclen,tlen-nclen+1)
  print,file1
  if nfile eq 1 then sourcestring=file1
  if nfile eq 4 and onefile eq 1 then sourcestring=file1
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
  variable_name = "AOD_CLIM"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aodclim
  variable_name = "AOD_ENSB"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aodensb
  dims = size(aodclim,/dimensions)
  zeilen = dims
  print, dims
  variable_name = "latitude"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, latitude
  variable_name = "longitude"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, longitude

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

  variable_name = "AOD_UNCERTAINTY3B"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u3b
  variable_name = "AOD_UNCERTAINTY4"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod_u4

  variable_name = "AOD"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, aod

  variable_name = "TIME"
  varID = ncdf_varid (FileID,variable_name)
  ncdf_varget,fileID, varID, time

  ncdf_CLOSE,FileID

  col=dims(0)
  lin=dims(1)

    num_g1=intarr(nlon,nlat)
    num2_g1=intarr(nlon,nlat,nmix)
    num3_g1=intarr(nlon,nlat)
    num4_g1=intarr(nlon,nlat)

    minlin=intarr(nlon,nlat)
    maxlin=intarr(nlon,nlat)
    for i=0,nlon-1 do for j=0,nlat-1 do minlin(i,j)=9999

    timemin_g1=fltarr(nlon,nlat)
    for i=0,nlon-1 do for j=0,nlat-1 do timemin_g1(i,j)=2000000000.
    timemax_g1=fltarr(nlon,nlat)

    aodclim_g1=fltarr(nlon,nlat)
    aodensb_g1=fltarr(nlon,nlat)
    aod_g1=fltarr(nlon,nlat,nmix)

    albedo_g1=fltarr(nlon,nlat)

    aod_uc_g1=fltarr(nlon,nlat)
    aod_uc12_g1=fltarr(nlon,nlat)
    aod_uc3b_g1=fltarr(nlon,nlat)
    aod_ui_g1=fltarr(nlon,nlat)
    aod_us_g1=fltarr(nlon,nlat)
    aod_u_g1=fltarr(nlon,nlat)

    aod_u_random_g1=fltarr(nlon,nlat)
    aod_u_correl_g1=fltarr(nlon,nlat)
    aod_u_sampling_g1=fltarr(nlon,nlat)
    aod_var_g1=fltarr(nlon,nlat)

    aodbest_g1=fltarr(nlon,nlat)
    aodbest2_g1=fltarr(nlon,nlat)

    naod_us_g1a=intarr(nlon,nlat,50)
    aod_us_g1a=fltarr(nlon,nlat,50)

count=0

  for i=0,col-1 do begin
    for j=0,lin-1 do begin

      indlat=fix(latitude(i,j)-30.)
      indlon=fix(longitude(i,j)+10.)

      if indlat ge 0 and indlat le nlat-1 and indlon ge 0 and indlon le nlon-1 then begin

        if abs(aodensb(i,j)) ne 999. then begin

          if minlin(indlon,indlat) gt j then minlin(indlon,indlat)=j
          if maxlin(indlon,indlat) lt j then maxlin(indlon,indlat)=j

        endif
      endif
    endfor
  endfor

maxval=0
for i=0,nlon-1 do for j=0,nlat-1 do if minlin(i,j) ne 9999 then if maxlin(i,j)-minlin(i,j) gt maxval then maxval=maxlin(i,j)-minlin(i,j)


  for i=0,col-1 do begin
    for j=0,lin-1 do begin

      indlat=fix(latitude(i,j)-30.)
      indlon=fix(longitude(i,j)+10.)

      if indlat ge 0 and indlat le nlat-1 and indlon ge 0 and indlon le nlon-1 then begin

        num0_g(indlon,indlat)=num0_g(indlon,indlat)+1

        if abs(aodensb(i,j)) ne 999. then begin
          num_g1(indlon,indlat)=num_g1(indlon,indlat)+1
          num1_g(indlon,indlat)=num1_g(indlon,indlat)+1
          aodensb_g1(indlon,indlat)=aodensb_g1(indlon,indlat)+aodensb(i,j)
          aodclim_g1(indlon,indlat)=aodclim_g1(indlon,indlat)+aodclim(i,j)
          albedo_g1(indlon,indlat)=albedo_g1(indlon,indlat)+albedo(i,j)

          aod_uc_in=sqrt(aod_uc(i,j)*aod_uc(i,j)+aod_u3b(i,j)*aod_u3b(i,j))
          aod_uc12_in=aod_uc(i,j)
          aod_uc3b_in=aod_u3b(i,j)
          aod_ui_in=sqrt(aod_ui(i,j)*aod_ui(i,j)+aod_u4(i,j)*aod_u4(i,j))
          aod_us_in=aod_us(i,j)

          aod_uc_g1(indlon,indlat)=aod_uc_g1(indlon,indlat)+aod_uc_in
          aod_uc12_g1(indlon,indlat)=aod_uc12_g1(indlon,indlat)+aod_uc12_in
          aod_uc3b_g1(indlon,indlat)=aod_uc3b_g1(indlon,indlat)+aod_uc3b_in
          aod_ui_g1(indlon,indlat)=aod_ui_g1(indlon,indlat)+aod_ui_in*aod_ui_in

          kk=j-minlin(indlon,indlat)
          if aod_us_in lt 10. and aod_us_in gt 0. then begin
            naod_us_g1a(indlon,indlat,kk)=naod_us_g1a(indlon,indlat,kk)+1
            aod_us_g1a(indlon,indlat,kk)=aod_us_g1a(indlon,indlat,kk)+aod_us_in*aod_us_in
          end

          aod_u_correl_g1(indlon,indlat)=aod_u_correl_g1(indlon,indlat)+aod_u_corr(i,j)
          aod_u_random_g1(indlon,indlat)=aod_u_random_g1(indlon,indlat)+aod_u_ran(i,j)*aod_u_ran(i,j)

          if aodensb(i,j) lt aod_min_g(indlon,indlat) then aod_min_g(indlon,indlat)=aodensb(i,j)
          if aodensb(i,j) gt aod_max_g(indlon,indlat) then aod_max_g(indlon,indlat)=aodensb(i,j)

          if time(i,j) lt timemin_g1(indlon,indlat) then timemin_g1(indlon,indlat)=time(i,j)
          if time(i,j) gt timemax_g1(indlon,indlat) then timemax_g1(indlon,indlat)=time(i,j)

        end

        if aod_u(i,j) lt uthresh and abs(aodensb(i,j)) ne 999. then begin
          num3_g1(indlon,indlat)=num3_g1(indlon,indlat)+1
          aodbest_g1(indlon,indlat)=aodbest_g1(indlon,indlat)+aodensb(i,j)
        end

        frac=1.
        if aodensb(i,j) ne 0. then frac = aod_u(i,j) / aodensb(i,j)
        if aod_u(i,j) lt uthresh2 and frac lt fthresh and abs(aodensb(i,j)) ne 999. then begin
          num4_g1(indlon,indlat)=num4_g1(indlon,indlat)+1
          aodbest2_g1(indlon,indlat)=aodbest2_g1(indlon,indlat)+aodensb(i,j)
        end

        for k=1,nmix-1 do begin
          if abs(aod(k,i,j)) ne 999. then begin
            num2_g1(indlon,indlat,k)=num2_g1(indlon,indlat,k)+1
            aod_g1(indlon,indlat,k)=aod_g1(indlon,indlat,k)+aod(k,i,j)
          endif
        end

      end

    end
  end

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    
    if num_g1(indlon,indlat) gt 0 then begin
      aodensb_g1(indlon,indlat)=aodensb_g1(indlon,indlat)/num_g1(indlon,indlat)
      albedo_g1(indlon,indlat)=albedo_g1(indlon,indlat)/num_g1(indlon,indlat)
      aodclim_g1(indlon,indlat)=aodclim_g1(indlon,indlat)/num_g1(indlon,indlat)

      aod_uc_g1(indlon,indlat)=aod_uc_g1(indlon,indlat)/num_g1(indlon,indlat)
      aod_uc12_g1(indlon,indlat)=aod_uc12_g1(indlon,indlat)/num_g1(indlon,indlat)
      aod_uc3b_g1(indlon,indlat)=aod_uc3b_g1(indlon,indlat)/num_g1(indlon,indlat)
      aod_u_correl_g1(indlon,indlat)=aod_u_correl_g1(indlon,indlat)/num_g1(indlon,indlat)

      for kk=0,14 do begin
        if naod_us_g1a(indlon,indlat,kk) gt 0 then aod_us_g1a(indlon,indlat,kk)=sqrt(aod_us_g1a(indlon,indlat,kk))/naod_us_g1a(indlon,indlat,kk)
      endfor

      nline=maxlin(indlon,indlat)-minlin(indlon,indlat)

      aod_us_out=0.
      for kk=0,nline do if aod_us_g1a(indlon,indlat,kk) gt 0. and aod_us_g1a(indlon,indlat,kk) lt 10. then aod_us_out=aod_us_out+aod_us_g1a(indlon,indlat,kk)*aod_us_g1a(indlon,indlat,kk)

       usum=0.
      for i=0,nline do begin
        usum=usum+aod_us_g1a(indlon,indlat,i)
        for j=i+1,nline do begin
          aod_us_out=aod_us_out+2.*cij(i,j)*aod_us_g1a(indlon,indlat,i)*aod_us_g1a(indlon,indlat,j)
        endfor
      endfor

        if nline gt 0 then aod_us_g1(indlon,indlat)=sqrt(aod_us_out)/(nline+1)

    end 

    if num_g1(indlon,indlat) gt 0 then begin
      scount=num_g1(indlon,indlat)
      aod_ui_g1(indlon,indlat)=sqrt(aod_ui_g1(indlon,indlat))/scount
      aod_u_random_g1(indlon,indlat)=sqrt(aod_u_random_g1(indlon,indlat))/scount
    end 

    if num3_g1(indlon,indlat) gt 0 then begin
      aodbest_g1(indlon,indlat) = aodbest_g1(indlon,indlat) / num3_g1(indlon,indlat)
    end 

    if num4_g1(indlon,indlat) gt 0 then begin
      aodbest2_g1(indlon,indlat) = aodbest2_g1(indlon,indlat) / num4_g1(indlon,indlat)
    end 

    for k=1,nmix-1 do begin
      if num2_g1(indlon,indlat,k) gt 0 then begin
        aod_g1(indlon,indlat,k)=aod_g1(indlon,indlat,k)/num2_g1(indlon,indlat,k)
      end 
    end

  end
end

for indlon=0,nlon-1 do begin
  for indlat=0,indlat-1 do begin

    if num_g1(indlon,indlat) gt 0 then begin

          num_g(indlon,indlat)=num_g(indlon,indlat)+1
          aodensb_g(indlon,indlat)=aodensb_g(indlon,indlat)+aodensb_g1(indlon,indlat)
          aodclim_g(indlon,indlat)=aodclim_g(indlon,indlat)+aodclim_g1(indlon,indlat)
          albedo_g(indlon,indlat)=albedo_g(indlon,indlat)+albedo_g1(indlon,indlat)

          aod_uc_g(indlon,indlat)=aod_uc_g(indlon,indlat)+aod_uc_g1(indlon,indlat)
          aod_uc12_g(indlon,indlat)=aod_uc12_g(indlon,indlat)+aod_uc12_g1(indlon,indlat)
          aod_uc3b_g(indlon,indlat)=aod_uc3b_g(indlon,indlat)+aod_uc3b_g1(indlon,indlat)
          aod_us_g(indlon,indlat)=aod_us_g(indlon,indlat)+aod_us_g1(indlon,indlat)*aod_us_g1(indlon,indlat)
          aod_ui_g(indlon,indlat)=aod_ui_g(indlon,indlat)+aod_ui_g1(indlon,indlat)*aod_ui_g1(indlon,indlat)

          aod_u_correl_g(indlon,indlat)=aod_u_correl_g(indlon,indlat)+aod_u_correl_g1(indlon,indlat)
          aod_u_random_g(indlon,indlat)=aod_u_random_g(indlon,indlat)+aod_u_random_g1(indlon,indlat)*aod_u_random_g1(indlon,indlat)

          if timemin_g1(indlon,indlat) lt timemin_g(indlon,indlat) then timemin_g(indlon,indlat)=timemin_g1(indlon,indlat)
          if timemax_g1(indlon,indlat) gt timemax_g(indlon,indlat) then timemax_g(indlon,indlat)=timemax_g1(indlon,indlat)

    end

        if num3_g1(indlon,indlat) gt 0 then begin
          num3_g(indlon,indlat)=num3_g(indlon,indlat)+1
          aodbest_g(indlon,indlat)=aodbest_g(indlon,indlat)+aodbest_g1(indlon,indlat)
        end

        if num4_g1(indlon,indlat) gt 0 then begin
          num4_g(indlon,indlat)=num4_g(indlon,indlat)+1
          aodbest2_g(indlon,indlat)=aodbest2_g(indlon,indlat)+aodbest2_g1(indlon,indlat)
        end

        for k=1,nmix-1 do begin
          if num2_g1(indlon,indlat) gt 0 then begin
            num2_g(indlon,indlat,k)=num2_g(indlon,indlat,k)+1
            aod_g(indlon,indlat,k)=aod_g(indlon,indlat,k)+aod_g1(k,indlon,indlat)
          endif
        end

  end
end

  nextfile:

end

endlist:

close,1

if kfile eq 0 then goto,nextday

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    
    if num_g(indlon,indlat) gt 0 then begin
      aodensb_g(indlon,indlat)=aodensb_g(indlon,indlat)/num_g(indlon,indlat)
      albedo_g(indlon,indlat)=albedo_g(indlon,indlat)/num_g(indlon,indlat)
      aodclim_g(indlon,indlat)=aodclim_g(indlon,indlat)/num_g(indlon,indlat)

      aod_uc_g(indlon,indlat)=aod_uc_g(indlon,indlat)/num_g(indlon,indlat)
      aod_uc12_g(indlon,indlat)=aod_uc12_g(indlon,indlat)/num_g(indlon,indlat)
      aod_uc3b_g(indlon,indlat)=aod_uc3b_g(indlon,indlat)/num_g(indlon,indlat)
      aod_u_correl_g(indlon,indlat)=aod_u_correl_g(indlon,indlat)/num_g(indlon,indlat)
    end else begin
      aodensb_g(indlon,indlat)=-999.
      albedo_g(indlon,indlat)=-999.
      aodclim_g(indlon,indlat)=-999.

      aod_uc_g(indlon,indlat)=-999.
      aod_uc12_g(indlon,indlat)=-999.
      aod_uc3b_g(indlon,indlat)=-999.
      aod_u_correl_g(indlon,indlat)=-999.
    endelse

    if num_g(indlon,indlat) gt 0 then begin
      scount=num_g(indlon,indlat)
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
    sq3=aod_uc_g(indlon,indlat)*aod_uc_g(indlon,indlat)
    aod_u_g(indlon,indlat)=sqrt(sq1+sq2+sq3)

  end
end

meanaod=0.
numaod=0

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    if aodensb_g(indlon,indlat) ne -999 then begin
      numaod=numaod+1
      meanaod=meanaod+aodensb_g(indlon,indlat)
    endif
  end
end

if numaod ne 0 then meanaod=meanaod/numaod

sigaod=0.
for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    if aodensb_g(indlon,indlat) ne -999 then begin
      diff=aodensb_g(indlon,indlat)-meanaod
      sigaod=sigaod+diff*diff
    endif
  end
end

if numaod gt 1 then begin
  sigaod=sqrt(sigaod)/sqrt(float(numaod)*float((numaod-1)))
end else begin
  if numaod eq 1 then sigaod=sqrt(sigaod)
endelse

for indlon=0,nlon-1 do begin
  for indlat=0,nlat-1 do begin
    if num0_g(indlon,indlat) gt 0 then begin
       pixfrac=0.
       if num0_g(indlon,indlat) ne 0 then pixfrac=num1_g(indlon,indlat)/num0_g(indlon,indlat)
       aod_u_sampling_g(indlon,indlat)=0.5*(aod_max_g(indlon,indlat)-aod_min_g(indlon,indlat)) * (1. - pixfrac)
    end else begin
       aod_u_sampling_g(indlon,indlat)=-999.
    endelse

    if aodensb_g(indlon,indlat) eq -999. then num_g(indlon,indlat)=-999
    if aodensb_g(indlon,indlat) eq -999. then num0_g(indlon,indlat)=-999
    if aodensb_g(indlon,indlat) eq -999. then num1_g(indlon,indlat)=-999

    if num1_g(indlon,indlat) lt minpixnum then begin
      aodensb_g(indlon,indlat)=-999.
      aodclim_g(indlon,indlat)=-999.
      albedo_g(indlon,indlat)=-999.
      aodbest_g(indlon,indlat)=-999.
      aodbest2_g(indlon,indlat)=-999.
      aod_g(indlon,indlat)=-999.
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

filename=outdir+platform+year+'/'+month+'/'+'FIDUCEO_CDR_AOD_AVHRR_N18ALL_'+year+month+day+'_L3_daily_Europe_v1.0.0_fv1.0.0.nc'
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
NCDF_ATTPUT, id, /GLOBAL, 'history', 'FIDUCEO AVHRR L1C ver.: v0.2Bet_fv2.0.0, FIDUCEO AVHRR AOD L2A/B ver.: 1.0.0_fv1.0.0'
;NCDF_ATTPUT, id, /GLOBAL, 'license', 'This dataset is released for use under CC-BY licence (https://creativecommons.org/licenses/by/4.0/) and was developed in the EC FIDUCEO project Fidelity and Uncertainty in Climate Data Records from Earth Observations. Grant Agreement: 638822.'

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
NCDF_ATTPUT, id, vid_aodusam,   'long_name', 'gridded AOD550 uncertainty due to sampling gaps'

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

nerr=11

NCDF_VARPUT, id, vid_aodu, transpose(aod_u_g)
NCDF_VARPUT, id, vid_aoduc, transpose(aod_uc_g)
NCDF_VARPUT, id, vid_aoduc12, transpose(aod_uc12_g)
NCDF_VARPUT, id, vid_aoduc3b, transpose(aod_uc3b_g)
NCDF_VARPUT, id, vid_aodus, transpose(aod_us_g)
NCDF_VARPUT, id, vid_aodui, transpose(aod_ui_g)
NCDF_VARPUT, id, vid_aoducorr, transpose(aod_u_correl_g)
NCDF_VARPUT, id, vid_aoduran, transpose(aod_u_random_g)
NCDF_VARPUT, id, vid_aodusam, transpose(aod_u_sampling_g)

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

end