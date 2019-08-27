#!/bin/bash
# !=====#################################################################
# ! NAME:
# !		fiduceo_aod.sh
# ! CALLING SEQUENCE:
# !		./fiduceo_aod.sh year month version number
# !		./fiduceo_aod.sh 2008 01 fv2.0 18
# ! PURPOSE:
# !		starting the Fiduceo AOD Routine in Fortran
# ! AUTHOR:
# !		adapted by Thomas Popp, 23.04.2019
# ! PROJECT:
# !		FIDUCEO
# !=====#################################################################

#############################################################
# FILEBASE
#############################################################
# see more on filenaming for Fiduceo at ...
#
 project="FIDUCEO"
 datatype="FCDR"
 level="1C"			# 1=level 1C
 sensor="AVHRR"
 satellite="N"
 number=$4
 switch='ALL'

 filebase=(${project}_${datatype}_L${level}_${sensor}_${satellite}${number}${switch})

#############################################################
 year=$1
 month=$2
 version=$3

# INPUT and PROCESSING paths
 datapath_org="/home/scheiner1/hpopp/fiduceo/"
 processingpath=${datapath_org}process/
 mkdir -p $processingpath'container/'

 datapath0=${datapath_org}data_download/europe_selection/${sensor}${number}'_G/'${year}/${month}/

#############################################################
# Find available file
#############################################################
  avhrr_files=($(ls ${datapath0}*/${filebase}_*${version}*.nc))
  nlv1=${#avhrr_files[@]}

	if  [ "$nlv1" -gt 0 ];then

#############################################################
# start reading of Europe selection Files
#############################################################

     for avhrr_file in "${avhrr_files[@]}"
     do
		echo $avhrr_file

#############################################################
# Settings
#############################################################

	   n=`echo ${avhrr_file} | awk '{print gsub(/\//,"")}'`
	   n2=$(($n + 1))




       basename=`echo ${avhrr_file} | cut -d'/' -f $n2`
       starttime0=`echo ${avhrr_file} | cut -d'/' -f $n2 | cut -d'_' -f 6`
       stoptime0=`echo ${avhrr_file} | cut -d'/' -f $n2 | cut -d'_' -f 7`
       starttime=$(echo ${starttime0:8:6} )
       stoptime=$(echo ${stoptime0:8:6} )
       day=$(echo ${stoptime0:6:2} )

 echo =====================================================================
 echo start orbit $day.$month.$year $starttime
 echo =====================================================================
 datepath=$year/$month/$day/
 datadir=$year/$month/$day
 period=$year$month$day

 datapath2=${datapath_org}data_download/europe_selection/${sensor}${number}'_G/'${datadir}/
 datapath3=${datapath_org}CDR/${sensor}${number}'_G/'${datadir}/
 mkdir -p $datapath3

echo $basename
echo $starttime0
echo $stoptime0
echo $starttime
echo $stoptime
#############################################################
# Configuration-file
#############################################################
configfile0=config_fiduceo_${period}_$starttime.txt
configfile=$processingpath'container/'$configfile0
echo $configfile
export configfile=$configfile
#############################################################
# writing Config-file for Fortran Code
#############################################################
echo "################################################################################" > $configfile
echo "# Here are the Inputs for SYNAER-retrieval" >> $configfile
echo "# created automatically with fiduceo_read.sh" >> $configfile
echo "################################################################################" >> $configfile
echo "# calculated on     :  `date -u`">> $configfile
echo "# user              : " $USER >> $configfile
echo "################################################################################" >> $configfile
echo ">>>" >> $configfile
echo "project             : " $project >> $configfile
echo "datatype            : " $datatype >> $configfile
echo "level               : " $level >> $configfile
echo "sensor              : " $sensor >> $configfile
echo "satellite           : " $satellite >> $configfile
echo "number              : " $number >> $configfile
echo "starttime           : " $starttime >> $configfile
echo "stoptime            : " $stoptime >> $configfile
echo "date                : " $period >> $configfile
echo "path                : " $datapath2 >> $configfile
echo "filename            : " $basename >> $configfile
#############################################################
# start reading with Fortran code
#############################################################
	/users/hpopp/syslin/src_noaa18/fiduceo/lib_fortran_test//fid_avhrr_aod 1

#############################################################
# Cleaning
#############################################################
	if [ -f "$configfile" ]; then
	echo removing configfile
	rm $configfile
        rm ${datapath2}*dta
        rm ${datapath2}*DAT
        rm ${datapath2}*cfg
        rm ${datapath2}*csh
        rm ${datapath2}*log
        rm ${datapath2}size
        echo ${datapath2}*aod*nc
        echo ${datapath3}	
        mv ${datapath2}*AOD*${startttime}*nc ${datapath3} 
	fi
#############################################################
     done

	fi

