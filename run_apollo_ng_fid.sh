#!/bin/bash
# call with 'bash run_apol13_ng_fid.sh'

cfgfile=$APOLORB"apollo_ng_config_fid.cfg"

echo $cfgfile

#inputpath=$APOLORB"out/" 
inputpath=$APOLORB 
#inputpath="/home/scheiner1/hpopp/test/out/"
# outputpath="/home/scheiner1/hpopp/test/out/"
outputpath=$inputpath

#granule=`ls /home/scheiner1/hpopp/test/out/*.N1`
granule="ls "$inputpath"latitude.dta"

echo $granule

for listel in $granule
do echo $listel
infile=`printf "%s" ${listel} | cut -c50-112`
echo $infile
frame=`printf "%s" ${infile} | cut -c15-29`
echo $frame

sensor=`printf "%s" 'AVHRR'`
platform=`printf "%s" 'NOAA-1X'`

auxiliarypath="/users/hpopp/sysneu/src/APOLLO_NG_Release1/APOLLO13_AUX/database/"
coefficientsfile="/users/hpopp/sysneu/src/APOLLO_NG_Release1/APOLLO13_AUX/database/apollo13_clp_coefficients.dat"
basename=$frame
basename='test'
echo $basename

cpthr='0.15'
#cpthr='0.25'
groundres=`printf "%s\n" "004.40"`
channels="-0.6---0.9---3.7--10.8--12.0"
cna="3"
ch3w=" 0.5"
qltag="NONE"
#qltag="DAY"

printf "%s\n" $inputpath $infile $outputpath $auxiliarypath > $cfgfile
printf "%s\n" $coefficientsfile $basename $sensor $platform $cpthr $groundres >> $cfgfile
printf "%s\n" $channels $cna $ch3w $qltag >> $cfgfile

echo "# line  1: Location of Inputdata" >> $cfgfile
echo "# line  2: input filename" >> $cfgfile
echo "# line  3: Location for Output" >> $cfgfile
echo "# line  4: Location of Lookup-Tables and coefficients for cloud products" >> $cfgfile
echo "# line  5: name of the coefficients-file" >> $cfgfile
echo "# line  6: Basename (Scene-Identifier)" >> $cfgfile
echo "# line  7: Sensor" >> $cfgfile
echo "# line  8: Platform" >> $cfgfile
echo "# line  9: minimum cloud probability for cloud mask (FMT=F4.2)" >> $cfgfile
echo "# line 10: FOV diameter of sensor at nadir (km)" >> $cfgfile
echo "# line 11: vector with central channel wavelengths used for retrieval (FMT=5(F4.1))" >> $cfgfile
echo "# line 12: channel number of absorbing channel to be used for cloud products (default=3)" >> $cfgfile
echo "# line 13: width of channel 3 filter function (relevant if absorbing channel is in 4µm range) (FMT = F4.1)" >> $cfgfile
echo "# line 14: Create RGB quicklook? ('FULL' or 'DAY_' or 'NONE')" >> $cfgfile

/users/hpopp/sysneu/src/APOLLO_NG_Release1/APOLLO13_SRC//apollo_ng $cfgfile

done;echo 'orbit/frame loop done'  

echo "all AATSR_GT data processed"

exit