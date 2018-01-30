#
while getopts "s:e:v:" Option 
do
  case $Option in
  s ) DATESTART=$OPTARG
  ;;
  e ) DATEEND=$OPTARG
  ;;
  v ) VAR=$OPTARG
  ;;
  * ) echo " not recognized Option ";;
  esac
done
#------------------------------------------------------------------------------
# date/time manipulations
YYYYbeg=${DATESTART:0:4}
MMbeg=${DATESTART:5:2}
DDbeg=${DATESTART:8:2}
HHbeg=06
YYYYend=${DATEEND:0:4}
MMend=${DATEEND:5:2}
DDend=${DATEEND:8:2}
HHend=09
SECbeg=`date +%s -d "$YYYYbeg-$MMbeg-$DDbeg $HHbeg:00:00"`
SECend=`date +%s -d "$YYYYend-$MMend-$DDend $HHend:00:00"`
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
SECcur=$SECbeg
echo `date +%Y-%m-%d" "%H:%M`
while (( "$SECcur" <= "$SECend" )) 
do
  DATEcur=`date --date="1970-01-01 $SECcur sec UTC" +%Y.%m.%d`
  YYYY=${DATEcur:0:4}
  MM=${DATEcur:5:2}
  DD=${DATEcur:8:2}
  echo "============================================================================="
  echo "Processing date ="$DATEcur
  script="/disk1/projects/TITAN/titan.R"
  dir="/disk1/data/case/case_20180112"
  diro="/disk1/data/case/case_20180112/aux/$VAR"_date_dqc/"$YYYY$MM"
  ffi="$dir/$VAR"_date/"$YYYY$MM/case_$VAR"_date_"$YYYY$MM$DD.txt"
  ffo="$diro/case_$VAR"_date_"$YYYY$MM$DD.txt"
  mkdir -p $diro
  $script $ffi $ffo --spatconv --month.clim $MM --varname.lat lat_dec --varname.lon lon_dec --varname.elev z --varname.value value --i.buddy 3 --separator , --dr.isol 250000 --n.isol 2 --grid.sct 5 5 --i.sct 3 --n.sct 10 --dem --dem.file /disk1/data/geoinfo/meps_gmted2010_1km_topo_topdown.nc --dem.fill --laf.sct --laf.file /disk1/data/geoinfo/meps_gmted2010_1km_laf_topdown.nc --varname.opt staid metnostaid souid cn etrs_laea_x etrs_laea_y utm33_x utm33_y date qcode -v
  # next timestep
  SECcur=$(( SECcur+3600*24 ))
done
echo `date +%Y-%m-%d" "%H:%M`
#--------------------------
exit 0
