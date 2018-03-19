#!/usr/bin/env Rscript
# << case_realtime_postproc.R >>
# Postproc real-time data.
#------------------------------------------------------------------------------
t0<-Sys.time()
#------------------------------------------------------------------------------
# load libraries
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("gibson")) # createTimeSeq()
#------------------------------------------------------------------------------
# FUNCTIONS

#+ write the output file
writeFile<-function() {
  outf<-vector(mode="logical",length=length(d[[ElCode[1]]]))
  outf[]<-F
  for (e in ElCode) {
    eq<-paste(e,"_qcode",sep="")
    outf<-outf | (!is.na(d[[e]]) & (d[[eq]] %in% argv$dqc_ok))
  }
  ix<-which(outf)
  str<-c("sourceId",
         "lon","lat",
         "x_lcc","y_lcc",
         "x_utm33","y_utm33",
         "x_laea","y_laea",
         "z")
  str<-c(str,ElCode)
  daux<-data.frame(d$sourceId[ix],
                   d$lon[ix],
                   d$lat[ix],
                   d$x_lcc[ix],
                   d$y_lcc[ix],
                   d$x_utm33[ix],
                   d$y_utm33[ix],
                   d$x_laea[ix],
                   d$y_laea[ix],
                   d$z[ix],
                   stringsAsFactors=F)
  for (e in ElCode) daux<-cbind(daux,d[[e]][ix],stringsAsFactors=F)
  names(daux)<-str
  write.table(file=ffout,
              daux,
              quote=F,
              row.names=F,
              dec=".",
              sep=";")
}

#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------
# create parser object
p <- arg_parser("pp")
# specify our desired options 
# by default ArgumentParser will add an help option 
p <- add_argument(p, "date_start",
                  help="date (formats: either YYYY-MM-DD or YYYY-MM-DDTHH)",
                  type="character")
p <- add_argument(p, "date_stop",
                  help="date (formats: either YYYY-MM-DD or YYYY-MM-DDTHH)",
                  type="character")
p <- add_argument(p, "--dirout",
                  help="path for the output files",
                  type="character",
 default="/lustre/storeB/project/metkl/senorge2/case/case_real/hourly_data")
p <- add_argument(p, "--elcode_hourly",
                  help=paste("oldElementCodes for the hourly variables",
                             "examples=FF,PR,TA,DG,DD,RR_1,FG,UU"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--elcode_diurnal",
                  help=paste("oldElementCodes for the diurnal variables",
                             "examples=TAMRR,TAM,RR,TAN,TAX"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--ppfun",
                  help=paste("post-processing function, one for each oldElementCode.",
                             "Implemented options are: sum, min, max, mean.)",
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--n_prev",
  help=paste("number of time units to substract from the provided date.",
             "Note date_start must be equal to date_stop for this option to be activated.",
             "Time units are deducted from the date format.",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--n_succ",
  help=paste("number of time units to add to the provided date.",
             "Note date_start must be equal to date_stop for this option to be activated.",
             "Time units are deducted from the date format.",
                  type="numeric",
                  default=NA)
# it might be useful to have here: --clima_mode, --clima_year_from, --clima_year_to
#
argv <- parse_args(p)
#
boom<-function(x) {print(paste("ERROR",x));quit(status=1)}
if ( ((is.na(argv$elcode_hourly) & is.na(argv$elcode_diurnal))) |
     (any(!is.na(argv$elcode_hourly)) & any(!is.na(argv$elcode_diurnal))) ) 
  boom("please specify either --elcode_hourly OR elcode_diurnal") 
if (is.na(argv$ppfun)) boom("please specify --ppfun") 

if (nchar(argv$date)==13) {
  ford<-"%Y-%m-%dT%H"
  ElCode<-argv$elcode_hourly
} else if (nchar(argv$date)==10) {
  ford<-"%Y-%m-%d"
  ElCode<-argv$elcode_diurnal
} else {
  print("ERROR date format not recognized")
  quit(status=1)
}
# date-time elaboration
yyyy<-format(strptime(argv$date,ford),"%Y")
mm<-format(strptime(argv$date,ford),"%m")
dd<-format(strptime(argv$date,ford),"%d")
if (nchar(argv$date)==13) {
  tstamp<-format(strptime(argv$date,ford),"%Y%m%d%H")
} else if (nchar(argv$date)==10) {
  tstamp<-format(strptime(argv$date,ford),"%Y%m%d")
}
dirout<-file.path(argv$dirout,yyyy,mm,dd)
dir.create(path=dirout,
           showWarnings=F,
           recursive=T)
createTimeSeq<-function(start_date="2015.01.01.01",
                        stop_date="2015.12.31.23",
                        format="%Y.%m.%d.%H",
                        time_step=1,
                        unit="hours",
                        season=NULL,
                        hourOFday.sel=NULL,
                        dayOFmonth.sel=NULL,
                        N.prev=NULL,
                        N.succ=NULL,
                        RdateOnlyOut=F,
                        verbose=F)

#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("KDVH download: Norwegian with WMO id not NA")
fout<-paste("kdvh_Norway_wmo_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
d<-frost_assembler(client_id=client_id,
                   oldElementCodes=ElCode,
                   start_date=argv$date,
                   stop_date=argv$date,
                   format=ford,
                   formatOUT=ford,
                   countries="NO",
                   spatial_extent=c(4,34,54,72),
                   stationholders=NULL,
                   stationholders.exclude=F,
                   WMOonly=T,
                   WMOin=T,
                   na.rm=T,
                   url.show=T,
                   coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
                   verbose=T)
if (!is.null(d)) {
  writeFile()
  rm(d)
  print(paste("written file",ffout))
}
#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("KDVH download: Norwegian, without WMO id, MET among station holders")
fout<-paste("kdvh_Norway_nowmo_met_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
d<-frost_assembler(client_id=client_id,
                   oldElementCodes=ElCode,
                   start_date=argv$date,
                   stop_date=argv$date,
                   format=ford,
                   formatOUT=ford,
                   countries="NO",
                   spatial_extent=c(4,34,54,72),
                   stationholders="MET.NO",
                   stationholders.exclude=F,
                   WMOonly=F,
                   WMOin=F,
                   na.rm=T,
                   url.show=T,
                   coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
                   verbose=T)
if (!is.null(d)) {
  writeFile()
  rm(d)
  print(paste("written file",ffout))
}
#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("KDVH download: Norwegian, without WMO id, MET not a station holder")
fout<-paste("kdvh_Norway_nowmo_nomet_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
d<-frost_assembler(client_id=client_id,
                   oldElementCodes=ElCode,
                   start_date=argv$date,
                   stop_date=argv$date,
                   format=ford,
                   formatOUT=ford,
                   countries="NO",
                   spatial_extent=c(4,34,54,72),
                   stationholders="MET.NO",
                   stationholders.exclude=T,
                   WMOonly=F,
                   WMOin=F,
                   na.rm=T,
                   url.show=T,
                   coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
                   verbose=T)
if (!is.null(d)) {
  writeFile()
  rm(d)
  print(paste("written file",ffout))
}
#
#------------------------------------------------------------------------------
# currently, Norwegian data only in Frost
#print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
#print("KDVH download: not Norwegian")
#fout<-paste("kdvh_noNorway_",tstamp,".txt",sep="")
#ffout<-file.path(dirout,fout)
#d<-frost_assembler(client_id=client_id,
##                   oldElementCodes=ElCode,
#                   oldElementCodes="TA",
#                   start_date=argv$date,
#                   stop_date=argv$date,
#                   format=ford,
#                   formatOUT=ford,
##                   countries=c("SE","FI","RU"),
#                   countries=NULL,
#                   spatial_extent=c(4,34,54,72),
#                   stationholders=NULL,
#                   stationholders.exclude=F,
#                   WMOonly=F,
#                   WMOin=T,
#                   na.rm=T,
#                   url.show=T,
#                   coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
#                                    y=c("lat","y_lcc","y_utm33","y_laea"),
#                                    proj4=c("+proj=longlat +datum=WGS84",
# "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
# "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
# "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
#                                    stringsAsFactors=F),
#                   verbose=T)
#if (!is.null(d)) {
#  writeFile()
#  rm(d)
#  print(paste("written file",ffout))
#}
#
#-----------------------------------------------------------------------------
t1<-Sys.time()
print(paste("normal exit /time",round(t1-t0,1),attr(t1-t0,"unit")))
quit(status=0)
