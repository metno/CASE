#!/usr/bin/env Rscript
# << case_realtime_fmi_opendata.R >>
# Create files with the FMI data for realtime applications.
# The data are downloaded in different files depending on WMO (yes/no)
#------------------------------------------------------------------------------
t0<-Sys.time()
#------------------------------------------------------------------------------
# load libraries
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("rgdal"))
suppressPackageStartupMessages(library("xml2"))
suppressPackageStartupMessages(library("gibson"))
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
p <- arg_parser("fmi")
# specify our desired options 
# by default ArgumentParser will add an help option 
p <- add_argument(p, "date",
                  help="date (formats: either YYYY-MM-DD or YYYY-MM-DDTHH)",
                  type="character")
p <- add_argument(p, "--dirout",
                  help="path for the output files",
                  type="character",
 default="/lustre/storeB/project/metkl/senorge2/case/case_real/hourly_data")
p <- add_argument(p, "--elcode_hourly",
                  help=paste("oldElementCodes for the hourly variables",
                             "deafult=FF,PR,TA,DG,DD,RR_1,FG,UU"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--elcode_diurnal",
                  help=paste("oldElementCodes for the diurnal variables",
                             "deafult=TAMRR,TAM,RR,TAN,TAX"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--dqc_ok",
  help=paste("data quality control codes indicating good-enough observations",
             "deafult=0,1,2 and NA"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
#
argv <- parse_args(p)
#
if (is.na(argv$elcode_hourly)) 
  argv$elcode_hourly<-c("FF","PR","TA","DD","RR_1","FG","UU")
if (is.na(argv$elcode_diurnal)) 
  argv$elcode_diurnal<-c("TAM","RR","TAN","TAX")
if (is.na(argv$dqc_ok)) 
  argv$dqc_ok<-c(0,1,2,NA)
#-----------------------------------------------------------------------------
# cristianl@data.fmi.fi apiKey
apiKey<-"5a06e175-b2a1-4789-95c3-943917414ec1"
#-----------------------------------------------------------------------------
if (nchar(argv$date)==13) {
  ford<-"%Y-%m-%dT%H"
  ElCode<-argv$elcode_hourly
  timeOffset<-0
} else if (nchar(argv$date)==10) {
  ford<-"%Y-%m-%d"
  ElCode<-argv$elcode_diurnal
  timeOffset<-3600*24
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
#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("FMI download: Finnish with WMO id not NA")
fout<-paste("fmi_Finland_wmo_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
d<-fmi_opendata_assembler(apiKey=apiKey,
                          oldElementCodes=ElCode,
                          timeOffset=timeOffset,
                          start_date=argv$date,
                          stop_date=argv$date,
                          format=ford,
                          formatOUT=ford,
                          spatial_extent=c(19.09,59.3,31.59,70.13),
                          stationholders=NULL,
                          stationholders.exclude=F,
                          WMOonly=T,
                          WMOin=T,
                          na.rm=T,
                          url.show=T,
                          coords=
                         data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
          url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",
                                   verbose=F)
if (!is.null(d)) {
  writeFile()
  rm(d)
  print(paste("written file",ffout))
}
#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("FMI download: Finnish with WMO id = NA")
fout<-paste("fmi_Finland_nowmo_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
d<-fmi_opendata_assembler(apiKey=apiKey,
                          oldElementCodes=ElCode,
                          timeOffset=timeOffset,
                          start_date=argv$date,
                          stop_date=argv$date,
                          format=ford,
                          formatOUT=ford,
                          spatial_extent=c(19.09,59.3,31.59,70.13),
                          stationholders=NULL,
                          stationholders.exclude=F,
                          WMOonly=F,
                          WMOin=F,
                          na.rm=T,
                          url.show=T,
                          coords=
                         data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
          url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",
                                   verbose=F)
if (!is.null(d)) {
  writeFile()
  rm(d)
  print(paste("written file",ffout))
}
#
#-----------------------------------------------------------------------------
t1<-Sys.time()
print(paste("normal exit /time",round(t1-t0,1),attr(t1-t0,"unit")))
quit(status=0)
