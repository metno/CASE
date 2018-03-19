#!/usr/bin/env Rscript
# << case_realtime_smhi_opendata.R >>
# Create files with the SMHI data for realtime applications.
#------------------------------------------------------------------------------
t0<-Sys.time()
#------------------------------------------------------------------------------
# load libraries
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("rgdal"))
suppressPackageStartupMessages(library("jsonlite"))
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
                   round(d$lon[ix],5),
                   round(d$lat[ix],5),
                   round(d$x_lcc[ix]),
                   round(d$y_lcc[ix]),
                   round(d$x_utm33[ix]),
                   round(d$y_utm33[ix],0),
                   round(d$x_laea[ix],0),
                   round(d$y_laea[ix],0),
                   round(d$z[ix],0),
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
p <- arg_parser("smhi")
# specify our desired options 
# by default ArgumentParser will add an help option 
#p <- add_argument(p, "date",
#                  help="date (format: YYYY-MM-DDTHH)",
#                  type="character")
p <- add_argument(p, "--dirout",
                  help="path for the output files",
                  type="character",
 default="/lustre/storeB/project/metkl/senorge2/case/case_real/hourly_data")
p <- add_argument(p, "--elcode_hourly",
                  help=paste("oldElementCodes for the hourly variables",
                             "deafult=FF,PR,TA,DD,RR_1,FG,UU"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--dqc_ok",
  help=paste("data quality control codes indicating good-enough observations",
             "deafult=Y,G and NA"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
#
argv <- parse_args(p)
#
if (is.na(argv$elcode_hourly)) 
  argv$elcode_hourly<-c("FF","PR","TA","DD","RR_1","FG","UU")
if (is.na(argv$dqc_ok)) 
  argv$dqc_ok<-c("Y","G",NA)
#-----------------------------------------------------------------------------
#if (nchar(argv$date)==13) {
  ford<-"%Y-%m-%dT%H"
  ElCode<-argv$elcode_hourly
#} else {
#  print("ERROR date format not recognized")
#  quit(status=1)
#}
# date-time elaboration
#yyyy<-format(strptime(argv$date,ford),"%Y")
#mm<-format(strptime(argv$date,ford),"%m")
#dd<-format(strptime(argv$date,ford),"%d")
#if (nchar(argv$date)==13) {
#  tstamp<-format(strptime(argv$date,ford),"%Y%m%d%H")
#}
#dirout<-file.path(argv$dirout,yyyy,mm,dd)
#dir.create(path=dirout,
#           showWarnings=F,
#           recursive=T)
#
#------------------------------------------------------------------------------
#
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><")
print("SMHI download: Swedish not NA")
d<-smhi_opendata_assembler(frost_oldElementCodes=ElCode,
                           version="1.0",
                           station_set_all=T,
                           latest_hour=T,
                           formatOUT="%Y-%m-%dT%H",
                           na.rm=T,
                           url.show=F,
                   coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),
                                    y=c("lat","y_lcc","y_utm33","y_laea"),
                                    proj4=c("+proj=longlat +datum=WGS84",
 "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
 "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
 "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                                    stringsAsFactors=F),
                           verbose=F)
if (length(d$timestamp)==0) {
  print("WARNING no data found")
  quit(status=1)
}
if (is.null(d)) {
  print("ERROR in gibson:smhi_opendata_assembler")
  quit(status=1)
}
yyyy<-format(strptime(d$timestamp[1],ford),"%Y")
mm<-format(strptime(d$timestamp[1],ford),"%m")
dd<-format(strptime(d$timestamp[1],ford),"%d")
tstamp<-format(strptime(d$timestamp[1],ford),"%Y%m%d%H")
dirout<-file.path(argv$dirout,yyyy,mm,dd)
dir.create(path=dirout,
           showWarnings=F,
           recursive=T)
fout<-paste("smhi_Sweden_",tstamp,".txt",sep="")
ffout<-file.path(dirout,fout)
writeFile()
rm(d)
print(paste("written file",ffout))
#
#-----------------------------------------------------------------------------
t1<-Sys.time()
print(paste("normal exit /time",round(t1-t0,1),attr(t1-t0,"unit")))
quit(status=0)
