#!/usr/bin/env Rscript

# --~- case_ecad_archive_by_date.R -~--
# Process ECA&D files to store the data into CASE.
# Input files: ECA&D timeseries (daily data)
# Output files: 
#  + one file for each day/element (i.e. aggregation strategy to get the daily data)
#  + selection based on Countries

# See the software repository here: https://github.com/

#..............................................................................
#Copyright and license
# Copyright (C) 2021 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 

#..............................................................................
# History:
# 20-05-2021 - Cristian Lussana. Original code.
# -----------------------------------------------------------------------------
#

rm( list = ls())

#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages( library( "argparser"))
#suppressPackageStartupMessages( library( "sp"))
#suppressPackageStartupMessages( library( "raster"))
#suppressPackageStartupMessages( library( "rgdal"))
#suppressPackageStartupMessages( library( "ncdf4"))
#suppressPackageStartupMessages( library( "dotnc"))
#options(warn = 2, scipen = 999)
options( scipen = 999999999)

#
# -----------------------------------------------------------------------------
# Constants
# CRS strings
#proj4.wgs84     <- "+proj=longlat +datum=WGS84"
#proj4.ETRS_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
#proj4.utm33     <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#proj4.lcc       <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
#ffout_default   <- "out.nc"

#
# -----------------------------------------------------------------------------
# FUNCTIONS

# + manage fatal error
boom <- function( str=NA, code=NA) {
  cat("Fatal Error ")
  if ( !is.na(code)) {
    if ( code == 1) cat("file not found ")
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 1)
}

#+ the end 
rip <- function( str=NA, code=NA, t0=NA) {
  cat( "the End : ")
  if ( !is.na(code) ) {
    if ( code == 1 ) cat( "normal exit : ")
  }
  if ( !is.na(t0)) {
    t1 <- Sys.time()
    cat( paste( "total time=", round(t1-t0,1), attr(t1-t0,"unit")," "))
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 0 )
}

#+
ECAlatlon2latlondec<-function(xy) {
  lat_dec<-as.numeric(substr(xy$LAT,2,3))+
           as.numeric(substr(xy$LAT,5,6))/60+
           as.numeric(substr(xy$LAT,8,9))/3600
  i<-which(substr(xy$LAT,1,1)=="-")
  if (length(i)>0) lat_dec[i]<-(-lat_dec[i])
  lon_dec<-as.numeric(substr(xy$LON,2,4))+
           as.numeric(substr(xy$LON,6,7))/60+
           as.numeric(substr(xy$LON,9,10))/3600
  i<-which(substr(xy$LON,1,1)=="-")
  if (length(i)>0) lon_dec[i]<-(-lon_dec[i])
  xyT<-cbind(lon_dec,lat_dec)
  xyT
}

#+
str2Rdate <- function(ts,format="%Y-%m-%d %H:%M:%S") {
# ===========================================================================
# converts a string into an R (POSIXt,POSIXct) date object
# date objects can be used with arithmetic operations +/-
# ts is a character or a vector of characters with date information
# in the format specified in format
# Output is a date object

     # the lengthy bunch of testing is necessary because strptime needs
     # explicit specifications for month and day, otherwise it returns NA.
     # this extension allows inputs of the format "%Y-%m" in which case the
     # the first day of the month is taken as a reference.

     #Êcheck if year/month/day is specified
     ysp <- length(c(grep("%Y",format,fixed=TRUE),
                     grep("%y",format,fixed=TRUE)))
     msp <- length(c(grep("%m",format,fixed=TRUE),
                     grep("%b",format,fixed=TRUE),
                     grep("%B",format,fixed=TRUE)))
     jsp <- length(c(grep("%j",format,fixed=TRUE)))
     dsp <- length(c(grep("%d",format,fixed=TRUE)))
     if (ysp > 1) { stop("ERROR: Multiple specification of year in 
                         date format.") }
     if (ysp == 0) { stop("ERROR: No year specification in 
                         date format.") }
     if (msp > 1) { stop("ERROR: Multiple specification of month in 
                         date format.") }
     if (dsp > 1) { stop("ERROR: Multiple specification of day in 
                         date format.") }

     # append month or day if not specified
     tss <- ts
     formati <- format
     if (jsp == 0) {
     if (msp == 0) { 
        tss <- paste(tss,"01",sep="")
        formati <- paste(formati,"%m",sep="")
     }
     if (dsp == 0) { 
        tss <- paste(tss,"01",sep="") 
        formati <- paste(formati,"%d",sep="")
     }
     }

     # this is necessary because strptime() returns NA otherwise
     as.POSIXct(strptime(tss,format=formati),tz="GMT")
}

#+ Create a time sequence having daily/hourly timestep
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
                        verbose=F) {
#==============================================================================
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
#==============================================================================
  # set parameters
  if (!is.null(stop_date))
    if  (is.na(stop_date)) stop_date<-NULL
  if (!is.null(season))
    if  (any(is.na(season))) season<-NULL
  if (!is.null(hourOFday.sel))
    if  (any(is.na(hourOFday.sel))) hourOFday.sel<-NULL
  if (!is.null(dayOFmonth.sel))
    if  (any(is.na(dayOFmonth.sel))) dayOFmonth.sel<-NULL
  if (!is.null(N.prev))
    if  (is.na(N.prev)) N.prev<-NULL
  if (!is.null(N.succ))
    if  (is.na(N.succ)) N.succ<-NULL
  #
  mon.s<-0:11
  if (!is.null(season)) {
    mon.s<-integer(0)
    if (any(season=="MAM")) mon.s<-c(mon.s,2,3,4)
    if (any(season=="JJA")) mon.s<-c(mon.s,5,6,7)
    if (any(season=="SON")) mon.s<-c(mon.s,8,9,10)
    if (any(season=="DJF")) mon.s<-c(mon.s,11,0,1)
  }
  hour.s<-0:23
  if (!is.null(hourOFday.sel)) hour.s<-hourOFday.sel
  day.s<-1:31
  if (!is.null(dayOFmonth.sel)) day.s<-dayOFmonth.sel
  # elaboration
  Rstart_date<-as.POSIXlt(str2Rdate(start_date,format=format))
  if (unit %in% c("sec","secs","second","seconds")) {
    bystr<-time_step
  } else {
    bystr<-paste(time_step," ",unit,sep="")
  }
  if (is.null(stop_date)) {
    Rstop_date<-Rstart_date+as.difftime(N.succ*time_step, unit=unit)
    Rstart_date<-Rstart_date-as.difftime(N.prev*time_step, unit=unit)
  } else {
    Rstop_date<-as.POSIXlt(str2Rdate(stop_date,format=format))
  }
  tseq<-as.POSIXlt(seq(Rstart_date,Rstop_date,by=bystr),"UTC")
  # 
  ix<-which( (tseq$mon %in% mon.s)  & 
             (tseq$mday %in% day.s) & 
             (tseq$hour %in% hour.s) )
  if (length(ix)==0) return(integer(0))
  if (RdateOnlyOut) return(tseq[ix])
  yyyymm.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                  formatC(tseq$mon[ix]+1,width=2,flag="0"),sep="")
  yyyymmddhh.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                      formatC(tseq$mon[ix]+1,width=2,flag="0"),
                      formatC(tseq$mday[ix],width=2,flag="0"),
                      formatC(tseq$hour[ix],width=2,flag="0"),sep="")
  yyyymmdd.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                    formatC(tseq$mon[ix]+1,width=2,flag="0"),
                    formatC(tseq$mday[ix],width=2,flag="0"),sep="")
  nt<-length(yyyymmddhh.v)
  return(list(n=nt,
              yyyymm=yyyymm.v,
              yyyymmdd=yyyymmdd.v,
              yyyymmddhh=yyyymmddhh.v,
              yyyy=formatC(tseq$year[ix]+1900,width=4,flag="0"),
              mm=formatC(tseq$mon[ix]+1,width=2,flag="0"),
              dd=formatC(tseq$mday[ix],width=2,flag="0"),
              hh=formatC(tseq$hour[ix],width=2,flag="0")))
}


#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()

p <- arg_parser("ecad")
#..............................................................................
p <- add_argument(p, "date1",
                  help="period start date",
                  type="character")
p <- add_argument(p, "date2",
                  help="period end date",
                  type="character",
                  default="none")
# time 
p <- add_argument(p, "--date.format",
                  help="format of the date/time",
                  type="character",
                  default="%Y-%m-%d")
# paths
p <- add_argument(p, "--ff_sources",
                  help="full path to sources.txt",
                  type="character",
                  default=NA)
p <- add_argument(p, "--ffin_pref",
                  help="input file prefix (e.g. TG_SOUID)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--dir_in",
                  help="input directory",
                  type="character",
                  default=NA)
p <- add_argument(p, "--ffout_pref",
                  help="output file prefix (e.g. ecad_)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--dir_out",
                  help="output directory",
                  type="character",
                  default=NA)
#
p <- add_argument(p, "--country",
                  help="which Country(ies)?",
                  type="character",
                  nargs=Inf,
                  default=NA)
p <- add_argument(p, "--country_exclude",
                  help="which Country(ies) not to use?",
                  type="character",
                  nargs=Inf,
                  default=NA)
p <- add_argument(p, "--extent",
                  help="domain (lon_min,lon_max,lat_min,lat_max)",
                  type="numeric",
                  nargs=4,
                  default=c(3,35,54,72))
p <- add_argument(p, "--coeff",
                  help="coefficient used to adjust the values",
                  type="numeric",
                  default=1)
#
p <- add_argument(p, "--config_file",
                  help="config file",
                  type="character",
                  default=NA)

#..............................................................................
argv <- parse_args(p)

#
#-----------------------------------------------------------------------------
# read configuration file
if (!is.na(argv$config_file)) {
  if (file.exists(argv$config_file)) {
    source(argv$config_file)
    argv_tmp<-append(argv,conf)
    names_argv_tmp<-names(argv_tmp)
    argv_def<-list()
    names_argv_def<-integer(0)
    k<-0
    for (i in 1:length(argv_tmp)) {
      if (names_argv_tmp[i] %in% names_argv_def) next
      k<-k+1
      j<-which(names_argv_tmp==names_argv_tmp[i])
      argv_def[[k]]<-argv_tmp[[j[length(j)]]]
      names_argv_def<-c(names_argv_def,names_argv_tmp[i])
    }
    names(argv_def)<-names_argv_def
    rm(argv_tmp,names_argv_tmp,names_argv_def)
    rm(argv)
    argv<-argv_def
    rm(argv_def)
  } else {
    print("WARNING: config file not found")
    print(argv$config_file)
  }
}

#
#------------------------------------------------------------------------------
# Time sequence

ts <- createTimeSeq( start_date = argv$date1,
                     stop_date  = argv$date2,
                     format     = argv$date.format,
                     time_step  = 1,
                     unit       = "day",
                     RdateOnlyOut=T,
                     verbose=F)
nts <- length( ts)

cat( paste( "number of time steps is", length( ts), "\n"))

#
#-----------------------------------------------------------------------------
# Read sources.txt

if ( !file.exists( argv$ff_sources) ) boom( code=1, str=argv$ff_sources)

sources_tmp <- read.csv( file             = argv$ff_sources,
                         header           = TRUE,
                         stringsAsFactors = FALSE,
                         quote            = "",
                         strip.white      = T,
                         fileEncoding     = "ISO-8859-1",
                         skip             = 22)
#sources$SOUID    sources$LAT      sources$ELEID    sources$PARID    
#sources$SOUNAME  sources$LON      sources$START    sources$PARNAME  
#sources$CN       sources$HGHT     sources$STOP     

coords <- ECAlatlon2latlondec(sources_tmp)
# coords[,1] lon ; coords[,2] lat

flag <- rep(T, length(sources_tmp$SOUID))

if (any( !is.na( argv$country)))
  flag <- flag & sources_tmp$CN %in% argv$country

if (any( !is.na( argv$country_exclude)))
  flag <- flag & !(sources_tmp$CN %in% argv$country_exclude)

if (any( !is.na( argv$extent)))
  flag <- flag &  coords[,1] >= argv$extent[1] &  coords[,1] <= argv$extent[2] &
                  coords[,2] >= argv$extent[3] &  coords[,2] <= argv$extent[4]

ix <- which( flag)

sources <- data.frame( souid = as.character( sources_tmp$SOUID[ix]),
                       lat   = coords[ix,2],
                       lon   = coords[ix,1],
                       z     = as.numeric( sources_tmp$HGHT[ix]),
                       eleid = as.character( sources_tmp$ELEID[ix]))

rm( sources_tmp, ix, flag)

nso <- length(sources$souid)

# diagnostic
eleids <- as.character( unique( sources$eleid))
for (i in eleids) {
  cat( paste("eleid=",i," #",length(which(sources$eleid==i)),"\n"))
}

#
#-----------------------------------------------------------------------------
# Create output data structure

values <- matrix( data=NA, ncol=nso, nrow=nts)

# loop over input files
for (i in 1:nso) {
  if ((i%%100)==0) cat( paste(i,"/",nso,"\n"))
  ffin <- file.path( argv$dir_in, paste0( argv$ffin_pref, sources$souid[i], ".txt"))
  if ( !file.exists( ffin)) next
  t <- read.csv( file             = ffin,
                 header           = TRUE,
                 stringsAsFactors = FALSE,
                 quote            = "",
                 strip.white      = T,
                 fileEncoding     = "ISO-8859-1",
                 skip             = 18)
  names(t) <- c( "STAID", "SOUID", "DATE", "value", "flag")
  tso  <- as.POSIXlt( str2Rdate( t$DATE, format="%Y%m%d"))
  # use only valid data
  indx <- which( tso %in% ts & t$flag == 0)
  if ( length( indx) == 0) next
  tso_ok <- tso[indx]
  val_ok <- t$value[indx] 
  rm( t, indx)
  match <- match( ts, tso_ok)
  values[1:nts,i] <- val_ok[match] * argv$coeff
}

#
#-----------------------------------------------------------------------------
# Write output files

# loop over time steps
for (i in 1:nts) {
  dir_out <- file.path( argv$dir_out, 
                        format( ts[i], "%Y", tz="UTC"), 
                        format( ts[i], "%m", tz="UTC"),
                        format( ts[i], "%d", tz="UTC"))
  dir.create( dir_out, showWarnings=F, recursive=T)

  # loop over elements
  for (el in eleids) {
    ffout <- file.path( dir_out,
                        paste0( argv$ffout_pref,"_",
                                el,"_",
                                format( ts[i], "%Y%m%d", tz="UTC"),
                                ".txt",sep=""))
    cat( file=ffout,
         append=F,
         paste0( "sourceId;lon;lat;z;value\n"))

    ix <- which( !is.na( values[i,]))

    if ( length(ix) == 0) next
    
    cat( file=ffout,
         append=T,
         paste(
         paste( paste0( "ECA", sources$souid[ix]),
                round( sources$lon[ix], 6),
                round( sources$lat[ix], 6),
                round( sources$z[ix]),
                round( values[i,ix],2),  
                sep=";",collapse="\n"),"\n",sep=""))
  } 
}
 
#
#------------------------------------------------------------------------------
rip( str="Normal Exit", code=0, t0=t0)
