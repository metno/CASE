#!/usr/bin/env Rscript
# << case_realtime_postproc.R >>
# Postproc real-time data.
#
# --elcode_in_hourly(diurnal) specifies the input file variables & their order
# --elcode_out specifies the output file variable names
# --elcode_link specified the input variable used to obtain the output
# --ppfun function applied to each "elcode_link" variable to obtain "elcode_out"
# 
# Command-line example
#  ./case_netatmo_postproc.R 2018-04-13T07 2018-04-14T06 none --dirin /home/cristianl/data/netatmo --elcode_in_hourly FF,PR,TA,DG,DD,RR_1,FG,UU --elcode_out TAMRR,TAXRR,TANRR,RR --elcode_link TA,TA,TA,RR_1 --ppfun mean,max,min,sum
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
  if (argv$netatmo) {
    str<-integer(0)
  } else {
    str<-"sourceId"
  }
  str<-c(str,
         "lon","lat",
         "x_lcc","y_lcc",
         "x_utm33","y_utm33",
         "x_laea","y_laea",
         "z",
         argv$elcode_out)
  posok<-apply(res,MARGIN=1,FUN=function(x){ifelse(any(!is.na(x)),T,F)})
  ix<-which(posok)
  if (length(ix)>0) {
    daux<-data.frame(meta$lon[ix],
                     meta$lat[ix],
                     meta$x_lcc[ix],
                     meta$y_lcc[ix],
                     meta$x_utm33[ix],
                     meta$y_utm33[ix],
                     meta$x_laea[ix],
                     meta$y_laea[ix],
                     meta$z[ix],
                     stringsAsFactors=F)
    if (!argv$netatmo) daux<-cbind(meta$sourceId[ix],daux)
    daux<-cbind(daux,round(res[ix,],3),stringsAsFactors=F)
    names(daux)<-str
    write.table(file=ffout,
                daux,
                quote=F,
                row.names=F,
                dec=".",
                sep=";")
  }
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
p <- add_argument(p, "bfin",
     help="basename for the input file (e.g., ''kdvh_Norway_nowmo_met_'')",
                  type="character")
#
p <- add_argument(p, "--dirout",
                  help="path for the output files",
                  type="character",
                  default=".")
p <- add_argument(p, "--bdirout",
                  help="path for the output files, time dependent part",
                  type="character",
                  default="yyyy/mm/dd")
p <- add_argument(p, "--dirin",
                  help="path for the input files",
                  type="character",
 default="/lustre/storeB/project/metkl/senorge2/case/case_real/hourly_data")
p <- add_argument(p, "--bdirin",
                  help="path for the output files, time dependent part",
                  type="character",
                  default="yyyy/mm/dd")
p <- add_argument(p, "--bfout",
                  help=paste0("basename for the output file (time-independent part,",
                       " default is (bfin)pp(_yyyymmdd.txt))"),
                  type="character",
                  default=NA)
#
p <- add_argument(p, "--elcode_in_hourly",
                  help=paste("oldElementCodes for the hourly variables",
                             "examples=FF,PR,TA,DG,DD,RR_1,FG,UU"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--elcode_in_diurnal",
                  help=paste("oldElementCodes for the diurnal variables",
                             "examples=TAMRR,TAM,RR,TAN,TAX"),
                  type="character",
                  nargs=Inf,
                  default=NULL)

p <- add_argument(p, "--elcode_out",
                  help=paste("oldElementCodes for the output variables",
                             "examples=TAMRR,TAM,RR,TAN,TAX"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--elcode_link",
                  help=paste("oldElementCodes for the output variables",
                             "examples=TAMRR,TAM,RR,TAN,TAX"),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--ppfun",
                  help=paste("post-processing function, one for each oldElementCode.",
                             "Implemented options are: sum, min, max, mean."),
                  type="character",
                  nargs=Inf,
                  default=NULL)
p <- add_argument(p, "--n_prev",
  help=paste("number of time units to substract from the provided date.",
             "Note date_start must be equal to date_stop for this option to be activated.",
             "Time units are deducted from the date format."),
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--n_succ",
  help=paste("number of time units to add to the provided date.",
             "Note date_start must be equal to date_stop for this option to be activated.",
             "Time units are deducted from the date format."),
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--timestep",
                  help=paste("number of time units to use as time step"),
                  type="numeric",
                  default=1)
p <- add_argument(p, "--season",
                  help=paste("subset data on seasons (JJA,MAM,SON,DJF)"),
                  type="character",
                  default=NULL)
p <- add_argument(p, "--hourOFday",
                  help=paste("subset data on the hour(s) of the day"),
                  type="numeric",
                  nargs=Inf,
                  default=NA)
p <- add_argument(p, "--dayOFmonth",
                  help=paste("subset data on the day(s) of the month"),
                  type="numeric",
                  nargs=Inf,
                  default=NA)
# it might be useful to have here: --clima_mode, --clima_year_from, --clima_year_to
#
p <- add_argument(p, "--tolz",
     help=paste("tolerance threshold for elevation difference (m)"),
                  type="numeric",
                  default=5)
p <- add_argument(p, "--tolh",
     help=paste("tolerance threshold for horizontal distance (degree)"),
                  type="numeric",
                  default=0.000001)
#
p <- add_argument(p, "--netatmo",
                  help=paste("netatmo file (coords lat-lon; no sourceID)"),
                  flag=T)
#
argv <- parse_args(p)
#
#------------------------------------------------------------------------------
# checks
boom<-function(x) {print(paste("ERROR",x));quit(status=1)}
if ( ((any(is.na(argv$elcode_in_hourly)) & any(is.na(argv$elcode_in_diurnal)))) |
     (any(!is.na(argv$elcode_in_hourly)) & any(!is.na(argv$elcode_in_diurnal))) ) 
  boom("please specify either --elcode_in_hourly OR elcode_in_diurnal") 
if (any(is.na(argv$ppfun))) boom("please specify --ppfun") 
#
if ( argv$date_start==argv$date_stop & 
     is.na(argv$n_prev) & is.na(argv$n_succ)) 
  boom("when date_start = date_stop, n_prev and/or n_succ must be provided")
if (argv$bfin=="none") argv$bfin<-""
#
nel_out<-length(argv$elcode_out)
if (length(argv$elcode_out)!=length(argv$elcode_link)) 
  boom("elcode_link and elcode_out must have the same lenght")
if (length(argv$elcode_out)!=length(argv$ppfun)) 
  boom("ppfun and elcode_out must have the same lenght")
if (argv$netatmo) {
  suppressPackageStartupMessages(library("sp"))
  suppressPackageStartupMessages(library("raster"))
  suppressPackageStartupMessages(library("rgdal"))
}
#
#------------------------------------------------------------------------------
# date/time elaboration
if (nchar(argv$date_start)==13) {
  ford<-"%Y-%m-%dT%H"
  elcode_in<-argv$elcode_in_hourly
  unit<-"hours"
} else if (nchar(argv$date_start)==10) {
  ford<-"%Y-%m-%d"
  elcode_in<-argv$elcode_in_diurnal
  unit<-"days"
}
# number of element to process
nel_in<-length(elcode_in)
# sequence of time steps to aggregate
tseq<-createTimeSeq(
  start_date=argv$date_start,
  stop_date=ifelse(argv$date_start==argv$date_stop,NA,argv$date_stop),
  format=ford,
  time_step=argv$timestep,
  unit=unit,
  season=ifelse(any(is.na(argv$season)),NA,argv$season),
  hourOFday.sel=ifelse(any(is.na(argv$hourOFday)),NA,argv$hourOFday),
  dayOFmonth.sel=ifelse(any(is.na(argv$dayOFmonth)),NA,argv$dayOFmonth),
  N.prev=ifelse(argv$date_start==argv$date_stop & is.na(argv$n_prev),
                0,argv$n_prev),
  N.succ=ifelse(argv$date_start==argv$date_stop & is.na(argv$n_succ),
                0,argv$n_succ),
  RdateOnlyOut=F,
  verbose=F)
#
#------------------------------------------------------------------------------
# define total metadata structure
bigar0<-array(data=NA,dim=c(100000,tseq$n,nel_in))
for (t in 1:tseq$n) {
  t10<-Sys.time()
  # netatmo has different filenames
  if (argv$netatmo) {
    ffin<-file.path(argv$dirin,
           replaceDate(argv$bdirin,date.str=tseq$yyyymmdd[t],format="%Y%m%d"),
           paste(argv$bfin,
            ifelse(nchar(argv$date_start)==13,
            paste0(tseq$yyyymmdd[t],"T",substr(tseq$yyyymmddhh[t],9,10),"Z"),
                   tseq$yyyymmdd[t]),
            ".txt",sep=""))
  } else {
    ffin<-file.path(argv$dirin,
           replaceDate(argv$bdirin,date.str=tseq$yyyymmdd[t],format="%Y%m%d"),
           paste(argv$bfin,
            ifelse(nchar(argv$date_start)==13,tseq$yyyymmddhh[t],tseq$yyyymmdd[t]),
            ".txt",sep=""))

  }
  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    next
  }
  # read data
  data_t<-read.table(file=ffin,header=T,sep=";",
                     stringsAsFactors=F,strip.white=T)
  varidx<-match(elcode_in,names(data_t))
  if (any(is.na(varidx))) 
    boom("at least one of the specified elcode_ins is not in the file")
  # read the first timestep
  if (!exists("meta")) {
    if (argv$netatmo) {
      meta<-data.frame(sourceId=1:length(data_t$lat),
                       lon=data_t$lon,
                       lat=data_t$lat,
                       z=data_t$elev,
                       stringsAsFactors=F)
    } else {
      meta<-data.frame(sourceId=data_t$sourceId,
                       lon=data_t$lon,
                       lat=data_t$lat,
                       x_lcc=data_t$x_lcc,
                       y_lcc=data_t$y_lcc,
                       x_utm33=data_t$x_utm33,
                       y_utm33=data_t$y_utm33,
                       x_laea=data_t$x_laea,
                       y_laea=data_t$y_laea,
                       z=data_t$z,
                       stringsAsFactors=F)
    }
    nmeta<-length(data_t$lat)
    bigar0[1:length(data_t$lat),t,1:nel_in]<-as.matrix(data_t[,varidx])
  # read the 2nd,3rd,... timesteps
  } else {
    # netatmo does not have station ids
    if (argv$netatmo) {
      match_ll<-apply(as.matrix(cbind(data_t$lon,data_t$lat,data_t$elev)),
                      MARGIN=1,
                      FUN=function(x){
                      ix<-which(abs(x[1]-meta$lon)<=argv$tolh)
                      if (length(ix)==0) return(NA)
                      if (length(ix)==1) { 
                        if (x[2]==meta$lat[ix] & x[3]==meta$z[ix])
                          return(ix)
                      }
                      iy<-which(abs(x[2]-meta$lat[ix])<=argv$tolh)
                      if (length(iy)==0) return(NA)
                      iz<-which(abs(x[3]-meta$z[ix[iy]])<=argv$tolz)
                      if (length(iz)==0) return(NA)
                      return(ix[iy[iz[1]]])})
      ix<-which(is.na(match_ll))
      if (length(ix)>0) {
        meta<-rbind(meta,
                    data.frame(sourceId=(nmeta+1):(nmeta+length(ix)),
                               lon=data_t$lon[ix],
                               lat=data_t$lat[ix],
                               z=data_t$elev[ix],
                               stringsAsFactors=F))
        bigar0[(nmeta+1):(nmeta+length(ix)),t,1:nel_in]<-as.matrix(data_t[ix,varidx])
        nmeta<-nmeta+length(ix)
      }
      ix<-which(!is.na(match_ll))
      if (length(ix)>0) 
        bigar0[match_ll[ix],t,1:nel_in]<-as.matrix(data_t[ix,varidx])
      rm(ix,match_ll)
    # kdvh, smhi, fmi use sourceID
    } else {
      ix<-which(!(data_t$sourceId %in% meta$sourceId))
      if (length(ix)>0) {
        meta<-rbind(meta,
                    data.frame(sourceId=data_t$sourceId[ix],
                               lon=data_t$lon[ix],
                               lat=data_t$lat[ix],
                               x_lcc=data_t$x_lcc[ix],
                               y_lcc=data_t$y_lcc[ix],
                               x_utm33=data_t$x_utm33[ix],
                               y_utm33=data_t$y_utm33[ix],
                               x_laea=data_t$x_laea[ix],
                               y_laea=data_t$y_laea[ix],
                               z=data_t$z[ix],
                               stringsAsFactors=F))
        nmeta<-nmeta+length(ix)
      }
      match<-match(data_t$sourceId,meta$sourceId)
      if (any(is.na(match)))
        # should never happen...
        boom("NAs found in match, this is not supposed to happen")
      bigar0[match,t,1:nel_in]<-as.matrix(data_t[,varidx])
      rm(ix,match)
    }
  } 
  rm(data_t,varidx)
  t11<-Sys.time()
  print(paste("hour",tseq$yyyymmddhh[t],
              "/ time=",round(t11-t10,2),attr(t11-t10,"units")))
} # end cycle over time steps
if (!exists("meta")) boom("STOP because of no input data found")
nmeta<-length(meta$sourceId)
if (nmeta==0) boom("STOP because of no data found")
bigar<-array(data=NA,dim=c(nmeta,tseq$n,nel_in))
bigar[1:nmeta,1:tseq$n,1:nel_in]<-bigar0[1:nmeta,1:tseq$n,1:nel_in]
rm(bigar0)
#
#------------------------------------------------------------------------------
# ad-hoc adjustments based on the observation provider
if (argv$netatmo) {
  bigar[bigar==-999]<-NA
}
#
#------------------------------------------------------------------------------
# Spatial Coordinates
if (argv$netatmo) {
  coords<-data.frame(x=c("x_lcc","x_utm33","x_laea"),
                     y=c("y_lcc","y_utm33","y_laea"),
                     proj4=c(
  "+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs",
  "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),
                     stringsAsFactors=F)
  ncrs<-length(coords$x)
  ncoords<-2*length(coords$x)
  nval<-length(meta$lon)
  if (nval>0) {
    coords.val<-as.data.frame(array(data=NA,dim=c(nval,ncoords)))
    # frost eturns coordinates as long-lat
    xy.from<-data.frame(x=meta$lon,
                        y=meta$lat)
    proj4.from<-"+proj=longlat +datum=WGS84"
    for (i in 1:3) {
      ctmp<-coordinates(crs_transform(xy.from=xy.from,
                                      proj4.from=proj4.from,
                                      proj4.to=coords$proj4[i]))
      switch(i,
        "1"={ meta$x_lcc<-ctmp[,1]; meta$y_lcc<-ctmp[,2]},
        "2"={ meta$x_utm33<-ctmp[,1]; meta$y_utm33<-ctmp[,2]},
        "3"={ meta$x_laea<-ctmp[,1]; meta$y_laea<-ctmp[,2]})
    }
    rm(xy.from,ctmp)
  } else {
    print("no metadata from frost")
  }
}
#
#------------------------------------------------------------------------------
# Aggregation
#
# + fill the gaps, only if they last 1 time unit and in between two valid obs
fill_the_gaps<-function(x){
  xorg<-x
  if (any(is.na(x[2:(length(x)-1)]))) {
    y<-x
    y[]<-1
    x[which(is.na(xorg))]<-0
    y[which(is.na(xorg))]<-0
    xfill<-as.vector(filter(x,filter=c(.5,0,.5),sides=2))
    yfill<-as.vector(filter(y,filter=c(1,1,1),sides=2))
    ix<-which(is.na(xorg) & !is.na(xfill) & yfill==2)
    if (length(ix)>0) xorg[ix]<-xfill[ix]
  }
  xorg
}
# apply functions
res<-array(data=NA,dim=c(nmeta,nel_out))
for (eo in 1:nel_out) {
  ei<-which(argv$elcode_link[eo]==elcode_in)
  bigar_less_gaps<-t(apply(bigar[,,ei],FUN=fill_the_gaps,MARGIN=1))
  res[,eo]<-apply(bigar_less_gaps,FUN=argv$ppfun[eo],MARGIN=1)
}
rm(bigar,bigar_less_gaps)
#
#------------------------------------------------------------------------------
# save output 
dirout<-file.path(argv$dirout,
         replaceDate(argv$bdirout,date.str=argv$date_start,format=ford))
dir.create(path=dirout,
           showWarnings=F,
           recursive=T)
if (is.na(argv$bfout)) argv$bfout<-paste0(argv$bfin,"pp_")
ffout<-file.path(dirout,
      paste(argv$bfout,
            ifelse(nchar(argv$date_start)==13,tseq$yyyymmddhh[t],tseq$yyyymmdd[t]),
            ".txt",sep=""))
writeFile()
print(paste("written file",ffout))
#
#------------------------------------------------------------------------------
# Apply functions
t1<-Sys.time()
print(paste(round(t1-t0,2),attr(t1-t0,"units")))
quit(status=0)
