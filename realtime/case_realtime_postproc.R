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
  str<-c("sourceId",
         "lon","lat",
         "x_lcc","y_lcc",
         "x_utm33","y_utm33",
         "x_laea","y_laea",
         "z")
  str<-c(str,ElCode)
  posok<-apply(res,MARGIN=1,FUN=function(x){ifelse(any(!is.na(x)),T,F)})
  ix<-which(posok)
  if (length(ix)>0) {
    daux<-data.frame(meta$sourceId[ix],
                     meta$lon[ix],
                     meta$lat[ix],
                     meta$x_lcc[ix],
                     meta$y_lcc[ix],
                     meta$x_utm33[ix],
                     meta$y_utm33[ix],
                     meta$x_laea[ix],
                     meta$y_laea[ix],
                     meta$z[ix],
                     stringsAsFactors=F)
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

#
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
argv <- parse_args(p)
#
#------------------------------------------------------------------------------
# checks
boom<-function(x) {print(paste("ERROR",x));quit(status=1)}
if ( ((any(is.na(argv$elcode_hourly)) & any(is.na(argv$elcode_diurnal)))) |
     (any(!is.na(argv$elcode_hourly)) & any(!is.na(argv$elcode_diurnal))) ) 
  boom("please specify either --elcode_hourly OR elcode_diurnal") 
if (any(is.na(argv$ppfun))) boom("please specify --ppfun") 
#
if ( argv$date_start==argv$date_stop & 
     is.na(argv$n_prev) & is.na(argv$n_succ)) 
  boom("when date_start = date_stop, n_prev and/or n_succ must be provided")
#
#------------------------------------------------------------------------------
# date/time elaboration
if (nchar(argv$date_start)==13) {
  ford<-"%Y-%m-%dT%H"
  ElCode<-argv$elcode_hourly
  unit<-"hours"
} else if (nchar(argv$date_start)==10) {
  ford<-"%Y-%m-%d"
  ElCode<-argv$elcode_diurnal
  unit<-"days"
}
# number of element to process
nEl<-length(ElCode)
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
for (t in 1:tseq$n) {
  ffin<-file.path(argv$dirin,
            replaceDate(argv$bdirin,date.str=tseq$yyyymmdd[t],format="%Y%m%d"),
      paste(argv$bfin,
            ifelse(nchar(argv$date_start)==13,tseq$yyyymmddhh[t],tseq$yyyymmdd[t]),
            ".txt",sep=""))
  # print(ffin)
  if (!file.exists(ffin)) next
  data_t<-read.table(file=ffin,header=T,sep=";",
                     stringsAsFactors=F,strip.white=T)
  varidx<-match(ElCode,names(data_t))
  if (any(is.na(varidx))) 
    boom("at least one of the specified elcodes is not in the file")
  if (!exists("meta")) {
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
    }
  } 
} # end cycle over time steps
nmeta<-length(meta$sourceId)
if (nmeta==0) boom("STOP because of no data found")
#
#------------------------------------------------------------------------------
# fill the data structure used to aggregate data
bigar<-array(data=NA,dim=c(nmeta,tseq$n,nEl))
for (t in 1:tseq$n) {
  ffin<-file.path(argv$dirin,
            replaceDate(argv$bdirin,date.str=tseq$yyyymmdd[t],format="%Y%m%d"),
      paste(argv$bfin,
            ifelse(nchar(argv$date_start)==13,tseq$yyyymmddhh[t],tseq$yyyymmdd[t]),
            ".txt",sep=""))
  if (!file.exists(ffin)) next
  data_t<-read.table(file=ffin,header=T,sep=";",
                     stringsAsFactors=F,strip.white=T)
  varidx<-match(ElCode,names(data_t))
  if (any(is.na(varidx))) 
    boom("at least one of the specified elcodes is not in the file")
  match<-match(data_t$sourceId,meta$sourceId)
  if (any(is.na(match)))
    # should not happen, perhaps only if input files are changed from the previous loop
    boom("NAs found in match, this is not supposed to happen")
  bigar[match,t,1:nEl]<-as.matrix(data_t[,varidx])
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
res<-array(data=NA,dim=c(nmeta,nEl))
for (e in 1:nEl) {
  bigar_less_gaps<-t(apply(bigar[,,e],FUN=fill_the_gaps,MARGIN=1))
  res[,e]<-apply(bigar_less_gaps,FUN=argv$ppfun[e],MARGIN=1)
}
#
#------------------------------------------------------------------------------
# save output 
dirout<-file.path(argv$dirout,
         replaceDate(argv$bdirout,date.str=argv$date_stop,format=ford))
dir.create(path=dirout,
           showWarnings=F,
           recursive=T)
ffout<-file.path(dirout,
      paste(argv$bfin,
            "pp_",
            ifelse(nchar(argv$date_stop)==13,tseq$yyyymmddhh[t],tseq$yyyymmdd[t]),
            ".txt",sep=""))
writeFile()
print(paste("written file",ffout))
#
#------------------------------------------------------------------------------
# Apply functions
quit(status=0)
