library(basilico)
library(raster)
library(ncdf4io)
# MAIN ========================================================================
options(scipen=999)
# Read command line arguments
arguments <- commandArgs()
arguments
date1.str<-arguments[3] 
date2.str<-arguments[4] 
var<-arguments[5]
#ffconfig<-arguments[6]
date.format<-"%Y.%m.%d"
#
#main<-"/home/cristianl/projects/CASE"
#source(file.path(main,"lib/case_metadata_ffname.R"))
#source(ffconfig)
#ffcasemeta<-case_metadata_ffname(case.conf)
#
#------------------------------------------------------------------------------
Rdate1<-as.POSIXlt(str2Rdate(date1.str,format=date.format))
yyyy<-Rdate1$year+1900
mm<-formatC(Rdate1$mon+1,width=2,flag="0")
dd<-formatC(Rdate1$mday,width=2,flag="0")
hh<-formatC(Rdate1$hour,width=2,flag="0")
#
tseq<-createTimeSeq(date1.str=date1.str,
                    date2.str=date2.str,
                    format=date.format,
                    byDay=T,byHour=F,
                    season=NULL,
                    hourOFday.sel=NULL,
                    dayOFmonth.sel=NULL,
                    N.prev=NULL,
                    N.succ=NULL)
Rtseq<-as.POSIXlt(str2Rdate(tseq$yyyymmdd,format="%Y%m%d"))
#------------------------------------------------------------------------------
for (j in 1:tseq$n) {
  if ((j%%100)==0) print(tseq$yyyymmdd[j])
  ffoet<-file.path("/lustre/storeA/users/oleet/C3S_SURF",
                   var,
                   "ProdData",
                   paste(var,"_",
                         tseq$yyyymmdd[j],
                        ".txt",sep=""))
  if (!file.exists(ffoet)) {
    print(paste("file not found",ffoet))
    next
  }
  ffdqc<-file.path("/lustre/storeB/project/metkl/senorge2/case/case_20180112/aux",
                   paste(var,"_date_dqc",sep=""),
                   tseq$yyyymm[j],
                   paste("case_",var,"_date_",tseq$yyyymmdd[j],".txt",sep=""))
  if (!file.exists(ffdqc)) {
    print(paste("file not found",ffdqc))
    next
  }

  doet<-read.table(file=ffoet,header=T,sep=" ",strip.white=T,stringsAsFactors=F)
  ddqc<-read.table(file=ffdqc,header=T,sep=";",strip.white=T,stringsAsFactors=F)

  m<-match(doet$V1,ddqc$souid)
  ix<-which(!is.na(m))
#  cbind(doet$V1[ix][1:10],ddqc$souid[m[ix]][1:10])
  doet$V5[doet$V5<(-90) | doet$V5>500]<-NA
  ddqc$value[ddqc$value<(-90) | ddqc$value>500]<-NA
  flags<- doet$V5[ix]!=ddqc$value[m[ix]] | 
          (is.na(doet$V5[ix]) & !is.na(ddqc$value[m[ix]]))
  if (any(flags)) {
    print("----")
    print(tseq$yyyymmdd[j])
    
    ixx<-which( flags)
    print(paste("diff ECA",length(ixx)))
    print("souid new old")
    print(cbind(doet$V1[ix][ixx],doet$V5[ix][ixx],ddqc$value[m[ix]][ixx]))
  }
  
  m<-match(doet$V1,ddqc$metnostaid)
  ix<-which(!is.na(m))
#  cbind(doet$V1[ix][1:10],ddqc$souid[m[ix]][1:10])
  flags<- doet$V5[ix]!=ddqc$value[m[ix]] | 
          (is.na(doet$V5[ix]) & !is.na(ddqc$value[m[ix]]))
  if (any(flags)) {
    print("----")
    print(tseq$yyyymmdd[j])
    ixx<-which(flags)
    print(paste("diff MET",length(ixx)))
    print("souid new old")
    print(cbind(doet$V1[ix][ixx],doet$V5[ix][ixx],ddqc$value[m[ix]][ixx]))
  }
} 
#
#------------------------------------------------------------------------------
q(status=0)
