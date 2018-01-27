library(basil)
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
ffconfig<-arguments[6]
date.format<-"%Y.%m.%d"
#
main<-"/home/cristianl/projects/CASE"
source(file.path(main,"lib/case_metadata_ffname.R"))
source(ffconfig)
ffcasemeta<-case_metadata_ffname(case.conf)
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
# Read case metadata
stations<-read.csv(file=ffcasemeta,
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   quote = "",
                   strip.white=T,
                   fileEncoding = "ISO-8859-1")
names(stations)<-c("count","staid","metnostaid","name","cn","z",
                   "lat_dec","lon_dec","etrs_laea_x","etrs_laea_y",
                   "utm33_x","utm33_y")
n<-length(stations$count)
print(n)
#
stations_alice<-read.csv(file="/disk1/data/database_daily/anag_filled_update.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         quote = "",
                         strip.white=T,
                         fileEncoding = "ISO-8859-1")
#
# files
dircase.in<-file.path(case.conf$main,
                      paste("case_",case.conf$reldate,sep=""),
                      paste(var,"_souid",sep=""))
dircase.out<-file.path(case.conf$main,
                       paste("case_",case.conf$reldate,sep=""),
                       paste(var,"_date",sep=""))
dir.create(dircase.out)
lf<-list.files(path=dircase.in,
               pattern=paste("case_",var,"_souid_",sep=""),
               full.names=T)
nlf<-length(lf)
print(nlf)
#
#oval<-matrix(data=NA,ncol=n,nrow=tseq$n)
#oqcd<-matrix(data=NA,ncol=n,nrow=tseq$n)
osou<-vector(length=n,mode="numeric")
#osta<-vector(length=n,mode="numeric")
#oist<-vector(length=n,mode="numeric")
#osta[]<-NA
#oist[]<-NA
j<-0
ondata.tot<-vector(length=n,mode="numeric")
ondata.par<-vector(length=n,mode="numeric")
oduplicate<-vector(length=n,mode="numeric")
onot2use<-vector(length=n,mode="numeric")
oyyyy1st<-vector(length=n,mode="numeric")
osou[]<-NA
ondata.tot[]<-NA
ondata.par[]<-NA
oduplicate[]<-NA
onot2use[]<-NA
for (i in 1:nlf) {
  if ((i%%100)==0) print(paste(i,"/",nlf))
  t<-read.csv(file=lf[i],
              header = TRUE,
              stringsAsFactors = FALSE,
              quote = "",
              strip.white=T,
              fileEncoding = "ISO-8859-1")
  k<-which(stations$staid==t$staid[1])
  osou[k]<-t$souid[1]
  t$value<-as.numeric(t$value)
  ix<-which(!is.na(t$value) & t$value>-98 & t$value<98)
  if (length(ix)==0) {
    ondata.tot[k]<-0
    ondata.par[k]<-0
  } else {
    Rtseqt<-as.POSIXlt(str2Rdate(t$date[ix],format="%Y%m%d"))
    oyyyy1st[k]<-as.numeric(substr(t$date[ix[1]],1,4))
    indx<-which(Rtseqt %in% Rtseq)
    ondata.tot[k]<-length(t$date[ix])
    ondata.par[k]<-as.integer(100*length(indx)/tseq$n)
  }
  print(paste(t$staid[1],osou[k],ondata.tot[k],ondata.par[k],oyyyy1st[k]))
  if (var=="RR") {
    ka<-which(stations_alice$staid==t$staid[1])
    if (length(ka)>0) {
      oduplicate[k]<-stations_alice$duplicate[ka]
      onot2use[k]<-stations_alice$quality[ka]
    }
  }
#  if (length(indx)==0) next
#  j<-j+1
#  osta[j]<-t$staid[1]
#  oist[j]<-k
#  match_bs<-match(Rtseq,Rtseqt)
#  oval[1:tseq$n,j]<-t$value[match_bs]
#  oqcd[1:tseq$n,j]<-t$qcode[match_bs]
}

if (var=="TG") {
  stations$TGsouid<-osou
  stations$TGtot_obs<-ondata.tot
  stations$TGperc_obs_backto1970<-ondata.par
  stations$TGduplicate<-oduplicate
  stations$TGblacklist<-onot2use
  stations$TGyear_1stObs<-oyyyy1st
} else if (var=="TN") {
  stations$TNsouid<-osou
  stations$TNtot_obs<-ondata.tot
  stations$TNperc_obs_backto1970<-ondata.par
  stations$TNduplicate<-oduplicate
  stations$TNblacklist<-onot2use
  stations$TNyear_1stObs<-oyyyy1st
} else if (var=="TX") {
  stations$TXsouid<-osou
  stations$TXtot_obs<-ondata.tot
  stations$TXperc_obs_backto1970<-ondata.par
  stations$TXduplicate<-oduplicate
  stations$TXblacklist<-onot2use
  stations$TXyear_1stObs<-oyyyy1st
} else if (var=="RR") {
  stations$RRsouid<-osou
  stations$RRtot_obs<-ondata.tot
  stations$RRperc_obs_backto1970<-ondata.par
  stations$RRduplicate<-oduplicate
  stations$RRblacklist<-onot2use
  stations$RRyear_1stObs<-oyyyy1st
}
#
#------------------------------------------------------------------------------
# write it
ffout<-paste(ffcasemeta,".step2",sep="")
write.csv(file=ffout,
          stations,
          fileEncoding="ISO-8859-1",
          row.names=FALSE)
#------------------------------------------------------------------------------
quit(status=0)
