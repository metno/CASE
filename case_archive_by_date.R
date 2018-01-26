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
yyyy.sta<-1950
print(nlf)
#
oval<-matrix(data=NA,ncol=n,nrow=tseq$n)
oqcd<-matrix(data=NA,ncol=n,nrow=tseq$n)
osou<-vector(length=n,mode="numeric")
osta<-vector(length=n,mode="numeric")
oist<-vector(length=n,mode="numeric")
osou[]<-NA
osta[]<-NA
oist[]<-NA
j<-0
for (i in 1:nlf) {
  if ((i%%100)==0) print(paste(i,"/",nlf))
  t<-read.csv(file=lf[i],
              header = TRUE,
              stringsAsFactors = FALSE,
              quote = "",
              strip.white=T,
              fileEncoding = "ISO-8859-1")
  Rtseqt<-as.POSIXlt(str2Rdate(t$date,format="%Y%m%d"))
  indx<-which(Rtseqt %in% Rtseq)
  if (length(indx)==0) next
  k<-which(stations$staid==t$staid[1])
  j<-j+1
  osou[j]<-t$souid[1]
  osta[j]<-t$staid[1]
  oist[j]<-k
  match_bs<-match(Rtseq,Rtseqt)
  oval[1:tseq$n,j]<-t$value[match_bs]
  oqcd[1:tseq$n,j]<-t$qcode[match_bs]
}
#
#------------------------------------------------------------------------------
ffn<-file.path(case.conf$main,
               paste("case_",case.conf$reldate,sep=""),
               "aux",
               paste("countValidObs_",var,".txt",sep=""))
cat(file=ffn,"date,ntot,nNO,nSE,nFI\n",append=F)
for (j in 1:tseq$n) {
  dircase.out.j<-file.path(dircase.out,tseq$yyyymm[j])
  dir.create(dircase.out.j,showWarnings=F,recursive=T)
  ff<-file.path(dircase.out.j,
                paste(case.conf$bnamedata,"_",
                      var,"_",
                      "date_",
                      tseq$yyyymmdd[j],
                      ".txt",sep=""))
  if (!file.exists(ff)) {
    cat(file=ff,append=F,
        paste("staid,metnostaid,souid,cn,z,",
              "lat_dec,lon_dec,etrs_laea_x,",
              "etrs_laea_y,utm33_x,utm33_y,",
              "date,value,qcode\n",sep=""))
  }
  indx<-which(!is.na(osta) & !is.na(oval[j,]))
  out<-data.frame(staid=stations$staid[oist[indx]],
                  metnostaid=stations$metnostaid[oist[indx]],
                  souid=osou[indx],
                  cn=substr(stations$cn[oist[indx]],2,3),
                  z=stations$z[oist[indx]],
                  lat_dec=stations$lat_dec[oist[indx]],
                  lon_dec=stations$lon_dec[oist[indx]],
                  etrs_laea_x=stations$etrs_laea_x[oist[indx]],
                  etrs_laea_y=stations$etrs_laea_y[oist[indx]],
                  utm33_x=stations$utm33_x[oist[indx]],
                  utm33_y=stations$utm33_y[oist[indx]],
                  date=tseq$yyyymmdd[j],
                  value=oval[j,indx],
                  qcode=oqcd[j,indx],
                  stringsAsFactors=F)
  cat(file=ff,append=T,
      paste(
      paste(out$staid,
            out$metnostaid,
            out$souid,
            out$cn,
            out$z,
            out$lat_dec,
            out$lon_dec,
            out$etrs_laea_x,
            out$etrs_laea_y,
            out$utm33_x,
            out$utm33_y,
            out$date,
            out$value,
            out$qcode,
            sep=",",collapse="\n"),"\n",sep=""))
  cat(file=ffn,append=T,
      paste(
      paste(out$date[1],
            length(which(!is.na(out$value) )),
            length(which(!is.na(out$value) & out$cn=="NO")),
            length(which(!is.na(out$value) & out$cn=="SE")),
            length(which(!is.na(out$value) & out$cn=="FI")),
            sep=",",collapse="\n"),"\n",sep=""))
} 
#
#------------------------------------------------------------------------------
q(status=0)
