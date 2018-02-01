library(basilico)
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
main<-"/home/senorge2/projects/CASE"
source(file.path(main,"lib/case_metadata_ffname.R"))
source(ffconfig)
ffcasemeta<-case_metadata_ffname(case.conf)
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
#
dircase<-file.path(case.conf$main,
                   paste("case_",case.conf$reldate,sep=""),
                   paste(var,"_souid",sep=""))
ffname<-file.path(dircase,paste("sources.txt"))
sources<-read.table(file=ffname,header=T,sep=",",stringsAsFactors=F,strip.white=T)
n<-length(sources$souid)
print(n)
#
# files
dircase.in<-file.path(case.conf$main,
                      paste("case_",case.conf$reldate,sep=""),
                      "aux",
                      paste(var,"_date_dqc",sep=""))
dircase.out<-file.path(case.conf$main,
                       paste("case_",case.conf$reldate,sep=""),
                      "aux",
                       paste(var,"_souid_dqc",sep=""))
dir.create(dircase.out)
#
oval<-array(data=NA,dim=c(n,tseq$n))
omet<-array(data=NA,dim=c(n,tseq$n))
oqcd<-array(data=NA,dim=c(n,tseq$n))
oqca<-array(data=NA,dim=c(n,tseq$n))
osou<-array(data=NA,dim=c(n,tseq$n))
osta<-array(data=NA,dim=c(n,tseq$n))
oist<-array(data=NA,dim=c(n,tseq$n))
#
for (t in 1:tseq$n) {
  if ((t%%100)==0) print(paste("I'm here",tseq$yyyymm[t]))
  ffin<-file.path(dircase.in,
                  tseq$yyyymm[t],
                  paste("case_",var,"_date_",tseq$yyyymmdd[t],".txt",sep=""))
  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    next
  }
  data<-read.table(file=ffin,header=T,sep=";",stringsAsFactors=T,strip.white=T)
# [1] "staid"       "metnostaid"  "souid"       "cn"          "elev"       
# [6] "lat"         "lon"         "etrs_laea_x" "etrs_laea_y" "utm33_x"    
#[11] "utm33_y"     "date"        "value"       "qcode"       "prid"       
#[16] "dqc"         "sct"         "rep" 
#  print(names(data))
  m<-match(as.numeric(sources$souid),as.numeric(data$souid))
  ix<-which(!is.na(m))
  if (length(ix)!=length(data$souid)) {
    print("what?!")
    quit(status=1)
  }
#  print(cbind(sources$souid[ix],data$souid[m[ix]]))
  osta[ix,t]<-data$staid[m[ix]]
  omet[ix,t]<-data$metnostaid[m[ix]]
  osou[ix,t]<-data$souid[m[ix]]
  oval[ix,t]<-data$value[m[ix]]
  oqcd[ix,t]<-data$qcode[m[ix]]
  oqca[ix,t]<-data$dqc[m[ix]]
}
# write output
percobs<-vector(length=n)
percerr<-vector(length=n)
percobs[]<-NA
percerr[]<-NA
for (s in 1:n) {
  if (any(!is.na(oval[s,]))) {
    ffname<-file.path(dircase.out,
                      paste(case.conf$bnamedata,"_",
                            var,"_",
                            "souid_",
                            formatC(sources$souid[s],width=6,flag="0"),
                            ".txt",sep=""))
    print(ffname)
    ix<-which(!is.na(oval[s,]))
    out<-data.frame(staid=osta[s,ix],
                    metnostaid=omet[s,ix],
                    souid=osou[s,ix],
                    date=tseq$yyyymmdd[ix],
                    value=oval[s,ix],
                    qcode=oqcd[s,ix],
                    dqc=oqca[s,ix],
                    stringsAsFactors=F)
    iy<-which(!is.na(oval[s,]) & (out$dqc!=0 | out$qcode>2) )
    percobs[s]<-round(100*(length(ix)/tseq$n),0)
    percerr[s]<-round(100*(length(iy)/tseq$n),2)
    cat(file=ffname,"staid,metnostaid,souid,date,value,qcode,dqc\n",append=F)
    cat(file=ffname,
        paste(paste(out$staid,
                    out$metnostaid,
                    out$souid,
                    out$date,
                    out$value,
                    out$qcode,
                    out$dqc,
                    sep=",",collapse="\n"),"\n",sep=""),
        append=T)

  }
}
#
out<-data.frame(staid=sources$staid[1:n],
                souid=sources$souid[1:n],
                metnostaid=sources$metnostaid[1:n],
                percobs=percobs[1:n],
                percerr=percerr[1:n])
ffname<-file.path(dircase.out,paste("sources.txt"))
cat(file=ffname,"staid,metnostaid,souid,percobs,percerr\n",append=F)
cat(file=ffname,
    paste(paste(out$staid,
                out$metnostaid,
                out$souid,
                out$percobs,
                out$percerr,
                sep=",",collapse="\n"),"\n",sep=""),
    append=T)
#
q(status=0)
