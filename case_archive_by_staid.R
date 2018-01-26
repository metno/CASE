# MAIN ========================================================================
options(scipen=999)
# Read command line arguments
arguments <- commandArgs()
arguments
var<-arguments[3]
ffconfig<-arguments[4]
#
main<-"/home/cristianl/projects/CASE"
source(file.path(main,"lib/case_metadata_ffname.R"))
source(ffconfig)
ffcasemeta<-case_metadata_ffname(case.conf)
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
# read ECA sources.txt
direca<-file.path(eca.conf$main,
                  paste("ECA_",eca.conf$reldate,sep=""),
                  var)
ffsources<-file.path(direca,"sources.txt")
if (!file.exists(ffsources)) {
  print("Error: file not found")
  print(ffsources)
  quit(status=1)
}
sources<-read.csv(file=ffsources,
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  quote = "",
                  strip.white=T,
                  fileEncoding = "ISO-8859-1",
                  skip=22)
print(names(sources))
#
# set output dir and initialize variables
dircase<-file.path(case.conf$main,
                   paste("case_",case.conf$reldate,sep=""),
                   paste(var,"_souid",sep=""))
dir.create(dircase)
staid<-vector()
souid<-vector()
souid_tot<-vector()
metnostaid<-vector()
#
# ECA files
lf<-list.files(path=direca,
               pattern=paste(var,"_SOUID",sep=""),
               full.names=T)
nlf<-length(lf)
print(nlf)
# k counts the number of sources (ECA+METno)
k<-0
for (i in 1:nlf) {
  print(paste("ECA",lf[i]))
  t<-read.csv(file=lf[i],
              header = TRUE,
              stringsAsFactors = FALSE,
              quote = "",
              strip.white=T,
              fileEncoding = "ISO-8859-1",
              skip=18)
  souid_tot[i]<-t$SOUID[1]
  # very short time series are not included
  if (length(t$STAID)<10) next
  names(t)<-c("staid","souid","date","value","qcode")
  # stations not included in case metadata are not included
  if (!(t$staid[1] %in% stations$staid)) next
  # stations only with NAs are not included in case
  if (!(any(t$value!=eca.conf$mv))) next
  # j links the current station to the case metadata
  j<-which(t$staid[1]==stations$staid)
  # if it is a case station but not a Norwegian one, then
  #  write the file in case
  if (is.na(stations$metnostaid[j])) {
    # output file
    ffname<-file.path(dircase,
                      paste(case.conf$bnamedata,"_",
                            var,"_",
                            "souid_",
                            formatC(t$souid[1],width=6,flag="0"),
                            ".txt",sep=""))
    indx<-1:length(t$staid)
    if (any(t$value==eca.conf$mv)) indx<-which(t$value!=eca.conf$mv)
    # adapt date to the Norwegian standards
    date.aux<-strptime(paste(t$date[indx],"12",sep=""),"%Y%m%d%H","UTC")
    if (var %in% c("RR","TG")) {
      date<-date.aux+(3600*24)
    } else {
      date<-date.aux
    }
    date.POSIXlt<-as.POSIXlt(date)
    yyyy<-date.POSIXlt$year+1900
    mm<-date.POSIXlt$mon+1
    dd<-date.POSIXlt$mday
    yyyymmdd<-paste(yyyy,
                    formatC(mm,width=2,flag="0",format="d"),
                    formatC(dd,width=2,flag="0",format="d"),
                    sep="")
    out<-data.frame(staid=t$staid[indx],
                    metnostaid=rep(NA,length=length(indx)),
                    souid=t$souid[indx],
                    date=yyyymmdd,
                    value=round(t$value[indx]/10,1),
                    qcode=t$qcode[indx],
                    stringsAsFactors=F)
    cat(file=ffname,"staid,metnostaid,souid,date,value,qcode\n",append=F)
    cat(file=ffname,
        paste(paste(out$staid,
                    out$metnostaid,
                    out$souid,
                    out$date,
                    out$value,
                    out$qcode,
                    sep=",",collapse="\n"),"\n",sep=""),
        append=T)
    k<-k+1
    staid[k]<-t$staid[1]
    souid[k]<-t$souid[1]
    metnostaid[k]<-NA
    print(paste("ECA",t$staid[1],"/",t$souid[1],
                "saved in file",ffname))
  # if it is a case station and a Norwegian one, then
  #  do not write the file in case
  } else {
    print(paste("ECA",t$staid[1],"=METno",stations$metnostaid[j]))
  }
}
#
# METno files
souidmx<-max(souid_tot)
vecaux<-unique(stations$metnostaid[which(!is.na(stations$metnostaid))])
n<-length(vecaux)
kk<-0
for (i in 1:n) {
  # input file
  ff<-file.path(metno.conf$main,
                paste("case_",metno.conf$case_reldate,sep=""),
                "aux",
                paste("tmp",var,sep=""),
                paste(var,"NO",
                      formatC(vecaux[i],width=6,flag="0"),
                      ".txt",sep=""))
  print(paste("MET",ff))
  if (!file.exists(ff)) {
    print("Warning: file not foud")
    print(ff)
    next
  }
  print(ff)
  t<-read.csv(file=ff,
              header = TRUE,
              stringsAsFactors = FALSE,
              quote = "",
              strip.white=T,
              fileEncoding = "ISO-8859-1")
  if (length(t$STAID)<10) {
    print("Warning: file with less than 10 lines")
    print(ff)
    next
  }
  names(t)<-c("staid","souid","date","value","qcode")
  kk<-kk+1
  souid_cur<-souidmx+kk
  j<-which(t$staid[1]==stations$metnostaid)
  print(length(j))
  if (length(j)>1) q()
  k<-k+1
  staid[k]<-stations$staid[j]
  souid[k]<-souid_cur
  metnostaid[k]<-t$staid[1]
  out<-data.frame(staid=rep(staid[k],length=length(t$value)),
                  metnostaid=t$staid,
                  souid=rep(souid_cur,length=length(t$value)),
                  date=t$date,
                  value=round(t$value/10,1),
                  qcode=t$qcode,
                  stringsAsFactors=F)
  # output file
  ffname<-file.path(dircase,
                    paste(case.conf$bnamedata,"_",
                          var,"_",
                          "souid_",
                          formatC(souid_cur,width=6,flag="0",format="d"),
                          ".txt",sep=""))
  cat(file=ffname,"staid,metnostaid,souid,date,value,qcode\n",append=F)
  cat(file=ffname,
      paste(paste(out$staid,
                  out$metnostaid,
                  out$souid,
                  out$date,
                  out$value,
                  out$qcode,
                  sep=",",collapse="\n"),"\n",sep=""),
      append=T)
  print(paste("MET no",t$staid[1],"/",t$souid[1],
              "saved in file",ffname))
}
#
#
print(k)
out<-data.frame(staid=staid[1:k],
                souid=souid[1:k],
                metnostaid=metnostaid[1:k])
ffname<-file.path(dircase,paste("sources.txt"))
cat(file=ffname,"staid,metnostaid,souid\n",append=F)
cat(file=ffname,
    paste(paste(out$staid,
                out$metnostaid,
                out$souid,
                sep=",",collapse="\n"),"\n",sep=""),
    append=T)
#
#
q(status=0)
