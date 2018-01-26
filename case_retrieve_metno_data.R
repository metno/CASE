# MAIN ========================================================================
undef<--9999
n.try<-2
# Read command line arguments
arguments <- commandArgs()
arguments
yyyy.sta<-arguments[3]
yyyy.end<-arguments[4]
var<-arguments[5]
ffconfig<-arguments[6]
#
outdir<-"/disk1/data/case/case_20180112"
#
main<-"/home/cristianl/projects/CASE"
source(file.path(main,"lib/case_metadata_ffname.R"))
print(ffconfig)
source(ffconfig)
ffcasemeta<-case_metadata_ffname(case.conf)
#
#1234567890
#yyyy.mm.dd
mm.sta<-"01"
dd.sta<-"01"
hh.sta<-"12"
mm.end<-"12"
dd.end<-"31"
hh.end<-"18"
#yyyymm<-paste(yyyy,mm,sep="")
#yyyymmdd<-paste(yyyymm,dd,sep="")
#------------------------------------------------------------------------------
date.sta<-strptime(paste(yyyy.sta,mm.sta,dd.sta,hh.sta,sep="."),"%Y.%m.%d.%H","UTC")
date.end<-strptime(paste(yyyy.end,mm.end,dd.end,hh.end,sep="."),"%Y.%m.%d.%H","UTC")
date.seq.sta<-as.POSIXlt(seq(as.POSIXlt(date.sta),as.POSIXlt(date.end),by="1 year"),"UTC")
date.seq.end<-rev(as.POSIXlt(seq(as.POSIXlt(date.end),as.POSIXlt(date.sta),by="-1 year"),"UTC"))
print(date.seq.sta)
print(date.seq.end)
n.seq<-length(date.seq.end)
yyyy.seq.sta<-date.seq.sta$year+1900
mm.seq.sta<-date.seq.sta$mon+1
dd.seq.sta<-date.seq.sta$mday
hh.seq.sta<-date.seq.sta$hour
yyyy.seq.end<-date.seq.end$year+1900
mm.seq.end<-date.seq.end$mon+1
dd.seq.end<-date.seq.end$mday
hh.seq.end<-date.seq.end$hour
print(paste(date.seq.sta,date.seq.end,"\n"))
#------------------------------------------------------------------------------
# Read Station Information 
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
#------------------------------------------------------------------------------
# define Vectors and Matrices
staid<-vector(mode="numeric",length=n)
staid<-as.numeric(as.vector(unique(stations$metnostaid[which(!is.na(stations$metnostaid))])))
n<-length(staid)
print(staid)
#------------------------------------------------------------------------------
#  1587,106924,19950101,-9999,    9
dd.mm.yyyy.sta<-paste(formatC(dd.seq.sta[1],width=2,flag="0",format="d"),".",
                      formatC(mm.seq.sta[1],width=2,flag="0",format="d"),".",
                      yyyy.seq.sta[1],sep="")
dd.mm.yyyy.end<-paste(formatC(dd.seq.end[n.seq],width=2,flag="0",format="d"),".",
                      formatC(mm.seq.end[n.seq],width=2,flag="0",format="d"),".",
                      yyyy.seq.end[n.seq],sep="")
if (var=="TG") varmetno<-"TAMRR"
if (var=="RR") varmetno<-"RR"
if (var=="TN") varmetno<-"TAN"
if (var=="TX") varmetno<-"TAX"
if (var=="TN12") varmetno<-"TAN_12"
if (var=="TX12") varmetno<-"TAX_12"
if (var=="TG") re<-"14"
if (var=="RR") re<-"14"
if (var=="TN") re<-"14"
if (var=="TX") re<-"14"
if (var=="TN12") re<-"17"
if (var=="TX12") re<-"17"
strheader<-paste("STAID,SOUID,DATE,",var,",Q_",var,"\n",sep="")
tmpVAR<-file.path(outdir,paste("tmp",var,"/",var,"NO",sep=""))
#
for (s in 1:n) {
  flag<-F
  print(paste("@@@",staid[s]))
  if (is.na(staid[s])) next
  fname<-paste(tmpVAR,formatC(staid[s],width=6,flag="0",format="d"),".txt",sep="")
#
#  ulric<-paste("http://klapp.oslo.dnmi.no/metnopub/production/",
  ulric<-paste("http://klapp/metnopub/production/",
               "metno?re=",re,"&p=",varmetno,"&fd=",dd.mm.yyyy.sta,"&td=",dd.mm.yyyy.end,
               "&nob=0.0&ddel=dot&del=semicolon&nmt=0",
               "&ct=text/plain&split=1&nod=",undef,"&s=",staid[s],sep="")
#print(ulric)
  o.cont<-1
  while (o.cont<=n.try) {
    o<-NULL
    try(o <- read.table(ulric, header = TRUE,  sep = ";", #nrows = nrows,
            stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1",
              encoding = "UTF-8", quote = "",na.string=-999))
    if (length(o$Stnr)<5) {
      o.cont<-o.cont+1
      Sys.sleep(1)
    } else {
      break
    }
  }
  if (o.cont>n.try) {
    print(paste("+ ",dd.mm.yyyy.sta," -~-->",dd.mm.yyyy.end,"- Uhu?",sep=" "))
    if (flag) {
      cat(file=fname,strheader,append=F)
      cat(file=fname,
          paste(staid[s],",",staid[s],",",yyyymmdd.seq,",",undef,",",9,"\n",sep=""),
          append=T)
    }
    next
  }
  aux.var<-vector(length=length(o$Year))
  if (var=="TG") {
    o$TAMRR[is.na(o$TAMRR)]<-undef
    o$TAMRR[o$TAMRR=="x"]<-undef
    o$TAMRR[!is.numeric(o$TAMRR)]<-undef
    aux.var<-as.numeric(o$TAMRR)
    aux.var[(aux.var<(-80)) | (aux.var>(50))]<-undef
  }
  if (var=="RR") {
    o$RR[is.na(o$RR)]<-undef
    o$RR[o$RR=="x"]<-undef
    o$RR[!is.numeric(o$RR)]<-undef
    aux.var<-as.numeric(o$RR)
    aux.var[(aux.var<0) | (aux.var>800)]<-undef
  }
  if (var=="TN") {
    o$TAN[is.na(o$TAN)]<-undef
    o$TAN[o$TAN=="x"]<-undef
    o$TAN[!is.numeric(o$TAN)]<-undef
    aux.var<-as.numeric(o$TAN)
    aux.var[(aux.var<(-80)) | (aux.var>50)]<-undef
  }
  if (var=="TX") {
    o$TAX[is.na(o$TAX)]<-undef
    o$TAX[o$TAX=="x"]<-undef
    o$TAX[!is.numeric(o$TAX)]<-undef
    aux.var<-as.numeric(o$TAX)
    aux.var[(aux.var<(-80)) | (aux.var>50)]<-undef
  }
  if (var=="TX12") {
    o$TAX_12[is.na(o$TAX_12)]<-undef
    o$TAX_12[o$TAX_12=="x"]<-undef
    o$TAX_12[!is.numeric(o$TAX_12)]<-undef
    aux.var<-as.numeric(o$TAX_12)
    aux.var[(aux.var<(-80)) | (aux.var>50)]<-undef
  }
  if (var=="TN12") {
    o$TAN_12[is.na(o$TAN_12)]<-undef
    o$TAN_12[o$TAN_12=="x"]<-undef
    o$TAN_12[!is.numeric(o$TAN_12)]<-undef
    aux.var<-as.numeric(o$TAN_12)
    aux.var[(aux.var<(-80)) | (aux.var>50)]<-undef
  }

  indx<-which(aux.var!=undef)
  if (length(indx)==0) next
  v<-vector(length=length(indx))
  y.v<-vector(length=length(indx))
  m.v<-vector(length=length(indx))
  d.v<-vector(length=length(indx))
  v<-aux.var[indx]
  y.v<-o$Year[indx]
  m.v<-o$Month[indx]
  d.v<-o$Day[indx]
# DQC flag
#  ulric<-paste("http://klapp.oslo.dnmi.no/metnopub/production/",
  ulric<-paste("http://klapp/metnopub/production/",
               "metno?re=",re,"&p=",varmetno,"&fd=",dd.mm.yyyy.sta,"&td=",dd.mm.yyyy.end,
               "&nob=0.0&ddel=dot&del=semicolon&nmt=0",
               "&ct=text/plain&split=1&nod=",undef,"&flag=10&s=",staid[s],sep="")
#print(ulric)
  q.cont<-1
  while (q.cont<=n.try) {
    q<-NULL
    try(q <- read.table(ulric, header = TRUE,  sep = ";", #nrows = nrows,
            stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1",
              encoding = "UTF-8", quote = "",na.string=-999))
    if (length(q$Stnr)<5) {
      q.cont<-q.cont+1
      Sys.sleep(1)
    } else {
      break
    }
  }
  if (q.cont>n.try) {
    print(paste("+ ",dd.mm.yyyy.sta," -~-->",dd.mm.yyyy.end,"- NoNoNo!",sep=" "))
  } else {
    aux.flag<-vector(length=length(q$Year))
    if (var=="TG") {
      q$TAMRR[is.na(q$TAMRR)]<-undef
      q$TAMRR[q$TAMRR=="x"]<-undef
      q$TAMRR[!is.numeric(q$TAMRR)]<-undef
      aux.flag<-as.numeric(q$TAMRR)
    }
    if (var=="RR") {
      q$RR[is.na(q$RR)]<-undef
      q$RR[q$RR=="x"]<-undef
      q$RR[!is.numeric(q$RR)]<-undef
      aux.flag<-as.numeric(q$RR)
    }
    if (var=="TN") {
      q$TAN[is.na(q$TAN)]<-undef
      q$TAN[q$TAN=="x"]<-undef
      q$TAN[!is.numeric(q$TAN)]<-undef
      aux.flag<-as.numeric(q$TAN)
    }
    if (var=="TX") {
      q$TAX[is.na(q$TAX)]<-undef
      q$TAX[q$TAX=="x"]<-undef
      q$TAX[!is.numeric(q$TAX)]<-undef
      aux.flag<-as.numeric(q$TAX)
    }
    if (var=="TN12") {
      q$TAN_12[is.na(q$TAN_12)]<-undef
      q$TAN_12[q$TAN_12=="x"]<-undef
      q$TAN_12[!is.numeric(q$TAN_12)]<-undef
      aux.flag<-as.numeric(q$TAN_12)
    }
    if (var=="TX12") {
      q$TAX_12[is.na(q$TAX_12)]<-undef
      q$TAX_12[q$TAX_12=="x"]<-undef
      q$TAX_12[!is.numeric(q$TAX_12)]<-undef
      aux.flag<-as.numeric(q$TAX_12)
    }
    indx<-which(aux.flag!=undef)
    flag.tmp<-vector(length=length(indx))
    y.f<-vector(length=length(indx))
    m.f<-vector(length=length(indx))
    d.f<-vector(length=length(indx))
    flag.tmp<-aux.flag[indx]
    y.f<-q$Year[indx]
    m.f<-q$Month[indx]
    d.f<-q$Day[indx]
    #
    date.v<-strptime(paste(y.v,m.v,d.v,sep="."),"%Y.%m.%d","UTC")
    date.f<-strptime(paste(y.f,m.f,d.f,sep="."),"%Y.%m.%d","UTC")
    match.date<-match(date.f,date.v)
    aux<-which(!is.na(match.date))
    m.date<-match.date[aux]
    flag.aux<-flag.tmp[aux]
    flag<-vector(length=length(date.v))
    flag[]<-rep(NA,length(date.v))
    flag[m.date]<-flag.aux
    #
#    v[flag>2]<-undef
  }
  indx.ok<-which(v!=undef)
  #
  n.ok<-length(indx.ok)
  v.ok<-v[indx.ok]
  y.ok<-y.v[indx.ok]
  m.ok<-m.v[indx.ok]
  d.ok<-d.v[indx.ok]
  #
  cat(file=fname,strheader,append=F)
  out<-data.frame(matrix(nrow=n.ok,ncol=6))
  names(out)<-c("Stnr","Year","Month","Day","var","DQC")
  out$Stnr[]<-as.numeric(staid[s])
  yyyymmdd.seq<-paste(y.ok, 
                      formatC(m.ok,width=2,flag="0",format="d"),
                      formatC(d.ok,width=2,flag="0",format="d"),
                      sep="")
  out$var<-v.ok*10
  out$DQC[]<-flag[indx.ok]
  cat(file=fname,
      paste(out$Stnr,",",out$Stnr,",",yyyymmdd.seq,",",
            round(out$var,0),",",out$DQC,"\n",sep=""),
      append=T)
  warnings()
  print("data saved")
}
q()
