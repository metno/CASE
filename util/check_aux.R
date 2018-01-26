var<-"RR"
date.str<-"19700101"
#sN2
yyyymm<-substr(date.str,1,6)
if (var=="RR") {
  ffsn2<-paste("/home/cristianl/data/seNorge2/TEMP1d/station_dataset/",yyyymm,"/seNorge_v2_0_TEMP1d_station_",date.str,".txt",sep="")
} else {
  ffsn2<-paste("/home/cristianl/data/seNorge2/TEMP1d/station_dataset/",yyyymm,"/seNorge_v2_0_TEMP1d_station_",date.str,".txt",sep="")
}
tab<-read.csv(file=ffsn2,header=T,sep=";",stringsAsFactors=F,strip.white=T)
indx<-which(!is.na(tab$yo))
staidsn2<-tab$stid[indx]
flagsn2<-vector(length=length(indx),mode="logical")
flagsn2[]<-F
#case
ffstat<-"/home/cristianl/data/case/case_20171001/case_stations_20171001.txt"
sou<-read.csv(ffstat,header=T,stringsAsFactors=F)
indx<-which(!is.na(sou$metnostaid))
cont<-0
totff<-0
tot<-0
for (i in indx) {
  ff<-file.path(
       paste("/home/cristianl/data/case/case_20171001/aux/tmp",var,sep=""),
       paste(var,"NO",formatC(sou$metnostaid[i],width=6,flag="0"),".txt",sep=""))
  if (file.exists(ff)) {
    t<-read.csv(ff,header=T,stringsAsFactors=F,strip.white=T)
#  print(names(t))
    indx<-which(t$DATE==date.str)
#  print(length(indx))
    if (length(indx)>0) cont<-cont+1
    totff<-totff+1
    j<-which(staidsn2==sou$metnostaid[i])
    flagsn2[j]<-T
  }
  tot<-tot+1
}
print(paste(cont,totff,tot))
print(staidsn2[!flagsn2])
q() 
