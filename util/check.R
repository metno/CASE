var<-"RR"
date.str<-"19700101"
ffsource<-paste("/home/cristianl/data/case/case_20171001/",var,"_souid/sources.txt",sep="")
sou<-read.csv(ffsource,header=T,stringsAsFactors=F)
indx<-which(!is.na(sou$metnostaid))
cont<-0
tot<-0
for (i in indx) {
  ff<-file.path(paste("/home/cristianl/data/case/case_20171001/",var,"_souid",sep=""),paste("case_",var,"_souid_",sou$souid[i],".txt",sep=""))
  t<-read.csv(ff,header=T,stringsAsFactors=F,strip.white=T)
#  print(names(t))
  indx<-which(t$date==date.str)
#  print(length(indx))
  if (length(indx)>0) cont<-cont+1
  tot<-tot+1
}
print(paste(cont,tot))
q() 
