library(basilico)
library(ncdf4io)
options(scipen=999)
# Read command line arguments
arguments <- commandArgs()
var<-arguments[3]
ffconfig<-arguments[4]
source(ffconfig)
ffn<-file.path(case.conf$main,
               paste("case_",case.conf$reldate,sep=""),
               "aux",
               paste("countValidObs_",var,".txt",sep=""))
ffo<-file.path(case.conf$main,
               paste("case_",case.conf$reldate,sep=""),
               "aux",
               paste("countValidObs_",var,".png",sep=""))
t<-read.csv(file=ffn,header=T,stringsAsFactors=F,strip.white=T)
date.format<-"%Y%m%d"
Rdate<-as.POSIXlt(str2Rdate(t$date,format=date.format))
png(file=ffo,width=800,height=800)
#quartzFonts(avenir=c("Avenir Book","Avenir Black","Avenir Book Oblique","Avenir Black Oblique"))
library(showtext)
font_add_google("Gochi Hand", "gochi")
font_add_google("Roboto", "roboto")
font_add_google("Open Sans", "osans")
font_add_google("Schoolbell", "bell")
font_add_google("Covered By Your Grace", "grace")
font_add_google("Rock Salt", "rock")
showtext_auto()
mx<-max(t$nNO+t$nSE+t$nFI)
plot(Rdate,t$nNO+t$nSE+t$nFI,col="gold",pch=19,cex=1.5,ylim=c(0,mx),
     xlab="date",ylab="number of observations",cex.lab=1.8,cex.axis=1.8,
     main=var,cex.main=2,family="osans",mgp=c(2.5,.8,0))
points(Rdate,t$nNO,col="red",pch=19,cex=1.5)
points(Rdate,t$nSE,col="blue",pch=19,cex=1.5)
points(Rdate,t$nFI,col="black",pch=19,cex=1.5)
abline(h=seq(0,10000,by=250),lty=2,col="gray",lwd=1.5)
abline(h=seq(0,10000,by=500),lty=1,col="gray",lwd=1.5)
abline(h=0,lty=1,col="gray",lwd=4)
abline(v=seq(as.POSIXlt(str2Rdate("19500101",format=date.format)),
             as.POSIXlt(str2Rdate("20500101",format=date.format)),
             by="1 year"),lty=2,lwd=1.5,col="gray" )
abline(v=seq(as.POSIXlt(str2Rdate("19500101",format=date.format)),
             as.POSIXlt(str2Rdate("20500101",format=date.format)),
             by="5 year"),lty=1,lwd=1.5,col="gray" )
aux<-"top"
if (var=="RR") aux<-"topright"
legend(x=aux,fill=c("gold","red","blue","black"),
       legend=c("TOT","NO","SE","FI"),horiz=T,cex=2)
dev.off()
q()
