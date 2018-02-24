#!/usr/bin/env Rscript
#
library(basilico)
library(ncdf4io)
library(showtext)
library(argparser)
library(raster)
library(rgdal)
font_add_google("Gochi Hand", "gochi")
font_add_google("Roboto", "roboto")
font_add_google("Open Sans", "osans")
font_add_google("Schoolbell", "bell")
font_add_google("Covered By Your Grace", "grace")
font_add_google("Rock Salt", "rock")
showtext_auto()
options(scipen=999,warns=2)
#------------------------------------------------------------------------------
t0<-Sys.time()
# [] Read command line arguments and/or set parameters to default
# create parser object
p <- arg_parser("plot")
# specify our desired options 
# by default ArgumentParser will add an help option 
p <- add_argument(p, "date",
                  help="date (format YYYY.MM.DD)",
                  type="character")
p <- add_argument(p, "var",
                  help="variable (RR prec, TG mean daily T, TX max, TN min)",
                  type="character")
p <- add_argument(p, "ffconfig",
                  help="case config file",
                  type="character")
#
p <- add_argument(p, "--case.path",
                  help="path to CASE",
                  type="character",
                  default="none")
p <- add_argument(p, "--geoinfo.path",
                  help="path to digital elevation model and grid mask",
                  type="character",
                  default="none")
#
p <- add_argument(p, "--xl",
                  help="min and max Easting coordinate",
                  type="numeric",
                  default=NA,
                  nargs=2)
p <- add_argument(p, "--yl",
                  help="min and max Northing coordinate",
                  type="numeric",
                  default=NA,
                  nargs=2)
#
argv <- parse_args(p)
#------------------------------------------------------------------------------
source(argv$ffconfig)
#------------------------------------------------------------------------------
# set Time-related variables
Rdate <- strptime(argv$date,"%Y.%m.%d")
yyyy<-Rdate$year[1]+1900
mm<-Rdate$mon[1]+1
dd<-Rdate$mday[1]
nday<-1
yyyymmdd<-paste(yyyy,
                formatC(mm,width=2,flag="0"),
                formatC(dd,width=2,flag="0"),sep="")
yyyymm<-paste(yyyy,
              formatC(mm,width=2,flag="0"),sep="")
yyyymmdd0600<-paste(yyyymmdd,"0600",sep="")
#------------------------------------------------------------------------------
# IO
# Read data from CASE 
ffi<-file.path(argv$case.path,
               paste(argv$var,"_date_dqc",sep=""),
               yyyymm,
               paste("case_",argv$var,"_date_",yyyymmdd,".txt",sep="")) 
if (!file.exists(ffi))
  ext<-error_exit(paste("File not found:",ffi))
data<-read.table(file=ffi,header=T,sep=";",
                 stringsAsFactors=F,strip.white=T)
n<-length(data$staid)
ffo<-file.path(case.conf$main,
               paste("case_",case.conf$reldate,sep=""),
               "aux",
               paste("domain_obs_",argv$var,"_",yyyymmdd,".png",sep=""))
#
filenamedem<-file.path(argv$geoinfo.path,"fennodem_NGCD.nc")
if (!file.exists(filenamedem)) 
  ext<-error_exit(paste("File not found:",filenamedem))
filenamemask<-file.path(argv$geoinfo.path,"fennomask_NGCD.nc")
if (!file.exists(filenamemask)) 
  ext<-error_exit(paste("File not found:",filenamemask))
fileborders<-file.path(argv$geoinfo.path,
                       "TM_WORLD_BORDERS_ETRS89_ETRS-LAEA",
                       "TM_WORLD_BORDERS_ETRS89_ETRS-LAEA-0.2.shp")
if (!file.exists(fileborders)) 
  ext<-error_exit(paste("File not found:",fileborders))
#------------------------------------------------------------------------------
# read gridded datasets & borders 
rtmp1<-raster(filenamedem)
rtmp2<-raster(filenamemask)
r.orog<-mask(rtmp1,mask=rtmp2)
crs(r.orog)<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
rm(rtmp1,rtmp2)
save.image("tmp")
borders<-readOGR(fileborders,"TM_WORLD_BORDERS_ETRS89_ETRS-LAEA-0.2")
#------------------------------------------------------------------------------
par<-list(xl=argv$xl,
          yl=argv$yl,
          br=c(-1,0,250,500,1000,1500,2500),
          col=c("cyan",gray.colors(6)[2:6]),
          tstr=c(paste("sea",sep=""),
                 paste("250m",sep=""),
                 paste("500m",sep=""),
                 paste("1000m",sep=""),
                 paste("1500m",sep=""),
                 paste("2500m",sep=""))
          )
dom_and_obs_figure<-function(fig.par,
                             borders, 
                             orog,
                             obs,
                             offsetx=60000,
                             offsety=70000) {
  if (any(is.na(fig.par$xl))) fig.par$xl<-c(xmin(orog),
                                            xmax(orog))  
  if (any(is.na(fig.par$yl))) fig.par$yl<-c(ymin(orog),
                                            ymax(orog))  
  #
  inside<-extract(orog,cbind(obs$x,obs$y))
  ix<-which(!is.na(inside) & !is.na(obs$value))
  #
  orogv<-getValues(orog)
  ixo<-which(!is.na(orogv))
  xy<-xyFromCell(orog,1:ncell(orog))
  # panel below
  par(mar=c(0.1,.5,48.5,7.))
  plot(xy[ixo,1],-orogv[ixo],
       col="darkgray",pch=19,cex=1.,
       xlim=c(fig.par$xl[1]+offsetx,fig.par$xl[2]-offsetx),axes=F,
#       xlim=c(fig.par$xl[1],fig.par$xl[2]),axes=F,
       main="",xlab="",ylab="")
  points(obs$x[ix],-obs$z[ix],col="navyblue",pch=19,cex=1)
  axis(side=4,at=c(-1000,-2000),labels=c("1000m","2000m"),cex=3,las=1)
  abline(h=seq(-3000,3000,by=500),lty=2)
  abline(h=0,lwd=1)
  box()
  # panel on the right
  par(new=T,mar=c(7,48.5,.5,.5))
  plot(orogv[ixo],xy[ixo,2],
       col="darkgray",pch=19,cex=1.,
       ylim=c(fig.par$yl[1]+offsety,fig.par$yl[2]-offsety),axes=F,
#       ylim=c(fig.par$yl[1],fig.par$yl[2]),axes=F,
       main="",xlab="",ylab="")
  points(obs$z[ix],obs$y[ix],col="navyblue",pch=19,cex=1)
  abline(v=seq(0,3000,by=500),lty=2)
  abline(v=0,lwd=1)
  axis(side=1,at=c(2000),labels=c("2000m"),cex=3,las=3)
  box()
  #
  par(new=T,mar=c(7,.5,.5,7))
  image(orog,
        breaks=fig.par$br,
        col=fig.par$col,
        main="",
        xlab="",
        ylab="",
        xlim=fig.par$xl,
        ylim=fig.par$yl,
        cex.main=1.6,
        axes=F)
  box()
  plot(borders,lwd=2,add=T,usePolypath=F)
  legend(x="bottomright",
         fill=rev(fig.par$col),
         legend=rev(fig.par$tstr),
         cex=2)
  points(obs$x[ix],obs$y[ix],pch=17,col="navyblue",cex=1.5)
  points(obs$x[ix],obs$y[ix],pch=2,col="black",cex=1.5)
}
png(file=ffo,width=800,height=800)
print(ffo)
dom_and_obs_figure(fig.par=par,
                   borders=borders, 
                   orog=r.orog,
                   obs=data.frame(x=data$etrs_laea_x,
                                  y=data$etrs_laea_y,
                                  z=data$elev,
                                  value=data$value))
text(x=(xmin(r.orog)+55000),y=(ymax(r.orog)-50000),labels=argv$var,
     cex=2.5,font=2)
text(x=(xmin(r.orog)+180000),y=(ymax(r.orog)-150000),labels=argv$date,
     cex=2.,font=3)
dev.off()
save.image("tmp")

q()
t<-read.csv(file=ffn,header=T,stringsAsFactors=F,strip.white=T)
date.format<-"%Y%m%d"
Rdate<-as.POSIXlt(str2Rdate(t$date,format=date.format))
png(file=ffo,width=800,height=800)
#quartzFonts(avenir=c("Avenir Book","Avenir Black","Avenir Book Oblique","Avenir Black Oblique"))
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
