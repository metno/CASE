library(raster)
library(rgdal)
#==============================================================================
#=========================  MAIN  =============================================
#==============================================================================
config<-"/disk1/data/case/case_20180112/etc/case.conf"
#
# read config file
source(config)
#
main<-"/home/cristianl/projects/CASE"
source(file.path(main,"lib/CRStransf.R"))
source(file.path(main,"lib/ECAlatlon2latlondec.R"))
source(file.path(main,"lib/ecameta_get.R"))
source(file.path(main,"lib/metnometa_get.R"))
source(file.path(main,"lib/case_metadata_ffname.R"))
#
# read metadata
ecameta<-ecameta_get(eca.conf,par.conf)
metnometa<-metnometa_get(metno.conf,par.conf)
metnostaid<-ecameta$data$staid
metnostaid[]<-NA
neca<-length(ecameta$data$staid)
nmetno<-length(metnometa$data$staid)
#
# prepare for merging
# used (metno-vector) T=eca-metno station match found
#                     F=eca-metno station match not found
used<-vector(mode="logical",length=nmetno)
used[]<-F
# reallyin (eca-vector) T=case-station F=non-case-station
reallyin<-vector(mode="logical",length=neca)
reallyin[]<-F
# if station in NO,SE,FI and not too far from the mainland then
#  it is suitable for case
iin<-which( (ecameta$data$cn=="NO"|
             ecameta$data$cn=="SE"|
             ecameta$data$cn=="FI") &
            (!is.na(ecameta$data$utm33_x) | 
             !is.na(ecameta$data$utm33_y)) &
            (ecameta$data$cn %in% c("NO","SE","FI") & ecameta$data$lon_dec>2.5) &
            ecameta$data$lat_dec<=72 )
reallyin[iin]<-T
#
# cycle over eca-Norwegian station to find matches with metno stations
#  output includes non-matching stations (for diagnostics)
potmatch<-0
for (i in 1:neca) {
  if (reallyin[i] & ecameta$data$cn[i]=="NO" ) {
    print("+-------------------------------------------------------------------")
    disthkm<-sqrt((ecameta$data$etrs_laea_x[i]-metnometa$data$etrs_laea_x)**2+
                  (ecameta$data$etrs_laea_y[i]-metnometa$data$etrs_laea_y)**2)/1000
    if (min(disthkm)>200) {
      reallyin[i]<-F
      print("Warning: eca Norwegian station too far from Norwegian mainland met station")
      print(paste("eca station:",ecameta$data$name[i],";",ecameta$data$staid[i],sep=""))
      next
    }
    potmatch<-potmatch+1
    distz<-abs(ecameta$data$z[i]-metnometa$data$z)
    match<-which(disthkm<par.conf$match_tol_x & distz<par.conf$match_tol_z)
    if (length(match)>0) {
      if (length(match)>1) {
        ii<-which.min(disthkm[match])
        match<-match[ii]
      }
      if (used[match]) {
        print("Error: two met stations coincide with the same eca stations")
        print(paste("eca:",ecameta$data$name[i],";",ecameta$data$staid[i],sep=""))
        print(paste("metno 1:",metnostaid[i],sep=""))
        print(paste("metno 2:",metnometa$data$name[match],";",metnometa$data$staid[match],sep=""))
        quit(status=1)
      }
      print(paste("matchOK"))
      print(paste("eca",ecameta$data$name[i],";",ecameta$data$staid[i]))
      print(paste("metno",metnometa$data$name[match],";",metnometa$data$staid[match]))
      metnostaid[i]<-metnometa$data$staid[match]
      used[match]<-T
    } else {
      if (length(which(disthkm<par.conf$match_tol_x & distz>=par.conf$match_tol_z))>0){ 
        match<-which(disthkm<par.conf$match_tol_x & distz>=par.conf$match_tol_z)
        print("Warning: met station coincide with the eca station, except for altitude that is different")
        print(paste("eca:",ecameta$data$name[i],";",ecameta$data$z[i],sep=""))
        print(paste("metno:",metnometa$data$name[match],";",round(metnometa$data$z[match],1),sep=""))
      } else {
        print("match not found")
        print(paste("eca:",ecameta$data$name[i],";",ecameta$data$z[i],sep=""))
        print(paste("dist min with metno:",round(min(disthkm),2),";",round(min(distz),1),sep=""))
        reallyin[i]<-F
      }
    }
  }
} # end cycle over eca-Norwegian station to find matches with metno stations
#
# eca stations not in NO,SE,FI
aux<-vector()
indx<-which(reallyin)
for (i in 1:neca) {
  if (!reallyin[i] & 
      (!(ecameta$data$cn[i] %in% c("NO","SE","FI"))) &
      ecameta$data$lat_dec[i]<=72 &
      !is.na(ecameta$data$utm33_x[i]) & !is.na(ecameta$data$utm33_y[i])) {
#    print("+-------------------------------------------------------------------")
    disthkm<-sqrt((ecameta$data$etrs_laea_x[i]-ecameta$data$etrs_laea_x[indx])**2+
                  (ecameta$data$etrs_laea_y[i]-ecameta$data$etrs_laea_y[indx])**2)/1000
    if (min(disthkm)<par.conf$maxdisthfromNOSEFI) {
      reallyin[i]<-T
    }
  }
}
#
# set case metadata
indxe<-which(reallyin)
indxm<-which(!used)
nuse<-length(indxm)
casemeta<-data.frame(staid=round(c(ecameta$data$staid[indxe],
                                 max(ecameta$data$staid)+1:nuse),0),
                     metnostaid=round(c(metnostaid[indxe],
                                      metnometa$data$staid[indxm]),0),
                     name=c(ecameta$data$name[indxe],
                            metnometa$data$name[indxm]),
                     cn=c(ecameta$data$cn[indxe],
                          rep("NO",nuse)),
                     z=round(c(ecameta$data$z[indxe],
                             metnometa$data$z[indxm]),1),
                     lat_dec=round(c(ecameta$data$lat_dec[indxe],
                                   metnometa$data$lat_dec[indxm]),6),
                     lon_dec=round(c(ecameta$data$lon_dec[indxe],
                                   metnometa$data$lon_dec[indxm]),6),
                     etrs_laea_x=round(c(ecameta$data$etrs_laea_x[indxe],
                                       metnometa$data$etrs_laea_x[indxm]),0),
                     etrs_laea_y=round(c(ecameta$data$etrs_laea_y[indxe],
                                   metnometa$data$etrs_laea_y[indxm]),0),
                     utm33_x=round(c(ecameta$data$utm33_x[indxe],
                                   metnometa$data$utm33_x[indxm]),0),
                     utm33_y=round(c(ecameta$data$utm33_y[indxe],
                                   metnometa$data$utm33_y[indxm]),0))
#
# output
#png(file="prova_etrs_laea.png",height=800,width=800)
#plot(casemeta$etrs_laea_x,casemeta$etrs_laea_y)
#dev.off()
#png(file="prova_utm33.png",height=800,width=800)
#plot(casemeta$utm33_x,casemeta$utm33_y)
#dev.off()
#png(file="prova_longlat.png",height=800,width=800)
#plot(casemeta$lon_dec,casemeta$lat_dec)
#dev.off()
#print(length(which(ecameta$data$cn=="NO")))
#print(length(which(used)))
#print(potmatch)
ffout<-case_metadata_ffname(case.conf)
write.csv(file=ffout,
          casemeta,
          fileEncoding="ISO-8859-1")
print("Metadata saved on file:")
print(ffout)
quit(status=0)
