#+
ecameta_get<-function(conf,par.conf) {
  ffname<-file.path(conf$main,
                    paste("ECA_",conf$reldate,sep=""),
                    "stations.txt")
  if (!file.exists(ffname)) return(NULL)
  data<-read.table(ffname,
                   skip=17,
                   sep=",",
                   header=T,
                   stringsAsFactors=F,
                   strip.white=T,
                   quote="",
                   blank.lines.skip=T)
#> names(ecameta$data)
#[1] "STAID"   "STANAME" "CN"      "LAT"     "LON"     "HGHT"   
  # longlat degree:min:sec 2 longlat dec
  aux<-ECAlatlon2latlondec(data)
  # longlat 2 etrs_laea
  data$lon_dec<-aux[,1]
  data$lat_dec<-aux[,2]
  rm(aux)
  aux<-CRStransf(cbind(data$lon_dec,data$lat_dec),
                 par.conf$proj4.wgs84,
                 par.conf$proj4.ETRS_LAEA)
  data$etrs_laea_x<-aux[,1] 
  data$etrs_laea_y<-aux[,2] 
  # longlat 2 utm33 (where defined, actually even far outside it)
  data$utm33_x<-data$lon_dec
  data$utm33_y<-data$lat_dec
  data$utm33_x[]<-NA
  data$utm33_y[]<-NA
  utm33ok<-which(data$lon_dec>0 & data$lon_dec<40)  
  if (length(utm33ok)>0) {
    aux1<-data$lon_dec[utm33ok]
    aux2<-data$lat_dec[utm33ok]
    aux<-CRStransf(cbind(aux1,aux2),
                   par.conf$proj4.wgs84,
                   par.conf$proj4.utm33)
    data$utm33_x[utm33ok]<-aux[,1]
    data$utm33_y[utm33ok]<-aux[,2]
  }
  #
#> names(data)
# [1] "STAID"       "STANAME"     "CN"          "LAT"         "LON"        
# [6] "HGHT"        "lon_dec"     "lat_dec"     "etrs_laea_x" "etrs_laea_y"
#[11] "utm33_x"     "utm33_y"    
  indx<-which(!is.na(data$LAT) & !is.na(data$LON) &
              !is.na(data$HGHT) &
              !is.na(data$lon_dec) & !is.na(data$lat_dec) &
              !is.na(data$etrs_laea_x) & !is.na(data$etrs_laea_y) )
  data1<-data.frame(staid=as.numeric(data$STAID[indx]),
                    name=as.character(data$STANAME[indx]),
                    cn=as.character(data$CN[indx]),
                    z=as.numeric(data$HGHT[indx]),
                    lat_dec=data$lat_dec[indx],
                    lon_dec=data$lon_dec[indx],
                    etrs_laea_x=data$etrs_laea_x[indx],
                    etrs_laea_y=data$etrs_laea_y[indx],
                    utm33_x=data$utm33_x[indx],
                    utm33_y=data$utm33_y[indx],
                    stringsAsFactors=F)
  return(list(data=data1,
              ffname=ffname))
}

