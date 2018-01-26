#+
ECAlatlon2latlondec<-function(xy) {
  lat_dec<-as.numeric(substr(xy$LAT,2,3))+
           as.numeric(substr(xy$LAT,5,6))/60+
           as.numeric(substr(xy$LAT,8,9))/3600
  i<-which(substr(xy$LAT,1,1)=="-")
  if (length(i)>0) lat_dec[i]<-(-lat_dec[i])
  lon_dec<-as.numeric(substr(xy$LON,2,4))+
           as.numeric(substr(xy$LON,6,7))/60+
           as.numeric(substr(xy$LON,9,10))/3600
  i<-which(substr(xy$LON,1,1)=="-")
  if (length(i)>0) lon_dec[i]<-(-lon_dec[i])
  xyT<-cbind(lon_dec,lat_dec)
  xyT
}

