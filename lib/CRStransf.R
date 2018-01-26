#+
CRStransf<-function(xy,CRSfrom,CRSto) {
  coord_from<-SpatialPoints(xy,proj4string=CRS(CRSfrom))
  coord_to<-spTransform(coord_from,CRS(CRSto))
  xyT<-coordinates(coord_to)
  xyT
}

