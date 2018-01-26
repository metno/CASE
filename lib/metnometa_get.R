#+
metnometa_get<-function(conf,par.conf) {
  ffname<-file.path(conf$main,
                    paste("case_",conf$case_reldate,sep=""),
                    "aux",
                    paste("metno_stations_",conf$stndate,".txt",sep=""))
  if (!file.exists(ffname)) return(NULL)
  data<-read.table(ffname,
                   fileEncoding="latin1",
                   sep=";",
                   header=T,
                   stringsAsFactors=F,
                   strip.white=T,
                   quote="",
                   blank.lines.skip=T)
#DEPARTMENT;DEPT_NO;MUNICIPALITY;MUNI_NO;ST_NAME;STNR;UTM_E;UTM_N;AMSL;LAT_DEC;LON_DEC;WMO_NO
  data$DEPT_NO<-as.numeric(data$DEPT_NO)
  data$LAT_DEC<-as.numeric(data$LAT_DEC)
  data$LON_DEC<-as.numeric(data$LON_DEC)
  data$UTM_N<-as.numeric(data$UTM_N)
  data$UTM_E<-as.numeric(data$UTM_E)
  data$AMSL<-as.numeric(data$AMSL)
  # consider only Norwegian mainland
  dep_ok<-c("ØSTFOLD","AKERSHUS","OSLO","HEDMARK","OPPLAND","BUSKERUD",
            "VESTFOLD","TELEMARK","AUST-AGDER","VEST-AGDER","ROGALAND",
            "HORDALAND","SOGN OG FJORDANE","MØRE OG ROMSDAL","NORDLAND",
            "TROMS","FINNMARK","TRØNDELAG")
  iok<-which(data$DEPARTMENT %in% dep_ok & 
             !is.na(data$LAT_DEC) & !is.na(data$LON_DEC) &
             !is.na(data$UTM_E) & !is.na(data$UTM_N) &
             !is.na(data$AMSL))
  if (length(iok)==0) return(NULL)
  # etrs_laea 2 utm3
  data$etrs_laea_x<-data$UTM_E
  data$etrs_laea_y<-data$UTM_N
  data$etrs_laea_x[]<-NA
  data$etrs_laea_y[]<-NA
  aux<-CRStransf(cbind(data$LON_DEC[iok],data$LAT_DEC[iok]),
                 par.conf$proj4.wgs84,
                 par.conf$proj4.ETRS_LAEA)
  data$etrs_laea_x[iok]<-aux[,1] 
  data$etrs_laea_y[iok]<-aux[,2] 
#
  i<-which(data$DEPARTMENT %in% dep_ok &  
           !is.na(data$LAT_DEC) & !is.na(data$LON_DEC) &
           !is.na(data$AMSL) &
           !is.na(data$UTM_E) & !is.na(data$UTM_N) &
           !is.na(data$etrs_laea_x) & !is.na(data$etrs_laea_y) )
  data1<-data.frame(staid=as.numeric(data$STNR[i]),
                    name=as.character(data$ST_NAME[i]),
                    z=as.numeric(data$AMSL[i]),
                    lat_dec=as.numeric(data$LAT_DEC[i]),
                    lon_dec=as.numeric(data$LON_DEC[i]),
                    etrs_laea_x=data$etrs_laea_x[i],
                    etrs_laea_y=data$etrs_laea_y[i],
                    utm33_x=data$UTM_E[i],
                    utm33_y=data$UTM_N[i],
                    stringsAsFactors=F)
  return(list(data=data1,
              ffname=ffname))
}

