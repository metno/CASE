case_metadata_ffname<-function(conf) {
file.path(conf$main,
          paste("case_",conf$reldate,sep=""),
          paste(conf$bname,"_",conf$reldate,".txt",sep=""))
}

