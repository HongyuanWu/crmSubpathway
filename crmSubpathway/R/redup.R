# Internal function
redup<-function(spw){
  quchuindex<-NULL
  for(j in 1:(length(spw)-1)){
    spw1<-spw[[j]]
    cd1<-length(spw1)
    xl<-seq(j+1,length(spw))
    for(i in xl){
      spw2<-spw[[i]]
      cd2<-length(spw2)
      spwjiao<-intersect(spw1,spw2)
      if(length(spwjiao)!=0){
        cdjiao<-length(spwjiao)
        spwbz1<-cdjiao/cd1
        spwbz2<-cdjiao/cd2
        if(spwbz1>=0.8|spwbz2>=0.8){
          quchuindex<-c(quchuindex,ifelse(cd1>cd2,i,j))
        }
      }
    }
  }
  return(quchuindex)
}
