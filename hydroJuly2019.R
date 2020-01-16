  data=read.csv("hydrom.csv",header=F)
  nn <- kNN(data, 20)
  kn=as.matrix(nn$id)
  
  for(i in 1:nrow(data)) 
  { 
    nam<- paste("dist", i, sep = "")
    assign(nam, as.matrix(dist(data[c(i,kn[i,]),])))
  }
  
  ##TRY1(perfect)
  
  sumhist=numeric(nrow(data))
  compare=numeric(nrow(data))
  for(i in 1:nrow(data)){
    d=get(paste("dist", i, sep = ""))
    mat=as.matrix(dino.mst(d))
    d[which(mat== 0)] <- 0
    
    sumhist[i]=sum(d)}
  for(i in 1:nrow(data)){
    compare[i]=sumhist[i]-mean(sumhist[c(kn[i,])])}
  
  summary=as.data.frame(compare)
  summary$obs=c(1:nrow(data))
  sumhistarranged=as.data.frame(arrange(summary,desc(compare)))
