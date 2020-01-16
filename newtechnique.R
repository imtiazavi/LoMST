d=numeric(195)
s<- data.frame(matrix(0,ncol = 195, nrow = nrow(results)))
t<- data.frame(matrix(0,ncol = 195, nrow = nrow(results)))

for(j in 1:ncol(results)){
  for(i in 1:nrow(results)){
    if(results[i,j]>=(mean(results[,j])+ 2*sd(results[,j]))){
      s[i,j]=results[i,j]}
    else{t[i,j]=results[i,j]}}
  d[j]=mean(s[which(s[,j]>0),j])-mean(t[which(t[,j]>0),j])}
dt=as.data.frame(d)
dat=read.csv("pima.csv",header=F)
data=dat[-c(9)]
data=data[!duplicated(data), ]
result=numeric(200)
highk=numeric(50)
for(j in 1:50){
  for(k in 1:200){
    nn <- kNN(data, k)
    kn=as.matrix(nn$id)
    
    for(i in 1:nrow(data)) 
    { 
      nam<- paste("dist", i, sep = "")
      assign(nam, as.matrix(dist(data[c(i,kn[i,]),])))
    }
    
    ##TRY1(perfect)
    
    sumhist=numeric(nrow(data))
    compare=numeric(nrow(data))
    Outlier_Score=numeric(nrow(data))
    for(i in 1:nrow(data)){
      d=get(paste("dist", i, sep = ""))
      mat=as.matrix(dino.mst(d))
      d[which(mat== 0)] <- 0
      
      sumhist[i]=sum(d)}
    for(i in 1:nrow(data)){
      compare[i]=sumhist[i]-mean(sumhist[c(kn[i,])])}
    for(i in 1:nrow(data)){  
      
      Outlier_Score[i]=(compare[i]-min(compare))/(max(compare)-min(compare))}
    result[[k]]=mean(tail(sort(Outlier_Score),j))-mean(head(sort(Outlier_Score),526-j))}
  results=as.data.frame(result)
  highk[[j]]=rownames(as.data.frame(results[results$result==max(results$result),,drop=FALSE]))}
finals=as.data.frame(highk)