#Loading Required packages

library(ggplot2)
library(fossil)
library(dbscan)
library(dplyr)

#Generating average outlier scores for k=1-100 to visually select the k-value, here we used the 'Cardiotocography' dataset)

dat=read.csv("cardioto.csv",header=F)
dat=dat[!duplicated(dat[,-22]), ]
data=dat[-c(22)]
result=numeric(100)
for(k in 1:100){
  nn <- kNN(data, k)
  kn=as.matrix(nn$id)
  
  for(i in 1:nrow(data)) 
  { 
    nam<- paste("dist", i, sep = "")
    assign(nam, as.matrix(dist(data[c(i,kn[i,]),])))
  }
  
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
  
  result[[k]]=as.data.frame(Outlier_Score)}
results=as.data.frame(result)
d=as.data.frame(apply(results,2,FUN=mean))
d$obs=1:100
#Saving the figure in the working directory
svg(filename="cardk.svg", 
    width=12, 
    height=5, 
    pointsize=12)
ggplot(d, aes(x=d$obs,y=d$`apply(results, 2, FUN = mean)`))+ geom_point()+ labs(x = "k values") +labs(y="Average LoMST scores") +theme(axis.text=element_text(size=20,face="bold"),axis.title=element_text(size=25,face="bold"))+ geom_segment(aes(x = 47, y = 0, xend = 47, yend = 0.20),size=1.5)+ geom_segment(aes(x = 27, y = 0, xend = 27, yend = 0.20),size=1.5)+ geom_segment(aes(x = 27, y = 0.20, xend = 47, yend = 0.20),size=1.5)
dev.off()

#Visually selecting the k-range where the mean scores are stable
krangestart=27
krangeend=47
f=as.data.frame(apply(results,2,FUN=sd))
#Selecting the k-value which gives maximum standard deviation from the range
selectedk=which(f==max(f[krangestart:krangeend,1]))


