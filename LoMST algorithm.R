#Loading Required packages

library(fossil)
library(dbscan)
library(dplyr)

#Loading data file (Here we used the 'Cardiotocography' dataset)
dat=read.csv("cardioto.csv",header=F)
dat=dat[!duplicated(dat), ]
data=dat[-c(ncol(dat))]

selectedk=27 #Selected visually, we get it using Kvalueselection.R 
n=86 #Cut-off value to evaluate the performance which is set to number of true anomalies for each dataset


#Following function implements the LoMST algorithm and returns the no.of true detection for a preselected k value and a cut-off value

no.oftruedetection=function(selectedk,n){
nn <- kNN(data, k=selectedk)
kn=as.matrix(nn$id)

for(i in 1:nrow(data)) 
{ 
  nam<- paste("dist", i, sep = "")
  assign(nam, as.matrix(dist(data[c(i,kn[i,]),])))
}

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
dat$d=c(1:nrow(data))

required=dat[,c(ncol(dat)-1,ncol(dat))]
names(required)=c("Outlier status","obs")
final=as.data.frame(merge(sumhistarranged,required,by=c("obs")))
final$Outlier_Score=(final$compare-min(final$compare))/(max(final$compare)-min(final$compare))
finalarranged=as.data.frame(arrange(final,desc(compare)))
as.numeric(table(finalarranged$`Outlier status`[1:n])[2])}
