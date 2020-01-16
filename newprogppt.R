a=read.csv("cardresults.csv")
a=a[,-1]
x=as.data.frame(apply(a[,-c(101:200)],2,FUN=mean))
x$data=1:nrow(x)
svg(filename="cardk.svg", 
    width=12, 
    height=5, 
    pointsize=12)
ggplot(x, aes(x=x$data,y=x$`apply(a[, -c(101:200)], 2, FUN = mean)`))+ geom_point()+ labs(x = "k values") +labs(y="Average LoMST scores") +theme(axis.text=element_text(size=20,face="bold"),axis.title=element_text(size=25,face="bold"))+ geom_segment(aes(x = 47, y = 0, xend = 47, yend = 0.235),size=1.5)+ geom_segment(aes(x = 27, y = 0, xend = 27, yend = 0.235),size=1.5)+ geom_segment(aes(x = 27, y = 0.235, xend = 47, yend = 0.235),size=1.5)
dev.off()
y=as.data.frame(apply(a[,-c(101:200)],2,FUN=sd))
x=as.data.frame(apply(a,2,FUN=mean))
y=as.data.frame(apply(a,2,FUN=sd))
c=as.data.frame(y)
d=as.data.frame(x)
plot(seq(1:100),x[[1]])
z=as.data.frame(a[,27])
mean(a$Outlier_Score.26)+3*sd(a$Outlier_Score.26)
mean(a$Outlier_Score.26)+2*sd(a$Outlier_Score.26)
rownames(z)[order(z[[1]], decreasing=TRUE)][1:1508]
s=read.csv("annthyroid.csv",header=F)
l=rownames(s)[order(s[[22]], decreasing=TRUE)][1:347]
f=rownames(z)[order(z[[1]], decreasing=TRUE)][1:200]
table(l %in% f)

y=as.data.frame(apply(a,2,FUN=sd))


x=as.data.frame(apply(a[,-c(101:150)],2,FUN=mean))
a=colnames(a[colSums(!is.na(a)) > 0])
mean(a$Outlier_Score.61)+3*sd(a$Outlier_Score.61)
mean(a$Outlier_Score.61)+2*sd(a$Outlier_Score.61)
ggplot(data=a, aes(a$Outlier_Score.101)) + 
  geom_histogram(breaks=seq(0, 1, by =0.2), 
                 col="red", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "red", high = "green")  + 
  labs(title="Histogram for Outlier scores of Forging (k=101)") +
  labs(x="OutlierScore", y="Count")





library(HighDimOut)
##SOD(Subspace outlier detection)
dat=read.csv("waveform.csv")
data=dat[,-ncol(dat)]
res.SOD <- Func.SOD(data = data, k.nn = 62, k.sel = 30, alpha = 0.8)
x=as.data.frame(res.SOD)