cluster<-read.table("C:/Users/63139/Documents/GitHub/dmc2018/users/XiaodanLyu/cluster_distance/cluster_hengfang.txt",sep="|",head=T)
a<-read.table("C:/Users/63139/Documents/GitHub/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt",sep="|",head=T)
pid.size<-c(1:12824)
cluster<-data.frame(cluster,pid.size)
data.c<-merge(a,cluster,by=c("pid","size"))
data.c$units[is.na(data.c$units)]<-0
subdata<-data.c[data.c$group5==5,]
cum<-tapply(subdata$units,subdata$pid.size,cumsum)
plot(c(1:92),unlist(cum[1])[1:92]/unlist(cum[1])[92],main="percentage group 5.5",type="l",xlim=c(0,92),ylim=c(0,1),xlab="day",ylab="% of total units sold")
i<-0
for(i in 2:length(cum)){
  lines(c(1:92),unlist(cum[i])[1:92]/unlist(cum[i])[92],col=i)
}