library(lubridate)
library(magrittr)
library(dplyr)
cluster<-load("C:/Users/63139/Documents/GitHub/dmc2018/users/XiaodanLyu/cluster_distance/Cluster_Yan.Rda")
a<-read.csv("C:/R programming/raw_data/combined.csv")
train.Jan <- a %>% 
  mutate(date = ymd(date))%>% 
  filter(date < ymd("2018-02-01"))
pid.size<-c(1:7520)
cluster<-Cluster.Label
cluster<-data.frame(cluster,pid.size)
data.c<-merge(train.Jan,cluster,by=c("pid","size"))
data.c$units[is.na(data.c$units)]<-0
## cumsum plot
subdata<-data.c[data.c$group5==1,]
cum<-tapply(subdata$units,subdata$pid.size,cumsum)
plot(c(1:92),unlist(cum[1])[1:92]/unlist(cum[1])[92],main="sold.per.cut group 9",type="l",xlim=c(0,92),ylim=c(0,1),xlab="day",ylab="% of total units sold")
i<-0
for(i in 2:length(cum)){
  lines(c(1:92),unlist(cum[i])[1:92]/unlist(cum[i])[92],col=i)
}
## diff (lag=1) plot
diff<-function(v){
  l<-length(v)
  diff<-v[2:l]-v[1:(l-1)]
  diff
}
subdata<-data.c[data.c$curve.cluster.label==9,]
diff<-tapply(subdata$units,subdata$pid.size,diff)
plot(c(1:122),unlist(diff[1])[1:122],main="sold speed group 9.9",type="l",xlim=c(0,122),ylim=c(-300,300),xlab="day",ylab="diff lag 1")
i<-0
for(i in 2:length(diff)){
  lines(c(1:122),unlist(diff[i])[1:122],col=i)
}