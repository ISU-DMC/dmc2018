library(lubridate)
library(dplyr)
cluster<-readRDS("C:/Users/63139/Documents/GitHub/dmc2018/users/hengfang/Cluster_Indicator_4_to_6_Lag_Specific.rds")
a<-read.csv("C:/R programming/raw_data/combined.csv")
key<-paste(a$pid,a$size,sep=" - ")
train.Jan <- data.frame(a,key) %>% 
  mutate(date = ymd(date))%>% 
  filter(date < ymd("2018-02-01"))
data.c<-merge(train.Jan,cluster,by="key")
data.c$units[is.na(data.c$units)]<-0
## cumsum plot
subdata<-data.c[data.c$Cluster_4==1,]
name<-"hengfang group 4.1 units="
n.units<-nrow(subdata)/123
cum<-tapply(subdata$units,as.character(subdata$key),cumsum)
plot(c(1:123),unlist(cum[1])[1:123]/unlist(cum[1])[123],main=c(name,n.units),type="l",xlim=c(0,123),ylim=c(0,1),xlab="day",ylab="% of total units sold")
i<-0
for(i in 2:length(cum)){
  lines(c(1:123),unlist(cum[i])[1:123]/unlist(cum[i])[123],col=i)
}
## diff (lag=1) plot
diff<-function(v){
  l<-length(v)
  diff<-v[2:l]-v[1:(l-1)]
  diff
}
subdata<-data.c[data.c$Cluster_4==4,]
name<-"hengfang group 4.4 units="
n.units<-nrow(subdata)/123
diff<-tapply(subdata$units,subdata$pid.size,diff)
plot(c(1:122),unlist(diff[1])[1:122],main=c(name,n.units),type="l",xlim=c(0,122),ylim=c(-200,200),xlab="day",ylab="diff lag 1")
i<-0
for(i in 2:length(diff)){
  lines(c(1:122),unlist(diff[i])[1:122],col=i)
}