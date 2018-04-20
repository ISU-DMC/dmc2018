rm(list = ls())
library('lubridate')
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
train<-read.table(filepath,sep='|',header=TRUE)
dim(train)
PIDSIZE=as.matrix(unique(train[,1:2]))
# delete NA iterms in the sold units
train=subset(train,subset=!is.na(train$units))

Whole=NULL
for(iter in 1:dim(PIDSIZE)[1]){
  index=which(train[,1]==PIDSIZE[iter,1] & train[,2]==PIDSIZE[iter,2])
  block=train[index,]
  sold=cumsum(train[index,'units'])
  sold_all=sum(train[index,'units'])
  if(block$stock[1] >= sold_all){
    T=as.numeric(ymd(block$date[which.max(sold)])-ymd(block$releaseDate)[1]+1)
    wait=block[1,c(1:2,6:13)]
    wait[,c('WT','START',"END","Adjusted")]=c(T,ymd(block$releaseDate[1]),ymd(block$date[which.max(sold)]),
                                              block$stock[1]>sold_all)
  } else {
    st=block$stock[1]
    N=floor(sold_all/st)
    split=c(sold_all-floor(sold_all/st)*st,sold_all-floor(sold_all/st)*st+1:N*st)
    if(split[1]==0) {
      split=split[-1]
      N=N-1
    }
    T.location=ymd(block$releaseDate[1])-1
    for(j in 1:(N+1)) T.location=c(T.location,ymd(block$date[which(sold>=split[j])[1]]))
    wait=block[1:(N+1),c(1:2,6:13)]
    T=as.numeric(T.location[2:(N+2)]-T.location[1:(N+1)])
    T[1]=T[1]/split[1]*st
    wait[,'WT']=T
    wait[,'START']=T.location[1:(N+1)]+1
    wait[,'END']=T.location[2:(N+2)]
    wait[,"Adjusted"]=c(sold_all%%st!=0,rep(0,N))
    Whole=rbind(Whole,wait)
  }
}
write.table(Whole,'~/Dropbox/DMC/waiting_time.txt')
  

