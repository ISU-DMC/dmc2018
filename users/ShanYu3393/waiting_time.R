rm(list = ls())
library('lubridate')
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
train<-read.table(filepath,sep='|',header=TRUE)
dim(train)
PIDSIZE=as.matrix(unique(train[,1:2]))
# delete NA iterms in the sold units
train=subset(train,subset=!is.na(train$units))

Whole=NULL
for(iter in tt){
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
    wait=block[rep(1,N+1),c(1:2,6:13)]
    T=as.numeric(T.location[2:(N+2)]-T.location[1:(N+1)])
    T[1]=T[1]/split[1]*st
    wait[,'WT']=T
    wait[,'START']=T.location[1:(N+1)]
    wait[,'END']=T.location[2:(N+2)]
    wait[,"Adjusted"]=c(sold_all%%st!=0,rep(0,N))
    Whole=rbind(Whole,wait)
  }
}
write.table(Whole,'~/Dropbox/DMC/waiting_time.txt')

Whole0=Whole
Whole$ID=1
for(iter in 1:dim(PIDSIZE)[1]){
  index=which(Whole[,2]==PIDSIZE[iter,1] & Whole[,3]==PIDSIZE[iter,2])
  Whole$ID[index]=iter
}

pred=tapply(Whole$WT, Whole$ID, 
            function(x) sum((1:length(x))*x)/sum(1:length(x)))
pred2=ceiling(pred)
end=tapply(Whole$END, Whole$ID, max)
sold_out=ymd(end)+pred2
sold_out[sold_out>ymd('2018-01-31')]=ymd('2018-01-31')
sold_out[sold_out<ymd('2018-01-01')]=ymd('2018-01-01')

tt=which(! 1:dim(PIDSIZE)[1] %in% unique(Whole$ID))
sold_out2=sample(1:31,length(tt),replace=TRUE)
sold_out2=ymd(paste0('2018-01-',16))
SOLD=rep(sold_out[1],7409)
SOLD[-tt]=sold_out
SOLD[tt]=sold_out2
SOLD2=data.frame(PIDSIZE,SOLD)
SOLD2$pid=as.character(SOLD2$pid)
SOLD2$pid=as.numeric(SOLD2$pid)
SOLD2$size=as.factor(SOLD2$size)
SOLD2$SOLD=as.factor(SOLD2$SOLD)
SOLD2$DD=0
SOLD2$DD[tt]=1
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/test_Jan.txt'
test<-read.table(filepath,sep='|',header=TRUE)

TEST2=left_join(test,SOLD2,by=c('pid','size'))
TEST2$soldOutDate=NULL
names(TEST2)=c('pid','size','soldOutDate')
write.table(TEST2,'~/Dropbox/DMC/pred2.txt',sep='|')
filepath='~/Dropbox/DMC/waiting_time.txt'
library('data.table')
Whole=fread(filepath)
names(Whole)=c('index',c("pid","size","color", "brand", "rrp", "mainCategory",
                         "category", "subCategory", "releaseDate", "stock", "WT",
                         "START", "END", "Adjusted"))
colSums(apply(Whole,2,is.na))

plot((1:7409)[-tt],ymd(TEST2$soldOutDate[TEST2$DD==0])-
       ymd(TEST2$SOLD[TEST2$DD==0]))
