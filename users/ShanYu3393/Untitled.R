## This function is based on the validation is January
# use glmnet
rm(list = ls())
library('data.table')
library('lubridate')
library('caret')
## Read in Data
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
Whole<-fread(filepath,sep='|',header=TRUE)
ID=Whole[, c(1,2,3)]
X=as.matrix(Whole[,c('price','stock')])
y=as.matrix(Whole$units)

## define summaryFunction
## This is for poisson distribution
CondMedian_poi <- function(para,replication=100){
  
  # adjust NA
  t0=sum(is.na(para))
  para=para[!is.na(para)]
  
  # r is the number of stock
  r=para[length(para)]
  
  # a branch of predicted value
  lambda=para[-length(para)]
  
  # number of day
  n=length(lambda)
  
  # generate soldout day
  A=sapply(1:replication,function(x) which(cumsum(rpois(n,lambda))>=r)[1])
  
  # calculate median
  median(A,na.rm=TRUE)
}

## Calculate loss function
Loss_MAE <- function(Para,ID,stock,Soldout){
  
  # create a wide matrix
  PARA=cbind(ID, Para) %>% spread(date, Para) %>% left_join(stock) %>%
    select(-pid,-size)
  
  # calculate conditional median for each item
  Pred=apply(PARA,1,CondMedian_poi)
  
  # sum of absolute difference
  sum(abs(Pred-Soldout))
  
}

## Split the data
# could modify the end day
PeriodLength=as.numeric(ymd('2018-01-01')-ymd('2017-10-01'))+1
AugtoJan=1:PeriodLength

# use timeslices to create the train and test set
Split=createTimeSlices(1:PeriodLength, initialWindow = 40, horizon = 31,
                       fixedWindow = FALSE, skip = 3)

# re-name the train set and test set
names(Split$train)=paste0('train', 1:length(Split$train))
names(Split$test)=paste0('test', 1:length(Split$test))


# extract the training data for the fold-th fold
# train set
fold=1
TrainEnd=ymd('2017-09-30')+max(Split$train[[fold]])

TrainIndex=which(ymd(Whole$date) <= TrainEnd)

# test set
TestStart=ymd('2017-09-30')+max(Split$train[[fold]])+1
TestEnd=ymd('2017-09-30')+max(Split$test[[fold]])
TestIndex=which(ymd(Whole$date) <= TestEnd & ymd(Whole$date) >= TestStart)

# create random stock and soldout date
Sold=data.frame(ID=ID[TestIndex,],y=y[TestIndex]) %>% spread(ID.date, y)
SoldSum=rowSums(Sold[,-c(1,2)],na.rm = TRUE)
stock=SoldOutDay=rep(0,length(SoldSum))
for(i in which(SoldSum > 0)) {
  dailysale=as.numeric(Sold[i,-c(1,2)])
  if (SoldSum[i]==1) {
    stock[i]=1
    SoldOutDay[i]=which(dailysale==1)
  } else{
    stock[i]=sample(1:SoldSum[i],1)
    SoldOutDay[i]=which(cumsum(dailysale)>=stock[i])[1]
  }
}


# fit the model using training data
fitted=glmnet(X[TrainIndex,], y[TrainIndex], family = 'poisson')

# pred for test set                              
pred=predict(fitted,newx=X[TestIndex,],type='response')


apply(pred,2,Loss_MAE,ID=ID[TestIndex,],stock=,Soldout=SoldOutDay)

