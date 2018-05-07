## This function is based on the validation is January

library('data.table')
library('lubridate')
library('caret')
## Read in Data
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
Whole<-fread(filepath,sep='|',header=TRUE)
Train <- Whole[ymd(Whole$releaseDate) < ymd('2018-01-01'),]
Test <- Whole[ymd(Whole$releaseDate) > ymd('2018-01-01'),]

## define summaryFunction
## This is for poisson distribution
CondMedian_poi <- function(para,replication=100){
  
  # r is the number of stock
  r=para[1]
  
  # a branch of predicted value
  lambda=para[-1]
  
  # number of day
  n=length(lambda)
  
  # generate soldout day
  A=sapply(1:replication,function(x) which(cumsum(rpois(n,lambda))>=r)[1])
  
  # calculate median
  median(A,na.rm=TRUE)
}

## Calculate loss function
Loss_MAE <- function(Para,Soldout){
  
  # 
  
  # calculate conditional median for each item
  Pred=apply(Para,1,CondMedian_poi)
  
  # sum of absolute difference
  sum(abs(Pred-Soldout))
  
}
trainControl(method='timeslice',initialWindow = 60, horizon = 31,fixedWindow = FALSE, skip = 3,
             summaryFunction = Loss_MAE)