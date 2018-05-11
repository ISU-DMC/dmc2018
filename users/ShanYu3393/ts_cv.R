## This function is based on the validation is January
# use glmnet
rm(list = ls())
library('data.table')
library('lubridate')
library('tidyverse')
library('caret')
library('glmnet')
library('parallel')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')
## Read in Response Data

Whole=alldata[ymd(alldata$date) <= ymd('2018-01-31'),]
X_all=model.matrix(units~., data = Whole %>% select(-pid, -size, -date))
Whole_train=Whole[ymd(Whole$date) <= ymd('2017-12-31'),]
Whole_test=Whole[ymd(Whole$date) > ymd('2017-12-31'),]

# ID for train: pid, size, date
ID=Whole_train %>% select(pid, size, date)
# feature in train
X=X_all[ymd(Whole$date) <= ymd('2017-12-31'),]
# sold units in train
y=as.matrix(Whole_train$units)

# ID for test: pid, size, date
ID_test=Whole_test %>% select(pid, size, date)
# feature for test
X_test=X_all[ymd(Whole$date) > ymd('2017-12-31'),]
# sold units in test
y_test=as.matrix(Whole_test$units)

#check features
dim(X)[2]==dim(X_test)[2]

## -----------------------------------Split the data
# could modify the end day
PeriodLength=as.numeric(ymd('2017-12-31')-ymd('2017-10-01'))+1
AugtoJan=1:PeriodLength

# use timeslices to create the train and test set
Split=createTimeSlices(1:PeriodLength, initialWindow = 40, horizon = 31,
                       fixedWindow = FALSE, skip = 3)

## -----------------------------give the tuning parameter
# lambda for lasso
lambda=exp(seq(log(10^-6),log(10^2),length.out=60))

## ----------------------------time series based cross validation
LOSS=NULL
glmnet_cv=function(fold){
  
  # train set
  TrainEnd=ymd('2017-09-30')+max(Split$train[[fold]])
  TrainIndex=which(ymd(Whole_train$date) <= TrainEnd)
  
  # test set
  TestStart=ymd('2017-09-30')+max(Split$train[[fold]])+1
  TestEnd=ymd('2017-09-30')+max(Split$test[[fold]])
  TestIndex=which(ymd(Whole_train$date) <= TestEnd & ymd(Whole_train$date) >= TestStart)
  
  # create random stock and soldout date
  Test_SoldoutD=Stock_Soldoutday(ID[TestIndex,],y=y[TestIndex])
  
  # May need further code to generate LLR or other features
  
  # fit the model using training data
  fitted=glmnet(X[TrainIndex,], y[TrainIndex], family = 'poisson', lambda = lambda)
  
  # pred for test set                              
  pred=predict(fitted,newx=X[TestIndex,],type='response')
  
  # calculate Loss function for each tunning parameter
  apply(pred,2,Loss_MAE,ID=ID[TestIndex,],stock=Test_SoldoutD$stock,
        Soldout=Test_SoldoutD$SoldOutDay)
}


LOSS=mclapply(1:length(Split$train),glmnet_cv,mc.cores=16)
LOSS=do.call(rbind,LOSS)

## ----------------------------choose the best parameter
BestPara=lambda[which.min(colMeans(LOSS))]

# fit model using whole train and selected tuning parameter
fitted=glmnet(X, y, family = 'poisson', lambda=BestPara)

# predict 
pred=predict(fitted,newx=X_test,type='response')

# calculate the MAE 

SoldoutD=Stock_Soldoutday(ID_test,y=y_test)

sqrt(Loss_MAE(pred,ID_test,stock=SoldoutD$stock,Soldout=SoldoutD$soldOutDate))
