## This function is based on the validation is January
# use glmnet
rm(list = ls())
library('data.table')
library('lubridate')
library('tidyverse')
library('caret')
library('glmnet')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
## Read in Response Data
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
Whole<-fread(filepath,sep='|',header=TRUE)[,c(1,2,3,5)]

Whole_train=Whole[ymd(Whole$date) <= ymd('2017-12-31'),]
Whole_test=Whole[ymd(Whole$date) > ymd('2017-12-31'),]

# ID for train: pid, size, date
ID=Whole_train %>% select(pid, size, date)
# feature in train
X=model.matrix(units~., data = Whole_train %>% select(-pid, -size, -date))
# sold units in train
y=as.matrix(Whole_train$units)

# ID for test: pid, size, date
ID_test=Whole_test %>% select(pid, size, date)
# feature for test
X_test=model.matrix(units~., data = Whole_test %>% select(-pid, -size, -date))


## Split the data
# could modify the end day
PeriodLength=as.numeric(ymd('2017-12-31')-ymd('2017-10-01'))+1
AugtoJan=1:PeriodLength

# use timeslices to create the train and test set
Split=createTimeSlices(1:PeriodLength, initialWindow = 40, horizon = 31,
                       fixedWindow = FALSE, skip = 3)

LOSS=NULL
for (fold in 1:length(Split$train)){
  
  # train set
  TrainEnd=ymd('2017-09-30')+max(Split$train[[fold]])
  TrainIndex=which(ymd(Whole_train$date) <= TrainEnd)
  
  # test set
  TestStart=ymd('2017-09-30')+max(Split$train[[fold]])+1
  TestEnd=ymd('2017-09-30')+max(Split$test[[fold]])
  TestIndex=which(ymd(Whole_train$date) <= TestEnd & ymd(Whole_train$date) >= TestStart)
  
  # create random stock and soldout date
  # Sold=data.frame(ID=ID[TestIndex,],y=y[TestIndex]) %>% spread(ID.date, y)
  # SoldSum=rowSums(Sold[,-c(1,2)],na.rm = TRUE)
  # stock=SoldOutDay=rep(0,length(SoldSum))
  # for(i in which(SoldSum > 0)) {
  #   dailysale=as.numeric(Sold[i,-c(1,2)])
  #   if (SoldSum[i]==1) {
  #     stock[i]=1
  #     SoldOutDay[i]=which(dailysale==1)
  #   } else{
  #     stock[i]=sample(1:SoldSum[i],1)
  #     SoldOutDay[i]=which(cumsum(dailysale[!is.na(dailysale)])>=stock[i])[1]+sum(is.na(dailysale))
  #   }
  # }
  SoldTest <- data.frame(ID[TestIndex,], y=y[TestIndex])
  SoldTest %>% group_by(pid, size) %>%
    mutate(cumunits = cumsum(y),
           stock = ifelse(max(cumunits) == 0, 0, sample(1:max(cumunits)))) %>%
    filter(cumunits >= stock) %>%
    arrange(date) %>%
    summarise(stock = unique(stock), soldOutDate = min(date)) -> Sold
  
  # May need further code to generate LLR or other features
  
  # fit the model using training data
  fitted=glmnet(X[TrainIndex,], y[TrainIndex], family = 'poisson')
  
  # pred for test set                              
  pred=predict(fitted,newx=X[TestIndex,],type='response')
  
  # calculate Loss function for each tunning parameter
  LOSS=rbind(LOSS,apply(pred,2,Loss_MAE,ID=ID[TestIndex,],stock=stock,Soldout=SoldOutDay))
}

BestPara=fitted$lambda[which.min(colMeans(LOSS))]

# fit model using whole train and selected tuning parameter
fitted=glmnet(X, y, family = 'poisson', lambda=BestPara)

# predict 
pred=predict(fitted,newx=X_test,type='response')

# calculate the MAE 
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/items_Jan.txt'
item_Jan<-read.table(filepath,sep='|',header=TRUE)
item_Jan <- item_Jan[,c(1,2,10)]
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/test_Jan.txt'
Soldout<-read.table(filepath,sep='|',header=TRUE)

A=left_join(ID_test,item_Jan,by=c('pid','size')) %>% 
  left_join(Soldout,by=c('pid','size')) %>% select(pid,size,stock,soldOutDate) %>%
  unique

sqrt(Loss_MAE(pred,ID_test,stock=A$stock,Soldout=A$soldOutDate))
