## Use 2017-10-01 to 2017-12-31 as train, predict 2018-01-01 to 2018-01-31
rm(list = ls())
## --------------------- library and source file
library('tidyr')
library(xgboost)
library(lubridate)
## --------------------- Read in data 
filepath='/Users/shanyu/Dropbox/DMC/raw_data/WT_alltrain.rds'
filepath2='/Users/shanyu/Dropbox/DMC/raw_data/WT_all_features_may12.rds'
Total_W=readRDS(filepath) 
Total_W0=readRDS(filepath2) 
Total_W$startdate=Total_W0$startdate[Total_W0$Rcr==0]
Total_W$pid=Total_W0$pid[Total_W0$Rcr==0]
Total_W$size=Total_W0$size[Total_W0$Rcr==0]

## --------------------- split 2018-01-03
# train
Train=Total_W[Total_W$startdate <= ymd('2018-01-03'),] %>% select(-startdate)
Weight=Train %>% group_by(pid,size) %>% tally()
Weight$n=1/Weight$n
Train=left_join(Train,Weight,by=c('pid','size'))
Train$pid=NULL
Train$size=NULL
Train$startdate=NULL
Weight_train=Train$n
Train$n=NULL
Train=data.matrix(Train)

# test
Test=Total_W[Total_W$startdate > ymd('2018-01-03'),]
Test$startdate=NULL
Test$size=NULL
Test=data.matrix(Test)

fitted = xgboost(data=Train[,-1], label=Train[,1], max_depth=40, eval_metric="mae",
                 weight=Weight_train,
        subsample=0.6, eta=0.3, objective="count:poisson",  nrounds = 100)

Pred=predict(fitted, Test[,-1])
Pmae=sqrt(sum(abs(Test[,1]-Pred)))

sqrt(sum(abs(Test[,1]-16)))

hist(Pred)
table(Test[,1]==1)

fitted = train(data=Train, label=Train[,1], max_depth=40, eval_metric="mae",
                 weight=Weight_train,
                 subsample=0.6, eta=0.3, objective="reg:linear", nrounds = 10)
