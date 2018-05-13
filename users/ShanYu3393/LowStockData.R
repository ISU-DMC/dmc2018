## Model target: stock = 1
rm(list = ls())

## ------- library and sourse file
library(tidyr)
library(lubridate)
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

## ---- read_data
setwd("~/Dropbox/DMC")
Sale=readRDS('/Users/shanyu/Desktop/alltrain_subfeatures.rds') %>% 
  select(pid,size,date,units)  %>% spread(date,units)
Feature=readRDS('/Users/shanyu/Desktop/alltrain_subfeatures.rds')

## ------- generate indicator matrix for sale records
IdSale=Sale
IdSale[,-c(1,2)]=1*IdSale[,-c(1,2)]>0

# rename
# names(IdSale)=c('pid','size',as.character(ymd('2017-10-01')+0:150))
# names(Sale)=c('pid','size',as.character(ymd('2017-10-01')+0:150))

## ------- split the first step dataset

# the dataset before 2018-01-04 
Train=IdSale %>% select(pid,size,as.character(ymd('2017-10-01')+0:95))
KeepData=rowSums(Train[,-c(1,2)],na.rm = TRUE)>2 
SelectedTrain=Train[KeepData,] %>% gather(date,IdSale, -pid, -size)
SelectedTrain=SelectedTrain[!is.na(SelectedTrain$IdSale),]
SelectedTrain$date=as.Date(SelectedTrain$date)
SelectedTrain=left_join(SelectedTrain,Feature,by=c('pid','size','date'))

write_rds(SelectedTrain,'LowStockTrain.rds')

# get the test data
Test=IdSale %>% select(pid,size,as.character(ymd('2018-01-04')+0:27))
Test=Test %>% gather(date,IdSale, -pid, -size)

TrueSale=Sale %>% select(pid,size,as.character(ymd('2018-01-04')+0:27))
TrueSale=TrueSale %>% gather(date, units, -pid, -size)
Test_StockSold=Stock_Soldoutday(TrueSale[,1:3],TrueSale[,4])

