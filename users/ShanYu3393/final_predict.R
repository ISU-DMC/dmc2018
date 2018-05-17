rm(list = ls())

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

cluster=4
method='nnet'

filepath1=paste0('/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalTune/FebPred',
               method,'_Month1_C4_',cluster,'.rds')
filepath2=paste0("/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalTune/FebPredxgbTree_Month1_C4_",
               cluster,'.rds')
filepath3='/Users/shanyu/Dropbox/DMC/dmc2018/data/raw_data/items.csv'

alltest1=readRDS(filepath1)
alltest1$pred.units <- c(alltest1$pred.units)
alltest2=readRDS(filepath2)
item=read.csv(filepath3,sep='|') %>% select(pid, size, stock)

SS <- alltest1 %>% left_join(alltest2,by=c('pid','size','date'))
SS$pred.units.y <- c(SS$pred.units.y)

alltest=SS %>% left_join(item,by=c('pid','size'))
  
alltest$pred_ensemble=0.4*alltest$pred.units.x+0.6*alltest$pred.units.y

stock <- unique(alltest %>% select(pid,size,stock))

Predict_Feb=Predict_sold(alltest$pred_ensemble,alltest %>% select(pid, size, date),
                         stock$stock,'poi')
