rm(list = ls())

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

cluster=3
method='knn'

filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalTune/FebPredxgbTree_Month1_C4_1_3_Complete.rds'
filepath3='/Users/shanyu/Dropbox/DMC/dmc2018/data/raw_data/items.csv'

alltest=readRDS(filepath)
alltest$pred.units <- c(alltest$pred.units)

item=read.csv(filepath3,sep='|') 
item %>% filter(category == 2 & subCategory == 27) %>% select(pid) %>% unlist -> shoelaces
item <- item %>% select(pid,size,stock)

SS <- alltest %>% 
  mutate(size = replace(size, pid %in% shoelaces, ""))
SS$pred.units.y <- c(SS$pred.units.y)

alltest=SS %>% left_join(item,by=c('pid','size'))

alltest$pred_ensemble=alltest$pred.units
alltest %>% select(pid, size) %>% unique %>% dim
sum(is.na(alltest$stock))

stock <- unique(alltest %>% select(pid,size,stock))

Predict_Feb=Predict_sold(alltest$pred_ensemble,alltest %>% select(pid, size, date),
                         stock$stock,'poi')
Predict_Feb$Pred=floor(Predict_Feb$Pred)


Predict_Feb$Pred=ymd('2018-01-31')+Predict_Feb$Pred
names(Predict_Feb) <- c('pid','size','soldOutDate')
write.table(Predict_Feb,'/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/ConditionalMedian_C3.csv',
            sep="|", row.names = F, quote = F)


