rm(list = ls())

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

cluster=2
method='knn'

filepath1=paste0('/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalTune/FebPred',
                 method,'_Month1_C4_',cluster,'.rds')
filepath2=paste0("/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalTune/FebPredxgbTree_Month1_C4_",
                 cluster,'.rds')
filepath3='/Users/shanyu/Dropbox/DMC/dmc2018/data/raw_data/items.csv'

alltest1=readRDS(filepath1)
alltest1$pred.units <- c(alltest1$pred.units)
alltest2=readRDS(filepath2)
item=read.csv(filepath3,sep='|') %>% select(pid, size, stock) %>%
  mutate(size = replace(size, size == "", "42"))

SS <- alltest1 %>% left_join(alltest2,by=c('pid','size','date'))
SS$pred.units.y <- c(SS$pred.units.y)

alltest=SS %>% left_join(item,by=c('pid','size'))

alltest$pred_ensemble=0*alltest$pred.units.x+1*alltest$pred.units.y

stock <- unique(alltest %>% select(pid,size,stock))

Predict_Feb=Predict_sold_mean(alltest$pred_ensemble,alltest %>% select(pid, size, date),
                         stock$stock,'poi')



Predict_Feb$Pred=ymd('2018-01-31')+Predict_Feb$Pred
names(Predict_Feb) <- c('pid','size','soldOutDate')
write.table(Predict_Feb,'/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/ConditionalMedian_C2.csv',
            sep="|", row.names = F, quote = F)


