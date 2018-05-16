rm(list = ls())

# source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
# source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

source('ShanYu3393/Loss_function.R')
source('ShanYu3393/generate_soldoutday.R')
setwd("XiaodanLyu/TuneResults/meiling_kNN_Binary")
k <- 4
Pred <- read_rds("BinaryKNN_version3/BinaryPredJankNN_Month1_C5_4.rds")
Data <- read_rds("/vol/data/zhuz/lyux/feature_rds/LLR_alltrain_subfeatures_may14.rds")
Index <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_cluster5_pcr_outlier_ID.rds")
Index <- Index %>% mutate(date = ymd(date))
Data_No_NA <- Data %>% filter(!is.na(Cluster_2))

cluster_id <- "Cluster_5"
Index_C <- Index %>% filter(cluster == cluster_id)
Data_C <- anti_join(Data_No_NA, Index_C)
Test <- Data_C %>% filter(date >= "2018-01-04")
Test <- Test %>% filter(get(cluster_id) == k) 

library(pROC)
alltest <- Test %>% select(pid, size, date, units) %>% 
  mutate(pred.prob = Pred$`0`)
roc(ifelse(alltest$units==0, 1, 0), alltest$pred.prob)$auc

# alltest <- readRDS('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Binary_kNN_5_3.rds')

True <- Stock_Soldoutday(alltest[,1:3],alltest[,4])

Error <- sqrt(sum(abs(Loss_MAE(alltest[,5],alltest[,1:3],True$stock,
                              True$SoldOutDay,'geom'))))
RG.Error <- sqrt(sum(abs(True$SoldOutDay - 15)))

sprintf("Model Error:%.5f", Error)
sprintf("Random Guess Error:%.5f", RG.Error)

## only for stock == 1
alltest.lowstock <- alltest %>% group_by(pid, size) %>%
  mutate(stock = sum(units)) %>% 
  filter(stock == 1) %>% 
  # filter(stock == 1 | stock == 2 | stock == 3) %>%
  ungroup

True <- alltest.lowstock %>% group_by(pid, size) %>%
  arrange(date) %>% 
  summarise(SoldOutDay = which(units == 1),
            pred.SoldOutDay = which.max(pred.prob))

Err <- sqrt(sum(abs(True$SoldOutDay-True$pred.SoldOutDay)))

RG.Error <- sqrt(sum(abs(True$SoldOutDay - 15)))

sprintf("Model Error:%.5f", Error)
sprintf("Random Guess Error:%.5f", RG.Error)
