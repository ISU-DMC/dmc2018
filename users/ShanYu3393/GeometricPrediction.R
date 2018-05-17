rm(list = ls())
library(xgboost)
library(ggplot2)
library(pROC)
library(tidyr);library(dplyr)
library(readr)

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

code_LLR <- function(train,response) {
  name <- colnames(train)
  fac <- which(sapply(train[1,], is.factor))
  code <- do.call("rbind", lapply(fac, LLR_cate, train, response = train[,1]))
  names(code) <- c("name.feature", "level", "value.feature")
  code <- code %>% 
    separate(name.feature, into = c("name.variable", "type.feature"), sep = "_", remove = FALSE)
  return(code)
}

LLR_cate <- function(i,train,response) {
  x_cate <- train[,i]
  df <- data.frame(name=rep(paste0(colnames(train)[i],'_LLR'),length(levels(x_cate))),
                   log((table(x_cate[response==1])+1)/(table(x_cate[response==0])+1)))
  return(df)
}

feature <- function(train, codebook){
  # browser()
  fac <- which(sapply(train[1,], is.factor))
  iter <- 0
  repeat({
    iter <- iter + 1
    name.fac <- colnames(train)[fac[iter]]
    code.fac <- codebook %>% filter(name.variable == name.fac) %>%
      dplyr::select(-type.feature, -name.variable) %>% spread(name.feature, value.feature)
    names(code.fac)[1] <- name.fac 
    train <- train %>% left_join(code.fac, by = name.fac)
    if(iter >= length(fac)) break
  })
  
  return(train)
}

TrainGeo_all = readRDS('/Users/shanyu/Desktop/DMC/Geo_LLR_alltrain_subfeatures_may14.rds')
## read in data

for (cluster in 1:4){
  TrainGeo = TrainGeo_all %>%
    mutate_if(is.character,as.factor) %>% filter(Cluster_4==cluster, date.day <= 95) %>%
    dplyr::select(-(Cluster_2:Cluster_9)) 
  
  ID_train=TrainGeo %>% group_by(pid,size,date) %>% tally()
  
  ID_train=left_join(TrainGeo, ID_train, by=c('pid','size','date')) %>% 
    dplyr::select(pid,size,date,n)
  
  TrainGeo = TrainGeo %>% dplyr::select(-pid, -size, -date)
  
  codebook <- code_LLR(TrainGeo, response = train$units)
  
  TrainGeo <- feature(TrainGeo,codebook)
  
  TrainGeo <- TrainGeo[,sapply(TrainGeo[1,],is.numeric)]
  
  xgb<-xgboost(data=data.matrix(TrainGeo[,-1]),label=TrainGeo[,1],
               weight = 1/ID_train$n,
               eta=0.3, max_depth=3, colsample_bytree = 1, 
               subsample=0.75, nrounds = 100,
               objective="binary:logistic")
  
  importance=xgb.importance(colnames(data.matrix(TrainGeo[,-1])), model = xgb)  
  xgb.plot.importance(importance_matrix = importance,top_n = 50)
  
  # --------- read in feature
  filepath='/Users/shanyu/Desktop/LLR_alltrain_subfeatures_may14.rds'
  Test_data <- readRDS(filepath) %>% mutate_if(is.character,as.factor) 
  Test_feature <- Test_data %>% filter(Cluster_4==cluster, date.day > 95) %>%
    dplyr::select(-(Cluster_2:Cluster_4)) %>% select(-pid, -size, -date)
  TRUE_data <- Test_data  %>% filter(Cluster_4==cluster, date.day > 95) %>%
    dplyr::select(-(Cluster_2:Cluster_4)) %>% select(pid, size, date, units)
  
  # ----------- create LLR
  test <- feature(Test_feature,codebook)
  test <- test[,sapply(test[1,],is.numeric)]
  
  y_pred<-predict(xgb,data.matrix(test[,-1]),type="response") 
  
  # --------- predict sold out day
  Sold <- Stock_Soldoutday(TRUE_data[,1:3],TRUE_data[,4])
  Result <- Loss_MAE(y_pred, TRUE_data[,1:3], Sold$stock, 
                     Sold$SoldOutDay, 'geom')
  hist(Result)
  Error <- sqrt(sum(abs(Result)))
  Error 
}













