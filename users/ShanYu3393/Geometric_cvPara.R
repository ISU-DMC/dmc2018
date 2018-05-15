library(ggplot2)
library(xgboost)
library(pROC)
library(tidyr);library(dplyr)
library(readr)
library(parallel)

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

Fold_train <- function(i,Train_10_12,ord,set) {
  
  train <- inner_join(Id[-ord[set==i],],Train_10_12)
  test <- inner_join(Id[ord[set==i],],Train_10_12)
  train <- train %>% select(-pid, -size, -date)
  test <- test %>% select(-pid, -size, -date)
  
  
  codebook <- code_LLR(train, response = train$units)
  
  train <- feature(train,codebook)
  test <- feature(test,codebook)
  
  train <- train[,sapply(train[1,],is.numeric)]
  test <- test[,sapply(test[1,],is.numeric)]
  
  MSPE2 = MSPE1 = rep(NA,num_paras)
  
  for(j in 1:num_paras) {
    
    fit_tmp <- xgboost(data=data.matrix(train[,-1]), label=train[,1], objective="binary:logistic", 
                       nrounds = paras[j,1], max_depth = paras[j,2], subsample = paras[j,3])
    
    pred_test <- predict(fit_tmp, newdata = data.matrix(test[,-1]))
    MSPE1[j] <- mean(pred_test - test[,1])^2
    MSPE2[j] <- auc(roc(test[,1],pred_test))
   
  }
  c(MSPE1,MSPE2)
}

Lowstock_CV_10 <- function(Train_10_12) {
  N <- nrow(Train_10_12)
  ord <- sample(N, N, replace = FALSE)
  set <- as.numeric(cut_number(1:N,num_cv))
  
  t1 <- Sys.time()
  result=mclapply(1:num_cv, Fold_train,  mc.cores=1,
                  Train_10_12=Train_10_12, ord=ord, set=set)
  result=do.call(rbind,result)
  Sys.time() - t1
  
  return(result)
}

TrainGeo = readRDS('Geo_LLR_alltrain_subfeatures_may14.rds') %>%
  mutate_if(is.character,as.factor)
Id = TrainGeo %>% select(pid,size,date) %>% unique

paras <- expand.grid(nrounds=c(10,20,30,40,50,80,100),
                     max_depth=c(4,6,8,10),
                     subsample=c(0.5,0.6,0.7,0.8))
num_paras <- nrow(paras)
num_cv <- 10

cluster=1
  
Result <- Lowstock_CV_10(TrainGeo %>% 
            filter(Cluster_9==cluster, date.day <= 95) %>% 
            dplyr::select(-(Cluster_2:Cluster_9))) 

write.csv(Result,paste0('Result/GeoLowCVErr_3_',cluster,'.csv'))    








