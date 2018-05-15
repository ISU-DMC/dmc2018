library(ggplot2)
library(xgboost)
library(pROC)
library(tidyverse)
library(readr)

code_LLR <- function(train,response) {
  name <- colnames(train)
  fac <- which(sapply(train[1,], is.factor))
  code <- do.call("rbind", lapply(fac, LLR_cate, train, response))
  names(code) <- c("name.feature", "level", "value.feature")
  code <- code %>% 
    separate(name.feature, into = c("name.variable", "type.feature"), sep = "_", remove = FALSE)
  return(code)
}

LLR_cate <- function(i,train,response) {
  x_cate <- train[,i]
  df <- data.frame(name=rep(paste0(colnames(train)[i],'_LLR'),length(levels(x_cate))),
                   log((table(x_cate[response==1])+1)/(table(x_cate[response==0])+1)))
  #print(i)
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



TrainGeo = readRDS('/Users/shanyu/Desktop/Geo_LLR_alltrain_subfeatures_may14.rds') %>%
  mutate_if(is.character,as.factor) %>% filter(Cluster_9==cluster)

Id = TrainGeo %>% select(pid,size,date) %>% unique


N <- nrow(Id)
ord <- sample(N, N, replace = FALSE)

num_cv <- 10
set <- as.numeric(cut_number(1:N,num_cv))

paras<-expand.grid(nrounds=c(20,30,50,100),max_depth=5,
                   subsample=0.5)
num_paras <- nrow(paras)

MSPE1 <- MSPE2 <- matrix(NA,nrow=num_cv,ncol=num_paras)

cv <- function(i) {
  train <- inner_join(Id[-ord[set==i],],TrainGeo)
  test <- inner_join(Id[ord[set==i],],TrainGeo)
  train <- train %>% select(-pid, -size, -date, -(Cluster_2:Cluster_9))
  test <- test %>% select(-pid, -size, -date, -(Cluster_2:Cluster_9))
  
  codebook <- code_LLR(train, response = train$response)
  
  train <- feature(train,codebook)
  test <- feature(test,codebook)
  
  train <- train[,sapply(train[1,],is.numeric)]
  test <- test[,sapply(test[1,],is.numeric)]
  
  for(j in 1:num_paras) {
    fit_tmp <- xgboost(data.matrix(train[,-1]), train[,1], objective="binary:logistic", 
                       nrounds = paras[j,1], max_depth = paras[j,2], subsample = paras[j,3])
    
    pred_test <- predict(fit_tmp, newdata = data.matrix(test[,-1]))   
    MSPE1[i,j] <- mean(pred_test - test[,1])^2
    MSPE2[i,j] <- auc(roc(test[,1],pred_test))
  }
  
}

results <- ma















