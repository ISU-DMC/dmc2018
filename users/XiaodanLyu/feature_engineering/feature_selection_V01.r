## response == "units" ####
alltrain <- readr::read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain.rds") 

## feature selection
## xgboost
library(dplyr)
library(xgboost)
library(readr)
sink("xgboost_rmse_V01.txt")
matrix_x <- as.matrix(alltrain[,-1])
xgb <- xgboost(data=matrix_x, label=alltrain$units, max_depth=40, eval_metric="rmse",
               subsample=0.6, eta=0.3, objective="count:poisson", nrounds = 160)
sink()
importance_xgb <- xgb.importance(colnames(matrix_x), model = xgb)  
write.csv(importance_xgb, "xgboost_importance_V01.csv", row.names = F, quote = F)
# head(importance)  
# xgb.plot.importance(importance_matrix = importance,top_n = 20) 

## lasso
# library(glmnet)
# matrix_x <- as.matrix(alltrain[,-1])
# y <- as.matrix(alltrain$units)
# a = matrix_x[1:1000,-1]
# b = y[1:1e3]
# max(abs(b%*%a/1e3))
# coef_lasso <- coef(
#   a <- glmnet(x = matrix_x[1:1000,-1], y = y[1:1000], alpha = 1,
#                           family = "poisson", nlambda = 10, maxit = 1e6)
#   )
# data.frame(var = rownames(coef_lasso), 
#            avg = rowMeans(abs(coef_lasso)) %>% round(3)) %>% arrange(desc(avg))
# write.csv(coef_lasso, "lasso_coef_V01.csv", row.names = F, quote = F)
# ## ridge
# coef_ridge <- coef(glmnet(x = matrix_x[,-1], y = y, alpha = 0,
#                           family = "poisson", lambda = 0.5))
# write.csv(coef_ridge, "ridge_coef_V01.csv", row.names = F, quote = F)

## random forest
library(randomForest)
library(dplyr)
alltrain_input <- readr::read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain.rds") 
alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 1, ] # Yudi
# alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 2, ] # Yang
# alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 0, ] # Hengfang
matrix_x <- as.matrix(alltrain[,-1])
y <- as.matrix(alltrain$units)
# system.time(RF <- randomForest(x = matrix_x[1:2e4,], y = y[1:2e4], mtry = 5))
RF <- randomForest(x = matrix_x, y = y, mtry = 5)
importance_RF <- importance(RF) %>% data.frame %>% tibble::rownames_to_column() %>%
  arrange(desc(IncNodePurity))
sink("random_forest_summary_V01.txt")
print(RF$mse)
sink()
write.csv(importance_RF, "randomforest_importance_V01.csv", row.names = F, quote = F)



## response == "wait_time"
rm(list = ls(all = T))
alltrain <- readr::read_rds("/vol/data/zhuz/lyux/feature_rds/WT_alltrain.rds") 
## feature selection
## xgboost
library(dplyr)
library(xgboost)
library(readr)
sink("WT_xgboost_rmse_V01.txt")
matrix_x <- as.matrix(alltrain[,-1])
xgb <- xgboost(data=matrix_x, label=alltrain$time, max_depth=40, eval_metric="rmse",
               subsample=0.6, eta=0.3, objective="count:poisson", nrounds = 160)
sink()
importance_xgb <- xgb.importance(colnames(matrix_x), model = xgb)  
write.csv(importance_xgb, "WT_xgboost_importance_V01.csv", row.names = F, quote = F)
# head(importance)  
# xgb.plot.importance(importance_matrix = importance,top_n = 20) 

## random forest
library(randomForest)
library(dplyr)
alltrain <- readr::read_rds("/vol/data/zhuz/lyux/feature_rds/WT_alltrain.rds") 
matrix_x <- as.matrix(alltrain[,-1])
y <- as.matrix(alltrain$time)
# system.time(RF <- randomForest(x = matrix_x[1:2e4,], y = y[1:2e4], mtry = 5))
RF <- randomForest(x = matrix_x, y = y, mtry = 5)
importance_RF <- importance(RF) %>% data.frame %>% tibble::rownames_to_column() %>%
  arrange(desc(IncNodePurity))
sink("WT_randomforest_summary_V01.txt")
print(RF$mse) 
sink()
write.csv(importance_RF, "WT_randomforest_importance_V01.csv", row.names = F, quote = F)

## cross-validation
library(caret)
ForestTune <- train(y=y,
                    x=matrix_x,
                    tuneGrid=data.frame(mtry=5),
                    method="rf",ntree=500,
                    trControl=trainControl(method="oob"))
RF <- ForestTune$finalModel
importance_RF <- importance(RF) %>% data.frame %>% tibble::rownames_to_column() %>%
  arrange(desc(IncNodePurity))
sink("WT_randomforest_summary_V01.txt")
print(RF$mse) 
sink()
write.csv(importance_RF, "WT_randomforest_importance_V01.csv", row.names = F, quote = F)

xgb_grid = expand.grid(
  eta = 0.3,
  nrounds = 50,
  max_depth = 40,
  subsample = 0.6
)
NumX <- matrix_x
for (i in 1:ncol(matrix_x)) {
   matrix_x[,i] <- as.double(matrix_x[,i])
}
XGBTune <- train(y=y,
                 x=NumX,
                 method="xgbTree",
                 tuneGrid = xgb_grid,
                 trControl=trainControl(method="repeatedcv",
                                        repeats=1, number=10))


