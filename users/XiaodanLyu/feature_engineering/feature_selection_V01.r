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

