library("xgboost")
library("dplyr")

DATA <- readRDS("/work/STAT/hengfang/DMC/alltrain.rds")





xgb <- xgboost(data=(DATA %>% dplyr::select(-alldata_thin.units)), label=DATA$units, max_depth=40, eval_metric="rmse",
               subsample=0.6, eta=0.3, objective="count:poisson", nrounds = 160)
importance <- xgb.importance(colnames((DATA %>% dplyr::select(-alldata_thin.units))), model = xgb)  
head(importance)



saveRDS(importance, "/work/STAT/hengfang/DMC/Feature_Results/Xgboost_Feature_Selection_Importance_160.rds")




