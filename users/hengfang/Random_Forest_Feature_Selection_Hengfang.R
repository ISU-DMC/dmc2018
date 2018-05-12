library(randomForest)
library(dplyr)
#alltrain_input <- readr::read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain.rds") 

alltrain_input <- readRDS("/work/STAT/hengfang/DMC/alltrain.rds")


#alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 1, ] # Yudi
# alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 2, ] # Yang
alltrain <- alltrain_input %>% .[1:nrow(.) %% 3 == 0, ] # Hengfang
matrix_x <- as.matrix(alltrain[,-1])
y <- as.matrix(alltrain[,1])
# system.time(RF <- randomForest(x = matrix_x[1:2e4,], y = y[1:2e4], mtry = 5))
# RF <- randomForest(x = matrix_x, y = y, mtry = 5)
importance_RF <- importance(RF) %>% data.frame %>% tibble::rownames_to_column() %>%
  arrange(desc(IncNodePurity))
sink("/work/STAT/hengfang/DMC/Feature_Results/random_forest_summary_V01.txt")
print(RF$mse)
sink()
write.csv(importance_RF, "/work/STAT/hengfang/DMC/Feature_Results/randomforest_importance_V01.csv", row.names = F, quote = F)