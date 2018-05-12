library("xgboost")
library("dplyr")

DATA <- readRDS("/work/STAT/hengfang/DMC/alltrain.rds")


Column_Names <- apply(DATA, MARGIN = 2, FUN = class)


saveRDS(Column_Names, "/work/STAT/hengfang/DMC/Feature_Results/Column_Names.rds")