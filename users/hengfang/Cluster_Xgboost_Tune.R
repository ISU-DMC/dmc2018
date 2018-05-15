library("caret")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library(doMC)
registerDoMC(cores = 13)



## Your netid
netid <- "hengfang"
## month = 1 or 3
Month <- 3
## cluster = 5 or 9
Cluster <- 5
## k = one of 1:Cluster
k <- 1

## create folder
Result_Dir_True <- sprintf("/work/STAT/%s/Tune_Results/", netid)
dir.create(Result_Dir_True)

# Cluster ID
cluster_id <- paste0("Cluster_", Cluster)

# Data read folder
WD <- "/work/STAT/lyux/dmc2018/"
setwd(WD)
#T1 <- Sys.time()
Data <- read_rds("Cache/LLR_alltrain_subfeatures_may14.rds")
Index <- read_rds("Cache/alltrain_freq4_outlier_ID.rds")
Index <- Index %>% mutate(date = ymd(date))
#Sys.time() - T1

Data_No_NA <- Data %>% filter(!is.na(Cluster_2))

Index_C <- Index %>% filter(cluster == cluster_id)
#Index_C9 <- Index %>% filter(cluster == "Cluster_9")

Data_C <- anti_join(Data_No_NA, Index_C)
#Data_C9 <- anti_join(Data_No_NA, Index_C9)


# Data Preprocessing

# Eliminate 
# 3-month
if(Month == 3){

	Train <- Data_C %>% filter(date <= "2018-01-03")
	Test <- Data_C %>% filter(date >= "2018-01-04")

}else{

	Train <- Data_C %>% filter(date <= "2018-01-03", date >= "2017-12-04")
	Test <- Data_C %>% filter(date >= "2018-01-04")

}

Train <- Train %>% filter(get(cluster_id) == k) %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)
Test <- Test %>% filter(get(cluster_id) == k) %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)

## delete columns with only one level
var.out <- names(Train)[apply(Train, 2, function(x) length(unique(x)) == 1)]
Train <- Train %>% select(-one_of(var.out))
Test <- Test %>% select(-one_of(var.out))


T1 <- Sys.time()


Tunexgboost <- train(y = Train$units,
	x = Train %>% select(-units),
	method = 'xgbTree',
	preProcess = c("center","scale"),
	tuneGrid = data.frame(
		expand.grid(nrounds = c(50,100,150),
		max_depth = c(1,2,3), eta= c(0.3, 0.4),
		subsample= c(0.5, 0.6, 0.75), 
		gamma= 0, min_child_weight = 1, colsample_bytree= c(0.8,1)),   
	trControl=trainControl(method="repeatedcv",repeats=1,number=10,
		allowParallel = TRUE), maximize = F)


Sys.time() - T1


Xg_Pred_Log <- predict(Tunexgboost, newdata=Test)
Error <- Xg_Pred_Log - Test$units
#write_rds(Tunexgboost, sprintf("%sTunexgboost_Month%s_C%s_%s.rds", Result_Dir_True, Month, Cluster, k))
write_rds(Error, sprintf("%sErrorxgboost_Month%s_C%s_%s.rds", Result_Dir_True, Month, Cluster, k))


MSE <- function(x, y)
{
	sum((x - y)^2)/length(x)
}

MSPE <- MSE(Xg_Pred_Log, Test$units)

sink(sprintf("%sErrorxgboost_Month%s_C%s_%s.txt", Result_Dir_True, Month, Cluster, k))
sprintf("Prediction Error: %.5f", MSPE)
cat("BestTune:", fill = T)
print(Tunexgboost$results %>% filter(RMSE == min(RMSE)))
sink()

# write_rds(Error, sprintf("%sErrorxgboost_Month%s_C5_%s.rds",Result_Dir_True, Month,  k ))
















