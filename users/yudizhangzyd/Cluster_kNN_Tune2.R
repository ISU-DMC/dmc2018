library("caret")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("doMC")
registerDoMC(cores = 16)

## Your netid
netid <- "yudiz"
## month = 1 or 3
Month <- 1
## cluster = 4
Cluster <- 4
## k = one of 1:Cluster
k <- 4

## create folder
Result_Dir_True <- sprintf("/work/STAT/%s/Tune_Results/", netid)
dir.create(Result_Dir_True)

# Cluster ID
cluster_id <- paste0("Cluster_", Cluster)

# Data read folder
WD <- "/work/STAT/lyux/dmc2018/"
setwd(WD)
#T1 <- Sys.time()
Data <- read_rds("Cache/alltrain_sub_prc_cl4_may16.rds")
# Index <- read_rds("Cache/alltrain_freq4_outlier_ID.rds")
# Index <- Index %>% mutate(date = ymd(date))
#Sys.time() - T1

# Data_No_NA <- Data %>% filter(!is.na(Cluster_2))

# Index_C <- Index %>% filter(cluster == cluster_id)
# Data_C <- anti_join(Data, Index_C)


# Data Preprocessing

# Eliminate 
# 3-month
if(Month == 3){

	Train <- Data %>% filter(date <= "2018-01-03")
	Test <- Data %>% filter(date >= "2018-01-04")

}else{

	Train <- Data %>% filter(date <= "2018-01-03", date >= "2017-12-04")
	Test <- Data %>% filter(date >= "2018-01-04")

}

Train <- Train[Train[,cluster_id] == k,] %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)
# Test <- Test[Test[,cluster_id] == k,] %>%
	 # select_if(function(col) is.numeric(col)) %>% select(-pid)

## delete columns with only one level
var.out <- names(Train)[apply(Train, 2, function(x) length(unique(x)) == 1)]
Train <- Train %>% select(-one_of(var.out))
# Test <- Test %>% select(-one_of(var.out))


T1 <- Sys.time()

cvControl <- trainControl(method = "repeatedcv", repeats = 10, number = 10, allowParallel = TRUE)
Tune <- train(y=Train$units,
                  x=Train %>% select(-units),
                  method="knn",
                  preProcess = c("center","scale"),
                  tuneGrid=data.frame(.k=seq(30, 40, by = 2)),
                  trControl=cvControl) ## repeats = 10
Sys.time() - T1


Pred <- predict(Tune, newdata=Test)
Test_results <- Test %>% select(pid, size, date) %>% 
  mutate(units.pred = Pred)
write_rds(Test_results, sprintf("%sPredKNN_Month%s_C%s_%s.rds", Result_Dir_True, Month, Cluster, k))


MSE <- function(x, y)
{
	sum((x - y)^2)/length(x)
}

MSPE <- MSE(Pred, Test$units)

sink(sprintf("%sErrorKNN_Month%s_C%s_%s.txt", Result_Dir_True, Month, Cluster, k))
sprintf("Prediction Error: %.5f", MSPE)
cat("BestTune:", fill = T)
print(Tune$results %>% filter(RMSE == min(RMSE)))
cat("TuneResults:", fill = T)
print(Tune$results)
sink()
