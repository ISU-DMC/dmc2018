library("caret")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("doMC")
registerDoMC(cores = 10)

## Your netid
netid <- "meiling"
## month = 1 or 3
Month <- 1
## cluster = 5 or 9
Cluster <- 5
## k = one of 1:Cluster
k <- 2

## create folder
Result_Dir_True <- sprintf("/work/STAT/%s/Tune_Results/", netid)
dir.create(Result_Dir_True)

# Cluster ID
cluster_id <- paste0("Cluster_", Cluster)

# Data read folder
WD <- "/work/STAT/lyux/dmc2018/"
setwd(WD)
#T1 <- Sys.time()
Data <- read_rds("Cache/alltrain_sub_prc_may15.rds")
Index <- read_rds("Cache/alltrain_freq4_cluster5_pcr_outlier_ID.rds")
Index <- Index %>% mutate(date = ymd(date))
#Sys.time() - T1

Data_No_NA <- Data %>% filter(!is.na(Cluster_2))

Index_C <- Index %>% filter(cluster == cluster_id)
Data_C <- anti_join(Data_No_NA, Index_C)


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

Train <- Train[Train[,cluster_id] == k, ] %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)
Test <- Test[Test[,cluster_id] == k, ] %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)

## delete columns with only one level
var.out <- names(Train)[apply(Train, 2, function(x) length(unique(x)) == 1)]
Train <- Train %>% select(-one_of(var.out))
Test <- Test %>% select(-one_of(var.out))


T1 <- Sys.time()

cvControl <- trainControl(method = "repeatedcv", repeats = 10, number = 10, allowParallel = TRUE)
TuneGlmnet <- train(y=Train$units,
                    x=Train %>% select(-units),
                    method = "glmnet",
                    family = "poisson",
                    tuneGrid=data.frame(alpha = seq(0, 1, length.out = 5),
                                        lambda = seq(0, 1, length.out = 5)),
                    trControl=cvControl) ## repeats = 10
Sys.time() - T1


Pred <- predict(TuneGlmnet, newdata=Test)
write_rds(Pred, sprintf("%sPredJanGlmnet_Month%s_C%s_%s.rds", Result_Dir_True, Month, Cluster, k))


MSE <- function(x, y)
{
	sum((x - y)^2)/length(x)
}

MSPE <- MSE(Pred, Test$units)

sink(sprintf("%sErrorGlmnet_Month%s_C%s_%s.txt", Result_Dir_True, Month, Cluster, k))
sprintf("Prediction Error: %.5f", MSPE)
cat("BestTune:", fill = T)
print(TuneGlmnet$results %>% filter(RMSE == min(RMSE)))
cat("ResultsTune:", fill = T)
print(TuneGlmnet$results)
sink()
