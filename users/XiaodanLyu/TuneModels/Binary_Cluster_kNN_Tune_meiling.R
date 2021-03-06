library("caret")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("doMC")
library("pROC")
registerDoMC(cores = 16)

## Your netid
netid <- "meiling"
## month = 1 or 3
Month <- 1
## cluster = 5 or 9
Cluster <- 5
## k = one of 1:Cluster
k <- 3

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

Train <- Train[Train[,cluster_id] == k,] %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)
Test <- Test[Test[,cluster_id] == k,] %>%
	 select_if(function(col) is.numeric(col)) %>% select(-pid)

## delete columns with only one level
var.out <- names(Train)[apply(Train, 2, function(x) length(unique(x)) == 1)]
Train <- Train %>% select(-one_of(var.out))
Test <- Test %>% select(-one_of(var.out))


T1 <- Sys.time()

cvControl <- trainControl(method = "repeatedcv", repeats = 1, number = 10, allowParallel = TRUE)
Tune <- train(y=ifelse(Train$units>0, 1, 0) %>% as.factor,
              x=Train %>% select(-units),
              method="knn",
              preProcess = c("center","scale"),
              tuneGrid=data.frame(.k=50),
              trControl=cvControl) ## repeats = 10
Sys.time() - T1


Pred <- predict(Tune, newdata=Test, type = "prob")
write_rds(Pred, sprintf("%sBinaryPredJankNN_Month%s_C%s_%s.rds", Result_Dir_True, Month, Cluster, k))

# AUC <- roc(ifelse(Test$units>0, 1, 0), Pred$`1`)$auc

sink(sprintf("%sBinaryPredkNN_Month%s_C%s_%s.txt", Result_Dir_True, Month, Cluster, k))
# print(AUC)
cat("BestTune:", fill = T)
print(Tune$results %>% filter(Accuracy == max(Accuracy)))
cat("TuneResults:", fill = T)
print(Tune$results)
sink()
