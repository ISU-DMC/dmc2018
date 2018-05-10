setwd("~/Google Drive/dmc2018/users/XiaodanLyu")

library(tidyverse)
library(lubridate)
## read raw datasets
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
## load item static features
items <- readRDS("feature_engineering/item_static_features_may9.rds")
prices_long <- readRDS("feature_engineering/prices_feature_may9.rds")

## trend
trend.google <- read.csv("feature_engineering/google_trend_Yuchen.txt", header = T, sep = "|")
trend.google %>% mutate(date = ymd(date)) -> trend.google
trend.google %>% glimpse

train <- train %>% mutate(date = ymd(date), size = replace(size, size == "", "42"))
prices_long <- prices_long %>% mutate(size = replace(size, size == "", "42"))
items <- items %>% mutate(pid = as.numeric(pid))

## join three datasets
alldata <- train %>%
  full_join(prices_long, by = c("pid", "size", "date")) %>% 
  full_join(items, by = c("pid", "size")) %>%
  full_join(trend.google, by = "date") %>%
  filter(ymd(date)>=ymd(releaseDate)) %>% ## remove data before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100)

alldata %>% dplyr::select(pid, size) %>% unique %>% dim
any(is.na(alldata %>% dplyr::select(-units)))

trendXstock <- as.matrix(alldata %>% dplyr::select(contains("May"))) * alldata$stock
colnames(trendXstock) <- paste0("trendXstock_", colnames(trendXstock))
trendXprice <- as.matrix(alldata %>% dplyr::select(contains("May"))) * alldata$price
colnames(trendXprice) <- paste0("trendXprice_", colnames(trendXprice))
trendXdiscount <- as.matrix(alldata %>% dplyr::select(contains("May"))) * alldata$discount
colnames(trendXdiscount) <- paste0("trendXdiscount_", colnames(trendXdiscount))

alldata_expand <- cbind(alldata, trendXstock, trendXprice, trendXdiscount)
alldata_expand_date <- alldata_expand %>% mutate(
  day = day(date) %>% as.character,
  weekday = weekdays(date),
  monthweek = ceiling((day(date) + first_day_of_month_wday(date) - 1) / 7) %>% as.character
)

write_rds(alldata_expand_date, "all_features_may9.rds")

## hengfang_cluster_allproducts_group5
# cluster_hf <- read.table("cluster_distance/cluster_hengfang.txt", sep = "|", header = T)
# cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))
# cluster_hf %>% glimpse
# table(cluster_hf$group5)
# 
# data5 <- cluster_hf %>% filter(group5 == 1) %>%
#   left_join(alldata, by = c("pid", "size")) %>%
#   select(-size1, -size2, -size3) %>% 
#   select(-group4, -group5) %>% filter(date < ymd("2018-02-01"))
# ## check number of products in selected group
# data5 %>% select(pid, size) %>% unique %>% dim

## cluster_freq4_group9
# filename <- "cluster_distance/cluster_yan_freq4_group9.RDS"
filename <- "cluster_distance/cluster_hengfang_freq4.rds"
cluster <- read_rds(filename)
cluster <- cluster %>% mutate(size = replace(size, size == "", "42"))
names(cluster)[3] <- "group9"
cluster %>% glimpse
table(cluster$group9)
train <- cluster %>% filter(group9 == 1) %>%
  left_join(alldata, by = c("pid", "size")) %>%
  mutate()
  dplyr::select(-size1, -size2, -size3, -group9) %>% 
  filter(date < ymd("2018-02-01"))
## check number of products in selected group
train %>% dplyr::select(pid, size) %>% unique %>% dim

## create matrix_feature
matrix_x <- model.matrix(units ~ .,
                         data = train %>% dplyr::select(-pid, -size, -date, -releaseDate))
## xgboost poisson
xgb <- xgboost(data=matrix_x, label=train$units, max_depth=40, eval_metric="rmse",
               subsample=0.6, eta=0.3, objective="count:poisson", nrounds = 160)
importance <- xgb.importance(colnames(matrix_x), model = xgb)  
head(importance)  
xgb.plot.importance(importance_matrix = importance,top_n = 20) 
