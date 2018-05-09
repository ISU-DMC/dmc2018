setwd("~/Google Drive/dmc2018/users/XiaodanLyu")

library(tidyverse)
library(lubridate)
## read raw datasets
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
prices <- read.csv("../../data/raw_data/prices.csv", sep = "|", stringsAsFactors = F)

## load item static features
items <- readRDS("feature_engineering/item_static_features.rds")

## join three tables
prices_long <- prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd())  %>% 
  group_by(pid, size) %>% 
  filter(!is.na(price)) %>%
  mutate(
    price.lag1.diff = price - lag(price),
    price.lag1.reldiff = price.lag1.diff/lag(price)*100,
    price.lag1.diff = replace(price.lag1.diff, is.na(price.lag1.diff), 0),
    price.lag1.reldiff = replace(price.lag1.reldiff, is.na(price.lag1.reldiff), 0)) %>%
  ungroup 

prices_long %>% summary()

## trend
trend.google <- read.csv("feature_engineering/google_trend_Yuchen.txt", header = T, sep = "|")
trend.google %>% mutate(date = mdy(date)) -> trend.google

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

alldata %>% select(pid, size) %>% unique %>% dim

any(is.na(alldata %>% select(-units)))

## cluster results
cluster_hf <- read.table("cluster_distance/cluster_hengfang.txt", sep = "|", header = T)
cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))
cluster_hf %>% glimpse
table(cluster_hf$group5)

## change group5 indicator
data5 <- cluster_hf %>% filter(group5 == 1) %>%
  left_join(alldata, by = c("pid", "size")) %>%
  select(-group4, -group5) %>% filter(date < ymd("2018-02-01"))
## check number of products in selected group
data5 %>% select(pid, size) %>% unique %>% dim
