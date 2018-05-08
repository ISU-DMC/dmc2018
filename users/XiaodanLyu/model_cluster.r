setwd("~/Google Drive/dmc2018/users/XiaodanLyu")

## read raw datasets
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
prices <- read.csv("../../data/raw_data/prices.csv", sep = "|", stringsAsFactors = F)

## load item static features
items <- read_rds("feature_engineering/item_static_features.rds")

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
trend.google <- read.csv("feature_engineering/")

## join three datasets
alldata <- left_join(prices_long, train, by = c("pid", "size", "date")) %>% 
  left_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate) %>% ## remove data before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100)

## cluster results
cluster_hf <- read.csv("cluster_distance/cluster_hengfang.csv")
