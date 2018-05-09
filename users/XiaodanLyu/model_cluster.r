setwd("~/Google Drive/dmc2018/users/XiaodanLyu")

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

## join three datasets
alldata <- train %>% mutate(date = ymd(date)) %>%
  full_join(prices_long, by = c("pid", "size", "date")) %>% 
  full_join(items %>% mutate(pid = as.numeric(pid)), by = c("pid", "size")) %>%
  full_join(trend.google, by = "date") %>%
  filter(date>=releaseDate) %>% ## remove data before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100)

any(is.na(alldata %>% select(-units)))

## cluster results
cluster_hf <- read.table("cluster_distance/cluster_hengfang.txt", sep = "|", header = T)
cluster_hf %>% glimpse

data5.4 <- alldata %>%
  left_join(cluster_hf %>% filter(group5 == 4), by = c("pid", "size")) %>%
  select(-group4, -group5) %>% filter(date < ymd("2018-02-01"))

