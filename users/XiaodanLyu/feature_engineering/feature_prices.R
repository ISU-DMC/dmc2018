rm(list = ls(all = T))
source("feature_engineering/help.r")

# library(tidyverse)
library(tidyr);library(dplyr)
library(lubridate)
## read raw datasets
train <- read.csv("../../data/train.csv", sep = "|", stringsAsFactors = F)
## load item static features
items <- readRDS("/vol/data/zhuz/lyux/feature_rds/item_static_features_may10.rds")

## dynamic feature
prices <- read.csv("../../data/prices.csv", sep = "|", stringsAsFactors = F)
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
prices_long %>% glimpse()
## price cut
library(readxl)
price_cut_relabel <- read_excel("feature_engineering/fct_relabel.xlsx", sheet = "price_cut")

prices_long %>% left_join(price_cut_relabel, by = c("price" = "old_levels")) -> prices_feature
any(is.na(prices_long))
prices_feature %>% glimpse()
prices_feature %>% is.na %>% any

## trend
trend.google <- read.csv("feature_engineering/google_trend_Yuchen.txt", header = T, sep = "|")
trend.google %>% mutate(date = ymd(date)) -> trend.google
trend.google %>% glimpse
trend.google %>% is.na %>% any

train <- train %>% mutate(date = ymd(date), size = replace(size, size == "", "42"))
prices_feature <- prices_feature %>% mutate(size = replace(size, size == "", "42"))
items <- items %>% mutate(pid = as.numeric(pid))

## join three datasets
alldata <- prices_feature %>% filter(date < ymd("2018-02-01")) %>%
  full_join(train, by = c("pid", "size", "date")) %>% 
  full_join(items, by = c("pid", "size")) %>%
  full_join(trend.google, by = "date") %>%
  filter(ymd(date)>=ymd(releaseDate)) %>% ## remove data before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100)

alldata %>% dplyr::select(pid, size) %>% unique %>% dim
any(is.na(alldata))

trendXstock <- as.matrix(alldata %>% dplyr::select(contains("trend"))) * alldata$stock
colnames(trendXstock) <- paste0("trendXstock_", colnames(trendXstock))
trendXprice <- as.matrix(alldata %>% dplyr::select(contains("trend"))) * alldata$price
colnames(trendXprice) <- paste0("trendXprice_", colnames(trendXprice))
trendXdiscount <- as.matrix(alldata %>% dplyr::select(contains("trend"))) * alldata$discount
colnames(trendXdiscount) <- paste0("trendXdiscount_", colnames(trendXdiscount))

(names.trend <- grep("trend", colnames(trend.google), value = T))
(names.freq <- grep("X", grep("freq", colnames(items), value = T), invert = TRUE, value = T))
names.trendXfreq <- combn(c(names.trend, names.freq), 2) %>% t()
names.selfXself <- rbind(expand.grid(names.trend, names.trend),
                         expand.grid(names.freq, names.freq))
names.trendXfreq.final <- data.frame(names.trendXfreq) %>%
  anti_join(names.selfXself, by = c("X1" = "Var1", "X2" = "Var2")) %>%
  mutate_if(is.factor, as.character)
names.trendXfreq.final %>% glimpse

trendXfreq_features <- data.frame(id = 1:nrow(alldata))
for(i in 1:nrow(names.trendXfreq.final)){
  name.var1 <- names.trendXfreq.final[i,1]
  name.var2 <- names.trendXfreq.final[i,2]
  trendXfreq_features <- cbind(trendXfreq_features,
                               alldata[,name.var1]*alldata[,name.var2])
  colnames(trendXfreq_features)[i+1] <- paste(name.var1, name.var2, sep = "X")
}
any(is.na(trendXfreq_features))

alldata_expand <- cbind(alldata, trendXstock, trendXprice, trendXdiscount, trendXfreq_features)
alldata_expand_date <- alldata_expand %>% mutate(
  date.age = (date - releaseDate) %>% as.numeric,
  date.day = (date - ymd("2017-09-30")) %>% as.numeric,
  # date.dm = day(date),
  date.wd = weekdays(date),
  date.wm = ceiling((day(date) + first_day_of_month_wday(date) - 1) / 7) %>% as.character
)
any(is.na(alldata_expand_date))

readr::write_rds(alldata_expand_date, "/vol/data/zhuz/lyux/feature_rds/all_features_may14.rds")
