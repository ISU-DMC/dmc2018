## ---- splitting
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
prices <- read.csv("../../data/raw_data/prices.csv", sep = "|", stringsAsFactors = F)
items <- read.csv("../../data/raw_data/items.csv", sep = "|", stringsAsFactors = F)

## format date
library(lubridate)
library(tidyverse)
train <- train %>% mutate(date = ymd(date))
items <- items %>% mutate(releaseDate = ymd(releaseDate))
prices_long <- prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd())

## join three datasets
alldata <- prices_long %>% 
  full_join(train, by = c("pid", "size", "date")) %>% 
  full_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate-1) %>% ## only keep price info since one day before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0))

## key variable for identifying item
alldata <- alldata %>% mutate(key = paste(pid, size, sep = " - "))
## check sales before and in January
sale.beforeJan <- alldata %>% filter(date < ymd("2018-01-01")) %>%
  group_by(key) %>% 
  summarise(nsale.beforeJan = sum(units))
sale.Jan <- alldata %>% filter(date >= ymd("2018-01-01")) %>%
  group_by(key) %>%
  summarise(nsale.Jan = sum(units, na.rm = T))
sale.beforeJan %>% full_join(sale.Jan, by = "key") %>%
  mutate(nsale.beforeJan = replace(nsale.beforeJan, is.na(nsale.beforeJan), 0)) %>%
  filter(nsale.beforeJan==0 | nsale.Jan == 0) -> items.aside

items.aside %>% glimpse

## put aside those items not both sold before and in January
## put aside obs for February
subdata <- alldata %>% filter(!(key %in% items.aside$key), 
                              date < ymd("2018-02-01")) %>% select(-stock)
## randomly assign stock for Jan, and save true sold-out dates under the assigned stock
set.seed(180201)
stock_Jan <- subdata %>% 
  left_join(sale.Jan, by = "key") %>%
  group_by(key) %>%
  summarise(stock = sample.int(n = nsale.Jan, size = 1))
stock_Jan %>% glimpse
stock_Jan %>% summary
test_Jan <- subdata %>% 
  left_join(stock_Jan, by = "key") %>%
  filter(date >= ymd("2018-01-01")) %>%
  group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>%
  filter(cumunits >= stock) %>%
  summarise(soldOutDate = min(date))
test_Jan %>% glimpse()
test_Jan %>% summary()

## training data, joint info about items, prices and sales 
## sale units in January have been set as missing
train_Jan <- subdata %>%
  full_join(stock_Jan, by = "key") %>%
  select(-key) %>%
  mutate(units = replace(units, date >= ymd("2018-01-01"), NA))
train_Jan %>% glimpse
train_Jan %>% select(units, releaseDate, stock) %>% summary

## properties of the 7409 items with the "faked" stock on 2018-01-01
items_Jan <- train_Jan %>% select(-date, -price, -units) %>% unique
items_Jan %>% glimpse

## ---- save
## save datasets as txt file, separated by "|", missing value as empty
write.table(train_Jan, file = "data_clean/train_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(items_Jan, file = "data_clean/items_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(test_Jan, file = "data_clean/test_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE)

## ---- subpopular
test_Jan <- read.table("data_clean/test_Jan.txt", sep = "|", header  = T)
train_Jan <- read.table("data_clean/train_Jan.txt", sep = "|", header = T)
## only keep items sold at least somedays
## average: 12 days
## median: 6 days
## 3rd quantile: 14 days
cutvalue <- 6
train_Jan %>% filter(!is.na(units), units>0) %>%
  group_by(pid, size) %>% summarise(daysale = sum(units>0, na.rm = T)) %>% 
  filter(daysale >= cutvalue) -> key_popular
test_Jan %>% inner_join(key_popular %>% select(-daysale), by = c("pid", "size")) -> test_Jan_popular
train_Jan_popular <- train_Jan %>% inner_join(key_popular %>% select(-daysale), by = c("pid", "size"))

## save datasets as txt file, separated by "|", missing value as empty
write.table(train_Jan_popular, file = paste0("data_clean/popular/train_Jan_pop", cutvalue, ".txt"), sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(test_Jan_popular, file = paste0("data_clean/popular/test_Jan_pop", cutvalue, ".txt"), sep = "|",
            row.names = FALSE, quote = FALSE)

## rare sale products
## only ever sold for one day before Jan
train_Jan %>% filter(!is.na(units), units>0) %>%
  group_by(pid, size) %>% summarise(daysale = sum(units>0, na.rm = T)) %>% 
  filter(daysale == 1) -> key_cold
test_Jan %>% inner_join(key_cold %>% select(-daysale), by = c("pid", "size")) -> test_Jan_cold
train_Jan_cold <- train_Jan %>% inner_join(key_cold %>% select(-daysale), by = c("pid", "size"))
items_cold <- train_Jan_cold %>% select(-date, -price, -units) %>% unique
write.table(test_Jan_cold, file = "C:/Users/lyux/Dropbox/DMC 2018/ForYuchen-冷门产品/test_Jan_cold.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(train_Jan_cold, file = "C:/Users/lyux/Dropbox/DMC 2018/ForYuchen-冷门产品/train_Jan_cold.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(items_cold, file = "C:/Users/lyux/Dropbox/DMC 2018/ForYuchen-冷门产品/items_cold.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
