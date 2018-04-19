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

## training data 
train_Jan <- subdata %>%
  full_join(stock_Jan, by = "key") %>%
  select(-key) %>%
  mutate(units = replace(units, date >= ymd("2018-01-01"), NA))
train_Jan %>% glimpse
train_Jan %>% select(units, releaseDate, stock) %>% summary

## ---- save
## save datasets as txt file
write.table(train_Jan, file = "data_clean/train_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(test_Jan, file = "data_clean/test_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE)
