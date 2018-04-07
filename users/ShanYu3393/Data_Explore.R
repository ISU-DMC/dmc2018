library(sugrrants)
library(tidyverse)
library(lubridate)
setwd("~/Dropbox/DMC")
train <- read.csv("raw_data/train.csv", sep = "|")
prices <- read.csv("raw_data/prices.csv", sep = "|")
items <- read.csv("raw_data/items.csv", sep = "|")
## ---- quality
## no missing in training data
apply(train, 2, function(x) sum(is.na(x)))
## some missing in subCategory
apply(items, 2, function(x) sum(is.na(x)))

## ---- prices
# For the majority of the items, the prices are kept the same over the time. 
#The prices tends to change more in the February (to sold out).
f=function(x) length(unique(x))
# check the price change in from 2017-10-01 to 2018-01-31
table(apply(prices[,3:125],1,f))
# check the price change in from 2017-10-01 to 2018-02-28
table(apply(prices[,3:153],1,f))

ItemPrice=left_join(items,prices, by = c("pid", "size"))
dateRelease=ymd(ItemPrice$releaseDate)
tt=dateRelease-dateRelease[1]
#released before 2017-10-01 don't have NAs.
sum(is.na(ItemPrice[tt==0,11:133]))
#released after 2017-10-01, the prices will be NA between 2017-10-01 and two days before released day.
## for example, in the record 8, the released day is "2017-10-27", the price between "2017-10-01" to "2017-10-25" will be NA. 
f2=function(x) max(which(is.na(x)))
sum(tt[tt>0]-1!=apply(ItemPrice[tt!=0,11:133], 1, f2))

