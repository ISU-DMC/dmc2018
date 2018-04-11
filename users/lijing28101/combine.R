#combine data with 13 variables
#Each item include everyday in the studying period

library(lubridate)
library(reshape2)
price <- read.csv("raw_data/prices.csv",sep = "|")
train <- read.csv("raw_data/train.csv",sep="|")
items <- read.csv("raw_data/items.csv",sep='|')
price2 <- melt(price,id=c("pid","size"),value.name = "price")
colnames(price2)[3] <- "date"
date <- gsub("X","",price2$date)
date <- gsub("\\.","-",date)
price2$date <- as.factor(date)
com1 <- merge(price2,train,all.x = T)
all.data <- merge(items,com1,all.y = T)