## ---- read_data

setwd("C:/Users/guyus/Desktop/DMC/Data")

items <- read.csv("items.csv", sep = "|")
prices <- read.csv("prices.csv", sep = "|")
train <- read.csv("train.csv", sep = "|")

train <- train[order(train$size),]
train <- train[order(train$pid),]

train$date <- as.Date(train$date)
Date <- as.Date(c(paste(substr(colnames(prices)[-c(1,2)],2,5),"-",substr(colnames(prices)[-c(1,2)],7,8),"-",substr(colnames(prices)[-c(1,2)],10,11), sep = "")))
colnames(prices)[-c(1,2)] <- as.character(Date) 

Item <- paste(items$pid,items$size)
items$Item <- Item
prices$Item <- Item
train$Item <- paste(train$pid, train$size)

items$numOfData <- tapply(train$units, train$Item, FUN = length)
items$totalSale <- tapply(train$units, train$Item, FUN = sum)


plotAllThree <- function(i){
  par(mar=c(2.5,4.5,1,1))
  layout(matrix(c(1,2,2,3,3), 5,1, byrow = TRUE))
  plot(Date, prices[i,-c(1,2,154)], xlim = as.Date(c("2017-10-01", "2018-02-28")), type = "l", ylab = "price", xlab = "")
  plot(train[which(train$Item == Item[i]),]$date,
       cumsum(train[which(train$Item == Item[i]),]$units), xlim = as.Date(c("2017-10-01", "2018-02-28")), type = "o", ylab = "Total Saled", xlab = "")
  par(mar=c(4.5,4.5,1,1))
  plot(train[which(train$Item == Item[i]),]$date,
       train[which(train$Item == Item[i]),]$units, xlim = as.Date(c("2017-10-01", "2018-02-28")), type = "o", ylab = "Daily Saled", xlab = paste(i,":",Item[i],sep = ""))
}

plotTotalSale <- function(i){
  par(mar=c(5.1,2.5,2.5,2.1))
  plot(train[which(train$Item == Item[i]),]$date,
       cumsum(train[which(train$Item == Item[i]),]$units), xlim = as.Date(c("2017-10-01", "2018-02-28")), type = "o", ylab = "", xlab = paste(i,":",Item[i],sep = ""))
}


plotAllThree(4391)
par(mfrow = c(1,1))
plotTotalSale(4391)




































































































































































