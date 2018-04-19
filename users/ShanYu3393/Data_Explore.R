## ---- read_data
library(sugrrants)
library(tidyverse)
library(lubridate)
library(plotly)
setwd("~/Dropbox/DMC")
train <- read.csv("raw_data/train.csv", sep = "|")
prices <- read.csv("raw_data/prices.csv", sep = "|")
items <- read.csv("raw_data/items.csv", sep = "|")
Sale=read.csv("sale.csv")

## ---- prices_change
# For the majority of the items, the prices are kept the same over the time. 
#The prices tends to change more in the February (to sold out).
f=function(x) length(unique(x[!is.na(x)]))
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

# The items without changed in price are overlapped with the items released before 2017-10-01.
Index1=which(apply(ItemPrice[,11:133],1,f)!=1)
sum(ymd(ItemPrice[Index1,'releaseDate'])!=dateRelease[1])

# the prices will be different based on different size
length(unique(prices[,1]))
f3=function(x) dim(unique(prices[prices[,1]==x,-2]))[1]
AA=sapply(unique(prices[,1]), f3)
AA[AA!=1]

## ---- generate_sale_data
Sale=ItemPrice[,1:2]
Sale=cbind(Sale,PP)
Sale[,3:153]=0
Records=paste0(Sale[,1],Sale[,2])
train$size=as.character(train$size)
train$date=as.character(train$date)
for(iter in 1:dim(train)[1]) {
  x=train[iter,]
  tt=which(Records==paste0(x[2],x[3]))
  Sale[tt,paste0('X',gsub('-','.',x[1]))]=
    Sale[tt,paste0('X',gsub('-','.',x[1]))]+x[4]
}
write.csv(Sale,'sale.csv',row.names=FALSE)

## ---- correlation
ItemPrice=left_join(items,prices, by = c("pid", "size"))
PP=ItemPrice[,11:161]/ItemPrice[,5]
PP1=PP[,1:123]
Change=apply(PP1,1,f)
relation=NULL
for (iter in which(Change>1)){
  tt=which(!is.na(PP1[iter,]))
  relation=c(relation,cor(t(PP1[iter,tt]),t(Sale[iter,tt+2])))
}
# density plot of correlation
plot(density(relation),main='Correlation between daily price and daily sale')
# high negative correlated
x=1:151
id=649
daily_sale=t(Sale[id,-(1:2)])
daily_price=t(PP[id,])*max(daily_sale)
data=data.frame(x,daily_sale,daily_price)
colnames(data)=c('x','daily_sale','daily_price')
plot(data$x,data$daily_sale,type='l',col='blue')
points(data$x,data$daily_price,type='l',col='red')
# midden negative correlated
x=1:151
id=887
daily_sale=t(Sale[id,-(1:2)])
daily_price=t(PP[id,])*max(daily_sale)
data=data.frame(x,daily_sale,daily_price)
colnames(data)=c('x','daily_sale','daily_price')
plot(data$x,data$daily_sale,type='l',col='blue')
points(data$x,data$daily_price,type='l',col='red')
# low negative correlated
x=1:151
id=8942
daily_sale=t(Sale[id,-(1:2)])
daily_price=t(PP[id,])*max(daily_sale)
data=data.frame(x,daily_sale,daily_price)
colnames(data)=c('x','daily_sale','daily_price')
plot(data$x,data$daily_sale,type='l',col='blue')
points(data$x,data$daily_price,type='l',col='red')
#p <- plot_ly(data, x=~x, y=~daily_sale,name='Sale',type='scatter',mode='lines+markers') %>%
#  add_trace(y=~daily_price, name='price', mode='lines+markers')
#p 
# positive correlated
x=1:151
id=6696
daily_sale=t(Sale[id,-(1:2)])
daily_price=t(PP[id,])*max(daily_sale)
data=data.frame(x,daily_sale,daily_price)
colnames(data)=c('x','daily_sale','daily_price')
plot(data$x,data$daily_sale,type='l',col='blue')
points(data$x,data$daily_price,type='l',col='red')

## ---- stock_size
plot(table(items$stock),xlab='frequency')
