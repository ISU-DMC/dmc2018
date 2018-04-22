library(lubridate)
library(tidyverse)
library(lmtest)
library(FitAR)
library(forecast)
library(fUnitRoots)

source("subset_days_stock.R")
train_Jan <- read.table("https://raw.githubusercontent.com/ISU-DMC/dmc2018/master/users/XiaodanLyu/data_clean/train_Jan.txt", sep = "|", header = T)
train_Jan <- train_Jan %>% 
  mutate(date = ymd(date),
         releaseDate = ymd(releaseDate))
train_6.4 <- subset.DS(train_Jan,start.day=6,end.day=123,start.stock=4,end.stock=520)
train_6.4_format <- train_6.4 %>%
  mutate(ID=paste(pid,size,sep = "_")) %>%
  mutate(ID=as.numeric(factor(ID,levels=unique(ID)))) %>%
  select(c(ID,pid,size,date,releaseDate,units,stock))

test_Jan <- read.table("https://raw.githubusercontent.com/ISU-DMC/dmc2018/master/users/XiaodanLyu/data_clean/test_Jan.txt", sep = "|", header  = T)
compare <- merge(unique(train_6.4_format[,1:3]),test_Jan)
compare <- compare[order(compare$ID),]
  
predict_ts <- function(dat,maxID,or1,or2,med){
  saleday <- character()
  for (i in 1:maxID){
    train <- subset(dat,ID==i)
    tsData = ts(train$units[1:92],start=c(10,1),frequency = 7)
    fitARIMA <- arima(tsData, order=or1,seasonal = list(order = or2, period = 7),method=med)
    pred <- predict(fitARIMA,n.ahead = 31)
    cumunits <- cumsum(pred$pred[1:31])
    if(max(cumunits)>unique(train$stock)){
    saleday[i] <- sum(cumunits<unique(train$stock))+1
    } else {saleday[i]=31}
  }
  return(saleday)
}

sale <- predict_ts(dat=train_6.4_format,maxID=1718,or1=c(0,0,0),or2=c(1,0,1),med="CSS")
compare$predDate_000101 <- ymd("2017-12-31")+as.numeric(sale)

err <- (ymd(compare$soldOutDate) - ymd(compare$predDate_000101)) %>% abs %>% sum %>% as.numeric %>% sqrt
err.guess <- (ymd(compare$soldOutDate) - ymd("2018-01-16")) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.TS.individual: %.3f, err.guess: %.3f", err, err.guess)

