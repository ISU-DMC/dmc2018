source("subset_days_stock.R")

## days: the total number of days that have unit $>$ 0; 
## stocks: the stocks  in January;

dat <- read.table('../data/Jan_data/train_Jan.txt',sep='|',header=T)

## example 1: days = 1, stocks = 1
sdat <- subset.DS(dat,start.day=1,end.day=1,start.stock=1,end.stock=1)

## example 1: days = 2~11 (includes ends), stocks = 2
sdat <- subset.DS(dat,start.day=2,end.day=11,start.stock=2,end.stock=2)
