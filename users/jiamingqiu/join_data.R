library(dplyr)
library(tidyr)


dat.price = read.table(file = 'prices.csv', sep='|', header = T)
dat.train = read.table('train.csv', sep = '|', header = T)
dat.info = read.table('items.csv', sep = '|', header = T)

for(i in 1:dim(dat.info)[2]){
  print(c(i,names(dat.info)[i], class(dat.info[,i])))
}
for(i in 1:dim(dat.price)[2]){
  print(c(i,names(dat.price)[i], class(dat.price[,i])))
}

# rename
tm = names(dat.price)
names(dat.price) = c(tm[1:2], paste0('price.', substr(tm[3:length(tm)], 2, 11)))
rm(tm)

for(i in c(1,6:8)){
  dat.info[,i] = as.factor(dat.info[,i])
}
dat.info$releaseDate = as.Date(dat.info$releaseDate)

dat.train$pid = as.factor(dat.train$pid)
dat.price$pid = as.factor(dat.price$pid)

table(dat.train$size)
table(dat.train$units)
table(dat.price$size)


dat.wide = spread(dat.train, date, units)
names(dat.wide)
dim(dat.wide)
as.Date(123, origin = '2017-10-01')
# well, it seems there is no gap in between, all together 123 days.


# rename
names(dat.wide) = c('pid', 'size', sapply(names(dat.wide)[3:125], function(x){paste0('sale.', x)}))
# join with dat.info
# check before join
dim(dat.wide)[1] == dim(dat.info)[1]
dat.wide = left_join(dat.info, dat.wide, by = c('pid', 'size'))
for(i in 1:dim(dat.wide)[2]){
  print(c(i,names(dat.wide)[i], class(dat.wide[,i])))
}

# check if any product has sale at the first day of release, and yes
lst_dates = unique(dat.info$releaseDate)
for(i in 1:length(lst_dates)){
  
  tm = filter(dat.wide, releaseDate == lst_dates[i])
  if(any(tm[,11+as.numeric(lst_dates[i] - as.Date('2017-10-01'))] != 0, na.rm=T)){
    print(lst_dates[i] )
  }  
}

# replace NA's after & on releaseDate with 0
for(i in 1:dim(dat.wide)[1]){
  if(dat.wide$releaseDate[i] == '2017-10-01'){
    dat.wide[i, (11:133)[is.na(dat.wide[i,11:133])] ] = 0
  }else{
    tm_idx = is.na(dat.wide[i,])
    tm_idx[1:10] = F
    tm_idx[11:(10+as.numeric(dat.wide$releaseDate[i] - as.Date('2017-10-01')))] = F
    dat.wide[i,tm_idx] = 0
  }
}

summary(dat.wide[,11:133])
table(dat.wide[,11])

# put a key col
dat.wide$key = 1:length(dat.wide$pid)
dat.info$key = 1:length(dat.info$pid)
dat.price$key = 1:length(dat.price$pid)

###########################################################################################

