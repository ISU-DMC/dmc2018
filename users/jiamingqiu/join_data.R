dat.price = read.table(file = 'prices.csv', sep='|', header = T)
dat.train = read.table('train.csv', sep = '|', header = T)
dat.info = read.table('items.csv', sep = '|', header = T)


library(dplyr)
library(tidyr)

dat.price.long =  gather(dat.price, key = 'date', value = 'price', X2017.10.01:X2018.02.28)
dat.train$date = as.Date(dat.train$date)
# convert X2017.10.01 to 2017-10-01
dat.price.long$date = as.Date(dat.price.long$date, format = "X%Y.%m.%d")

# join dat.train with dat.price.long
dat.join = left_join(dat.train, dat.price.long, by = c("date", 'pid', 'size'))
# you can join dat.join further with dat.info
anyNA(dat.join)
all(sort(unique(dat.join$pid)) == sort(unique(dat.info$pid)))

# any pid price changed?
# get a temp id to sum up pid & size
pid.size = 1:(dim(dat.info)[1])
tm.dat = data.frame(pid.size = pid.size, pid = dat.info$pid, size = dat.info$size)
dat.join = left_join(dat.join, tm.dat, by = c("pid", "size"))

diff.price = (apply(as.matrix(dat.price[,c(-1,-2)]), 1, function(x){all(mean(x, na.rm = T) == x, na.rm = T)}))
anyNA(diff.price)
table(diff.price)
# only 1866 produce ever change price
dat.price[(1:length(diff.price))[diff.price == F][1:5],]
