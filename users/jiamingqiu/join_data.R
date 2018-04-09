dat.price = read.table(file = 'prices.csv', sep='|', header = T)
dat.train = read.table('train.csv', sep = '|', header = T)
dat.info = read.table('items.csv', sep = '|', header = T)

table(dat.train$size)
table(dat.train$units)
table(dat.price$size)

library(dplyr)
library(tidyr)

dat.full = full_join(dat.price, dat.train)

dat.price.long =  gather(dat.price, key = 'date', value = 'price', X2017.10.01:X2018.02.28)
typeof(dat.price.long$date)
typeof(dat.train$date)
class(dat.train$date)

# some research on dataobject
# in short one can use dat class in data frame and 
# make comparison in the simple way
tm.dat = as.Date(dat.train$date)
tst.dt = data.frame(time = tm.dat, val = 1:length(tm.dat))
class(tst.dt$time)
tm.dat[1] == "2017-10-01"
dat.train$date = as.Date(dat.train$date)

# convert X2017.10.01 to 2017-10-01
names(dat.price.long)
typeof(dat.price.long$date[1])
as.Date(c("X2017.10.01", "X2017.10.01"), format = "X%Y.%m.%d")
as.Date(dat.price.long$date, format = "X%Y.%m.%d")
dat.price.long$date = as.Date(dat.price.long$date, format = "X%Y.%m.%d")

# join dat.train with dat.price.long
dat.join = left_join(dat.train, dat.price.long, by = c("date", 'pid', 'size'))
anyNA(dat.join)
all(sort(unique(dat.join$pid)) == sort(unique(dat.info$pid)))

# NA in tst.join is from dat.info
# all pid and size in dat.info can be found in dat.join
tst.join = left_join(dat.info, dat.join, by = c('pid', 'size'))
tst.join[apply(tst.join, 1, anyNA),]

anyNA(tst.join$date)

dat.14393.2 = filter(dat.join, pid == 14393, size == '2 ( 37-39 )')
plot(dat.14393.2$date, dat.14393.2$unit, type = 'h')


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


as.Date('2017-10-01')

as.Date(150, origin = '2017-10-01')
# try to make it into wide format
dat.wide = spread(dat.train, date, units)
names(dat.wide)
dim(dat.wide)
as.Date(123, origin = '2017-10-01')
# well, it seems there is no gap in between, all together 123 days.
# replace NA with 0
dat.wide[is.na(dat.wide)] = 0
# rename
names(dat.wide) = c('pid', 'size', sapply(names(dat.wide)[3:125], function(x){paste0('sale.', x)}))
# join with dat.info
# check before join
dim(dat.wide)[1] == dim(dat.info)[1]
dat.wide = left_join(dat.info, dat.wide, by = c('pid', 'size'))
# direct pca?
pca.sale = princomp(scale(as.matrix(dat.wide[,11:133])))
loadings(pca.sale)
dim(pca.sale$scores)
plot(pca.sale$scores[,1:2])
# a mess

# try smooth them a bit?
dat.wide[,11:133] = apply(dat.wide[,11:133], 1,
                          function(x){
                            ksmooth(11:133, x, kernel = 'normal', bandwidth = 2)$y
                          }
)
# and unify, say sale.date = #sale.date/#all.sale
dat.wide[,11:133] = dat.wide[,11:133] / rowSums(dat.wide[,11:133])
# pca again?
pca.psale = prcomp(scale(as.matrix(dat.wide[,11:133])))
dim(pca.psale$x)
col.arr = sapply(as.numeric(as.character(dat.wide$mainCategory)),
                 function(x){
                   if(x== 1) return(rgb(1,0,0,0.5))
                   if(x== 9) return(rgb(0,1,0,0.5))
                   if(x==15) return(rgb(0,0,1,0.5))
                 }
)

plot(pca.psale$x[,1:2], col = col.arr)


plot(pca.psale$x[,1:2], col = rainbow(10, alpha = 0.5)[
  as.numeric(as.character(factor(dat.info$category, labels = 1:10, levels = sort(unique(dat.info$category)))))])

plot(pca.psale$x[,1:2], col = rainbow(32, alpha = 0.5)[
  as.numeric(as.character(factor(dat.info$subCategory, labels = 1:32, levels = sort(unique(dat.info$subCategory)))))])

col.arr = dat.info$color
table(col.arr)
n.type = length(unique(col.arr))
plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.25)[
  as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])

col.arr = dat.info$brand
table(col.arr)
n.type = length(unique(col.arr))
plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.25)[
  as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])

col.arr = dat.info$rrp
table(col.arr)
n.type = length(unique(col.arr))
plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.45)[
  as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])

col.arr = filter(dat.info, releaseDate!='2017-10-01')$releaseDate
table(col.arr)
n.type = length(unique(col.arr))
plot(pca.psale$x[dat.info$releaseDate!='2017-10-01',1:2], col = rainbow(n.type, alpha = 0.65)[
  as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])

