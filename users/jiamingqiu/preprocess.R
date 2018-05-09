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

# replace NA's after & on releaseDate with 0


summary(dat.wide[,11:133])
table(dat.wide[,11])

# put a key col
dat.wide$key = 1:length(dat.wide$pid)
dat.info$key = 1:length(dat.info$pid)
dat.price$key = 1:length(dat.price$pid)

###########################################################################################

# 
# 
# 
# dat.full = full_join(dat.price, dat.train)
# 
# dat.price.long =  gather(dat.price, key = 'date', value = 'price', X2017.10.01:X2018.02.28)
# typeof(dat.price.long$date)
# typeof(dat.train$date)
# class(dat.train$date)
# 
# for(i in 1:dim(dat.train)[2]){
#   print(c(i,names(dat.train)[i], class(dat.train[,i])))
# }
# 
# for(i in 1:dim(dat.price.long)[2]){
#   print(c(i,names(dat.price.long)[i], class(dat.price.long[,i])))
# }
# 
# 
# # some research on dataobject
# # in short one can use dat class in data frame and 
# # make comparison in the simple way
# tm.dat = as.Date(dat.train$date)
# tst.dt = data.frame(time = tm.dat, val = 1:length(tm.dat))
# class(tst.dt$time)
# tm.dat[1] == "2017-10-01"
# dat.train$date = as.Date(dat.train$date)
# 
# # convert X2017.10.01 to 2017-10-01
# names(dat.price.long)
# typeof(dat.price.long$date[1])
# as.Date(c("X2017.10.01", "X2017.10.01"), format = "X%Y.%m.%d")
# as.Date(dat.price.long$date, format = "X%Y.%m.%d")
# dat.price.long$date = as.Date(dat.price.long$date, format = "X%Y.%m.%d")
# 
# # join dat.train with dat.price.long
# dat.join = left_join(dat.train, dat.price.long, by = c("date", 'pid', 'size'))
# anyNA(dat.join)
# all(sort(unique(dat.train$pid)) == sort(unique(dat.info$pid)))
# all(sort(unique(dat.join$pid)) == sort(unique(dat.info$pid)))
# 
# 
# # NA in tst.join is from dat.info
# # all pid and size in dat.info can be found in dat.join
# tst.join = left_join(dat.info, dat.join, by = c('pid', 'size'))
# tst.join[apply(tst.join, 1, anyNA),]
# 
# anyNA(tst.join$date)
# 
# dat.14393.2 = filter(dat.join, pid == 14393, size == '2 ( 37-39 )')
# plot(dat.14393.2$date, dat.14393.2$unit, type = 'h')
# 
# 
# # any pid price changed?
# # get a temp id to sum up pid & size
# pid.size = 1:(dim(dat.info)[1])
# tm.dat = data.frame(pid.size = pid.size, pid = dat.info$pid, size = dat.info$size)
# dat.join = left_join(dat.join, tm.dat, by = c("pid", "size"))
# 
# diff.price = (apply(as.matrix(dat.price[,c(-1,-2)]), 1, function(x){all(mean(x, na.rm = T) == x, na.rm = T)}))
# anyNA(diff.price)
# table(diff.price)
# # only 1866 produce ever change price
# dat.price[(1:length(diff.price))[diff.price == F][1:5],]
# 
# 
# as.Date('2017-10-01')
# 
# as.Date(150, origin = '2017-10-01')
# # try to make it into wide format
# dat.wide = spread(dat.train, date, units)
# names(dat.wide)
# dim(dat.wide)
# as.Date(123, origin = '2017-10-01')
# # well, it seems there is no gap in between, all together 123 days.
# 
# 
# # rename
# names(dat.wide) = c('pid', 'size', sapply(names(dat.wide)[3:125], function(x){paste0('sale.', x)}))
# # join with dat.info
# # check before join
# dim(dat.wide)[1] == dim(dat.info)[1]
# dat.wide = left_join(dat.info, dat.wide, by = c('pid', 'size'))
# for(i in 1:dim(dat.wide)[2]){
#   print(c(i,names(dat.wide)[i], class(dat.wide[,i])))
# }
# 
# # check if any product has sale at the first day of release, and yes
# lst_dates = unique(dat.info$releaseDate)
# for(i in 1:length(lst_dates)){
#   
#   tm = filter(dat.wide, releaseDate == lst_dates[i])
#   if(any(tm[,11+as.numeric(lst_dates[i] - as.Date('2017-10-01'))] != 0, na.rm=T)){
#     print(lst_dates[i] )
#   }  
# }
# 
# # replace NA's after & on releaseDate with 0
# for(i in 1:dim(dat.wide)[1]){
#   if(dat.wide$releaseDate[i] == '2017-10-01'){
#     dat.wide[i, (11:133)[is.na(dat.wide[i,11:133])] ] = 0
#   }else{
#     tm_idx = is.na(dat.wide[i,])
#     tm_idx[1:10] = F
#     tm_idx[11:(10+as.numeric(dat.wide$releaseDate[i] - as.Date('2017-10-01')))] = F
#     dat.wide[i,tm_idx] = 0
#   }
# }
# 
# summary(dat.wide[,11:133])
# table(dat.wide[,11])
# 
# 
# 
# 
# 
# 
# # direct pca?
# pca.sale = princomp(scale(as.matrix(dat.wide[,11:133])))
# loadings(pca.sale)
# dim(pca.sale$scores)
# plot(pca.sale$scores[,1:2])
# # a mess
# 
# # backup before I ruin everything
# dat.wide.origin = dat.wide
# 
# # try smooth them a bit?
# dat.wide[,11:133] = apply(dat.wide.origin[,11:133], 1,
#                           function(x){
#                             ksmooth(11:133, x, kernel = 'normal', bandwidth = 2)$y
#                           }
# )
# # and unify, say sale.date = #sale.date/#all.sale
# dat.wide[,11:133] = dat.wide[,11:133] / rowSums(dat.wide[,11:133])
# # pca again?
# pca.psale = prcomp(scale(as.matrix(dat.wide[,11:133])))
# dim(pca.psale$x)
# col.arr = sapply(as.numeric(as.character(dat.wide$mainCategory)),
#                  function(x){
#                    if(x== 1) return(rgb(1,0,0,0.5))
#                    if(x== 9) return(rgb(0,1,0,0.5))
#                    if(x==15) return(rgb(0,0,1,0.5))
#                  }
# )
# 
# plot(pca.psale$x[,1:2], col = col.arr)
# 
# 
# plot(pca.psale$x[,1:2], col = rainbow(10, alpha = 0.5)[
#   as.numeric(as.character(factor(dat.info$category, labels = 1:10, levels = sort(unique(dat.info$category)))))])
# 
# plot(pca.psale$x[,1:2], col = rainbow(32, alpha = 0.5)[
#   as.numeric(as.character(factor(dat.info$subCategory, labels = 1:32, levels = sort(unique(dat.info$subCategory)))))])
# 
# col.arr = dat.info$color
# table(col.arr)
# n.type = length(unique(col.arr))
# plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.25)[
#   as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])
# 
# col.arr = dat.info$brand
# table(col.arr)
# n.type = length(unique(col.arr))
# plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.25)[
#   as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])
# 
# col.arr = dat.info$rrp
# table(col.arr)
# n.type = length(unique(col.arr))
# plot(pca.psale$x[,1:2], col = rainbow(n.type, alpha = 0.45)[
#   as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])
# 
# col.arr = filter(dat.info, releaseDate!='2017-10-01')$releaseDate
# table(col.arr)
# n.type = length(unique(col.arr))
# plot(pca.psale$x[dat.info$releaseDate!='2017-10-01',1:2], col = rainbow(n.type, alpha = 0.65)[
#   as.numeric(as.character(factor(col.arr, labels = 1:n.type, levels = sort(unique(col.arr)))))])
# 
# # cannot MDS for 12824 points, consider k-means, i.e. MDS on centers of k-means
# set.seed(100)
# k.res = kmeans(as.matrix(dat.wide[,11:133]), centers = 1000)
# # test if RAM and time enough
# tst.dat = matrix(rnorm(123*1000), ncol = 123)
# d = dist(tst.dat)
# rm(tst.dat, d)
# # yes, MDS, though make no sense
# mds.res = cmdscale(dist(k.res$centers), k=2)
# plot(mds.res)
# 
# dat.wide.gb.cat = data.frame(category = sort(unique(dat.wide$category)),
#                              rowsum(dat.wide.origin[,11:133], dat.wide$category)
# )
# dat.wide.gb.cat
# names(dat.wide.gb.cat) = c('category', as.character(as.Date(1:123, origin = '2017-10-01')))
# plot(as.Date(1:123, origin = '2017-10-01'), as.numeric(dat.wide.gb.cat[1,2:124]), type = 'h')
# 
# barplot(as.matrix(dat.wide.gb.cat[,2:124], dimnames=list(dat.wide.gb.cat$category,
#                                                          as.Date(1:123, origin = '2017-10-01'))), width = 1, col = rainbow(10),
#         legend.text = dat.wide.gb.cat$category, args.legend = list(x = 'topleft'),
#         main = 'sale per day by category')
# 
# 
# barplot(as.matrix(dat.wide.gb.cat[,2:124]), width = 1, col = rainbow(10),
#         legend.text = dat.wide.gb.cat$category, args.legend = list(x = 'topleft'),
#         main = 'sale per day by category')
# 
# rowsum(dat.wide.origin[,11:133], dat.wide$mainCategory) %>% as.matrix %>% barplot(col = rainbow(3),
#         legend.text = sort(unique(dat.wide$mainCategory)), main = 'sale per day by maincategory', names.arg = as.character(as.Date(1:123, origin = '2017-10-01')))
# 
# 
# # check whether poisson is reasonable
# hist(as.numeric(dat.wide.gb.cat[1,]), breaks = 20)
# # with aggregation w.r.t. category, it seems each day the distribution 
# # of sales is similar to a poisson one, except category == 7.
# # and for sure they does not look like a homogeneous one
# par(mfrow = c(3,4))
# for(i in 1:10){
#   qqplot(as.numeric(dat.wide.gb.cat[i,]), qpois(ppoints(500), mean(as.numeric(dat.wide.gb.cat[i,]))),
#          main = i, xlab = 'dat', ylab = 'poisson')
# }
# # and if looking at individual pidXsize, too bad, mostly zero
# dat.wide[5,11:133]  %>% as.numeric %>% hist
# apply(dat.wide[1:10,11:133], 1, table)
# # I have no idea how to model this
# 
