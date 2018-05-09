#source('preprocess.R')
library(dplyr)
library(purrr)
library(keras)
# For Model --------history + demo info---------------------
# splitting data into train / test
dat.sale.train = select(dat.wide,`sale.2017-10-01`:`sale.2017-12-31`, pid, size, releaseDate, key)
# get max sale per day
lst_max = as.numeric(apply(dat.sale.train[,1:92], 1, max, na.rm = T))
# if == -Inf, means all NA, turn to 0
lst_max[lst_max == -Inf] = 0
# drop those with no sale in these months
dat.sale.train = dat.sale.train[lst_max > 0,]
# use Jan. as test
dat.sale.test = select(dat.wide, `sale.2018-01-01`:`sale.2018-01-31`, pid, size, releaseDate, key)
dat.sale.test = dat.sale.test[lst_max > 0,]

# drop short history items
dat.sale.train = filter(dat.sale.train, releaseDate <= as.Date('2018-01-01') - maxlen  - next_len)
dat.sale.test = filter(dat.sale.test, releaseDate <= as.Date('2018-01-01') - maxlen - next_len)

# get 'stock' and 'soldouttime' for testing
tm_mat = as.matrix(dat.sale.test[,1:31])
set.seed(0)
dat.sale.test$stock = apply(tm_mat, 1, 
                            function(x){
                              x=x[x>0]
                              if(length(x) == 0){
                                NA
                              }else{
                                sum(x[1:sample.int(length(x), 1)])
                              }
                              }) %>% as.integer
dat.sale.train = dat.sale.train[!is.na(dat.sale.test$stock),]
tm_mat = tm_mat[!is.na(dat.sale.test$stock),]

dat.sale.test = dat.sale.test[!is.na(dat.sale.test$stock), ]

dat.sale.test$sOT = map2(split(tm_mat, c(row(tm_mat))),
                         dat.sale.test$stock,
                            ~which(cumsum(.x) == .y)[1]
                          ) %>% as.integer
rm(tm_mat)
anyNA(dat.sale.test$Sot)

#all((dat.sale.test$stock == 0) == (is.na(dat.sale.test$sOT)))


dim(dat.sale.train) == dim(dat.sale.test)

# to prepare demo info for embedding,
# say, different variable get different set of indices.
# and I decided to feed every thing
# even NA is subCategory
# but not pid for the moment, it will inflate # of para

dat.info.recode = data.frame(pid = as.integer(as.character(dat.info$pid)))
range.recode = list(pid = range(dat.info.recode$pid))

tm = dat.info$size %>% as.character
length(unique(tm))
range.recode
range.recode$size = c(1,length(unique(tm)))
tm.idx = 0+(1:length(unique(tm)))
dat.info.recode$size = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))

tm = dat.info$color %>% as.character
length(unique(tm))
range.recode
tm.idx = 200+(1:length(unique(tm)))
range.recode$color = range(tm.idx)
range.recode
dat.info.recode$color = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
names(dat.info)

tm = dat.info$brand %>% as.character
length(unique(tm))
range.recode
tm.idx = 250+(1:length(unique(tm)))
range.recode$brand = range(tm.idx)
range.recode
dat.info.recode$brand = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)


tm = dat.info$mainCategory %>% as.character
length(unique(tm))
range.recode
tm.idx = 290+(1:length(unique(tm)))
range.recode$mainCategory = range(tm.idx)
range.recode
dat.info.recode$mainCategory = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$category)
length(unique(tm))
range.recode
tm.idx = 300+(1:length(unique(tm)))
range.recode$category = range(tm.idx)
range.recode
dat.info.recode$category = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$subCategory)
tm[is.na(tm)] = 1
length(unique(tm))
range.recode
tm.idx = 320+(1:length(unique(tm)))
range.recode$subCategory = range(tm.idx)
range.recode
dat.info.recode$subCategory = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$releaseDate)
length(unique(tm))
range.recode
tm.idx = 400+(1:length(unique(tm)))
range.recode$releaseDate = range(tm.idx)
range.recode
dat.info.recode$releaseDate = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)
dat.info.recode$rrp = dat.info$rrp
dat.info.recode$stock = dat.info$stock
dat.info.recode$key = dat.info$key
summary(dat.info.recode)

# select those in dat.sale.train
dat.info.recode.train = dat.info.recode[is.element(dat.info.recode$key, dat.sale.train$key),]
dim(dat.info.recode.train)
