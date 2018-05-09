# functions:
######### functions #################

# cut seq into small piece
# if not_from_1st_place == T
# the first element of in_seq should record
# where the sequence start, say in_seq[1] = 3
# then in_seq[1:2] will be discarded
library(purrr)
semi_redundant_cut = function(in_seq, maxlen, by, next_len=1, not_from_1st_place = F){
  if(not_from_1st_place){
    in_seq = in_seq[in_seq[1]:length(in_seq)]
  }
  
  map(
    seq(1, length(in_seq) - maxlen - next_len + 1, by = by), 
    ~list(history = in_seq[.x:(.x + maxlen - 1)], 
          next_day = in_seq[(.x + maxlen) : (.x + maxlen + next_len - 1)],
          first_day_of_history = .x)
  )
}

# function prepare training responses
# seq_sale = entire sequence of sales available in training set
# since_when = index of the 1st day treated as "future"
# return: a list of
#   stock:   a stock for train
#   soldOut: days it take to sale out the stock 
get_train_response = function(seq_sale, since_when){
  lst_date_sale = which(seq_sale > 0)
  lst_date_sale = lst_date_sale[lst_date_sale>=since_when]
  map(
    lst_date_sale,
    ~list(stock = seq_sale[.x],
          soldOut = .x - since_when + 1)
  )
}

# function to get feed
# 
hist_seq_to_feed = function(seq_sale, maxlen, subseq_gap, next_len=1, start = 1){
  
  # trim
  seq_len = length(seq_sale)
  seq_sale = seq_sale[start:seq_len]
  seq_len = seq_len - start + 1
  
  idx_sale = seq_sale > 0
  
  idx_1st_day_history = seq(1, seq_len - maxlen - next_len + 1, by = subseq_gap)
  idx_future_sale = map(idx_1st_day_history + maxlen, ~which(idx_sale[.x:seq_len])+.x-1)
  n_future_sale = map(idx_future_sale, ~length(.x)) %>% unlist
  # n_future_sale = map(idx_1st_day_history + maxlen, ~sum(idx_sale[.x:seq_len])) %>% unlist
  idx_future_sale = idx_future_sale %>% unlist
  idx_1st_day_history = rep(idx_1st_day_history, n_future_sale)
  
  # a little bit of "dynamic Programming" to get "stock"
  # in the end, mat_stock[i,j] = sum(seq_sale[i:j])
  # i>j part not cleaned, just leave it there
  mat_stock = matrix(cumsum(seq_sale), ncol = seq_len, nrow = seq_len) %>% t
  for(i in 2:seq_len){
    mat_stock[i,] = mat_stock[i,] - mat_stock[1,(i-1)]
  }
  
  
  map2(
    idx_1st_day_history,
    idx_future_sale,
    ~list(history = seq_sale[.x:(.x + maxlen - 1)], 
          next_day = seq_sale[(.x + maxlen) : (.x + maxlen + next_len - 1)],
          stock = mat_stock[.x+maxlen, .y],
          soldOutTime = .y - .x - maxlen + 1,
          first_day_of_history = .x)
  )
}
# tst = hist_seq_to_feed(tst_seq_sale, 20, 3)
# tst_seq_sale = list(1:10, 11:20)
# tst = map2(tst_seq_sale, c(2,5), ~hist_seq_to_feed(.x, maxlen = 3, subseq_gap = 2,  next_len = 1, start = .y))
# unlist(tst, recursive = F) %>% transpose
# as.numeric(datm$first_day_of_history)

#-------------------------------------------------------------------------------------------

# data read in and pre process
library(dplyr)
library(tidyr)


dat.price = read.table(file = 'prices.csv', sep='|', header = T, stringsAsFactors = F)
dat.train = read.table('train.csv', sep = '|', header = T, stringsAsFactors = F)
dat.info = read.table('items.csv', sep = '|', header = T, stringsAsFactors = F)

dat.price = left_join(dat.price, clst_45, by = c('pid', 'size')) %>% filter(group5 == 5) %>% select(-group4, -group5)
dat.train = left_join(dat.train, clst_45, by = c('pid', 'size')) %>% filter(group5 == 5)%>% select(-group4, -group5)
dat.info = left_join(dat.info, clst_45, by = c('pid', 'size')) %>% filter(group5 == 5)%>% select(-group4, -group5)


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

library(dplyr)
library(purrr)
library(keras)
# For neural netModel  preparing input and response---------------------
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



# preprocess
maxlen = 20
next_len = 7
gap = 3
source('pre_histANDdemo.R')
names(dat.info.recode.train)
range.recode
apply(dat.info.recode.train, 2, range)


# -----------------------get input------------------------------------------

mat.x = as.matrix(dat.sale.train[,1:92])
# arr of releaseDate
lst_releaseDate = dat.sale.train$releaseDate
lst_releaseDate = as.numeric(lst_releaseDate - as.Date('2017-10-01') + 1)

#mat.x = cbind(lst_releaseDate+1, mat.x)
colnames(mat.x) = NULL

# cut the sequence into semi-redundant seq of maxlen
# and add stock + soldout time
datm = map2(split(mat.x, c(row(mat.x))), lst_releaseDate,
            ~hist_seq_to_feed(seq_sale = .x, maxlen = maxlen, subseq_gap = gap,
                              next_len = next_len, start = .y)
)
idx.demo = rep(1:nrow(dat.info.recode.train), sapply(datm, length))

datm = unlist(datm, recursive = F)
datm = transpose(datm)


datm$first_day_of_history = unlist(datm$first_day_of_history)
datm$stock = unlist(datm$stock)
datm$soldOutTime = unlist(datm$soldOutTime)

length(datm$next_day)

rnn.x = array(0, dim = c(length(datm$stock), maxlen, 1))
demo.cate.x = array(0, dim = c(length(datm$stock), 7))
demo.numer.x = array(0, dim = c(length(datm$stock), 2))
rnn.next_sales = array(0, dim = c(length(datm$stock), next_len))
y.sOT = array(0, dim = c(length(datm$stock), 1))


for(i in 1:length(datm$stock)){
  rnn.x[i,,] = as.integer(datm$history[[i]])
  demo.cate.x[i,] = as.integer(dat.info.recode.train[idx.demo[i], 2:8])
  rnn.next_sales[i,] = as.integer(datm$next_day[[i]])
  demo.numer.x[i,1] = as.integer(datm$stock[i])
  demo.numer.x[i,2] = dat.info.recode.train[idx.demo[i],9]
  y.sOT[i,] = as.integer(datm$soldOutTime[i])
}

map(list(rnn.x, demo.cate.x, rnn.next_sales, demo.numer.x, y.sOT), ~c(anyNA(.x), dim(.x)))
