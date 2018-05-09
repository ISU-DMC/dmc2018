# preprocess
maxlen = 20
next_len = 7
gap = 3
source('pre_histANDdemo.R')
names(dat.info.recode.train)
range.recode
apply(dat.info.recode.train, 2, range)


# -----------------------get input------------------------------------------
source('func.R')

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

# expand.mat.x = mat.x[rep(1:nrow(mat.x), sapply(datm, length)),]
# length(idx.demo)
# dim(expand.mat.x)

datm = unlist(datm, recursive = F)
datm = transpose(datm)

# tst = datm$history %>% unlist %>% matrix(nrow = maxlen) %>% t
# tst2 = datm$next_day %>% unlist %>% matrix(nrow = next_len) %>% t
# dim(tst)
# for(i in 1:240386){
#   if(any(tst[i,] != datm$history[i][[1]])){
#     print(i)
#   }
#   if(any(tst2[i,] != datm$next_day[i][[1]])){
#     print(i)
#   }
#   
# }

datm$first_day_of_history = unlist(datm$first_day_of_history)
datm$stock = unlist(datm$stock)
datm$soldOutTime = unlist(datm$soldOutTime)
# datm$history = datm$history %>% unlist %>% matrix(nrow = maxlen) %>% t
# datm$next_day = datm$next_day %>% unlist %>% matrix(nrow = next_len) %>% t

#mapply(function(x,y){x[1]+y}, list(c(1,2), c(5,6)), c(100,200))

# tst = split(expand.mat.x[,2:ncol(expand.mat.x)], c(row(expand.mat.x[,2:ncol(expand.mat.x)])))
# head(tst)
# tst[[1]] %>% length

# tst2 = mapply(get_train_response, split(expand.mat.x[,2:ncol(expand.mat.x)], row(expand.mat.x[,2:ncol(expand.mat.x)])), datm$first_day_of_history)
#datm$demo = list(dat.info.recode.train[idx.demo, 2:8])

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

# Model Definition --------------direct prediction--------------------

# no longer a sequential model
demo_input_cate = layer_input(shape = c(7), name = 'feed_demo_categorical', dtype = 'int32')
demo_input_numeric = layer_input(shape = c(2), name = 'feed_demo_numerical')

encode_demo_cate = demo_input_cate %>%
  layer_embedding(input_dim = 468, output_dim = 32, input_length = 7, name = 'code_demo') %>%
  layer_flatten

demo_info = layer_concatenate(c(encode_demo_cate, demo_input_numeric))  %>%
  layer_dense(units = 32, name = 'demo_info')

history_input = layer_input(shape = list(maxlen,1), name = 'feed_history')
history_info = history_input %>%
  #  layer_embedding(input_dim = max(mat.x[,2:93], na.rm = T), 
  #                  output_dim = 4, input_length = maxlen,
  #                  name = 'encode_history') %>%
  layer_lstm(16) %>%
  layer_lstm(8, name = 'history_info')

next_sales_output = history_info %>%
  layer_dense(next_len, name = 'forecast_next_sale', activation = 'softplus')

predSOT_output = layer_concatenate(c(demo_info, history_info)) %>%
  layer_dense(32, name = 'merge_info') %>%
#  layer_dense(16, name = 'hidden_dense') %>%
  layer_dense(1, activation = 'softplus', name = 'forecast_sold_out_time')

mdl = keras_model(
  inputs = c(demo_input_cate, demo_input_numeric, history_input),
  outputs = c(next_sales_output, predSOT_output)
)

summary(mdl)

mdl %>% keras::compile(
  # poisson loss, ref: https://github.com/keras-team/keras/blob/master/keras/losses.py
  # namely the -log likelihood of Poisson(y_pred) at y_true, with const(w.r.t. y_pred) dropped
  loss = list(forecast_next_sale = 'poisson', forecast_sold_out_time = 'mean_absolute_error'),
  loss_weights = list(forecast_next_sale = 0.2, forecast_sold_out_time = 1.0),
  optimizer = optimizer_rmsprop(lr = 0.001)
)

# -----------------fit------------------------------------------------------

mdl %>% fit(
  x = list(feed_demo_categorical = demo.cate.x, 
           feed_demo_numerical = demo.numer.x,
           feed_history = rnn.x),
  y = list(forecast_next_sale = rnn.next_sales,
           forecast_sold_out_time = y.sOT),
  batch_size = 512,
  epochs = 3
)


# #save model
# save_model_hdf5(object = mdl,
#                 filepath = 'mdl.direct_prediction.hdf5',
#                 overwrite = TRUE,
#                 include_optimizer = TRUE)
# 
# #load model
# mdl = load_model_hdf5('mdl.direct_prediction.hdf5', custom_objects = NULL, compile = TRUE)


# -----------------predict--------------------------------------------------

# some pidXsize no sale during test month, just ignore them
# make a prediction
set.seed(0)

#lst_whichPidxSize = c(sample(which(dat.sale.test$stock > 0), size = 100))

lst_whichPidxSize = which(dat.sale.test$stock > 0)
#whichPidxSize = 3333
  
st.time = proc.time()
mat_predSOT = matrix(NA, nrow = nrow(dat.sale.test), ncol = 1)
for(whichPidxSize in lst_whichPidxSize){
  history.org = mat.x[whichPidxSize, (92 - maxlen+1):92] %>% array_reshape(c(1,maxlen,1))
  #history.org
  new.demo.cate = as.integer(dat.info.recode.train[whichPidxSize, 2:8]) %>%
    array_reshape(c(1,7))
  new.demo.numer = c(dat.sale.test$stock[whichPidxSize],             # stock
                     dat.info.recode.train$rrp[whichPidxSize])%>%  # rrp
    array_reshape(c(1,2))
    
  
  for(i in 1:ncol(mat_predSOT)){
    mat_predSOT[whichPidxSize, i] = predict(mdl, list(feed_demo_categorical = new.demo.cate, 
                               feed_demo_numerical = new.demo.numer,
                               feed_history = history.org))[[2]] %>% as.integer
  }
}
beep(4)
predSOT = apply(mat_predSOT, 1, median) %>% as.integer
sqrt(sum(abs((predSOT - dat.sale.test$sOT)),na.rm=T))
(predSOT - dat.sale.test$sOT) %>% abs %>% mean(na.rm = T)
(sample.int(31, length(predSOT), replace = T) - dat.sale.test$sOT) %>% abs %>% sum(na.rm = T) %>% sqrt
(sample.int(31, length(predSOT), replace = T) - dat.sale.test$sOT) %>% abs %>% mean(na.rm = T)

(predSOT - dat.sale.test$sOT) %>% hist(breaks = 20, main = 'pred - true sold out', xlim = c(-31,31))
(sample.int(31, length(predSOT), replace = T) - dat.sale.test$sOT)%>% hist(breaks = 20, main = 'rndGuss - true sold out', xlim = c(-31,31))

els.time = proc.time() - st.time
els.time / length(lst_whichPidxSize)

