# splitting data into train / test
dat.sale.train = select(dat.wide,`sale.2017-10-01`:`sale.2017-12-31`, pid, size, releaseDate, key)
dim(dat.sale.train)
names(dat.sale.train)

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
as.Date('2018-01-01') - maxlen - 2
dat.sale.train = filter(dat.sale.train, releaseDate <= as.Date('2018-01-01') - maxlen - 2)
dat.sale.test = filter(dat.sale.test, releaseDate <= as.Date('2018-01-01') - maxlen - 2)

lst_max = as.numeric(apply(dat.sale.train[,1:92], 1, max))
# check dim
dim(dat.sale.train)
dim(dat.sale.test)

# unify, i.e., units of sale per day / row max
#dat.sale.train[,3:94] = dat.sale.train[,3:94]/lst_max
head(dat.sale.train)

anyNA(dat.sale.train)


###########################pre-process done#########################################
maxlen = 20

mat.x = as.matrix(dat.sale.train[,1:92])
# arr of releaseDate
lst_releaseDate = dat.sale.train$releaseDate
lst_releaseDate = as.numeric(lst_releaseDate - as.Date('2017-10-01') + 1)
# some products have very short history 
table(92 - lst_releaseDate + 1)
# but history length should satisfy
# length(history) - maxlen - 1 >= 1
# i.e. length(history >= 2 + maxlen)
# and length(history) = 92 - lst_releaseDate + 1
# drop some(back to pre-process)


mat.x = cbind(lst_releaseDate+1, mat.x)
colnames(mat.x) = NULL

# cut the sequence into semi-redundant seq of maxlen
datm = apply(mat.x, 1,
      semi_redundant_cut, maxlen = maxlen, by = 3, next_len=1, not_from_1st_place = T)

datm = unlist(datm, recursive = F)
datm = transpose(datm)
length(datm$next_day)

rnn.x = array(0, dim = c(length(datm$history), maxlen))
rnn.y = array(0, dim = c(length(datm$history), 1))

for(i in 1:length(datm$history)){
  rnn.x[i,] = as.numeric(datm$history[[i]])
  rnn.y[i,] = as.numeric(datm$next_day[[i]])
}

anyNA(rnn.x) | anyNA(rnn.y)

library(keras)

# Model Definition ----------LSTM RNN with history sale only----------

mdl = keras_model_sequential()
mdl %>%
  layer_embedding(input_dim = max(mat.x[,2:93], na.rm = T), 
                  output_dim = 32, input_length = maxlen,
                  name = 'encode') %>%
  layer_lstm(32, return_sequences = T, name = 'hidden_lstm') %>%
  layer_lstm(32, name = '2nd_lstm') %>%
  layer_dense(1) %>%
  layer_activation('softplus')

summary(mdl)

optimizer = optimizer_rmsprop(lr = 0.001)
mdl %>% keras::compile(
  # poisson loss, ref: https://github.com/keras-team/keras/blob/master/keras/losses.py
  # namely the -log likelihood of Poisson(y_pred) at y_true, with const(w.r.t. y_pred) dropped
  loss = 'poisson',
  optimizer = optimizer
)

mdl %>% fit(rnn.x, rnn.y, batch_size = 512, epochs = 50)

# save model
save_model_hdf5(object = mdl,
                filepath = 'mdl.lstm.rnn.history.hdf5',
                overwrite = TRUE,
                include_optimizer = TRUE)
# load model
# mdl = load_model_hdf5('mdl.lstm.rnn.history.hdf5', custom_objects = NULL, compile = TRUE)

# make a prediction
set.seed(0)
whichPidxSize = sample(1:length(dat.sale.train$pid), size = 1)

dat.train$pid[which.max(dat.train$units)]
which(dat.sale.train$pid == 14178)
whichPidxSize = 3564
#whichPidxSize = 3563

history.org = mat.x[whichPidxSize, (93 - maxlen+1):93]
history.org

generated = matrix(0, ncol = 31, nrow = 30)
for(idx.row in 1:nrow(generated)){
  history = history.org
  for(i in 1:31){
    new.x = array_reshape(history, c(1, maxlen))
    pred = predict(mdl, new.x)
    next.day = rpois(1, lambda = pred[1])
    generated[idx.row, i] = next.day
    history = c(history[2:maxlen], next.day)
  }
}
mean.generated = colMeans(generated)

#cat(generated * lst_max[whichPidxSize])
cat(mean.generated)
as.numeric(dat.sale.test[whichPidxSize,1:31])
plot(1:31, mean.generated, type = 'h', col = rgb(0,0,1,0.5), lwd = 2)
points(1:31, as.numeric(dat.sale.test[whichPidxSize,1:31]), type = 'h', col = rgb(1,0,0,0.5), lwd = 2)
plt.y_true = cumsum(as.numeric(dat.sale.test[whichPidxSize,1:31]))
plt.y_pred = cumsum(mean.generated)
plot(1:31, plt.y_true, type = 'l', col = rgb(1,0,0,0.5), lwd = 2, ylim = range(c(plt.y_pred, plt.y_true)), main = whichPidxSize)
points(1:31, plt.y_pred, type = 'l', col = rgb(0,0,1,0.5), lwd = 2)
# ???
# what I see is, if trained for 1 epoch,
# pretty good, trained for 50 epoches,
# under fit

par(mfrow = c(2,1))





# Model Definition --------------include demo info--------------------
# preprocess
source('pre_histANDdemo.R')
names(dat.info.recode.train)
range.recode
apply(dat.info.recode.train, 2, range)


# no longer a sequential model
demo_input = layer_input(shape = c(7), name = 'feed_demo', dtype = 'int32')
demo_info = demo_input %>%
  layer_embedding(input_dim = 468, output_dim = 8, input_length = 7, name = 'code_demo') %>%
  layer_flatten %>%
  layer_dense(units = 32, name = 'demo_info')

history_input = layer_input(shape = list(maxlen,1), name = 'feed_history')
history_info = history_input %>%
#  layer_embedding(input_dim = max(mat.x[,2:93], na.rm = T), 
#                  output_dim = 4, input_length = maxlen,
#                  name = 'encode_history') %>%
  layer_lstm(32, return_sequences = T, name = 'hidden_lstm') %>%
  layer_lstm(32, name = 'history_info')

output = layer_concatenate(c(demo_info, history_info)) %>%
  layer_dense(32, name = 'merge_info') %>%
  layer_dense(1, activation = 'softplus', name = 'prediction')

mdl = keras_model(
  inputs = c(demo_input, history_input),
  outputs = output
)

summary(mdl)

optimizer = optimizer_rmsprop(lr = 0.001)
mdl %>% keras::compile(
  # poisson loss, ref: https://github.com/keras-team/keras/blob/master/keras/losses.py
  # namely the -log likelihood of Poisson(y_pred) at y_true, with const(w.r.t. y_pred) dropped
  loss = 'poisson',
  optimizer = optimizer
)

# -----------------------get input------------------------------------------

maxlen = 20

mat.x = as.matrix(dat.sale.train[,1:92])
# arr of releaseDate
lst_releaseDate = dat.sale.train$releaseDate
lst_releaseDate = as.numeric(lst_releaseDate - as.Date('2017-10-01') + 1)

mat.x = cbind(lst_releaseDate+1, mat.x)
colnames(mat.x) = NULL

# cut the sequence into semi-redundant seq of maxlen
datm = apply(mat.x, 1,
             semi_redundant_cut, maxlen = maxlen, by = 3, next_len=1, not_from_1st_place = T)

idx.demo = rep(dat.info.recode.train$key, sapply(datm, length))
length(idx.demo)

datm = unlist(datm, recursive = F)
datm = transpose(datm)
#datm$demo = list(dat.info.recode.train[idx.demo, 2:8])

length(datm$next_day)

rnn.x = array(0, dim = c(length(datm$history), maxlen, 1))
demo.x = array(0, dim = c(length(datm$history), 7))
rnn.y = array(0, dim = c(length(datm$history), 1))

for(i in 1:length(datm$history)){
  rnn.x[i,,1] = as.integer(datm$history[[i]])
  demo.x[i,] = as.integer(dat.info.recode.train[idx.demo[i], 2:8])
  rnn.y[i,] = as.integer(datm$next_day[[i]])
}

# -----------------fit------------------------------------------------------

mdl %>% fit(
  x = list(feed_demo = demo.x, feed_history = rnn.x),
  y = rnn.y,
  batch_size = 512,
  epochs = 5
)

# -----------------predict--------------------------------------------------

# make a prediction
set.seed(0)
whichPidxSize = sample(1:length(dat.sale.train$pid), size = 1)

dat.train$pid[which.max(dat.train$units)]
which(dat.sale.train$pid == 14178)

set.seed(0)
lst_whichPidxSize = c(3563, 3564, sample(1:length(dat.sale.train$pid), size = 7))

#whichPidxSize = 3563

par(mfrow = c(3,3))
for(whichPidxSize in lst_whichPidxSize){
  history.org = mat.x[whichPidxSize, (93 - maxlen+1):93]
  history.org
  new.demo = as.integer(dat.info.recode.train[whichPidxSize, 2:8]) %>%
              array_reshape(c(1,7))
  
  generated = matrix(0, ncol = 31, nrow = 50)
  for(idx.row in 1:nrow(generated)){
    history = history.org
    for(i in 1:31){
      new.x = array_reshape(history, c(1, maxlen, 1))
      pred = predict(mdl, list(feed_demo = new.demo, feed_history = new.x))
      next.day = rpois(1, lambda = pred[1])
      generated[idx.row, i] = next.day
      history = c(history[2:maxlen], next.day)
    }
  }
  mean.generated = colMeans(generated)
  
  #cat(generated * lst_max[whichPidxSize])
  cat(mean.generated)
  cat(as.numeric(dat.sale.test[whichPidxSize,1:31]))
  # par(mfrow = c(2,1))
  # plot(1:31, mean.generated, type = 'h', col = rgb(0,0,1,0.5), lwd = 2)
  # points(1:31, as.numeric(dat.sale.test[whichPidxSize,1:31]), type = 'h', col = rgb(1,0,0,0.5), lwd = 2)
  plt.y_true = cumsum(as.numeric(dat.sale.test[whichPidxSize,1:31]))
  plt.y_pred = cumsum(mean.generated)
  plot(1:31, plt.y_true, type = 'l', col = rgb(1,0,0,0.5), lwd = 2, ylim = range(c(plt.y_pred, plt.y_true)), main = whichPidxSize)
  points(1:31, plt.y_pred, type = 'l', col = rgb(0,0,1,0.5), lwd = 2)
}




