#install.packages(c('readr','stringr','purrr','tokenizers'))

# modified from
# https://keras.rstudio.com/articles/examples/lstm_text_generation.html

# This model (LSTM RNN) take input of a sentence of length = maxlen
# and predict the next character recurrently. I.e., once a prediction
# is made, it update the sentence by dropped the 1st character and
# append the predicted charater, take the updated sentence and make
# prediction in the next round. 
#
# While trainning, the input x is a colection of matries whose rows
# are 1-hot array, one matrix represents one sentence.
# Response y is just an matrix of 1-hot array of characters representing
# the next character for the corresponding sentence.
#
# This structure might be modify to suit our purpose, though, if we were
# really doing this, we have to set this up on clusters with GPU and sufficient
# RAM, fitting this on our DMC data with laptop will be a nightmare.
# And it leads to another problem of properly configure this "keras" package
# and Tensorflow backend, which is kind of painful, and I have no idea how to
# do this on clusters.

# To run this example code with GPU, you need to first install Python and 
# GPU version of Tensorflow, then install R package keras. (make sure everything
# is up-to-date.)
# For detail, refer to 
# https://tensorflow.rstudio.com/keras/reference/install_keras.html
# If you just want to try this with CPU, which, from my experience, tend to be 5
# to 10 times slower, just follow
# https://cran.r-project.org/web/packages/keras/vignettes/getting_started.html
# (I did not try this.)


library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)

# Parameters ---the length of a sentence for input-----------------------

maxlen <- 40

# Data Preparation --------------------------------------------------------

# Retrieve text
# pride and prejudice
path <- get_file(
  '1342-0.txt', 
  origin='https://www.gutenberg.org/files/1342/1342-0.txt'
)

# Load, collapse, and tokenize text
text <- read_lines(path) %>%
  str_to_lower() %>%
  str_c(collapse = "\n") %>%
  tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)

# Too long, cut
text <- text[1:20000]

print(sprintf("corpus length: %d", length(text)))

chars <- text %>%
  unique() %>%
  sort()

print(sprintf("total chars: %d", length(chars)))  

# Cut the text in semi-redundant sequences of maxlen characters
dataset <- map(
  seq(1, length(text) - maxlen - 1, by = 3), 
  ~list(sentece = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
)

dataset <- transpose(dataset)

# Vectorization
x <- array(0, dim = c(length(dataset$sentece), maxlen, length(chars)))
y <- array(0, dim = c(length(dataset$sentece), length(chars)))

for(i in 1:length(dataset$sentece)){
  # x[i,,] is a matrix whose rows are the 1-hot array of dataset$sentence[[i]]
  # say x[i,j,k] = (dataset$sentence[[i]][k] == chars[j]
  x[i,,] <- sapply(chars, function(x){
    as.integer(x == dataset$sentece[[i]])
  })
  # y[i,] is a 1-hot array of next char,
  # say y[i,j] = (dataset$next_char[[i]] == chars[j])
  y[i,] <- as.integer(chars == dataset$next_char[[i]])
  
}

# Model Definition --------------------------------------------------------

model <- keras_model_sequential()

model %>%
  # input_shape = dim(x[i,,])
  # adding one LSTM cell, at time t, taking input x[i,t,], and previous cell input h_{t-1}
  # give output o_t of dim(o_t) = 128, hidden state h_t, dim(h_t) = dim(o_t),
  # at time = t + 1, take x[i, t+1, ] and h_t, keep going till t = T of ith sample
  # and return the last output, say o_T, an array of length 128.
  # according to http://deeplearning.net/tutorial/lstm.html, equation (1) - (7),
  # the number of parameters should be 128*43 * 4 + 128*128 * 4 + 128 * 4 = 88064
  layer_lstm(128, input_shape = c(maxlen, length(chars))) %>%
  # a dense layer, ref. documentation, output an array
  # just a length(chars) X 128 matrix operation + bias then activation(linear by default)
  # number of parameters = 128 * 43 + 43 = 5547
  layer_dense(length(chars)) %>%
  # activation layer, so that output in (0,1)^length(chars)
  layer_activation("softmax")

# further remarks on this model:
# since hidden states are not passed through, we shall not expect 
# this model to capture long term dependence between samples(sentences).
# ref: http://philipperemy.github.io/keras-stateful-lstm/, "Questions and Answers" part.

summary(model)

# optimizer w/ learning rate, as for RMSProp, c.f. http://ruder.io/optimizing-gradient-descent/
optimizer <- optimizer_rmsprop(lr = 0.01)

# compile the model, (I think this is initialization the parameters)
# with loss = cate.crossentropy,
# for loss, c.f. ?loss_mean_squared_error
# or https://keras.rstudio.com/reference/loss_mean_squared_error.html
# or https://keras.io/losses/#available-loss-functions for its python version
model %>% keras::compile(
  loss = "categorical_crossentropy", 
  optimizer = optimizer
)

# Training & Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
  # take an array preds in (0,1)^d, transform it into a 1-hot array
  # say (0.1,0.2,0.3) -> (0,0,1)
  # for temperature, think about tempering in MCMC
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

on_epoch_end <- function(epoch, logs) {
  
  cat(sprintf("epoch: %02d ---------------\n\n", epoch))
  
  for(diversity in c(0.2, 0.5, 1, 1.2)){
    
    cat(sprintf("diversity: %f ---------------\n\n", diversity))
    
    # randomly pick the start index
    start_index <- sample(1:(length(text) - maxlen), size = 1)
    # get the selected sentence
    sentence <- text[start_index:(start_index + maxlen - 1)]
    # create variable to save later output
    generated <- ""
    
    # for generating a sentence of length 400
    for(i in 1:400){
      # recall x[i,,] previously
      # just a matrix whose jth rows are 1-hot arrs of the jth character in the sentence
      new.x <- sapply(chars, function(x){
        as.integer(x == sentence)
      })
      # reshape into tensor, so that the 
      # dim is consistent with the x[i,,] we had previously
      new.x <- array_reshape(new.x, c(1, dim(new.x)))
      
      # prediction, an array whose elements in (0,1) of dim = specified in modeling part
      preds <- predict(model, new.x)
      # use sample_mod to transform the preds into a 1-hot array for predicted char
      next_index <- sample_mod(preds, diversity)
      next_char <- chars[next_index]
      
      # append the next_char to generated sentence
      generated <- str_c(generated, next_char, collapse = "")
      # trim the sentence used for prediction
      # drop the 1st char, append the predicted next_char
      sentence <- c(sentence[-1], next_char)
      
    }
    
# remarks on the prediction:
# it seems that the state is not passed through each prediction,
# i.e., once finishing a prediction, next time for a new prediction,
# what is the value of the state?
    
    cat(generated)
    cat("\n\n")
    
  }
}

# callback_lambda, a function called after each epoch end.
# basically make prediction after each epoch end, take this as
# a handle to take a look during epoches running.
print_callback <- callback_lambda(on_epoch_end = on_epoch_end)

# fitting, in short
# batch_size = # of sample per gradient update
# epochs = maxiter
# should be fairly quick if you run this with some GPU
model %>% fit(
  x, y,
  batch_size = 64,
  epochs = 20,
  callbacks = print_callback
)
