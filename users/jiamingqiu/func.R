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
