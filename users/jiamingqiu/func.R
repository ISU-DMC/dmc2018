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
    seq(1, length(in_seq) - maxlen - 1, by = by), 
    ~list(history = in_seq[.x:(.x + maxlen - 1)], next_day = in_seq[(.x + maxlen) : (.x + maxlen + next_len - 1)])
  )
}

