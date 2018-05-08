library(tidyverse)
feature <- function(train, codebook){
  # browser()
  fac <- which(sapply(1:ncol(train), function(i) is.factor(train[,i]))==1)
  iter <- 0
  repeat({
    iter <- iter + 1
    name.fac <- colnames(train)[fac[iter]]
    code.fac <- codebook %>% filter(name.variable == name.fac) %>%
      dplyr::select(-type.feature, -name.variable) %>% spread(name.feature, value.feature)
    names(code.fac)[1] <- name.fac 
    train <- train %>% left_join(code.fac, by = name.fac)
    if(iter >= length(fac)) break
  })
  
  return(train)
}

codebook <- function(train, response) {
  # browser()
  name <- colnames(train)
  fac <- which(sapply(1:ncol(train), function(i) is.factor(train[,i]))==1)
  code <- rbind(do.call("rbind", lapply(fac, freq_cate,
                                        train %>% filter(!duplicated(cbind(pid, size)))))
                # do.call("rbind", lapply(fac, LLR_cate, train, response = response))
                )
  names(code) <- c("name.feature", "level", "value.feature")
  code <- code %>% 
    separate(name.feature, into = c("name.variable", "type.feature"), sep = "_", remove = FALSE)
  return(code)
}

freq_cate <- function(i,train) {
  # browser()
  x_cate <- train[,i]
  df <- data.frame(name = rep(paste0(colnames(train)[i],'_freq'),length(levels(x_cate))),
                   # origin_level = levels(x_cate)
                   # table(factor(x_cate))/length(x_cate)*100
                   boot::logit(table(factor(x_cate))/length(x_cate))
                   )
  return(df)
}

LLR_cate <- function(i, train, response) {
  y <- train[,response]
  # temp0 <- temp1 <- temp2 <- temp3 <- x_cate
  # temp0 <- x_cate
  # levels(temp0) <- table(x_cate[y>0])/table(x_cate[y==0])
  # levels(temp1) <- table(x_cate[y==1])/table(x_cate[y==0])
  # levels(temp2) <- table(x_cate[y>1])/table(x_cate[y==0])
  # levels(temp3) <- table(x_cate[y>2])/table(x_cate[y==0])
  
  # temp0 <- as.numeric(as.character(temp0))
  # temp1 <- as.numeric(as.character(temp1))
  # temp2 <- as.numeric(as.character(temp2))
  # temp3 <- as.numeric(as.character(temp3))
  x_cate <- train[,i]
  df <- data.frame(name=rep(paste0(colnames(train)[i],'_LLR'),length(levels(x_cate))),
                   log(table(x_cate[y>0])/table(x_cate[y==0])))
  return(df)
}


first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}