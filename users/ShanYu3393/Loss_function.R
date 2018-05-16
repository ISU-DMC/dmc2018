## define summaryFunction
## This is for poisson distribution
CondMedian_poi <- function(para,replication=500,type){
  
  # adjust NA
  t0=sum(is.na(para))
  para=para[!is.na(para)]
  
  # r is the number of stock
  r=para[length(para)]
  
  # a branch of predicted value
  lambda=para[-length(para)]
  
  # number of day
  n=length(lambda)
  
  # generate soldout day
  if (type=='poi'){
    A=sapply(1:replication,function(x) which(cumsum(rpois(n,lambda))>=r)[1])
  } if (type=='binomial') {
    A=sapply(1:replication,function(x) which(cumsum(rbinom(n,1-lambda))>=r)[1])
  }else {
    A=sapply(1:replication,function(x) which(cumsum(rgeom(n,1-lambda))>=r)[1])
  }
  
  
  # calculate median
  median(A,na.rm=TRUE)
}

## sold out day calculated by cumsum
Mean_poi <- function(para){
  
  # adjust NA
  t0=sum(is.na(para))
  para=para[!is.na(para)]
  
  # r is the number of stock
  r=para[length(para)]
  
  # a branch of predicted value
  lambda=para[-length(para)]
  
  # number of day
  n=length(lambda)
  
  # generate soldout day
  which(cumsum(lambda)>=r)[1]+t0
}


## Calculate loss function
Loss_MAE <- function(Para,ID,stock,Soldout,type){
  ## Input: 
  # Para: estimated daily parameters
  # ID: pid size date
  # stock：the number of item need to be soldout
  # Soldout: the truth sold out date
  
  
  # create a wide matrix
  PARA=cbind(ID, Para) %>% spread(date, Para) 
  PARA=cbind(PARA,stock) %>% dplyr::select(-pid,-size)
  
  # calculate conditional median for each item
  Pred=rep(0,length(Soldout))
  Pred2=apply(data.matrix(PARA[Soldout!=0,]),1,CondMedian_poi,type=type)
  Pred[Soldout!=0]=Pred2
  
  # if this item has not sold out within the preiod
  Pred[is.na(Pred)]=27
  
  # sum of absolute difference
  Pred-Soldout
}
