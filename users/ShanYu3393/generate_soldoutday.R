## function to create simulated stock and sold out date
Stock_Soldoutday=function(ID, y){
  ## Input: 
  # ID: (pid size, date) for the item and time period need to predict
  # y: daily sell w.r.t. ID
  Sold=data.frame(ID=ID,y=y) %>% spread(ID.date, y)
  SoldSum=rowSums(Sold[,-c(1,2)],na.rm = TRUE)
  # if SoldSum=0, then the Soldoutday is 0
  stock=SoldOutDay=rep(0,length(SoldSum))
  for(i in which(SoldSum > 0)) {
    dailysale=as.numeric(Sold[i,-c(1,2)])
    if (SoldSum[i]==1) {
      # if SoldSum=1, then the soldoutday is the day sold item
      stock[i]=1
      SoldOutDay[i]=which(dailysale==1)
    } else{
      # if SoldSum>1, the stock is random selected and then soldoutday is decided by 
      # cumsum
      stock[i]=sample(1:SoldSum[i],1)
      SoldOutDay[i]=which(cumsum(dailysale[!is.na(dailysale)])>=stock[i])[1]+sum(is.na(dailysale))
    }
  }
  list(stock=stock,SoldOutDay=SoldOutDay)
}
