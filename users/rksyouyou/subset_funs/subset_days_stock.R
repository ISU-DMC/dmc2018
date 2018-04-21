## By Meiling Liu Apr. 21

subset.DS <- function(dat,start.day,end.day,start.stock,end.stock){
    ## dat: data.frame. The format should be consistent with the train data.

    ## construct auxiliary table
    keys <- paste0(dat$pid,dat$size)
    days <- aggregate(dat$units>0,by=list(keys=keys),sum,na.rm=T)
    stock <- dat[!duplicated(keys),c(1,2,13)]
    stock$keys <- paste0(stock$pid,stock$size)
    tmp <- merge(stock,days,by='keys')[,-c(2,3)]
    colnames(tmp) <- c('keys','stock','day')

    sdat <- dat[which(keys%in%tmp[which(tmp$day>=start.day&tmp$day<=end.day&tmp$stock>=start.stock&tmp$stock<=end.stock),1]),]
    return(sdat)
}
