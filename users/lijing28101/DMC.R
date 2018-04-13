library(lubridate)
library(reshape2)
library(ggplot2)
price <- read.csv("raw_data/prices.csv",sep = "|")
train <- read.csv("raw_data/train.csv",sep="|")
items <- read.csv("raw_data/items.csv",sep='|')
price2 <- melt(price,id=c("pid","size"),value.name = "price")
colnames(price2)[3] <- "date"
date <- gsub("X","",price2$date)
date <- gsub("\\.","-",date)
price2$date <- as.factor(date)
com1 <- merge(price2,train,all.x = T)
all.data <- merge(items,com1,all.y = T)

xtabs(~mainCategory+category,items)
main1 <- subset(items,mainCategory==1)
as.data.frame(xtabs(~category+subCategory,main1))
main9 <- subset(items,mainCategory==9)
xtabs(~category+subCategory,main9)
main15 <- subset(items,mainCategory==15)
table(main15$category)

items$category <- factor(items$category, levels = c(2,7,37,10,18,36,16,24,30,33))
ggplot(items,aes(x=as.factor(category),y=rrp,fill=as.factor(mainCategory)))+
  geom_boxplot()+stat_boxplot(geom ='errorbar')+
  scale_fill_discrete(name="mainCategory")+
  labs(x="category",title="Distribution of rrp for each category")

date <- data.frame(date=unique(all.data$date))
sale <- aggregate(units~mainCategory+category+date,all.data,sum,na.rm=T)
sale2 <- merge(date,sale,all.x=T)
sale$cat <- paste(sale$mainCategory,sale$category,sep="_")
sale$logunits <- log(sale$units+1)
cum <- aggregate(units~cat,sale,cumsum)
sale <- transform(sale, cum=ave(units,cat,FUN=cumsum))
sale$logcum <- log(sale$cum+1)


ggplot(sale,aes(x=date,y=logcum)) + geom_point(aes(colour=as.factor(cat),shape=as.factor(mainCategory)))

f <- ggplot(PlantHeights304, aes(x = Day, y = HeightCM)) +
  geom_boxplot(aes(colour=PlantID))