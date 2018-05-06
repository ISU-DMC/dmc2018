#======
library(caret)
library(tidyr)
library(rpart)
library(MASS)
library(reshape2)
library(tidyr)
#
set.seed(1234)
#------
dmc_temp <- dmc <- read.csv("C:\\Users\\omnijust\\Desktop\\DMC\\dmc\\train_wide.csv", header=T)
#selling_rec <- read.csv("C:\\Users\\omnijust\\Desktop\\DMC\\dmc\\selling_rec.csv", header=T)

rownames(dmc) = dmc[,1 ]
dmc <- dmc[,-1]

selling_rec <- t(dmc)

selling_rec[1:8,1:10]

selling_rec_cum <- apply(selling_rec,2,cumsum)
selling_rec_cum[1:8,1:10]

#------
#In the following, selling_rec_cum will NOT be used for classification
selling_sum <- apply(selling_rec,2,sum)
table(selling_sum)

sum(selling_sum<=5)/length(selling_sum)
#at 5, items loss is 51.44%, meaning that about 48% of the items would be used

selling_rec_sub <- selling_rec[,as.numeric(which(selling_sum>50))]

simu <- function(x){
  temp <- c(x, sample(x, 60, replace = T))
  if(sum(tail(temp,60))==0){
    simu(x)
  }else{
    return(temp)
  }
}

selling_rec_simu <- apply(selling_rec_sub,2,simu)

y <- c(rep(1,123),rep(0,60))

#SAS
selling_sas <- cbind(selling_rec_simu,y)
write.csv(selling_sas,"C:/Users/omnijust/Desktop/DMC/R stuff/DMC/selling_sas.csv")

dist.mat <- matrix(ncol=1166,nrow=1166)
for(i in 1:1166){
  for(j in i:1166){
    selling_entry <- selling_rec_simu[,c(i,j)]
    test_ <- apply(selling_entry,1,sum)
    selling_bind <- cbind(selling_entry,y)[which(test_!=0),]
    selling_cart <- rpart(y ~ .,
                          data=data.frame(selling_bind),
                          method='class')
    selling_pred <- predict(selling_cart,type='class')
    dist.mat[i,j] <- mean(selling_pred==selling_bind[,3])
  }
}

temp_bind_test_[,c(1,3,4)]

selling_cart <- rpart(y ~ .,
                      data=data.frame(temp_bind_test_[,c(2,1,4)]),
                      method='class')
selling_pred <- predict(selling_cart,type='class')
mean(selling_pred==temp_bind_test_[,4])

#longer distance mean less likeliness
dist.mat.bk <- dist.mat
dist.mat <- dist.mat.bk
for(j in 1:1166){
  for(i in j:1166){
    dist.mat[i,j] <- dist.mat[j,i]
  }
}
dist.mat <- 1-dist.mat
dist.mat.bk2 <- dist.mat
#hclust
clust <- hclust(as.dist(dist.mat))
plot(clust, cex = 0.00000000000000001)
plot(clust, labels = F)
length(cutree(clust, k = 20))

#k-medoids clustering--1
install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
fviz_nbclust(t(selling_rec_sub), pam, method = "silhouette")+
  theme_classic()

pam.res <- pam(t(selling_rec_sub), 2)
comp1 <- as.numeric(pam.res$clustering)
comp2 <- cutree(clust, k = 2)
sum(comp1!=comp2)

pam.res$medoids

fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

#k-medoids clustering--2
fviz_nbclust(t(selling_rec_sub), pam, method = "gap_stat")+
  theme_classic()

pam.res.r1 <- pam(t(selling_rec_sub), 3)
comp1.r1 <- as.numeric(pam.res.r2$clustering)
comp2.r1 <- cutree(clust, k = 3)
sum(comp1.r1!=comp2.r1)

#src_sub
src_sub <- selling_rec_sub[,which(comp2==1)]
src_sub_cum <- apply(src_sub,2,cumsum)
reg1 <- function(x){
  x <- x/max(x)
}
src_sub_cum <- apply(src_sub_cum,2,reg1)
plot(0,xlim=c(0,123),ylim=c(0,1))
for(i in 1:dim(src_sub)[2]){
  lines(src_sub_cum[,i])
}

#If try more clusters
comp.clust <- cutree(clust, k = 10)
par(mfrow=c(2,5),oma=c(0,0,0,0))
for(k in 1:10){
  temp <- selling_rec_sub[,which(comp.clust==k)]
  temp_cum <- apply(temp,2,cumsum)
  temp_cum <- apply(temp_cum,2,reg1)
  plot(0,xlim=c(0,123),ylim=c(0,1))
  for(i in 1:dim(temp)[2]){
    lines(temp_cum[,i])
  }
}
dev.off()

#transforming
dist.mat <- dist.mat.bk2
dist.mat <- log(dist.mat/(1-dist.mat))
clust <- hclust(as.dist(dist.mat))

comp.clust.trans <- cutree(clust, k = 10)
par(mfrow=c(2,5))
for(k in 1:10){
  temp <- selling_rec_sub[,which(comp.clust.trans==k)]
  temp_cum <- apply(temp,2,cumsum)
  temp_cum <- apply(temp_cum,2,reg1)
  plot(0,xlim=c(0,123),ylim=c(0,1))
  for(i in 1:dim(temp)[2]){
    lines(temp_cum[,i])
  }
}
dev.off()
#no influence detected

#feature finding
item <- read.csv("C:/Users/omnijust/Desktop/DMC/items.csv",header=T,sep = "|")
price <- read.csv("C:/Users/omnijust/Desktop/DMC/prices.csv",header=T,sep = "|")
#if NA if releaseDate is later than observed date.

data_raw <- merge(item,price,c("pid","size"))

#testing for cluster 3
selling_rec_sub[1,which(comp.clust.trans==3)]
names <- colnames(selling_rec_sub)#names of 1166 items
names.3 <- names[which(comp.clust.trans==3)]

data_raw$id=paste(data_raw$pid,data_raw$size,sep="_")
rownames(data_raw) <- data_raw$id

# temp.d1 <- d1[1:5,]
# temp.d1.2 <- melt(temp.d1,id.vars=c("id","color","brand","rrp","mainCategory","category","subCategory","stock","releaseDate"))
# temp.d1.2$date <- as.Date(substr(temp.d1.2$variable,2,length(temp.d1.2$variable)),"%Y.%m.%d")
# temp.d1.2$weekday <- weekdays(temp.d1.2$date)

data_all <- melt(data_raw,id.vars=c("id","pid","size","color","brand","rrp","mainCategory","category","subCategory","stock","releaseDate"))
data_all$date <- as.Date(substr(data_all$variable,2,length(data_all$variable)),"%Y.%m.%d")
data_all$weekday <- weekdays(data_all$date)
data_all$price <- data_all$value

#Black Friday
data_all$blackF <- ifelse(data_all$date=="2017-11-24",1,0)

#release age
require(lubridate)
library(dplyr)
data_all$age <- 
  interval(ymd(data_all$releaseDate),ymd(data_all$date))%>%
  time_length(.,"day")

#factor(cat)
data_all$cat <- paste(data_all$mainCategory, data_all$category, data_all$subCategory, sep="_")

#splitting all the data
d.train <- data_all[which(data_all$date<="2018-01-31"),]
d.test <- data_all[which(data_all$date>"2018-01-31"),]


#FOR EXAMPLE ONLY, SHOULD CREATE TWO FILES FOR CLUSTER 3,
#ONE TRAINING
#ONE TESTING
{{{{{{{{{{{{{}}}}}}}}}}}}}
d.test.3 <- d.test[which(d.train$id %in% names.3),]

d1 <- d.train[which(d.train$id %in% names.3),]

#CUM OR NOT??????
d2 <- melt(t(selling_rec[,names.3]))
colnames(d2) <- c("id","var2","amount")
d2$date <- as.Date(substr(d2$var2,2,length(d2$var2)),"_%m_%d_%Y")

d.train.3 <- merge(d1,d2,c("id","date"))
d.train.3$brand <- factor(d.train.3$brand)
d.train.3$color <- factor(d.train.3$color)
d.train.3$cat <- factor(d.train.3$cat)

#levels(d$cat)

library(lme4)
colnames(d)
# [1] "id"           "date"         "pid"          "size"         "color"        "brand"        "rrp"          "mainCategory" "category"    
# [10] "subCategory"  "stock"        "releaseDate"  "variable"     "value"        "weekday"      "price"        "blackF"       "age"         
# [19] "cat"          "var2"         "amount"   

write.csv(d.test, "C:/Users/omnijust/Desktop/DMC/R stuff/DMC/d_test.csv")
write.csv(d.train, "C:/Users/omnijust/Desktop/DMC/R stuff/DMC/d_train.csv")
write.csv(d, "C:/Users/omnijust/Desktop/DMC/R stuff/DMC/d.csv")

#glmer fit
d.test.pred <- with(d.test.3,
                    data.frame(price,
                               weekday,
                               age,
                               rrp,
                               color,
                               blackF,
                               brand,
                               cat,id)
)

o <- glmer(amount~
             weekday+
             factor(color)+
             blackF+
             (1|brand)+
             (1|cat),
           family=poisson(link = "log"),
           data=d.train.3)
summary(o)
glm.predz <- predict(o, d.test.pred, na.action=na.exclude,
                     type = c("link", "response"), allow.new.levels = FALSE)
d.test.pred$amount_glm <- glm.predz

plot(d.train.3$amount~d.train.3$date, pch=20)
lines(d.test.pred$amount_glm~d.test.3$date, col="red")

#mlr fit
d.test.pred <- with(d.test.3,
                    data.frame(price,
                               weekday,
                               age,
                               rrp,
                               color,
                               blackF,
                               brand,
                               cat,id)
)
library(leaps)
swmlr <- regsubsets(amount~price+
                      weekday+
                      age+
                      rrp+
                      factor(color)+
                      blackF+
                      brand+
                      cat,
                    data=d.train.3)

plot(swmlr, scale="bic")

null <- lm(amount~1, data=d.train.3)
full <- lm(amount~price+
             weekday+
             age+
             rrp+
             factor(color)+
             blackF+
             brand+
             cat,
           data=d.train.3)
step(null, scope = list(upper=full), data=d.train.3, direction="both")
  
mlr <- lm(amount~
             weekday+
             factor(color)+
             blackF+
             brand+
             cat,
           data=d.train.3)
summary(mlr)
mlr.predz <- predict(mlr, d.test.pred, se.fit = TRUE)
d.test.pred$amount <- mlr.predz$fit

plot(d.train.3$amount~d.train.3$date, pch=20)
lines(d.test.pred$amount~d.test.3$date, col="red")

plot(d.test.pred$amount~d.test.pred$amount_glm)
{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}

#rpart
rpS <- rpartScore(amount~price+
                         weekday+
                         age+
                         rrp+
                         factor(color)+
                         blackF+
                         brand+
                         cat,
                        data=d.train.3,
                        prune = "mr")
plotcp(rpS)

rpS.pruned<-prune(rpS,cp=0.02)
plot(rpS.pruned)
#############subset date
#############cross validation

#try svm
svclassWisc<-ksvm(y~.,data=data.frame(temp_bind_test_[,c(2,1,4)]),kernel="vanilladot",C=.0085)
error(svclassWisc)
cross(svclassWisc)
predict(svclassWisc)

temp <- colnames(selling_rec_sub)
temp
ss = strsplit(temp,"_") 
pid <- sapply(ss,function(x)x[1])
size <- sapply(ss,function(x)x[2])
temp1 <- data.frame(pid,size,comp.clust.trans)
write.csv(temp1,"./sub1166.csv")
temp1

#----------
dist.mat <- matrix(ncol=1166,nrow=1166)
for(i in 1:1166){
  for(j in i:1166){
    selling_entry <- selling_rec_simu[,c(i,j)]
    test_ <- apply(selling_entry,1,sum)
    selling_bind <- cbind(selling_entry,y)[which(test_!=0),]
    selling_cart <- rpart(y ~ .,
                          data=data.frame(selling_bind),
                          method='class')
    selling_pred <- predict(selling_cart,type='class')
    dist.mat[i,j] <- mean(selling_pred==selling_bind[,3])
  }
}

#-----------
dist <- function(a,b){
  selling_entry <- cbind(a,b)
  test_ <- apply(selling_entry,1,sum)
  selling_bind <- cbind(selling_entry,y)[which(test_!=0),]
  selling_cart <- rpart(y ~ .,
                        data=data.frame(selling_bind),
                        method='class')
  selling_pred <- predict(selling_cart,type='class')
  return(mean(selling_pred==selling_bind[,3]))
}
VecFun <- Vectorize(dist)
