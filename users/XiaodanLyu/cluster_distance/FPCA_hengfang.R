library(dplyr)
library(fda)
#library(plot3D)
#library(openxlsx)
#library(ggmap)
#library(mapproj)
#library(xtable)



load("DMC.Data.rda")

setwd("/Users/www1stat/Documents/DMC_Trial/raw_data/")






Result_Raw <- DMC.Data



Result_Raw_0 <-  Result_Raw[, -(1:11)]


Result <- t(Result_Raw_0)


Result_Standardized <- (apply(Result, MARGIN = 2, function(x)
{
  x/x[length(x)]
}))


#matrix that stores pm2.5 of 36 sites in Beijing
# Result <- matrix(rep(0, 365*36), ncol = 36, nrow = 365)
  
# for(i in 1:length(FILE))
# {
#   X <- read.csv(file = paste("/Users/www1stat/Desktop/547Project/Data/160313/", 
#                       FILE[i], sep = ""), header = TRUE)

#   print(i)
#   T1 <- Sys.time()
#   XX <- filter(X[,-c(1,2)], year!= 2010 | !(month == 2 & day == 29))
#   PMtmp <- XX[,1:5]
#   JJ <- PMtmp[!is.na(PMtmp$PM2.5),]
#   #12 times 31 matrix
#   Adjpm <- tapply(JJ$PM2.5, list(JJ$month, JJ$day), FUN = mean)

#   #days in every months
#   K <- 1
#   MV <- c(31,28,31,30,31,30,31,31,30,31,30,31)
#   for(j in 1:12)
#   {
#     Result[K:(K+MV[j]-1),i] <- Adjpm[j,1:MV[j]]
#     K <- K+MV[j]
#   }
#   print(Sys.time()-T1)
# }

# head(Result)

# plot(NULL,xlim = c(0,366), ylim = c(0,500), ylab = "PM2.5", xlab = "Day")
# Year <- 1:365
# for(i in 1:36)
# {
#   lines(Year, Result[,i])
# }


#We only have around three years' data. Therefore, the average might not be smooth enough. Therefore,
#we choose less basis to 'smooth' such random effect.
Day_Length <- dim(Result)[1]


daybasis21 <- create.fourier.basis(rangeval=c(0, Day_Length), nbasis = 10)

#  -----------  set up the harmonic acceleration operator  ----------

harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/Day_Length)^2,0), c(0, Day_Length))
#  ---------  create fd objects for temp. and prec. ---------------

# First check the distributiond
#qqnorm(Result, datax=TRUE)

daytempfd <- smooth.basis((1:Day_Length)-0.5, Result_Standardized,
         daybasis21, fdnames=list("Day", "Station", "Deg C"))$fd


tempmeanfd  <- mean.fd(daytempfd)
tempstdvfd  <- sd.fd(daytempfd)


#plot(tempmeanfd, main="Mean PM2.5 Curve of 36 Sites in Beijing", ylab = "PM2.5 Concentration")
#plot(tempstdvfd, main="Standard Deviation PM2.5 Curve of\n 36 Sites in Beijing", log="y", ylab = "PM2.5 Concentration")

tempvarbifd <- var.fd(daytempfd)
tempvarmat  <- eval.bifd(weeks,weeks,tempvarbifd)

# op <- par(mfrow=c(1,2), pty="s")
# #contour(tempvarmat, xlab="Days", ylab="Days")
# contour(weeks, weeks, tempvarmat,
#         xlab="PM2.5 by day",
#         ylab="PM2.5 by day",
#         main=paste("Variance function across locations\n",
#           "for Beijing Anual PM2.5 Cycle"),
#         cex.main=0.8, axes=FALSE)
# axisIntervals(1, atTick1=seq(0, 365, length=5), atTick2=NA,
#             atLabels=seq(1/8, 1, 1/4)*365,
#             labels=paste("Q", 1:4) )
# axisIntervals(2, atTick1=seq(0, 365, length=5), atTick2=NA,
#             atLabels=seq(1/8, 1, 1/4)*365,
#             labels=paste("Q", 1:4) )
# #persp(tempvarmat,xlab="Days", ylab="Days", zlab="Covariance")
# persp3D(weeks, weeks, tempvarmat,
#       xlab="Days", ylab="Days", zlab="Covariance",phi = 20, main = "PM2.5 Covariance")

# cov2cor(tempvarmat)
# persp3D(weeks, weeks, cov2cor(tempvarmat),
#       xlab="Days", ylab="Days", zlab="Covariance",phi = 20, main = "PM2.5 Correlation")



harmfdPar <- fdPar(daybasis21, harmaccelLfd365, 1)

daytemppcaobj <- pca.fd(daytempfd, nharm = 4 , harmfdPar)

#daytemppcaobjVM <- varmx.pca.fd(daytemppcaobj)


dimnames(daytemppcaobj$scores)[[2]] <- paste("PCA", 1:4, sep=".")
Score <- round(daytemppcaobj$scores)

#  plot harmonics

#par(mfrow=c(1,1), pty="m")
#plot.pca.fd(daytemppcaobj)

#the first four PCA proportion
#87.1+6.8+1.6+1.3

#plot(Score[,1],Score[,2])

?kmeans





Ind <- kmeans(Score, centers = 5, nstart = 25)
Ind$cluster




Final_Result <- cbind(Result_Raw[,1:2], Ind$cluster)


saveRDS(Final_Result, "Cluster_Indicator_5.rds")

#get latitude and longitude
# LL <- read.xlsx(xlsxFile = 
#                   "/Users/www1stat/Desktop/547Project/Data_information_map.xlsx")
# LLtmp <- LL[1:36,]
# LLtmp
# SER <- sub(pattern = "_Arrange.csv", replacement = "", x = FILE)
# #Match the filename
# IND <- match(x = SER, LLtmp$file_name)
# Re_arrangeLL <- LL[IND,]



#putting PCA score , latitude, longitude together
site <- sub(pattern = "Beijing_", replacement = "", Re_arrangeLL$file_name)
mydata <- data.frame(Site = site, LAT = Re_arrangeLL$lat,
                     LON = Re_arrangeLL$lon, SC1 = Score[,1], SC2 = Score[,2],
                     PCA_cluster = Ind$cluster)


#ploting the regionalization plot for beijing
map <- get_map(location = 'beijing', zoom = 9)
ggmap(map)+ geom_point(data=mydata,aes(x=LON, y=LAT, color = PCA_cluster),
                        size = 2) +
         scale_size(name="Proportion")+geom_point() + 
    scale_colour_gradientn(colours=rainbow(4))+ 
   geom_text(data = mydata,aes(x=LON, y=LAT,label = Site),size = 3,check_overlap = TRUE)+labs(x = "Longitude", y = "Latitude", title = "Distribution of 36 detection sites in Beijing\n after K-means clustering")


#basic distribution
ggmap(map)+ geom_point(data=mydata,aes(x=LON, y=LAT),color = "red",
                        size = 2)+geom_point()+
    scale_colour_gradientn(colours=rainbow(4))+
   geom_text(data = mydata,aes(x=LON, y=LAT,label = Site),size = 3,check_overlap = TRUE) +labs(x = "Longitude", y = "Latitude", title = "Distribution of 36 detection sites in Beijing")
tail(X)
xtable(Score2)
youdata <- data.frame(Site = site,  Score1 = Score[,1], 
                  Score2 =  Score[,2], PCA_cluster = Ind$cluster)

xtable(youdata)
