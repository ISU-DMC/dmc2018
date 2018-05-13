

# n_cluster is a vector
FPCA <- function(curve, n_cluster, n_pca, n_start, n_basis)
{

	#library(dplyr)
  	library(fda)



	DMC.data_Raw <- curve
	Column_Number <- dim(DMC.data_Raw)[2]


	DMC.data <- DMC.data_Raw[, 2:Column_Number]




	Result_Raw <- DMC.data


	Result <- t(Result_Raw)



	Result_Standardized <- (apply(Result, MARGIN = 2, function(x)
	{
	  x/x[length(x)]
	}))



	Day_Length <- dim(Result_Standardized)[1]


	daybasis21 <- create.fourier.basis(rangeval=c(0, Day_Length), nbasis = n_basis)

#  -----------  set up the harmonic acceleration operator  ----------

	harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/Day_Length)^2,0), c(0, Day_Length))
#  ---------  create fd objects for temp. and prec. ---------------


	daytempfd <- smooth.basis((1:Day_Length)-0.5, Result_Standardized,
         daybasis21, fdnames=list("Day", "Station", "Deg C"))$fd



	harmfdPar <- fdPar(daybasis21, harmaccelLfd365, 1)

	daytemppcaobj <- pca.fd(daytempfd, nharm = n_pca, harmfdPar)



	dimnames(daytemppcaobj$scores)[[2]] <- paste("PCA", 1:n_pca, sep=".")
	Score <- daytemppcaobj$scores


	Final_Result <- DMC.data_Raw[,1]

	Cluster_Number <- length(n_cluster)

	for(i in n_cluster)
	{

		N_Cluster <- i

		Ind <- kmeans(Score, centers = N_Cluster, nstart = n_start)
		#Ind$cluster

	    #assign(paste0("Cluster_",i), Ind$cluster)


		#Final_Result <- eval(parse(
		#	text = paste0("bind_rows(Final_Result,Cluster_", i,")")))

		Final_Result <- eval(parse(
			text = paste0("data.frame(Final_Result, Cluster_", i, " =   Ind$cluster)")))
		
	}
	
	colnames(Final_Result)[2:(Cluster_Number+1)] <- paste0("Cluster_", n_cluster)


	Final_Result
	

}






