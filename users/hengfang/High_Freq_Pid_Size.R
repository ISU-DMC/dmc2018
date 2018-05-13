library("dplyr")
library("readr")
library("lubridate")
library("data.table")


setwd("/Users/www1stat/Documents/DMC_Trial/")
source("FPCA_Source.R")

# Units 

Units_Curve <- read_rds("Cache/units_wide.rds")


#### Regenrate Weird Units


# Daily_Sale <- Units_Curve %>% select(-key)

# Before <- Daily_Sale %>% select(starts_with('2017'))

# After <- Daily_Sale %>% select(starts_with('2018'))


# Before_True <- apply(Before, MARGIN = 1, FUN = sum)
# After_True <- apply(After, MARGIN = 1, FUN = sum)

# Cross <- (Before_True * After_True) >0 


# Cum_Units

Cum_Units_Curve <- read_rds("Cache/cumunits_wide.rds")

# Weird Items Indices

Weird_Units_Indices <- read_rds("Cache/split.jan.rds")

# Unify the key

colnames(Weird_Units_Indices)[1] <- "key"


Weird_Key_Reborn <- gsub(pattern = "\\t", replacement = ",", x = Weird_Units_Indices$key)


Weird_Units_Indices$key <- Weird_Key_Reborn




# First of all, eliminate the precise one
# Use Weird_Units_Indices as the benchmark


Eliminate_First_Stage_Bench <- Weird_Units_Indices %>% filter(cut.Jan == "Both")


Index_First_Stage_Units <- match(Eliminate_First_Stage_Bench$key,Units_Curve$key)  


Eliminate_First_Stage_Units <- Units_Curve[Index_First_Stage_Units, ]


Index_First_Stage_Cum_Units <- match(Eliminate_First_Stage_Bench$key, Cum_Units_Curve$key)  


Eliminate_First_Stage_Cum_Units <- Cum_Units_Curve[Index_First_Stage_Cum_Units, ]


# Secondly, eliminate items whose freq is less than 4(\leq 3)
# Still Eliminate_First_Stage_Bench as the benchmark



Pure_Units <- Eliminate_First_Stage_Units %>% select(-key) %>% apply(
			 		MARGIN  = 1,  FUN = function(x){
			 			sum(x > 0)
			 		})

Index_Second_Stage_Bench <- Pure_Units >= 4

Bench_Final <- Eliminate_First_Stage_Bench[Index_Second_Stage_Bench,]
Units_Final <- Eliminate_First_Stage_Units[Index_Second_Stage_Bench,]
Cum_Units_Final <- Eliminate_First_Stage_Cum_Units[Index_Second_Stage_Bench,]




# Cheers! 6317 items sell more than 3 times during the whole period, 
#          also in both period
# Clustering Now
# Need to wrap the FPCA function

N_Cluster <- 2:9
N_PCA <- 4
N_Start <- 13
N_Basis <- 17



FPCA_Result <- FPCA(curve = Cum_Units_Final,
					n_cluster = N_Cluster, 
					n_pca = N_PCA,
					n_start = N_Start, 
					n_basis = N_Basis)


# Cluster_Plot
# Modified from Yuchen's code


cluster<-FPCA_Result

a <- fread("Cache/combined.csv", sep = "auto")

key<-paste(a$pid,a$size,sep=" - ")
train.Jan <- data.frame(a,key) %>% 
  mutate(date = ymd(date))%>% 
  filter(date < ymd("2018-02-01"))

data.c<-merge(train.Jan,cluster,by="key")

data.c$units[is.na(data.c$units)]<-0




pdf(file="Cluster_Figure.pdf") 



for(i in N_Cluster)
{
	print(i)
	for(j in 1:i)
	{
		subdata<-   eval(parse( 
			text = paste0(  "data.c[data.c$Cluster_", i, "==", j,",]" )))
		name<-paste0("Hengfang group , Cluster", i, ".", j, " units=")


		###################################


		n.units<-nrow(subdata)/123
		cum<-tapply(subdata$units,as.character(subdata$key),cumsum)
		plot(c(1:123),unlist(cum[1])[1:123]/unlist(cum[1])[123],main=c(name,n.units),type="l",xlim=c(0,123),ylim=c(0,1),xlab="day",ylab="% of total units sold")
		k<-0
		for(k in 2:length(cum)){
		  lines(c(1:123),unlist(cum[k])[1:123]/unlist(cum[k])[123],col=k)
		}
	}

}

dev.off() 










































