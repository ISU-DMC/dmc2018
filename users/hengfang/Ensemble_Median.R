library("readr")
library("dplyr")
library("tidyr")
library("lubridate")


# Cluster by Clusert Ensemble

k <- 3
Month <- 1
Rep_Time <- 10

source("/Users/www1stat/Desktop/Loss_function.R")
Result_Dir <- "/Users/www1stat/Documents/DMC_Trial/Ensemble_Results_Version2/"


# Weight Intervel
Weight_Interval <- 0.1
Weight_Number <- 1/Weight_Interval

# Input method here
Method <- c("nnet", "xgbTree")
Method_Number <- length(Method)


setwd("/Users/www1stat/my_project/dmc2018/users/")

cluster_hf <- read_rds("hengfang/Cluster_Indicator_4_to_6_Specific.rds")
cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))


# Variable Name Example: Var1, Var2, ...
Weight_List <- list()

for(i in 1:(Method_Number))
{
  Weight_List[[i]] <- (0:Weight_Number) * Weight_Interval
}

Weight_Fine <- do.call("expand.grid", Weight_List)



# Weight Final is the weight that we want, and it's the
#    most slim version, now weight matrix has been done

eval(parse(
      text =   paste0("Weight_Final <- Weight_Fine %>% mutate(Sum = ",
  paste(paste0( "Var", 1:Method_Number), collapse = "+"   ), ")%>%
     filter(Sum == 1)  %>% select(-Sum)" )))


## only in cluster

#cluster_hf %>% glimpse



setwd("/Users/www1stat/my_project/dmc2018/users/XiaodanLyu/TuneResults_Cl4/")



Pred_List <- lapply(Method, FUN = function(method, k){
  read_rds(sprintf("PredJan%s_Month1_C4_%s.rds",method, k))
}, k= k)


#Pred <- read_rds(sprintf("PredJannnet_Month1_C4_%s.rds", k))
#Pred %>% glimpse





#Pred_cl <- Pred #%>% left_join(cluster_hf, by = c("pid", "size")) %>%
  #filter(Cluster_4 == k)




Pred_Base <- Pred_List[[1]]
Pred_Base$pred.units <- c(Pred_Base$pred.units)
Ensemble_Pred <- lapply(Pred_List, FUN = function(x){
      x$pred.units
})


Ensemble_Number <-  dim(Weight_Final)[1]



Error_Matrix <- matrix(0, ncol = 5, nrow = Ensemble_Number)




T1 <- Sys.time()
for(Loop in 1:Ensemble_Number)
{

  print(Loop)
  Pred_0 <- rep(0, dim(Pred_Base)[1])


  for(h in 1:Method_Number)
  {

    Pred_0 <- Pred_0 +  Weight_Final[Loop,h] * Ensemble_Pred[[h]]

  }


  Pred_cl <- Pred_Base %>% mutate(pred.units = Pred_0)
  Pred_cl$pred.units <- c(Pred_cl$pred.units)

  Error_Tmp <- matrix(0, ncol = 5, nrow = Rep_Time)

  for(s in 1:Rep_Time)
  {
    pred_Jan <- Pred_cl %>% group_by(pid, size) %>%
    mutate(
      pred.cumunits = cumsum(pred.units),
      cumunits = cumsum(units),
      # stock = sum(units)
      stock = sample(1:sum(units), 1)
    ) %>% ungroup %>%
    mutate(
      pred_yn.soldout = (pred.cumunits >= stock),
      yn.soldout = (cumunits >= stock)) %>%
    group_by(pid, size) %>%
    summarise( stock = unique(stock),
      pred_soldOutDate = ymd("2018-02-01") - sum(pred_yn.soldout),
      soldOutDate = ymd("2018-02-01") - sum(yn.soldout))  %>%
    ungroup

   pred_Jan <- pred_Jan %>%
    mutate(pred_soldOutDate_adjust = 
             replace(pred_soldOutDate,
                     pred_soldOutDate == ymd("2018-02-01"), ymd("2018-01-23")))

    ## before adjustment
    Error_Tmp[s, 1] <- (pred_Jan$pred_soldOutDate - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
    ## after adjustment
    Error_Tmp[s, 2] <- (pred_Jan$pred_soldOutDate_adjust - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
    ## random guess middle month
    Error_Tmp[s, 3] <- (ymd("2018-01-18") - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
    ## random guess random day
    Error_Tmp[s, 4] <- (ymd("2018-01-03") + sample(1:28, 1) - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
    ## conditional median
    Result <- Loss_MAE(Pred_cl$pred.units, Pred_cl %>% select(pid, size, date),
      pred_Jan$stock, as.numeric(pred_Jan$soldOutDate-ymd("2018-01-03")),'geom')
    Error_Tmp[s, 5] <- sqrt(sum(abs(Result[pred_Jan$soldOutDate!='2018-02-01'])))


  }

  # Repeated Results
  Error_Matrix[Loop, ] <- c(apply(Error_Tmp, MARGIN = 2, FUN = mean))
 
}

Sys.time() -T1
colnames(Error_Matrix) <- c("Before_Adj", "After_Adj", "RGMM", "RGRD", "Cond_Med")


# Index for minimum Before_Adj
Index_BA <- which.min(Error_Matrix %>% data.frame %>% select(Before_Adj) %>%
 data.matrix %>% c)
# Index for minimum After_Adj
Index_AA <- which.min(Error_Matrix %>% data.frame %>% select(After_Adj) %>%
 data.matrix %>% c)
# Index for minimum RGRD
Index_CM <- which.min(Error_Matrix %>% data.frame %>% select(Cond_Med) %>%
 data.matrix %>% c)




write_rds(unlist(Weight_Final[Index_BA,]), paste0(Result_Dir, 
                  "Weight_Before_Adj_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".rds" ))

write_rds(unlist(Weight_Final[Index_AA,]), paste0(Result_Dir, 
                  "Weight_After_Adj_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".rds" ))

write_rds(unlist(Weight_Final[Index_CM,]), paste0(Result_Dir, 
                  "Weight_Cond_Med_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".rds" ))


write_rds(Error_Matrix, paste0(Result_Dir, 
                  "Error_Matrix_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".rds" ))


write_rds(Weight_Final, paste0(Result_Dir, 
                  "Weight_Final_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".rds" ))



sink( paste0(Result_Dir, 
                  "Ensemble_", paste(Method, collapse = "_"),
                  "_Month", Month, "_C4_", k, ".txt" ))
sprintf("Best Before Adjustment Error: %.5f", Error_Matrix[Index_BA,1])
cat("BestBAWeight:", fill = T)
print(unlist(Weight_Final[Index_BA,]))
sprintf("Best After Adjustment Error: %.5f", Error_Matrix[Index_AA,2])
cat("BestAAWeight:", fill = T)
print(unlist(Weight_Final[Index_AA,]))
sprintf("Best Conditional_Mean Error: %.5f", Error_Matrix[Index_CM,5])
cat("BestCMWeight:", fill = T)
print(unlist(Weight_Final[Index_CM,]))
sprintf("Random Guess Error: %.5f", mean(Error_Matrix[,3]))
sink()








