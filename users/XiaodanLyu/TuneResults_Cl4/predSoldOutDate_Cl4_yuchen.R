setwd("C:/Users/63139/Documents/GitHub/dmc2018/users/XiaodanLyu/TuneResults_CL4")
library(readr)
library(lubridate)
library(dplyr)
## only in cluster
cluster_hf <- read_rds("C:/Users/63139/Documents/GitHub/dmc2018/users/hengfang/Cluster_Indicator_4_to_6_Specific.rds")
cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))
cluster_hf %>% glimpse

k <- 4
Pred <- read_rds(sprintf("PredJannnet_Month1_C4_%s.rds", k))
Pred %>% glimpse

Pred_cl <- Pred
Pred_cl %>% select(pid, size) %>% unique %>% dim
  # left_join(cluster_hf, by = c("pid", "size")) %>%
  # filter(Cluster_4 == k)

pred_Jan <- Pred_cl %>% group_by(pid, size) %>%
  mutate(
    pred.cumunits = cumsum(pred.units),
    cumunits = cumsum(units),
    #stock = sum(units)
    stock = sample(1:sum(units), 1)
  ) %>% ungroup %>%
  mutate(
    pred_yn.soldout = (pred.cumunits >= stock),
    yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(
    pred_soldOutDate = ymd("2018-02-01") - sum(pred_yn.soldout),
    soldOutDate = ymd("2018-02-01") - sum(yn.soldout))  %>%
  ungroup
pred_Jan_sold<-pred_Jan%>%
  filter(pred_soldOutDate != ymd("2018-02-01"))
pred_Jan <- pred_Jan %>%
  mutate(pred_soldOutDate_adjust = 
           replace(pred_soldOutDate,
                   pred_soldOutDate == ymd("2018-02-01"), ymd("2018-01-23")))
## only sold items
error.sold<-(pred_Jan_sold$pred_soldOutDate - pred_Jan_sold$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
error.sold.guess<-(ymd("2018-01-18") - pred_Jan_sold$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## before adjustment
error<-(pred_Jan$pred_soldOutDate - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## after adjustment
error.adjust<-(pred_Jan$pred_soldOutDate_adjust - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## random guess middle month
error.guess<-(ymd("2018-01-18") - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## random guess random day
error.random<-(ymd("2018-01-03") + sample(1:28, 1) - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
error.adjust.day<-error.adjust^2/nrow(pred_Jan)
error.guess.day<-error.guess^2/nrow(pred_Jan)
error.sold.day<-error.sold^2/nrow(pred_Jan_sold)
error.sold.guess.day<-error.sold.guess^2/nrow(pred_Jan_sold)

data.frame(error.sold,error.sold.day,error.sold.guess,nrow(pred_Jan_sold),error,error.adjust,error.adjust.day,error.guess,error.guess.day,error.random,nrow(pred_Jan))
