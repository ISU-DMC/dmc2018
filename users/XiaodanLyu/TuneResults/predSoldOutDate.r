rm(list = ls(all = T))
setwd("TuneResults/meiling_kNN/kNN_version1/")
cluster_id <- "Cluster_5"
Index_C <- Index %>% filter(cluster == cluster_id)
Data_C <- anti_join(Data_No_NA, Index_C)
# Data <- read_rds("/vol/data/zhuz/lyux/feature_rds/LLR_alltrain_subfeatures_may14.rds")
# Index <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_outlier_ID.rds")
Data <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_sub_prc_may15.rds")
Index <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_cluster5_pcr_outlier_ID.rds")
Index <- Index %>% mutate(date = ymd(date))
Data_No_NA <- Data %>% filter(!is.na(Cluster_2))

k <- 2
Results <- read_rds("PredJankNN_Month1_C5_2.rds")

Test <- Data_C %>% filter(date >= "2018-01-04")
Test <- Test %>% filter(get(cluster_id) == k) 

# Pred <- Results + Test$units
Pred <- Results
pred_Jan <- Test %>% select(pid, size, date, units) %>%
  mutate(pred_units = Pred) %>% 
  group_by(pid, size) %>%
  mutate(
    pred_cumunits = cumsum(pred_units),
    cumunits = cumsum(units),
    # stock = sum(units)
    stock = sample(1:sum(units), 1)
    ) %>% ungroup %>%
  mutate(
    pred_yn.soldout = (pred_cumunits >= stock),
    yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(
    pred_soldOutDate = ymd("2018-02-01") - sum(pred_yn.soldout),
    soldOutDate = ymd("2018-02-01") - sum(yn.soldout)) %>%
  ungroup

(pred_Jan$pred_soldOutDate - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
(ymd("2018-01-18") - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
(ymd("2018-01-03") + sample(1:28, 1) - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
