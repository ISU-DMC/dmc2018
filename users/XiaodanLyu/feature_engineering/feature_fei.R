pathtofile <- "/vol/data/zhuz/lyux/feature_rds/alltrain_may14.rds"
alltrain_cluster <- read_rds(pathtofile)
alllabel <- read_rds("/vol/data/zhuz/lyux/feature_rds/alllabel.rds")

alltrain_cluster <- cbind(alllabel, alltrain_cluster)

(name.trend.main <- grep("X", grep("trend", names(alltrain_cluster), value = T), invert = T, value = T))
(name.price.main <- grep("X", grep("price", names(alltrain_cluster), value = T), invert = T, value = T))
names.var <- c("pid", "size", "date", "units", "stock", "discount", name.price.main, name.trend.main)

alltrain_fei <- alltrain_cluster %>% select(one_of(names.var))

## cluster results
cluster_hf <- read_rds("feature_engineering/cluster_hf_2_9.rds")

alltrain_pcr40 <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_sub_prc_may15.rds")
alltrain_pcr30 <- read_rds("/vol/data/zhuz/lyux/feature_rds/alltrain_pcr30_may14.rds")

alltrain_fei %>% left_join(cluster_hf, by = c("pid", "size")) -> alltrain_fei_cluster
data.frame(alltrain_fei_cluster, alltrain_pcr30 %>% select(-units)) -> alltrain_fei_cluster_pcr

alltrain_fei_final <- alltrain_fei_cluster_pcr %>%
  mutate(units_lag1 = lag(units, 1, default = 0),
         units_lag2 = lag(units, 2, default = 0),
         units_lag3  = lag(units, 3, default = 0),
         units_lag4 = lag(units, 4, default = 0),
         units_lag5 = lag(units, 5, default = 0), 
         units_lag6 = lag(units, 6, default = 0),
         units_lag7 = lag(units, 7, default = 0),
         units_lagwk_avg = rowMeans(alltrain_fei_final %>% select(units_lag1:units_lag7))) 
alltrain_fei_final %>% glimpse

write_rds(alltrain_fei_final, "/vol/data/zhuz/lyux/feature_rds/alltrain_feature_fei.rds")
