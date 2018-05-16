setwd("XiaodanLyu/TuneResults_CL4")

## only in cluster
cluster_hf <- read_rds("../../hengfang/Cluster_Indicator_4_to_6_Specific.rds")
cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))
cluster_hf %>% glimpse

k <- 2
Pred <- read_rds(sprintf("PredJanGlmnet_Month1_C4_%s.rds", k))
Pred %>% glimpse

Pred_cl <- Pred %>% left_join(cluster_hf, by = c("pid", "size")) %>%
  filter(Cluster_4 == k)

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
  summarise(
    pred_soldOutDate = ymd("2018-02-01") - sum(pred_yn.soldout),
    soldOutDate = ymd("2018-02-01") - sum(yn.soldout))  %>%
  ungroup

pred_Jan <- pred_Jan %>%
  mutate(pred_soldOutDate_adjust = 
           replace(pred_soldOutDate,
                   pred_soldOutDate == ymd("2018-02-01"), ymd("2018-01-23")))

## before adjustment
(pred_Jan$pred_soldOutDate - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## after adjustment
(pred_Jan$pred_soldOutDate_adjust - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## random guess middle month
(ymd("2018-01-18") - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
## random guess random day
(ymd("2018-01-03") + sample(1:28, 1) - pred_Jan$soldOutDate) %>% as.numeric %>% abs %>% sum %>% sqrt
