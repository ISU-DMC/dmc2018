rm(list = ls())

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

filepath="/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/TuneResults_Cl4/PredJanxgbTree_Month1_C4_4.rds"

alltest=readRDS(filepath)

# alltest=read.csv(filepath)[,-1]

Soldout <- alltest %>% group_by(pid, size) %>%
  mutate(
    pred.cumunits = cumsum(pred.units),
    cumunits = cumsum(units),
    # stock = sum(units)
    stock = sample(1:max(1,sum(units)), 1)
  ) %>% ungroup  %>%
  mutate(
    pred_yn.soldout = (pred.cumunits >= stock),
    yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(
    stock = unique(stock),
    pred_soldOutDate = ymd("2018-02-01") - sum(pred_yn.soldout),
    soldOutDate = ymd("2018-02-01") - sum(yn.soldout))  %>% ungroup

pred_Jan <-Soldout %>%
  mutate(pred_soldOutDate_adjust =
           replace(pred_soldOutDate,
                   pred_soldOutDate == ymd("2018-02-01"), ymd("2018-01-23")))

(pred_Jan$pred_soldOutDate - pred_Jan$soldOutDate)%>%
  as.numeric %>% abs %>% sum %>% sqrt 

#True <- Stock_Soldoutday(alltest[,1:3],alltest[,4])

Result <- Loss_MAE(alltest$pred.units,alltest %>% select(pid,size,date),
         Soldout$stock, as.numeric(Soldout$soldOutDate-ymd("2018-01-03")),'poi')
Error <- sqrt(sum(abs(Result)))

Error

RG.Error <- sqrt(sum(abs(as.numeric(Soldout$soldOutDate - ymd("2018-01-18")))))

RG.Error

sprintf("Model Error:%.5f", Error)
sprintf("Random Guess Error:%.5f", RG.Error)

## only for stock == 1
alltest.lowstock <- alltest %>% group_by(pid, size) %>%
  mutate(stock = sum(units)) %>% 
  filter(stock == 1) %>% 
  # filter(stock == 1 | stock == 2 | stock == 3) %>%
  ungroup

True <- alltest.lowstock %>% group_by(pid, size) %>%
  arrange(date) %>% 
  summarise(SoldOutDay = which(units == 1),
            pred.SoldOutDay = which.min(pred.prob>=0.5))

Err <- sqrt(sum(abs(True$SoldOutDay-True$pred.SoldOutDay)))

RG.Error <- sqrt(sum(abs(True$SoldOutDay - 15)))

sprintf("Model Error:%.5f", Error)
sprintf("Random Guess Error:%.5f", RG.Error)
