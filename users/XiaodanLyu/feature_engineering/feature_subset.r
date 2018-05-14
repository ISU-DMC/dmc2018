## join importance tables
# xgboost_importance <- read.csv("xgboost_importance_V01.csv")
# rf_importance3 <- read.csv("randomforest_importance_V01(3).csv")
# rf_importance2 <- read.csv("randomforest_importance_V01(2).csv")
# 
# all_importance <- full_join(rf_importance2, rf_importance3,
#                             by = "rowname", suffix = c("2", "3")) %>%
#   full_join(xgboost_importance, by = c("rowname" = "Feature"))
# all_importance %>% arrange(desc(IncNodePurity2)) %>% 

# library(DT)
# datatable(all_importance) %>%
#   formatStyle(
#     'Gain',
#     background = styleColorBar(all_importance$Gain, 'steelblue'),
#     backgroundSize = '100% 100%',
#     backgroundRepeat = 'no-repeat',
#     backgroundPosition = 'center'
#   ) %>%
#   formatStyle(
#     'IncNodePurity2',
#     background = styleColorBar(all_importance$IncNodePurity2, 'steelblue'),
#     backgroundSize = '100% 100%',
#     backgroundRepeat = 'no-repeat',
#     backgroundPosition = 'center'
#   ) %>% 
#   formatStyle(
#     'IncNodePurity3',
#     background = styleColorBar(all_importance$IncNodePurity3, 'steelblue'),
#     backgroundSize = '100% 100%',
#     backgroundRepeat = 'no-repeat',
#     backgroundPosition = 'center'
#   )

## response == "units" ####
## feature selection indicators by Yuchen
rm(list=ls(all=T))
importance.feature <- read.csv("../yuchenw2015/feature_V01.csv") 
var.in <- importance.feature %>% filter(High2.6. == 1 | Low2.6.100. == 1) %>% 
  select(feature) %>% unlist %>% unname

## pid size date information
filetolabel <- "/vol/data/zhuz/lyux/feature_rds/alllabel.rds"
alllabel <- readr::read_rds(filetolabel)

filetoalltrain <- "/vol/data/zhuz/lyux/feature_rds/alltrain_may14.rds"
alltrain_input <- readr::read_rds(filetoalltrain)
all(var.in %in% names(alltrain_input))
cbind(alllabel, alltrain_input %>% select(one_of(c("units", as.character(var.in))))) -> alltrain_subfeature

write_rds(alltrain_subfeature, "/vol/data/zhuz/lyux/feature_rds/alltrain_subfeatures_may14.rds")

## response = "wait_time" ####
## feature selection indicators by Yuchen
importance.feature <- read.csv("../yuchenw2015/WT_feature_V01.csv") 
var.in <- importance.feature %>% filter(High == 1 | Low == 1) %>% 
  select(feature) %>% unlist %>% unname %>% as.character

filetoalltrain <- "/vol/data/zhuz/lyux/feature_rds/WT_alltrain.rds"
alltrain_input <- readr::read_rds(filetoalltrain)
alltrain_subfeature <- alltrain_input %>% select(one_of(c("time", as.character(var.in))))

write_rds(alltrain_subfeature, "/vol/data/zhuz/lyux/feature_rds/WT_alltrain_subfeatures.rds")
write.table(alltrain_subfeature, "/vol/data/zhuz/lyux/feature_rds/WT_alltrain_subfeatures.txt",
            sep = "|", row.names = F, quote = F)

