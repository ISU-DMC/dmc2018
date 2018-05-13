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

## feature selection indicators by Yuchen
importance.feature <- read.csv("../yuchenw2015/feature_V01.csv") 
var.in <- importance.feature %>% filter(High2.6. == 1 | Low2.6.100. == 1) %>% 
  select(feature) %>% unlist %>% unname

## pid size date information
filetolabel <- "/vol/data/zhuz/lyux/feature_rds/alllabel.rds"
alllabel <- readr::read_rds(filetolabel)

## cluster_hf_norounding
cluster_hf <- read_rds("../hengfang/Cluster_Indicator_4_to_6_Specific.rds")

cut.Jan <- read_rds("../yuchenw2015/Intermediate.rds")
split.Jan <- cut.Jan %>% spread(Data_Ind, Count) %>%
  rename(Before = `FALSE`, After = `TRUE`) %>%
  mutate(Before = replace(Before, is.na(Before), 0),
          After = replace(After, is.na(After), 0)) %>%
  mutate(cut.Jan = "Both",
         cut.Jan = replace(cut.Jan, Before == 0, "After"),
         cut.Jan = replace(cut.Jan, After == 0, "Before"))

filetoalltrain <- "/vol/data/zhuz/lyux/feature_rds/alltrain.rds"
alltrain_input <- readr::read_rds(filetoalltrain)
cbind(alllabel, alltrain_input %>% select(one_of(c("units", as.character(var.in))))) -> alltrain_subfeature

write_rds(alltrain_subfeature, "/vol/data/zhuz/lyux/feature_rds/alltrain_subfeatures.rds")
