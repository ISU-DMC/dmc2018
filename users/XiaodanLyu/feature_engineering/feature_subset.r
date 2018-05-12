xgboost_importance <- read.csv("xgboost_importance_V01.csv")
rf_importance3 <- read.csv("randomforest_importance_V01(3).csv")
rf_importance2 <- read.csv("randomforest_importance_V01(2).csv")

all_importance <- full_join(rf_importance2, rf_importance3,
                            by = "rowname", suffix = c("2", "3")) %>%
  full_join(xgboost_importance, by = c("rowname" = "Feature"))
all_importance %>% arrange(desc(IncNodePurity2)) %>% 


library(DT)
datatable(all_importance) %>%
  formatStyle(
    'Gain',
    background = styleColorBar(all_importance$Gain, 'steelblue'),
    backgroundSize = '100% 100%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'IncNodePurity2',
    background = styleColorBar(all_importance$IncNodePurity2, 'steelblue'),
    backgroundSize = '100% 100%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>% 
  formatStyle(
    'IncNodePurity3',
    background = styleColorBar(all_importance$IncNodePurity3, 'steelblue'),
    backgroundSize = '100% 100%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )
