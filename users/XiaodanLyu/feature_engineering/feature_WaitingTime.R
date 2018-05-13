rm(list = ls(all = T))
source("feature_engineering/help.r")
library(readr)
## Meiling
wait_time <- read.table("feature_engineering/waiting_time_dat.txt", sep = "|", header = T) 
wait_time %>% dim
wait_time$startdate %>% ymd %>% summary()

## Jiayi
wait_date <- read_rds("feature_engineering/Total_Waiting.rds")
wait_date %>% dim
wait_date$startdate %>% ymd %>% summary()
  
items <- read_rds("/vol/data/zhuz/lyux/feature_rds/item_static_features_may10.rds")
wait_date <- wait_date %>% mutate(pid = as.character(pid),
                                  size = replace(size, size == "", "42")) 
alldata_wait <- full_join(wait_date, items, by = c("pid", "size"))
alldata_wait_expand <- alldata_wait %>% mutate(
  date.age = (startdate - releaseDate) %>% as.numeric,
  date.day = (startdate - ymd("2017-09-30")) %>% as.numeric,
  # startdate.dm = day(startdate),
  date.wd = weekdays(startdate),
  date.wm = ceiling((day(startdate) + first_day_of_month_wday(startdate) - 1) / 7) %>% as.character
)
alldata_wait_expand %>% glimpse

readr::write_rds(alldata_wait_expand, "/vol/data/zhuz/lyux/feature_rds/WT_all_features_may12.rds")
