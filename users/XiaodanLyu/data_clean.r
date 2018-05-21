rm(list = ls(all = T))
library(lubridate)
train <- read.csv("raw_data/train.csv", sep = "|")
train <- train %>% mutate(size = replace(size, size == "", "42"))
items <- read.csv("raw_data/items.csv", sep = "|")
items <- items %>% mutate(size = replace(size, size == "", "42"))
all_dates_keys <- expand.grid(date = seq(ymd("2017-10-01"), ymd("2018-01-31"), 1) %>% as.character(),
                              key = paste(items$pid, items$size, sep = " - "))
train_long <- train %>% mutate(key = paste(pid, size , sep = " - ")) %>%
  full_join(all_dates_keys, by = c("key", "date"))
train_long_format <- train_long %>% mutate(units = replace(units, is.na(units), 0)) %>%
  group_by(key) %>% arrange(date) %>% mutate(cumunits = cumsum(units))
train_long_format %>% arrange(key) %>% glimpse

train_wide_lag <- train_long_format %>% select(-pid, -size) %>% group_by(key) %>%
  mutate(lagunits = units - lag(units)) %>% 
  # filter(!is.na(lagunits)) %>% 
  ungroup

# readr::write_rds(train_wide_lag, "train_wide_lag.rds")
library(tidyr)
cumunits_wide <- train_wide_lag %>% select(-units, -lagunits) %>% spread(date, cumunits)
units_wide <- train_wide_lag %>% select(-cumunits, -lagunits) %>% spread(date, units)
lagunits_wide <- train_wide_lag %>% filter(!is.na(lagunits)) %>% 
  select(-cumunits, -units) %>% spread(date, lagunits)

readr::write_rds(cumunits_wide, "cumunits_wide.rds")
readr::write_rds(units_wide, "units_wide.rds")
readr::write_rds(lagunits_wide, "lagunits_wide.rds")

units_wide <- read_rds("~/dmc_2018/units_wide.rds")
split_jan <- units_wide %>% 
  mutate(Before = rowSums(units_wide %>% dplyr::select(`2017-10-01`:`2017-12-31`)), 
         After = rowSums(units_wide %>% dplyr::select(`2018-01-01`:`2018-01-31`))) %>%
  dplyr::select(key, Before, After) %>%
  mutate(cut.Jan = "Both",
         cut.Jan = replace(cut.Jan, Before == 0, "After"),
         cut.Jan = replace(cut.Jan, After == 0, "Before"))
