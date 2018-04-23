alldata %>%
  mutate(week = week(date), month = month(date)) %>%
  group_by(date) %>% mutate(bestseller.oneday = ifelse(units == max(units), 1, 0)) %>%
  ungroup %>% group_by(week, key) %>% 
  mutate(units.week = sum(units))  %>% ungroup %>% group_by(week) %>% 
  mutate(bestseller.oneweek = ifelse(units.week == max(units.week), 1, 0)) %>% 
  ungroup %>% group_by(month, key) %>%
  mutate(units.month = sum(units, na.rm = T)) %>% ungroup %>% group_by(month) %>%
  mutate(bestseller.onemonth = ifelse(units.month == max(units.month), 1, 0)) %>%
  ungroup %>% mutate(age = as.numeric(date-releaseDate)) -> alldata_more

alldata_more <- alldata_more %>% 
  mutate(bestseller.oneweek = replace(bestseller.oneweek, date >= ymd("2018-02-01"), 0),
         bestseller.onemonth = replace(bestseller.onemonth, date >= ymd("2018-02-01"), 0))

alldata_more %>% filter(bestseller.oneweek == 1) %>%
  select(week, units.week, key) %>% unique %>% glimpse
alldata_more %>% filter(bestseller.onemonth == 1) %>% 
  select(month, units.month, key) %>% unique %>% glimpse

alldata_more %>%
  select(key, pid, size, date, age, units, bestseller.oneday,
         price, discount, diffprice, reldiffprice) -> feature.date0
feature.date <- feature.date0 %>%
  filter(age >= 0) %>%
  mutate(diffprice = replace(diffprice, is.na(diffprice), 0),
         reldiffprice =  replace(reldiffprice, is.na(reldiffprice), 0),
         day = day(date), weekday = weekdays(date),
         is.BF = ifelse(date == ymd("2017-11-24"), 1, 0),
         is.unityday = ifelse(date == ymd("2017-10-03"), 1, 0),
         is.reformday = ifelse(date == ymd("2017-10-31"), 1, 0))

write.csv(feature.date,
          file = paste0("../../../DMC 2018/Data/Features/feature_date_Lyu_", Sys.Date(), ".csv"))

train %>% group_by(pid, size) %>% summarise(daysold = n()) -> key_daysold 
alldata_more %>% group_by(pid, size) %>% 
  summarise_at(vars(bestseller.oneday, bestseller.oneweek, bestseller.onemonth), 
               funs(sum(., na.rm = T))) -> key_bestseller
alldata_more %>% group_by(pid, size) %>%
  summarise_at(vars(units, units.week, units.month), max, na.rm = T) %>%
  rename(record.day = units, record.week = units.week, record.month = units.month) -> key_bestrecord
feature.date0 %>% filter(age < 0) %>% select(pid, size, price, discount) %>%
  rename(price.beforeRD = price, discount.beforeRD = discount) -> beforeReleaseDate

items %>% left_join(check, by = c("pid", "size", "brand", "ctgroup")) %>% 
  left_join(key_daysold, by = c("pid", "size")) %>%
  left_join(key_bestseller, by = c("pid", "size")) %>% 
  left_join(key_bestrecord, by = c("pid", "size")) %>%
  left_join(beforeReleaseDate, by = c("pid", "size")) -> feature.item0
feature.item <- feature.item0 %>% 
  mutate(pho = replace(pho, is.na(pho), 0),
         is.soccer = ifelse(subCategory == 3, 1, 0))

write.csv(feature.item,
          file = paste0("../../../DMC 2018/Data/Features/feature_items_Lyu_", Sys.Date(), ".csv"))

feature.date$date <- ymd(feature.date$date)
feature.item$releaseDate <- ymd(feature.item$releaseDate)
apply(feature.date, 2, function(x) sum(is.na(x))) -> Num.NA.feature.date
apply(feature.item, 2, function(x) sum(is.na(x))) -> Num.NA.feature.items
feature_layout <- 
  data.frame(filename	= c(rep("feature_date_Lyu_2018-04-23.csv", ncol(feature.date)),
                          rep("feature_items_Lyu_2018-04-23.csv", ncol(feature.item))),
             feature_name = c(colnames(feature.date), colnames(feature.item)),
             type	= c(sapply(feature.date, class), sapply(feature.item, class)),
             Num.NA = c(Num.NA.feature.date, Num.NA.feature.items),
             dataset_used = "train,price,items",
             description = "")
write.csv(feature_layout,
          file = paste0("../../../DMC 2018/Data/Features/layout_feature_Lyu_", Sys.Date(), ".csv"))

daysold_stock <- feature.item0 %>% 
  mutate(stock = cut(stock, breaks = c(0:3, 6, 10, 50, Inf)),
         daysold = cut(daysold, breaks = c(0:6, 10, 20, Inf)))
write.csv(daysold_stock %>% select(daysold, stock) %>% table(),
          file = "../../../DMC 2018/subset_based_on_days&stocks/days_stocks_alldata.csv")
