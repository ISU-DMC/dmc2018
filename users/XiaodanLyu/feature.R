## 2018-04-23
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


## 2018-4-24 
# source("../../../DMC 2018/Data/Features/feature-feilittle.R")
items <- read.table("../../data/raw_data/items.csv", sep = "|", header = T)
train_Jan <- read.table("data_clean/train_Jan.txt", sep = "|", header = T)
y <- train_Jan$units

## mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####
# freq_cate <- function(x_cate) {
#   
#   table(x_cate) %>% prop.table %>% as.data.frame %>% mutate(Freq = log(Freq*100)) %>%
#   
# }

##### rrp #####
rrp <- train_Jan$rrp
table(rrp) %>% sort(decreasing = T)
getmode(rrp)
hist(rrp)
abline(v = 114.23, col = "red", lty = 2)
items %>% group_by(ctgroup) %>% 
  do(data.frame(t(quantile(.$rrp)), quantile(.$rrp, 0.9), mean(.$rrp)))
rrp_cut.c1 <- cut(rrp, breaks = c(0, 12.7, 48.3, 170, Inf))
rrp_cut.c2 <- cut(rrp, breaks = c(0, 12.6, 44.4, 127, Inf))
rrp_cut.c3 <- cut(rrp, breaks = c(0, 19.0, 82.5, 222, Inf))
rrp_cut.q3 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 3)),
                              mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)
rrp_cut.q5 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 5)),
                               mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)
rrp_cut.q10 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 10)),
                               mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)

rrp_cut_freq <- freq_cate(rrp_cut.c1)
rrp_cut_LLR <- LLR_cate(rrp_cut.c)

rrp_cut3_LLR <- LLR_cate(rrp_cut.q3)
rrp_cut5_LLR <- LLR_cate(rrp_cut.q5)
rrp_cut10_LLR <- LLR_cate(rrp_cut.q10)

