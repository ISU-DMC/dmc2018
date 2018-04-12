## ---- input
library(sugrrants)
library(tidyverse)
library(lubridate)
library(forcats)
library(corrplot)
setwd("~/Dropbox/dmc2018")
train <- read.csv("data/raw_data/train.csv", sep = "|")
prices <- read.csv("data/raw_data/prices.csv", sep = "|")
items <- read.csv("data/raw_data/items.csv", sep = "|")
## ---- quality
## no missing in training data
apply(train, 2, function(x) sum(is.na(x)))
## some missing in subCategory
apply(items, 2, function(x) sum(is.na(x)))

## all keys appear in every data set
anti_join(train, items, by = c("pid", "size")) %>% dim()
anti_join(items, train, by = c("pid", "size")) %>% dim()
anti_join(items, prices, by = c("pid", "size")) %>% dim()

## ---- dates
## sales data available everyday from 2017-10-01 to 2018-01-31
train$date <- ymd(train$date)
range(train$date)
range(train$date) %>% diff
unique(train$date) %>% length()
## about 85% of the items released before 2017-10-01
items$releaseDate <- ymd(items$releaseDate)
mean(items$releaseDate == ymd("2017-10-01"))

## ---- prices
## gather date and price by product ID and size
prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd()) -> prices_long
prices_long %>% glimpse
## releaseDate is the same for a product across different sizes, 4770 different pid
items %>% group_by(pid) %>%
  summarise(ndate = unique(releaseDate) %>% length) %>%
  select(ndate) %>% table()
## * before releasedate, any non-missing prices? Yes 
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date < releaseDate, !is.na(price)) -> prices_suspicious
prices_suspicious %>% glimpse
## all products released after 2017-10-01 have price info before release date 
anti_join(items %>% filter(releaseDate > ymd("2017-10-01")),
          prices_suspicious, by = "pid") %>% dim
## in each row, date is one-day earlier than releaseDate
prices_suspicious %>% mutate(diff_date = date-releaseDate) %>% select(diff_date) %>% table()
## * after releasedate, any missing prices? No 
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date >= releaseDate) %>% select(price) %>% summary()

## ---- calendar
## remarkably more sales on Black Friday 2017-11-24
train.sales <- train %>% group_by(date) %>% summarise(n = sum(units)) %>% ungroup 
train.sales %>%
  frame_calendar(x = 1, y = 1, date = date) %>%
  ggplot(aes(x = .x, y = .y)) + 
  ggtitle("log-scale daily sold units") + 
  theme(legend.position = "bottom") +
  geom_tile(aes(x = .x+(1/13)/2, y = .y+(1/9)/2, fill = log(n)), colour = "grey50") +
  scale_fill_distiller(name = "", palette = "RdYlBu") -> p2.sale
prettify(p2.sale, label = c("label", "text", "text2"))
## use raw releasedate
## more new products released around Black Friday
items.release <- items %>% group_by(releaseDate) %>% tally() %>% ungroup
## consider release dates other than 2017-10-01
items.release %>% filter(releaseDate != ymd("2017-10-01")) %>%
  frame_calendar(x = 1, y = 1, date = releaseDate, calendar = "monthly") %>%
  ggplot(aes(x = .x, y = .y)) + 
  ggtitle("log-scale daily released products") +
  theme(legend.position = "bottom") +
  geom_tile(aes(x = .x+(1/13)/2, y = .y+(1/9)/2, fill = log(n)), colour = "grey50") +
  scale_fill_distiller(name = "", palette = "RdYlBu") -> p2.release
prettify(p2.release, label = c("label", "text", "text2"))

## ---- brands
## color, brand, rrp, category(main/sub), releaseDate same for product of different sizes
items %>% select(-size, -stock) %>% unique %>% glimpse
items$pid %>% unique() %>% glimpse
## 25 brands
items %>% group_by(brand) %>% 
  summarise(npid = length(unique(pid)), 
            nitem = n(),
            nstock = sum(stock),
            medrrp = median(rrp[!duplicated(pid)]),
            nmaincat = length(unique(mainCategory)),
            ncat = length(unique(category)),
            nsubcat = length(unique(subCategory)),
            nnewrelease = length(unique(pid[releaseDate>ymd("2017-10-01")]))
  ) %>% 
  arrange(desc(npid)) -> brands
brands <- brands %>% mutate(brand = fct_reorder(brand, nstock, mean)) %>%
  mutate(order = as.numeric(brand)) %>% mutate(brand = paste(order, brand, sep = "-"))
str(brands)
## standardize variables before computing correlation
brandst <- data.frame(t(scale(brands[,-1])))
colnames(brandst) <- unlist(brands$brand)
str(brandst)
brands.cor <- cor(brandst)
corrplot(brands.cor, type = "upper", order = "hclust")

## rename brand by adding rank of # of stock
items %>% mutate(brand = fct_reorder(brand, stock, sum)) %>%
  mutate(rank = as.numeric(brand)) %>%
  mutate(brand = paste(rank, brand, sep = "-")) -> items.brand
## stock by brand, checking new names
## first 12 brands with total stock less than 15
ggplot(data = items.brand %>% mutate(brand = fct_reorder(brand, stock, sum))) +
  ggtitle("stock on Feb 1st by brand") +
  geom_boxplot(aes(x=brand, y=stock, color = brand)) + 
  scale_y_log10() + theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45)) 
## rrp by brand 
## some brands are low-stock due to high price
ggplot(data = items.brand %>% mutate(brand = fct_reorder(brand, rrp, median))) +
  ggtitle("recommended retail price by brand") +
  geom_boxplot(aes(x=brand, y=rrp, color = brand)) + 
  theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45)) 
## sales by brand during Oct-Jan
## stock strongly positively correlated with sales
ggplot(data = train %>% left_join(items.brand, by = c("pid", "size")) %>%
         mutate(brand = fct_reorder(brand, units, sum))) + 
  ggtitle("sales during Oct-Jan by brand") +
  geom_boxplot(aes(x = brand, y = units, color = brand), alpha = 0.6) +
  scale_y_log10() + theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45))

## ---- prices_again
## same product has difference daily prices for different sizes
prices_long %>% filter(pid == 16427, date == ymd("2017-10-01"))

## ---- data_join
## any sale before releaseDate? No
left_join(prices_long, train, by = c("pid", "size", "date")) %>% 
  left_join(items, by = c("pid", "size")) %>%
  filter(date<releaseDate, !is.na(units)) %>% dim
## joining three tables: items, prices, train
## sale unit is zero if not appearing in the train data for a particular day
## discount says how many percent off the rrp
## diffprice says price differences from previous day
## reldiffprice says how many percent down/up from previous day
left_join(prices_long, train, by = c("pid", "size", "date")) %>% 
  left_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate) %>% 
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100) %>% 
  group_by(pid, size) %>% 
  mutate(diffprice = price - lag(price)) %>%
  mutate(reldiffprice = diffprice/lag(price)*100) -> alldata
## all prices lower than rrp? Yes
summary(alldata$discount)
## any rising prices? Yes
summary(alldata$diffprice)
summary(alldata$reldiffprice)

## ---- rising_prices
## only new released items may have a rising price 
alldata %>% group_by(pid, size) %>% 
  summarise(yn.priceincr = any(reldiffprice > 0, na.rm = T),
            yn.newrelease = all(releaseDate > ymd("2017-10-01"))) %>%
  ungroup -> check
check %>% select(yn.priceincr, yn.newrelease) %>% table

## ---- best_seller
## bestseller among (pid, size)
alldata %>% group_by(pid, size) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
## being sold everyday, prices never change, 40% off
alldata %>% filter(pid == 12985, size == "L") %>% glimpse
## bestseller among which there is an increase in price
alldata %>% group_by(pid, size) %>% 
  filter(any(reldiffprice > 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
alldata %>% filter(pid == 20828, size == "L") %>% glimpse
## bestseller among which there is no discount all the time
alldata %>% group_by(pid, size) %>% filter(all(discount == 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
alldata %>% filter(pid == 22144, size == "L ( 42-46 )") %>% glimpse

itemsofinterest <- data.frame(
  label = c("bestseller", "bestseller - rising price", "bestseller - no discount"),
  pid = c(12985, 20828, 22144), size = c("L", "L", "L ( 42-46 )"))
alldata %>% inner_join(itemsofinterest, by = c("pid", "size")) -> plotdata
## time series plot
plotdata %>% group_by(pid, size) %>% mutate(avg.discount = mean(discount)) %>% ungroup %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = units, color = "daily sale (unit)")) +
  geom_line(aes(y = reldiffprice, color = "daily price change (%)")) +
  geom_label(aes(x = max(date), y = max(units, na.rm = T),
                 label = sprintf("brand: %s, rrp: %.2f, %.0f%% off", brand, rrp, avg.discount)),
             hjust = 1, vjust = 1, size = 5, fontface = "bold") +
  scale_x_date(limits = c(ymd("2017-10-01"), ymd("2018-02-28"))) + 
  labs(x = "date", y = "", color = "") +
  theme_bw(base_size = 15) + theme(legend.position = "bottom") +
  facet_wrap(~label, nrow = 3)

