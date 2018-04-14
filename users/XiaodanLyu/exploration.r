## ---- input
library(sugrrants)
library(tidyverse)
library(lubridate)
library(forcats)
library(corrplot)
# setwd("~/Dropbox/dmc2018")
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
prices <- read.csv("../../data/raw_data/prices.csv", sep = "|", stringsAsFactors = F)
items <- read.csv("../../data/raw_data/items.csv", sep = "|", stringsAsFactors = F)
items <- items %>% mutate_at(vars(mainCategory:subCategory), funs(factor))
## Q: set of keys the same for the three data sets? Yes
key.items <- paste(items$pid, items$size, sep = " & ") 
key.train <- paste(train$pid, train$size, sep = " & ") %>% unique
key.prices <- paste(prices$pid, prices$size, sep = " & ") %>% unique
identical(sort(key.items), sort(key.train))
identical(sort(key.items), sort(key.prices))

## ---- items
## Q: any missing in items? Yes, subCategory
apply(items, 2, function(x) sum(is.na(x)))
## 4770 different pid
items$pid %>% unique() %>% glimpse
## Q: brand, rrp, category(main/sub), releaseDate, even color is the same for pid? Yes
items %>% select(-size, -stock) %>% unique %>% glimpse
## Q: each pid only has one color in different size? Yes
items %>% group_by(pid) %>% 
  summarise(ncol = length(unique(color))) %>%
  select(ncol) %>% table()
## Q:  How many of the items released before 2017-10-01? about 85%
items$releaseDate <- ymd(items$releaseDate)
mean(items$releaseDate == ymd("2017-10-01"))

## ---- train
## Q: any missing in training? No
apply(train, 2, function(x) sum(is.na(x)))
## Q: sales data available everyday? Yes, from 2017-10-01 to 2018-01-31
train$date <- ymd(train$date)
range(train$date)
all(seq(ymd("2017-10-01"), ymd('2018-01-31'), by = 1) %in% train$date)
## Q: any sale before releaseDate? No
train %>% left_join(items, by = c("pid", "size")) %>% 
  mutate(yn.salebeforerelease = date<releaseDate) %>%
  select(yn.salebeforerelease) %>% table

## ---- prices
## gather date and price by product ID and size
prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd()) -> prices_long
prices_long %>% glimpse
## Q: same product may have difference prices in sizes? Yes
prices_long %>% filter(pid == 16427, date == ymd("2017-10-01"))
## Q: after releasedate, any missing prices? No 
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date >= releaseDate) %>% select(price) %>% is.na %>% any
## Q: before releasedate, any non-missing prices? Yes 
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date < releaseDate) %>% select(price) %>% is.na %>% any
## Q: any charateristic for a price before release date? 
## released after 2017-10-01 , one-day earlier pre-sale adversing price 
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(!is.na(price)) %>% group_by(pid, size) %>%
  summarise(yn.pricebeforerelease = any(date < releaseDate),
            yn.newrelease = any(releaseDate>ymd("2017-10-01"))) %>%
  ungroup %>% select(yn.pricebeforerelease, yn.newrelease) %>% table
## one-day earlier
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date < releaseDate, !is.na(price)) %>% 
  mutate(diff_date = date-releaseDate) %>% 
  select(diff_date) %>% table()
## Q: presale price the same as the price on releaseDate?
## No, mostly lower than the actual sale price
items %>% left_join(prices_long, by = c("pid", "size")) %>%
  filter(date<=releaseDate, releaseDate > ymd("2017-10-01"), !is.na(price)) %>%
  group_by(pid, size) %>% summarise(diff = diff(price) %>% round(2)) %>% ungroup %>%
  select(diff) %>% table

## ---- calendar
## remarkably more sales on Black Friday 2017-11-24
daily.sales <- train %>% group_by(date) %>% summarise(n = sum(units)) %>% ungroup 
daily.sales %>%
  frame_calendar(x = 1, y = 1, date = date) %>%
  ggplot(aes(x = .x, y = .y)) + 
  ggtitle("log-scale daily sold units") + 
  theme(legend.position = "bottom") +
  geom_tile(aes(x = .x+(1/13)/2, y = .y+(1/9)/2, fill = log(n)), colour = "grey50") +
  scale_fill_distiller(name = "", palette = "RdYlBu") -> p2.sale
prettify(p2.sale, label = c("label", "text", "text2"))
## use raw releasedate
## more new products released around Black Friday
daily.release <- items %>% group_by(releaseDate) %>% tally() %>% ungroup
## consider release dates other than 2017-10-01
daily.release %>% filter(releaseDate != ymd("2017-10-01")) %>%
  frame_calendar(x = 1, y = 1, date = releaseDate, calendar = "monthly") %>%
  ggplot(aes(x = .x, y = .y)) + 
  ggtitle("log-scale daily released products") +
  theme(legend.position = "bottom") +
  geom_tile(aes(x = .x+(1/13)/2, y = .y+(1/9)/2, fill = log(n)), colour = "grey50") +
  scale_fill_distiller(name = "", palette = "RdYlBu") -> p2.release
prettify(p2.release, label = c("label", "text", "text2"))

## ---- data_join
## joining three tables: items, prices, train
## sale unit is zero if not appearing in the train data for a particular day
## discount: how many percent off the rrp
## diffprice: price differences from the day before
## reldiffprice: how many percent off price the day before
left_join(prices_long, train, by = c("pid", "size", "date")) %>% 
  left_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate-1) %>% ## only keep price info since one day before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         discount = (rrp-price)/rrp*100) %>% 
  group_by(pid, size) %>% 
  mutate(diffprice = price - lag(price)) %>%
  mutate(reldiffprice = diffprice/lag(price)*100) -> alldata
## Q: all prices lower than rrp? Yes
summary(alldata$discount)
## Q: any rising prices? Yes
summary(alldata$diffprice)

## ---- brands
## 25 brands
items %>% group_by(brand) %>% 
  summarise(npid = length(unique(pid)), 
            # nitem = n(),
            nstock = sum(stock),
            nsize = length(unique(size)),
            ncolor = length(unique(color)),
            nmaincat = length(unique(mainCategory)),
            ncat = length(unique(category)),
            nsubcat = length(unique(subCategory)),
            nnewrelease = length(unique(pid[releaseDate>ymd("2017-10-01")])),
            medrrp = median(rrp[!duplicated(pid)])
  ) -> brands
## label brand by rank of # of stock on Feb 1st
brands <- brands %>% mutate(brand = fct_reorder(brand, nstock, mean),
                            order = as.numeric(brand),
                            no.brand = paste(order, brand, sep = "-")) %>%
  select(brand, no.brand, everything())
brands %>% arrange(desc(npid)) %>% glimpse

## stock by brand, checking new names
## first 12 brands with total stock less than 15
items %>% left_join(brands %>% select(brand, no.brand), by = "brand") -> items.brand
ggplot(data = items.brand %>% mutate(no.brand = fct_reorder(no.brand, stock, sum))) +
  ggtitle("stock on Feb 1st by brand") +
  geom_boxplot(aes(x=no.brand, y=stock, color = no.brand)) + 
  scale_y_log10() + theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45)) 
## rrp by brand 
## Q: any brands are low-stock due to high rrp price? Yes
ggplot(data = items.brand %>% mutate(no.brand = fct_reorder(no.brand, rrp, median))) +
  ggtitle("recommended retail price by brand") +
  geom_boxplot(aes(x=no.brand, y=rrp, color = no.brand)) + 
  theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45)) 
## sales by brand during Oct-Jan
## stock strongly positively correlated with sales
ggplot(data = train %>% left_join(items.brand, by = c("pid", "size")) %>%
         mutate(no.brand = fct_reorder(no.brand, units, sum))) + 
  ggtitle("sales during Oct-Jan by brand") +
  geom_boxplot(aes(x = no.brand, y = units, color = no.brand), alpha = 0.6) +
  scale_y_log10() + theme_bw(base_size = 15) + guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 45))
## category by brand
items.brand %>% group_by(no.brand, mainCategory) %>% tally %>% ungroup %>%
  mutate(no.brand = fct_reorder(no.brand, n, sum),
         mainCategory = fct_relevel(factor(mainCategory), 15, 9, 1)) %>%
  ggplot(aes(x = no.brand, y = n, fill = mainCategory)) +
  geom_bar(stat = "identity") + scale_y_sqrt() +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45))
items.brand %>% group_by(no.brand, category) %>% tally %>% ungroup %>%
  mutate(no.brand = fct_reorder(no.brand, n, sum)) %>%
  ggplot(aes(x = no.brand, y = n, fill = category)) +
  geom_bar(stat = "identity") + scale_y_sqrt() +
  theme_bw(base_size = 15) +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45))
items.brand %>% group_by(no.brand, subCategory) %>% tally %>% ungroup %>%
  mutate(no.brand = fct_reorder(no.brand, n, sum)) %>%
  ggplot(aes(x = no.brand, y = n, fill = subCategory)) +
  geom_bar(stat = "identity") + scale_y_sqrt() +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45))

## ---- rising_prices
alldata %>% group_by(pid, size, brand) %>% 
  summarise(yn.priceincr = any(reldiffprice > 0, na.rm = T),
            yn.newrelease = any(releaseDate > ymd("2017-10-01")),
            yn.pricechange = any(reldiffprice != 0, na.rm = T)) %>%
  ungroup -> check
## Q: changing prices only happen to new released products? Yes
## Q: all new released products have changing prices? No
check %>% select(yn.pricechange, yn.newrelease) %>% table
## Q: all changing prices are at least increased once? No
check %>% select(yn.pricechange, yn.priceincr) %>% table
## distribution by brand
check %>% group_by(brand) %>% 
  summarise(nanyincr = sum(yn.priceincr), 
            nalldecr = sum(yn.pricechange) - sum(yn.priceincr),
            nsame = sum(yn.newrelease) - sum(yn.pricechange)
  ) %>% ungroup() %>%
  left_join(brands, by = "brand") -> brands.item.price
brands.item.price %>% select(nanyincr:no.brand) %>%
  mutate(no.brand = fct_reorder(no.brand, nanyincr, identity)) %>%
  gather(group, value, -no.brand) %>% 
  # filter(value>0) %>%
  mutate(group = fct_relevel(group, "nalldecr", "nanyincr", after = Inf)) %>%
  ggplot(aes(x = no.brand)) + scale_y_sqrt() +
  geom_bar(stat = "identity", aes(y = value, fill = group)) +
  labs(title = "# of new released products by brand", fill = "") + 
  theme_bw(base_size = 15) + theme(axis.text.x = element_text(angle = 45)) 

## ---- nike&adidas
## competing brands: nike and adidas
items %>% filter(releaseDate > ymd("2017-10-01"), brand %in% c("Nike", "adidas")) %>%
  group_by(releaseDate, brand) %>% tally() %>% ungroup %>% 
  left_join(brands %>% select(brand, no.brand), by = "brand") %>%
  mutate(no.brand = fct_reorder(no.brand, n, sum, .desc = TRUE),
         month = paste(year(releaseDate), month(releaseDate), sep = "-"),
         week = ceiling(day(releaseDate)/7),
         wday = wday(releaseDate)) %>% 
  ggplot(aes(x = releaseDate, y = n, fill= no.brand)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # scale_x_discrete(labels = )
  facet_wrap(~month, scales = "free_x", nrow = 4) +
  labs(y = "# of released products", fill = "") +
  theme_bw(base_size = 15)

## Q: similarities among brands? brand type
train %>% left_join(items %>% select(pid, size, brand), by = c("pid", "size")) %>%
  group_by(brand) %>% summarise(nsale = sum(units)) %>% ungroup %>%
  left_join(brands.item.price, by = "brand") -> brands.all
brands.all %>% arrange(desc(nstock)) %>% glimpse
brands.cols <- brands.all %>% select(-brand, -no.brand, -order)
## standardize variables before computing correlation
brandst <- data.frame(t(scale(brands.cols)))
## not standardized columns
# brandst <- data.frame(t(brands.cols))
colnames(brandst) <- unlist(brands$no.brand)
brands.cor <- cor(brandst)
corrplot(brands.cor, type = "upper", order = "FPC")
corrplot(brands.cor, type = "full", order = "hclust", addrect = 13)

## ---- best_seller
## Q: which is the bestseller among all (pid, size)?
alldata %>% group_by(pid, size) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
## being sold everyday, prices never change, 40% off
alldata %>% filter(pid == 12985, size == "L") %>% arrange(desc(units)) %>% head(10)
## Q: which is the bestseller among which there is an increase in price?
alldata %>% group_by(pid, size) %>% 
  filter(any(reldiffprice > 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
alldata %>% filter(pid == 20828, size == "L") %>% arrange(desc(abs(reldiffprice))) %>% head(10)
## Q: which is the bestseller among which there is no discount all the time?
alldata %>% group_by(pid, size) %>% filter(all(discount == 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
alldata %>% filter(pid == 22144, size == "L ( 42-46 )") %>% arrange(desc(units)) %>% head(10)

itemsofinterest <- data.frame(
  label = c("bestseller", "bestseller - rising price", "bestseller - no discount"),
  pid = c(12985, 20828, 22144), size = c("L", "L", "L ( 42-46 )"))
alldata %>% inner_join(itemsofinterest, by = c("pid", "size")) -> plotdata
## time series plot
## holiday reference: www.timeanddate.com/holidays/germany/2017
plotdata %>% group_by(pid, size) %>% mutate(avg.discount = mean(discount)) %>% ungroup %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = units, color = "daily sale (unit)")) +
  geom_line(aes(y = reldiffprice, color = "daily price change (%)")) +
  geom_label(aes(x = max(date), y = max(units, na.rm = T),
                 label = sprintf("brand: %s, rrp: %.2f, %.0f%% off", brand, rrp, avg.discount)),
             hjust = 1, vjust = 1, size = 5, fontface = "bold") +
  geom_vline(aes(color = "NH - German Unity", xintercept = ymd("2017-10-03")), show.legend = T) +
  geom_vline(aes(color = "NH - Reformation Day", xintercept = ymd("2017-10-31")), show.legend = T) +
  geom_vline(aes(color = "NH - Black Friday", xintercept = ymd("2017-11-24")), show.legend = T) +
  scale_x_date(limits = c(ymd("2017-10-01"), ymd("2018-02-28"))) + 
  labs(x = "date", y = "", color = "") +
  theme_bw(base_size = 15) + theme(legend.position = "bottom") +
  facet_wrap(~label, nrow = 3)

