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
train$date <- ymd(train$date)
items$releaseDate <- ymd(items$releaseDate)
items <- items %>% 
  mutate(ctgroup = ifelse(subCategory %in% c(11, 12, 13, 27, 43), "accessories",
                          ifelse(category %in% c(2, 37, 18, 36), "shoes", "clothes")),
         category = paste(mainCategory, category, sep = "-"),
         subCategory = paste(category, subCategory, sep = "-")) %>%
  mutate_at(vars(mainCategory:subCategory), funs(factor))
## Q: set of keys the same for the three data sets? Yes
key.items <- paste(items$pid, items$size, sep = " & ") 
key.train <- paste(train$pid, train$size, sep = " & ") %>% unique
key.prices <- paste(prices$pid, prices$size, sep = " & ") %>% unique
identical(sort(key.items), sort(key.train))
identical(sort(key.items), sort(key.prices))

## ---- ggplot
theme_mine <- function(){
  theme_bw(base_size = 15)  +
    theme(axis.text.x = element_text(angle = 45))
}

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
## Q: How many of the items released before 2017-10-01? about 85%
mean(items$releaseDate == ymd("2017-10-01"))
## Q: category decode
items %>% group_by(subCategory) %>%
  summarise(sizes = paste(unique(size), collapse = ","), 
            rrps = paste(unique(rrp), collapse = ","), 
            brands = paste(unique(brand), collapse = ",")) -> items.subcat
items %>%
  ggplot(aes(x = subCategory, y = rrp, fill = category), color = FALSE) +
  scale_fill_brewer(palette = "Paired") + scale_y_log10() +
  geom_boxplot(outlier.size = .5) + guides(fill = FALSE) +
  geom_hline(yintercept = 100, color = "red", linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2) +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) + 
  theme_mine()

## ---- train
## Q: any missing in training? No
apply(train, 2, function(x) sum(is.na(x)))
## Q: sales data available everyday? Yes, from 2017-10-01 to 2018-01-31
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
## Q: any charateristic for price before release date? 
## released after 2017-10-01 , one-day earlier pre-sale advertising price 
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
  group_by(pid, size) %>% arrange(date) %>% 
  summarise(diff = diff(price) %>% round(2)) %>% ungroup %>%
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
## joining the three tables: items, prices, train
## sale unit is zero if not appearing in the train data for a particular day
## discount: how many percent off the rrp
## diffprice: price differences from the day before
## reldiffprice: how many percent off price the day before
prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd()) -> prices_long
left_join(prices_long, train, by = c("pid", "size", "date")) %>% 
  left_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate-1) %>% ## only keep price info since one day before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0),
         key = paste(pid, size, sep = " - "),
         discount = (rrp-price)/rrp*100) %>% 
  group_by(pid, size) %>% 
  mutate(diffprice = price - lag(price),
         reldiffprice = diffprice/lag(price)*100) %>%
  ungroup -> alldata
alldata %>% glimpse
## Q: all prices lower than rrp? Yes
summary(alldata$discount)
## Q: any rising prices? Yes
summary(alldata$diffprice)

## ---- brands
## 25 brands
items %>% group_by(brand) %>% 
  summarise(npid = length(unique(pid)), 
            nitem = n(),
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

items %>% left_join(brands %>% select(brand, no.brand), by = "brand") -> items.brand
## category by brand
items.brand %>% group_by(no.brand, category, ctgroup) %>% tally %>% ungroup %>%
  mutate(no.brand = fct_reorder(no.brand, n, sum)) %>%
  ggplot(aes(x = no.brand, y = n, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = 100, linetype = 2, col = "red") +
  scale_fill_brewer(palette = "Paired") + scale_y_log10() +
  labs(y = "# of items") +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) +
  theme_mine()
## stock by brand
## first 12 brands with total stock less than 15
## Reebok only has one piece of clothes in stock, others are all shoes, sold nine times
## Both Selles and Kempa have only one stock, a piece of expensive clothes, sold once
## Onitsuka has only one stock, a pair of shoes, sold once  
items.brand %>% mutate(no.brand = fct_reorder(no.brand, stock, sum)) %>%
  group_by(no.brand, category, ctgroup) %>% mutate(stock = sum(stock)) %>% ungroup %>%
  ggplot(aes(x=no.brand, y=stock, fill = category)) +
  ylab("stock on Feb 1st by brand") +
  scale_fill_brewer(palette = "Paired") + scale_y_log10() + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = 1000, linetype = 2, col = "red") +
  facet_wrap(~ctgroup, nrow = 3, scales = "free_x") +
  theme_mine() 

alldata %>% filter(brand %in% c("Sells", "Kempa", "Onitsuka")|pid == 12742, units > 0) %>% glimpse

## rrp by brand 
## Q: any brands are low-stock due to high rrp price? Yes
items.brand %>%
  mutate(no.brand = fct_reorder(no.brand, rrp, median)) %>%
  group_by(no.brand, category, ctgroup) %>%
  summarise(rrp = median(rrp)) %>% ungroup %>%
  ggplot(aes(x=no.brand, y=rrp, fill = category)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("median recommended retail price") +
  geom_hline(yintercept = 25, linetype = 2) +
  geom_hline(yintercept = 100, linetype = 2, col = "red") +
  facet_wrap(~ctgroup, nrow = 3, scales = "free_x") +
  scale_fill_brewer(palette = "Paired") +
  guides(color = FALSE) + theme_mine()
## sale volumes by brand during Oct-Jan
## stock strongly positively correlated with sales
alldata %>% group_by(brand, category, ctgroup) %>% 
  summarise(n = sum(units, na.rm = T)) %>% ungroup %>%
  mutate(brand = fct_reorder(brand, n, sum)) %>%
  ggplot(aes(x = brand, y = n, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = 1000, linetype = 2, col = "red") +
  scale_fill_brewer(palette = "Paired") + scale_y_log10() +
  labs(y = "sale volumes during Oct-Jan by brand") +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) +
  theme_mine()

## ---- rising_prices
alldata %>% group_by(pid, size, brand, ctgroup) %>% 
  summarise(yn.priceincr = any(reldiffprice > 0, na.rm = T),
            yn.newrelease = any(releaseDate > ymd("2017-10-01")),
            yn.pricechange = any(reldiffprice != 0, na.rm = T),
            n.pricechange = sum(reldiffprice != 0, na.rm  = T),
            n.priceincr = sum(reldiffprice > 0, na.rm = T),
            n.priceconst  = sum(reldiffprice == 0, na.rm = T),
            nsale = sum(units, na.rm = T),
            med.sale = median(units, na.rm = T),
            mean.sale = mean(units, na.rm = T),
            n.discount = sum(discount > 0, na.rm = T),
            avg.discount = mean(discount),
            med.discount = median(discount),
            avg.price = mean(price),
            med.price = mean(price),
            pho = cor(units, discount, use = "complete.obs")) %>%
  ungroup -> check
## Q: changing prices only happen to new released products? Yes
## Q: all new released products have changing prices? No
check %>% select(yn.pricechange, yn.newrelease) %>% table
## Q: all changing prices are at least increased once? No
check %>% select(yn.pricechange, yn.priceincr) %>% table
## distribution by brand
check %>% group_by(brand, ctgroup) %>% 
  summarise(nanyincr = sum(yn.priceincr), 
            nalldecr = sum(yn.pricechange) - sum(yn.priceincr),
            nsame = sum(yn.newrelease) - sum(yn.pricechange)
  ) %>% ungroup() %>%
  left_join(brands %>% select(brand, no.brand), by = "brand") -> brands.check
brands.check %>% select(ctgroup, nanyincr:no.brand) %>%
  mutate(no.brand = fct_reorder(no.brand, nanyincr, sum)) %>%
  gather(group, value, -no.brand, -ctgroup) %>% 
  mutate(group = fct_relevel(group, "nalldecr", "nanyincr", after = Inf)) %>%
  ggplot(aes(x = no.brand, y = value, fill = group)) + scale_y_sqrt() +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "# of new released products by brand", fill = "") + 
  theme_mine()

## ---- brand_correlation
## Q: similarities among brands? brand type
train %>% left_join(items %>% select(pid, size, brand), by = c("pid", "size")) %>%
  group_by(brand) %>% summarise(nsale = sum(units)) %>% ungroup %>%
  left_join(brands.check %>% group_by(brand) %>%
              summarise_at(vars(nanyincr:nsame), funs(sum)),
            by = "brand") %>%
  left_join(brands, by = "brand")-> brands.all
brands.all %>% arrange(desc(nstock)) %>% glimpse
brands.cols <- brands.all %>% select(-brand, -no.brand, -order)
## standardize variables before computing correlation
brandst <- data.frame(t(scale(brands.cols)))
## not standardized columns
# brandst <- data.frame(t(brands.cols))
colnames(brandst) <- unlist(brands$no.brand)
brands.cor <- cor(brandst)
# corrplot(brands.cor, type = "upper", order = "FPC")
corrplot(brands.cor, type = "full", order = "hclust", addrect = 13)

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

## ---- best_seller
## Q: which is the bestseller among all (pid, size)?
alldata %>% group_by(pid, size) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
## Q: which is the bestseller among which there is no discount all the time?
alldata %>% group_by(pid, size) %>% filter(all(discount == 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)
## Q: which is the bestseller among which there is an increase in price?
alldata %>% group_by(pid, size) %>% 
  filter(any(reldiffprice > 0)) %>% 
  summarise(nsale = sum(units, na.rm = T)) %>% 
  arrange(desc(nsale)) %>% head(1)

itemsofinterest <- data.frame(
  label = c("bestseller (12985, L)", "bestseller - rising price (20828, L)", 
            "bestseller - no discount (22144, L)"),
  pid = c(12985, 20828, 22144), size = c("L", "L", "L ( 42-46 )"))
alldata %>% inner_join(itemsofinterest, by = c("pid", "size")) -> plotdata
plotdata %>% filter(!duplicated(cbind(pid, size))) %>% glimpse
## time series plot
## holiday reference: www.timeanddate.com/holidays/germany/2017
plotdata %>% group_by(pid, size) %>% mutate(avg.discount = mean(discount)) %>% ungroup %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = units, color = "daily sale (unit)")) +
  geom_line(aes(y = reldiffprice, color = "daily price change (%)")) +
  geom_label(aes(x = max(date), y = max(units, na.rm = T),
                 label = sprintf("subCategory: %s, %s, %.0f%% off", subCategory, ctgroup, avg.discount)),
             hjust = 1, vjust = 1, size = 5, fontface = "bold") +
  geom_vline(aes(color = "NH - German Unity", xintercept = ymd("2017-10-03")), show.legend = T) +
  geom_vline(aes(color = "NH - Reformation Day", xintercept = ymd("2017-10-31")), show.legend = T) +
  geom_vline(aes(color = "NH - Black Friday", xintercept = ymd("2017-11-24")), show.legend = T) +
  scale_x_date(limits = c(ymd("2017-10-01"), ymd("2018-02-28"))) + 
  labs(x = "date", y = "", color = "") +
  theme_bw(base_size = 15) + theme(legend.position = "bottom") +
  facet_wrap(~label, nrow = 3)

## ---- stock
## 60% of the products only had one stock on Feb 1st
mean(items$stock==1)
items %>%
  ggplot(aes(x = subCategory, y = stock, fill = category)) + 
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 10, linetype = 2) +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ctgroup, nrow = 3, scales = "free_x") +
  scale_y_log10() + theme_mine()

items %>% mutate(yn.onestock = (stock==1)) %>%
  left_join(check, by = c("pid", "size", "ctgroup", "brand")) %>% 
  select(yn.onestock, yn.pricechange) %>% table()

## ---- sales
alldata %>% group_by(pid, size, subCategory) %>%
  mutate(nsale = sum(units, na.rm = T)) %>% ungroup %>%
  ggplot(aes(x = subCategory, y = nsale, fill = category)) + 
  ylab("sale volumes during Oct-Jan by category") +
  geom_boxplot(outlier.size = .5) + guides(color = FALSE) +
  facet_wrap(~ctgroup, nrow = 3, scales = "free_x") +
  geom_hline(yintercept = 10, linetype = 2) + 
  scale_fill_brewer(palette = "Paired") + scale_y_log10() +
  theme_mine()

## ---- discount
## products with changing prices
## 1-7 tops and bottoms 
## 9-18 and 9-36 are sports shoes
alldata %>% inner_join(check, by = c("pid", "size", "brand", "ctgroup")) %>%
  # mutate(category = paste(mainCategory, category, sep = "-")) %>%
  filter(yn.pricechange) %>% group_by(date, category, ctgroup) %>%
  summarise(discount = median(discount)) %>% ungroup %>%
  ggplot(aes(x = date, y = discount, color = factor(category))) + 
  ggtitle("median discount by category and date") + 
  geom_line(size = rel(1)) + 
  geom_hline(yintercept = 25, linetype = 2) +
  facet_grid(ctgroup~.) +
  scale_y_continuous(limits = c(0, 75)) +
  scale_color_brewer(palette = "Paired") + theme_bw(base_size = 15) +
  geom_vline(xintercept = ymd("2017-10-31"), linetype = 2, size = rel(0.8)) +
  geom_vline(xintercept = ymd("2017-11-24"), linetype = 2, size = rel(0.8)) +
  geom_vline(xintercept = ymd("2018-02-14"), linetype = 2, size = rel(0.8))

## correlation between sale-volume and discount
tb <- items %>% left_join(check, by = c("pid", "size", "brand", "ctgroup"))
tb %>% arrange(desc(abs(pho))) %>% glimpse
## pho only available for items with changing prices before Feb 1st
tb %>% filter(!is.na(pho)) %>% 
  ggplot(aes(x = subCategory, y = pho)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot(aes(fill = category), outlier.size = .5) +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ctgroup, nrow = 3, scales = "free_x") +
  labs(title = "correlation between sale-volume and discount by item") +
  theme_mine()

## old products, constant prices
## 1-2-27, nitem = 9, no size, no-discount, cheap, shoelaces?
## 9-36-38, nitem = 4, npid = 2, Nike, women-shoes?
alldata %>% inner_join(check, by = c("pid", "size", "brand", "ctgroup")) %>%
  filter(!yn.newrelease, !duplicated(cbind(pid, size))) %>% 
  ggplot(aes(x = subCategory, y = discount, fill = category), color = FALSE) +
  scale_fill_brewer(palette = "Paired") +
  geom_boxplot(outlier.size = 0.5) + guides(fill = FALSE) +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = 25, color = "red", linetype = 2) +
  theme_mine()

## new released products, constant prices
## 1-7-43 all released after 2017-10-01, nitem = 3 constant prices
alldata %>% inner_join(check, by = c("pid", "size", "brand", "ctgroup")) %>%
  filter(yn.newrelease, !yn.pricechange, !duplicated(cbind(pid, size))) %>% 
  ggplot(aes(x = subCategory, y = discount, fill = category), color = FALSE) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_boxplot(outlier.size = 0.5) + guides(fill = FALSE) +
  geom_hline(yintercept = 25, color = "red", linetype = 2) +
  facet_wrap(~ctgroup, scales = "free_x", nrow = 3) +
  theme_mine()



