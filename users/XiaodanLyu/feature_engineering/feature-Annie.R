## windows
source("../../../../../Dropbox/DMC 2018/Data/Features/help.r")
## Mac
source("feature_engineering/help.r")
all_Jan <- read.table("data_clean/train_Jan.txt", sep = "|", header = T)
items <- read.csv("../../data/raw_data/items.csv", sep = "|")
any(ymd(all_Jan$date) < ymd(all_Jan$releaseDate))
all_Jan$date %>% ymd %>% range

## color ####
label_color <-  function(old_levels){
  c('beige','blue','brown','yellow','gold',
    'gray','green','khaki','purple','orange',
    'dark_pink','pink','red','black','silver','turquoise','white')
}
## brand ####
label_brand.nation <- function(old_levels){
  c('German','Japan','German','US','German','Denmark','German','US','US','Italy',
    'Japan','US','US','German','UK','German','German','US','German','US')
}

label_brand.product <- function(old_levels){
  c('athletic','footwear','football','footwear','athletic','athletic','athletic','athletic',
    'footwear', 'footwear', 'athletic', 'footwear','athletic','athletic','footwear',
    'football','outdoor', 'underwear','football','footwear')
}

## releaseDate ####
all_Jan$releaseDate <- as.Date(all_Jan$releaseDate)
# NewRelease <- 1-(all_Jan$releaseDate==as.Date('2017-10-01'))*1
# relday <- day(all_Jan$releaseDate)
# relweekday <- as.numeric(factor(weekdays(all_Jan$releaseDate)))
# relmonthweek <- ceiling((day(all_Jan$releaseDate) + first_day_of_month_wday(all_Jan$releaseDate) - 1) / 7)

## rrp ####
# rrp <- all_Jan$rrp
# rrp.cut.c1 <- cut(rrp, breaks = c(0, 12.7, 48.3, 170, Inf))
# rrp.cut.c2 <- cut(rrp, breaks = c(0, 12.6, 44.4, 127, Inf))
# rrp.cut.c3 <- cut(rrp, breaks = c(0, 19.0, 82.5, 222, Inf))
# rrp.cut.q3 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 3)),
#                                   mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)
# rrp.cut.q5 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 5)),
#                                   mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)
# rrp.cut.q10 <- cut(rrp, breaks = c(quantile(rrp, probs = seq(0, 1, length.out = 10)),
#                                    mean(rrp), getmode(rrp)) %>% unique, include.lowest = T)

## append ####
all_Jan_Jumbo <- all_Jan %>%
  mutate(
    ## color
    color = fct_relabel(color, label_color),
    ## brand
    brand.nation = fct_relabel(brand, label_brand.nation),
    brand.product = fct_relabel(brand, label_brand.product),
    ## releaseDate
    NewRelease = 1-(releaseDate==as.Date('2017-10-01'))*1,
    relday = day(all_Jan$releaseDate),
    relweekday = as.numeric(factor(weekdays(all_Jan$releaseDate))),
    relmonthweek = ceiling((day(all_Jan$releaseDate) + first_day_of_month_wday(all_Jan$releaseDate) - 1) / 7),
    ## rrp
    rrp.cut.c1 = cut(rrp, breaks = c(0, 12.7, 48.3, 170, Inf)),
    rrp.cut.c2 = cut(rrp, breaks = c(0, 12.6, 44.4, 127, Inf)),
    rrp.cut.c3 = cut(rrp, breaks = c(0, 19.0, 82.5, 222, Inf)),
    rrp.cut.q3 = cut(rrp, breaks = c(quantile(items$rrp, probs = seq(0, 1, length.out = 3)),
                                     mean(items$rrp), getmode(items$rrp)) %>% unique, include.lowest = T),
    rrp.cut.q5 = cut(rrp, breaks = c(quantile(items$rrp, probs = seq(0, 1, length.out = 5)),
                                     mean(items$rrp), getmode(items$rrp)) %>% unique, include.lowest = T),
    ## pid
    pid.cut = cut(as.numeric(as.character(pid)), breaks = c(10001, 12938, 17662, 20896, 22881), include.lowest = T),
    stock.cut = cut(stock, breaks = c(0, 2.5, 4.5, 14.5, 38.5, 64.5, Inf), include.lowest = T)
    )
all_Jan_Jumbo %>% glimpse

all_Jan_Jumbo_format <- all_Jan_Jumbo %>%
  mutate(date = ymd(date), size = as.character(size)) %>%
  mutate_at(vars(color, brand, mainCategory:subCategory, brand.nation:rrp.cut.q5), as.factor) 
all_Jan_Jumbo_format %>% glimpse
sapply(all_Jan_Jumbo_format, class) %>% data.frame

## codebook ####
train_Jan_Jumbo <- all_Jan_Jumbo_format %>% filter(date < ymd("2018-01-01"))

code_train <- codebook(train_Jan_Jumbo, response = "units")
code_train %>% glimpse

code_train_format <- code_train %>% 
  separate(name.feature, into = c("name.variable", "type.feature"), sep = "_", remove = FALSE)

code_train_format %>% glimpse

## output ####
train_Jan_feature <- feature(train_Jan_Jumbo, code_train_format)
train_Jan_feature %>% glimpse

test_Jan_Jumbo <- all_Jan_Jumbo_format %>% filter(date >= ymd("2018-01-01"))
test_Jan_Jumbo %>% glimpse

test_Jan_feature <- feature(test_Jan_Jumbo, code_train_format)
test_Jan_feature %>% glimpse

write.csv(train_Jan_feature,
          file = paste0("../../../../../Dropbox/DMC 2018/Data/Features/feature_train_Jan_",
                        Sys.Date(), ".csv"))
write.csv(test_Jan_feature,
          file = paste0("../../../../../Dropbox/DMC 2018/Data/Features/feature_test_Jan_",
                        Sys.Date(), ".csv"))
