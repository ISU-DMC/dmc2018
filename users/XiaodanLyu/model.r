## ---- input
library(lubridate)
library(tidyverse)
test_Jan <- read.table("data_clean/test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)

## ---- baseline
## error for random guess "2018-01-16": 244.524
pred.guess <- ymd("2018-01-16")
err.guess <- (test_Jan$soldOutDate - pred.guess) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.guess: %.3f", err.guess)

## ---- MLR
## MLR modeling daily sale
train_Jan <- read.table("data_clean/train_Jan.txt", sep = "|", header = T)
## format date
train_Jan <- train_Jan %>% mutate(date = ymd(date), releaseDate = ymd(releaseDate))

## only use weekday and day info from the date variable
train_Jan_format <- train_Jan %>%
  mutate(day = day(date),
         weekday = wday(date),
         subCategory = replace(subCategory, is.na(subCategory), 0)) %>%
  filter(date >= releaseDate) %>% ## drop obs one-day before releasedate
  mutate_at(vars(mainCategory:subCategory, day, weekday), funs(factor))
train_Jan_format %>% summary

## not using pid, size, date, releaseDate for modelling
data_Jan <- train_Jan_format %>% filter(date < ymd("2018-01-01")) %>%
  select(-pid, -size, -releaseDate)
data_Jan %>% glimpse
data_Jan %>% summary
MLR <- lm(units~., data = data_Jan %>% select(-date))
summary(MLR)$r.squared
summary(MLR)$sigma^2

## prediction
test_Jan_format <- train_Jan_format %>% filter(date >= ymd("2018-01-01"))
pred.MLR <- MLR %>% predict(newdata = test_Jan_format) ## some negative values
## heads up! some negative predicted daily sales
summary(pred.MLR)
## adjust negative predicted units to be zero
## obtain predicted soldOutDate from predicted units
pred_Jan <- test_Jan_format %>% 
  mutate(units = pmax(0, pred.MLR)) %>% 
  group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>%
  filter(cumunits >= stock) %>%
  summarise(soldOutDate = min(date))
## note items not appear in the dataset has max(cumunits) < stock
pred_Jan %>% glimpse()
pred_Jan %>% summary()

## err.model
## adjust predicted dates to be the end of the month for items with max(cumunits) < stock
comparison <- test_Jan %>% 
  full_join(pred_Jan, by = c("pid", "size"), suffix = c(".true", "")) %>%
  mutate(soldOutDate = replace(soldOutDate, is.na(soldOutDate), ymd("2018-01-31")))
comparison %>% glimpse
err.model <- (comparison$soldOutDate.true - comparison$soldOutDate) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.model: %.3f, err.guess: %.3f", err.model, err.guess)
write.table(comparison %>% select(-soldOutDate.true),
            file = "data_clean/models/MLR.txt", sep = "|", quote = FALSE, row.names = FALSE)

## ---- solutions
## (1a) delete-BF
data_Jan_nBF <- data_Jan %>% filter(date != ymd("2017-11-24"))
MLR_nBF <- lm(units~., data = data_Jan_nBF %>% select(-date))
summary(MLR_nBF)$r.squared
pred.MLR_nBF <- MLR_nBF %>% predict(newdata = test_Jan_format) ## some negative values

## (1b) delete-BF and add discount info
data_Jan_1b <- data_Jan_nBF %>% mutate(discount = (rrp-price)/rrp*100) 
MLR_1b <- lm(units~., data = data_Jan_1b %>% select(-date))
summary(MLR_1b)$r.squared
pred.MLR_1b <- MLR_1b %>% predict(newdata = test_Jan_format %>% 
                                    mutate(discount = (rrp-price)/rrp*100)) ## some negative values

## (1c) glmnet
library(glmnet)
x_train <- model.matrix(~., data = data_Jan %>% select(-date, -units))
lasso_1c <- glmnet(x = x_train,
                   y = data_Jan$units) 

## (1d) k-NN
## poor results, far underestimate, k = ? need more useful features
write.table(data_Jan %>% select(-date),
            file = paste0("C:/Users/lyux/Dropbox/DMC 2018/ForZijian-MLAlgorithm/train_Jan_", Sys.Date(), ".txt"),
            sep = "|", row.names = FALSE, quote = FALSE, na = "")
test_Jan_format %>% left_join(train, by = c("date", "pid", "size"), suffix = c(".na", "")) %>%
  mutate(units = replace(units, is.na(units), 0)) %>% select(date, pid, size, units) -> truth_Jan
write.table(test_Jan_format,
            file = paste0("C:/Users/lyux/Dropbox/DMC 2018/ForZijian-MLAlgorithm/test_Jan_", Sys.Date(), ".txt"),
            sep = "|", row.names = FALSE, quote = FALSE, na = "")
write.table(truth_Jan,
            file = paste0("../../../DMC 2018/ForZijian-MLAlgorithm/truth_Jan_", Sys.Date(), ".txt"),
            sep = "|", row.names = FALSE, quote = FALSE, na = "")
kNN_output <- read.csv("../../../DMC 2018/ForZijian-MLAlgorithm/testPreUnits_k_10.csv", sep = "|")
kNN <- kNN_output %>% select(pid, size, date, stock, pred = preUnits) %>% mutate(date = ymd(date))
pred_Jan <- kNN %>%
  mutate(units = pred) %>% 
  group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>% ungroup %>%
  mutate(yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(soldOutDate = ymd("2018-02-01") - sum(yn.soldout)) %>%
  ungroup %>%
  mutate(soldOutDate = replace(soldOutDate, soldOutDate == ymd("2018-02-01"), ymd("2018-01-16")))


## (2) ZI-LN
## consider zero-inflated log-normal, plug-in estimates
# data_Jan_ynsale <- data_Jan %>% mutate(delta = units>0) %>% select(-units)
## my pc get stuck running this
# glm.binary <- glm(delta~., data = data_Jan_ynsale, family = "binomial") 

## 

## (3) adding more features
feature.date <- read.csv("../../../DMC 2018/Data/Features/feature_date_Lyu_2018-04-23.csv")
feature.date <- feature.date[,-1]
feature.date %>% glimpse
feature.item <- read.csv("../../../DMC 2018/Data/Features/feature_items_Lyu_2018-04-23.csv")
feature.item <- feature.item[,-1]
feature.item %>% glimpse  

feature <- feature.date %>% left_join(feature.item, by = c("pid", "size"))
feature$date <- ymd(feature$date)
feature_Jan <- feature %>% filter(key %in% paste(test_Jan$pid, test_Jan$size, sep = " - "))
names(feature_Jan)
lm(units~., data = feature_Jan %>% filter(date<ymd("2018-01-01")) %>%
     select(-date, -key, -pid, -size, -bestseller.oneday.x, -bestseller.oneday.y,
            -releaseDate, -price.beforeRD, -discount.beforeRD)) -> MLR.feature.422
summary(MLR.feature.422)$r.squared
pred.MLR <- MLR.feature.422 %>% predict(
  newdata = feature_Jan %>% filter(date >= ymd("2018-01-01"), date < ymd("2018-02-01"))) 
pred_Jan <- feature_Jan %>% filter(date >= ymd("2018-01-01"), date < ymd("2018-02-01")) %>% 
  mutate(units = pmax(0, pred.MLR)) %>% 
  group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>% ungroup %>%
  mutate(yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(soldOutDate = ymd("2018-02-01") - sum(yn.soldout)) %>%
  ungroup %>%
  mutate(soldOutDate = replace(soldOutDate, soldOutDate == ymd("2018-02-01"), ymd("2018-01-16")))

MLR.422.sig <- lm(units~., data = feature_Jan %>% filter(date<ymd("2018-01-01")) %>%
                    select(units, age, discount, diffprice, reldiffprice, day, weekday,
                           is.BF, is.unityday, is.reformday, mean.sale, avg.discount))

## Time-Series By Category
setwd("C:/Users/lyux/Dropbox/DMC 2018/Prediction")
TS_Category <- read.table(file = "prediction_Jan_Yudi.txt", sep = "|", header = T)
items <- read.csv("../Data/raw_data/items.csv", sep = "|")
items %>% group_by(category, mainCategory) %>% summarise(n = n()) -> count_cat
TS_Category_Unique <- TS_Category %>% select(date, mainCategory, category, sumunits = units) %>% unique()
TS_Category_Unique %>% left_join(count_cat, by = c("category", "mainCategory")) %>%
  mutate(units = sumunits/n) -> TS_Category_Mean_Unique

write.table(TS_Category_Mean_Unique, file = "TS_Category_Yudi.txt", sep = "|", quote = F, row.names = F)

ggplot(data = TS_Category_Mean_Unique %>%
         mutate(key = paste(mainCategory, category, sep = "-")),
       mapping = aes(x = day(date), y = units, color = key)) +
  geom_point() + geom_line() + scale_color_brewer(palette = "Paired") +
  # scale_y_log10() +
  facet_grid(mainCategory~.)

TS_Category_Mean_Unique %>% group_by(mainCategory, category) %>% 
  mutate(cumunits = cumsum(units)) %>% glimpse

items_Jan <- read.table("../Data/items_Jan.txt", sep = "|", header = T)
TS_Category_Mean_Unique %>% inner_join(items_Jan, by = c("mainCategory", "category")) %>%
  mutate(date = ymd(date)) -> pred_TS_category

pred_TS_category %>% group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>% ungroup %>%
  mutate(yn.soldout = (cumunits >= stock)) %>%
  group_by(pid, size) %>%
  summarise(soldOutDate = ymd("2018-02-01") - sum(yn.soldout)) %>%
  ungroup %>%
  mutate(soldOutDate = replace(soldOutDate, soldOutDate == ymd("2018-02-01"), ymd("2018-01-16"))) -> pred_Jan

comparison %>% left_join(items, by = c("pid", "size")) %>%
  mutate(err = abs(as.numeric(soldOutDate.true - soldOutDate)),
         key = paste(mainCategory, category, sep = "-")) %>%
  ggplot(aes(x = key, y = err)) + geom_boxplot()

