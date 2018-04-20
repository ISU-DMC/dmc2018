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
train_Jan <- train_Jan %>% 
  mutate(date = ymd(date),
         releaseDate = ymd(releaseDate))

## only use weekday and day info from the date variable
train_Jan_format <- train_Jan %>%
  mutate(day = day(date),
         weekday = wday(date),
         subCategory = replace(subCategory, is.na(subCategory), 0)) %>%
  mutate_at(vars(mainCategory:subCategory, day, weekday), funs(factor))
train_Jan_format %>% summary

## not using pid, size, date, releaseDate for modelling
data_Jan <- train_Jan_format %>% filter(date < ymd("2018-01-01")) %>%
  select(-pid, -size, -date, -releaseDate)
data_Jan %>% glimpse
data_Jan %>% summary
MLR <- lm(units~., data = data_Jan)

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

## error for MLR
## adjust predicted dates to be the end of the monthe for items with max(cumunits) < stock
comparison <- test_Jan %>% 
  full_join(pred_Jan, by = c("pid", "size"), suffix = c(".true", ".pred")) %>%
  mutate(soldOutDate.pred = replace(soldOutDate.pred, is.na(soldOutDate.pred), ymd("2018-01-31")))
comparison %>% glimpse
err.MLR <- (comparison$soldOutDate.true - comparison$soldOutDate.pred) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.MLR: %.3f, err.guess: %.3f", err.MLR, err.guess)


## ---- solutions
## (1) delete-BF

## (2) ZI-LN
## consider zero-inflated log-normal, plug-in estimates

## (3) adding more features
## product age for example to use info from releaseDate
## 19 is the 1st quantile of ages of new products
# train_Jan %>% filter(releaseDate > ymd("2017-10-01"), date >= releaseDate) %>% 
#   mutate(age = as.numeric(date-releaseDate)) %>% select(age) %>% summary
# train_Jan_format <- train_Jan %>%
#   mutate(base_age = ifelse(releaseDate == ymd("2017-10-01"), 19, 0),
#          age = as.numeric(date - releaseDate) + base_age) %>%
#   select(-releaseDate, -date, -base_age)
# train_Jan_format %>% glimpse

