My findings
-----------

#### read data

``` r
library(sugrrants)
library(tidyverse)
library(lubridate)
setwd("~/Dropbox/dmc2018")
train <- read.csv("data/raw_data/train.csv", sep = "|")
prices <- read.csv("data/raw_data/prices.csv", sep = "|")
items <- read.csv("data/raw_data/items.csv", sep = "|")
```

#### data quality

``` r
## no missing in training data
apply(train, 2, function(x) sum(is.na(x)))
```

    ##  date   pid  size units 
    ##     0     0     0     0

``` r
## some missing in subCategory
apply(items, 2, function(x) sum(is.na(x)))
```

    ##          pid         size        color        brand          rrp 
    ##            0            0            0            0            0 
    ## mainCategory     category  subCategory        stock  releaseDate 
    ##            0            0         1134            0            0

``` r
## all keys appear in every data set
anti_join(train, items, by = c("pid", "size")) %>% dim()
```

    ## [1] 0 4

``` r
anti_join(items, train, by = c("pid", "size")) %>% dim()
```

    ## [1]  0 10

``` r
anti_join(items, prices, by = c("pid", "size")) %>% dim()
```

    ## [1]  0 10

#### dates at a glance

``` r
## sales data available everyday from 2017-10-01 to 2018-01-31
train$date <- ymd(train$date)
range(train$date)
```

    ## [1] "2017-10-01" "2018-01-31"

``` r
range(train$date) %>% diff
```

    ## Time difference of 122 days

``` r
unique(train$date) %>% length()
```

    ## [1] 123

``` r
## about 85% of the items released before 2017-10-01
items$releaseDate <- ymd(items$releaseDate)
mean(items$releaseDate == ymd("2017-10-01"))
```

    ## [1] 0.849345

#### price & date

``` r
## gather date and price by product ID and size
prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd()) -> prices_long
prices_long %>% glimpse
```

    ## Observations: 1,936,424
    ## Variables: 4
    ## $ pid   <int> 19671, 19671, 19671, 19671, 19671, 19671, 19671, 19671, ...
    ## $ size  <fctr> 39 1/3, 40, 41 1/3, 42, 42 2/3, 43 1/3, 44, 44 2/3, 45 ...
    ## $ date  <date> 2017-10-01, 2017-10-01, 2017-10-01, 2017-10-01, 2017-10...
    ## $ price <dbl> 133.31, 133.31, 133.31, 133.31, 133.31, 133.31, 133.31, ...

``` r
## releaseDate is the same for a product across different sizes, 4770 different pid
items %>% group_by(pid) %>%
  summarise(ndate = unique(releaseDate) %>% length) %>%
  select(ndate) %>% table()
```

    ## .
    ##    1 
    ## 4770

``` r
## * before releasedate, any non-missing prices? Yes ####
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date < releaseDate, !is.na(price)) -> prices_suspicious
prices_suspicious %>% glimpse
```

    ## Observations: 1,932
    ## Variables: 12
    ## $ pid          <int> 10013, 10013, 10013, 10042, 10050, 10115, 10115, ...
    ## $ size         <fctr> L, M, S, 37,5, M, 43 1/3, 44, 33, XL ( 44/46 ), ...
    ## $ color        <fctr> schwarz, schwarz, schwarz, blau, schwarz, weiss,...
    ## $ brand        <fctr> adidas, adidas, adidas, Nike, PUMA, adidas, adid...
    ## $ rrp          <dbl> 69.78, 69.78, 69.78, 88.83, 50.73, 126.93, 126.93...
    ## $ mainCategory <int> 1, 1, 1, 1, 1, 9, 9, 1, 1, 9, 9, 9, 9, 1, 1, 1, 1...
    ## $ category     <int> 7, 7, 7, 37, 7, 18, 18, 37, 7, 10, 18, 18, 18, 2,...
    ## $ subCategory  <int> 16, 16, 16, 6, 16, 32, 32, 39, 14, 22, 32, 32, 32...
    ## $ stock        <int> 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 3, 1, 2...
    ## $ releaseDate  <date> 2017-10-27, 2017-10-27, 2017-10-27, 2017-11-10, ...
    ## $ date         <date> 2017-10-26, 2017-10-26, 2017-10-26, 2017-11-09, ...
    ## $ price        <dbl> 41.21, 41.21, 41.21, 69.95, 27.97, 99.95, 99.95, ...

``` r
## 688 different pid
prices_suspicious %>% select(pid) %>% unique %>% dim
```

    ## [1] 688   1

``` r
## in each row, date is one-day earlier than releaseDate, suggesting wrong releaseDate?
prices_suspicious %>% mutate(diff_date = date-releaseDate) %>% select(diff_date) %>% table()
```

    ## .
    ##   -1 
    ## 1932

``` r
## * after releasedate, any missing prices? No ####
## no missing price
items %>% left_join(prices_long, by = c("pid", "size")) %>% 
  filter(date >= releaseDate) %>% select(price) %>% summary()
```

    ##      price       
    ##  Min.   :  2.47  
    ##  1st Qu.: 23.76  
    ##  Median : 48.85  
    ##  Mean   : 78.41  
    ##  3rd Qu.:102.81  
    ##  Max.   :419.03

#### calendar plots

``` r
## remarkable more sales on Black Friday 2017-11-24
train.sales <- train %>% group_by(date) %>% summarise(n = sum(units)) %>% ungroup 
train.sales %>%
  frame_calendar(x = 1, y = 1, date = date) %>%
  ggplot(aes(x = .x, y = .y)) + 
  ggtitle("log-scale daily sold units") + 
  theme(legend.position = "bottom") +
  geom_tile(aes(x = .x+(1/13)/2, y = .y+(1/9)/2, fill = log(n)), colour = "grey50") +
  scale_fill_distiller(name = "", palette = "RdYlBu") -> p2.sale
prettify(p2.sale, label = c("label", "text", "text2"))
```

![](figures/calendar-1.png)

``` r
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
```

![](figures/calendar-2.png)
