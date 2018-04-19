``` r
train <- read.csv("../../data/raw_data/train.csv", sep = "|", stringsAsFactors = F)
prices <- read.csv("../../data/raw_data/prices.csv", sep = "|", stringsAsFactors = F)
items <- read.csv("../../data/raw_data/items.csv", sep = "|", stringsAsFactors = F)

## format date
library(lubridate)
library(tidyverse)
train <- train %>% mutate(date = ymd(date))
items <- items %>% mutate(releaseDate = ymd(releaseDate))
prices_long <- prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd())

## join three datasets
alldata <- prices_long %>% 
  full_join(train, by = c("pid", "size", "date")) %>% 
  full_join(items, by = c("pid", "size")) %>%
  filter(date>=releaseDate-1) %>% ## only keep price info since one day before releasedate
  mutate(units = replace(units, is.na(units) & date < ymd("2018-02-01"), 0))

## key variable for identifying item
alldata <- alldata %>% mutate(key = paste(pid, size, sep = " - "))
## check sales before and in January
sale.beforeJan <- alldata %>% filter(date < ymd("2018-01-01")) %>%
  group_by(key) %>% 
  summarise(nsale.beforeJan = sum(units))
sale.Jan <- alldata %>% filter(date >= ymd("2018-01-01")) %>%
  group_by(key) %>%
  summarise(nsale.Jan = sum(units, na.rm = T))
sale.beforeJan %>% full_join(sale.Jan, by = "key") %>%
  mutate(nsale.beforeJan = replace(nsale.beforeJan, is.na(nsale.beforeJan), 0)) %>%
  filter(nsale.beforeJan==0 | nsale.Jan == 0) -> items.aside

items.aside %>% glimpse
```

    ## Observations: 5,415
    ## Variables: 3
    ## $ key             <chr> "10000 - XL ( 158-170 )", "10003 - 3 (35-38 )"...
    ## $ nsale.beforeJan <dbl> 1, 15, 3, 1, 0, 1, 2, 1, 4, 1, 1, 0, 1, 1, 0, ...
    ## $ nsale.Jan       <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1...

``` r
## put aside those items not both sold before and in January
## put aside obs for February
subdata <- alldata %>% filter(!(key %in% items.aside$key), 
                              date < ymd("2018-02-01")) %>% select(-stock)
## randomly assign stock for Jan, and save true sold-out dates under the assigned stock
set.seed(180201)
stock_Jan <- subdata %>% 
  left_join(sale.Jan, by = "key") %>%
  group_by(key) %>%
  summarise(stock = sample.int(n = nsale.Jan, size = 1))
stock_Jan %>% glimpse
```

    ## Observations: 7,409
    ## Variables: 2
    ## $ key   <chr> "10001 - L", "10003 - 5 ( 43-46 )", "10008 - XL", "10013...
    ## $ stock <int> 2, 1, 2, 3, 1, 1, 1, 1, 1, 1, 3, 1, 1, 2, 2, 12, 20, 4, ...

``` r
stock_Jan %>% summary
```

    ##      key                stock        
    ##  Length:7409        Min.   :  1.000  
    ##  Class :character   1st Qu.:  1.000  
    ##  Mode  :character   Median :  2.000  
    ##                     Mean   :  5.704  
    ##                     3rd Qu.:  4.000  
    ##                     Max.   :512.000

``` r
test_Jan <- subdata %>% 
  left_join(stock_Jan, by = "key") %>%
  filter(date >= ymd("2018-01-01")) %>%
  group_by(pid, size) %>%
  mutate(cumunits = cumsum(units)) %>%
  filter(cumunits >= stock) %>%
  summarise(soldOutDate = min(date))
test_Jan %>% glimpse()
```

    ## Observations: 7,409
    ## Variables: 3
    ## $ pid         <int> 10001, 10003, 10008, 10013, 10020, 10035, 10035, 1...
    ## $ size        <chr> "L", "5 ( 43-46 )", "XL", "M", "XL", "L ( 152-158 ...
    ## $ soldOutDate <date> 2018-01-23, 2018-01-30, 2018-01-13, 2018-01-25, 2...

``` r
test_Jan %>% summary()
```

    ##       pid            size            soldOutDate        
    ##  Min.   :10001   Length:7409        Min.   :2018-01-01  
    ##  1st Qu.:13076   Class :character   1st Qu.:2018-01-10  
    ##  Median :16297   Mode  :character   Median :2018-01-18  
    ##  Mean   :16362                      Mean   :2018-01-18  
    ##  3rd Qu.:19601                      3rd Qu.:2018-01-26  
    ##  Max.   :22881                      Max.   :2018-01-31

``` r
## training data, joint info about items, prices and sales 
## sale units in January have been set as missing
train_Jan <- subdata %>%
  full_join(stock_Jan, by = "key") %>%
  select(-key) %>%
  mutate(units = replace(units, date >= ymd("2018-01-01"), NA))
train_Jan %>% glimpse
```

    ## Observations: 885,150
    ## Variables: 13
    ## $ pid          <int> 19671, 19671, 19671, 19671, 19671, 19671, 19671, ...
    ## $ size         <chr> "39 1/3", "40", "41 1/3", "42", "42 2/3", "43 1/3...
    ## $ date         <date> 2017-10-01, 2017-10-01, 2017-10-01, 2017-10-01, ...
    ## $ price        <dbl> 133.31, 133.31, 133.31, 133.31, 133.31, 133.31, 1...
    ## $ units        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ color        <chr> "schwarz", "schwarz", "schwarz", "schwarz", "schw...
    ## $ brand        <chr> "adidas", "adidas", "adidas", "adidas", "adidas",...
    ## $ rrp          <dbl> 190.43, 190.43, 190.43, 190.43, 190.43, 190.43, 1...
    ## $ mainCategory <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ category     <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2...
    ## $ subCategory  <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5...
    ## $ releaseDate  <date> 2017-10-01, 2017-10-01, 2017-10-01, 2017-10-01, ...
    ## $ stock        <int> 1, 2, 3, 2, 9, 3, 2, 7, 2, 1, 1, 2, 1, 4, 6, 11, ...

``` r
train_Jan %>% select(units, releaseDate, stock) %>% summary
```

    ##      units         releaseDate             stock        
    ##  Min.   :  0.00   Min.   :2017-10-01   Min.   :  1.000  
    ##  1st Qu.:  0.00   1st Qu.:2017-10-01   1st Qu.:  1.000  
    ##  Median :  0.00   Median :2017-10-01   Median :  2.000  
    ##  Mean   :  0.33   Mean   :2017-10-03   Mean   :  5.761  
    ##  3rd Qu.:  0.00   3rd Qu.:2017-10-01   3rd Qu.:  4.000  
    ##  Max.   :472.00   Max.   :2017-12-27   Max.   :512.000  
    ##  NA's   :229679

``` r
## properties of the 7409 items with the "faked" stock on 2018-01-01
items_Jan <- train_Jan %>% select(-date, -price, -units) %>% unique
items_Jan %>% glimpse
```

    ## Observations: 7,409
    ## Variables: 10
    ## $ pid          <int> 19671, 19671, 19671, 19671, 19671, 19671, 19671, ...
    ## $ size         <chr> "39 1/3", "40", "41 1/3", "42", "42 2/3", "43 1/3...
    ## $ color        <chr> "schwarz", "schwarz", "schwarz", "schwarz", "schw...
    ## $ brand        <chr> "adidas", "adidas", "adidas", "adidas", "adidas",...
    ## $ rrp          <dbl> 190.43, 190.43, 190.43, 190.43, 190.43, 190.43, 1...
    ## $ mainCategory <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ category     <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2...
    ## $ subCategory  <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5...
    ## $ releaseDate  <date> 2017-10-01, 2017-10-01, 2017-10-01, 2017-10-01, ...
    ## $ stock        <int> 1, 2, 3, 2, 9, 3, 2, 7, 2, 1, 1, 2, 1, 4, 6, 11, ...

``` r
## save datasets as txt file, separated by "|", missing value as empty
write.table(train_Jan, file = "data_clean/train_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(items_Jan, file = "data_clean/items_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE, na = "")
write.table(test_Jan, file = "data_clean/test_Jan.txt", sep = "|",
            row.names = FALSE, quote = FALSE)
```
