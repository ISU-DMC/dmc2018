``` r
library(lubridate)
library(tidyverse)
test_Jan <- read.table("data_clean/test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)
```

``` r
## error for random guess "2018-01-16": 244.524
pred.guess <- ymd("2018-01-16")
err.guess <- (test_Jan$soldOutDate - pred.guess) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.guess: %.3f", err.guess)
```

    ## [1] "err.guess: 244.524"

``` r
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
```

    ##       pid             size             date                price       
    ##  Min.   :10001   M      :100735   Min.   :2017-10-01   Min.   :  2.47  
    ##  1st Qu.:13079   L      :100556   1st Qu.:2017-11-02   1st Qu.: 22.46  
    ##  Median :16287   XL     : 71100   Median :2017-12-03   Median : 42.81  
    ##  Mean   :16351   S      : 64068   Mean   :2017-12-02   Mean   : 77.34  
    ##  3rd Qu.:19601   44     : 36103   3rd Qu.:2018-01-02   3rd Qu.:101.53  
    ##  Max.   :22881   2XL    : 34537   Max.   :2018-01-31   Max.   :419.03  
    ##                  (Other):478051                                        
    ##      units            color                 brand             rrp        
    ##  Min.   :  0.00   schwarz:327937   Nike        :464800   Min.   :  2.47  
    ##  1st Qu.:  0.00   blau   :175370   adidas      :283928   1st Qu.: 31.68  
    ##  Median :  0.00   rot    :124444   PUMA        : 48362   Median : 63.43  
    ##  Mean   :  0.33   weiss  :113042   Jako        : 42312   Mean   : 94.98  
    ##  3rd Qu.:  0.00   grau   : 46106   Under Armour:  9717   3rd Qu.:114.23  
    ##  Max.   :472.00   gruen  : 32191   Jordan      :  5960   Max.   :444.43  
    ##  NA's   :229679   (Other): 66060   (Other)     : 30071                   
    ##  mainCategory    category       subCategory      releaseDate        
    ##  1 :699188    7      :408941   3      :161897   Min.   :2017-10-01  
    ##  9 : 92772    2      :262185   0      : 93190   1st Qu.:2017-10-01  
    ##  15: 93190    10     : 46413   21     : 85677   Median :2017-10-01  
    ##               16     : 42441   8      : 59845   Mean   :2017-10-03  
    ##               18     : 36904   16     : 54033   3rd Qu.:2017-10-01  
    ##               33     : 34072   5      : 45717   Max.   :2017-12-27  
    ##               (Other): 54194   (Other):384791                       
    ##      stock              day         weekday   
    ##  Min.   :  1.000   30     : 29097   1:129276  
    ##  1st Qu.:  1.000   29     : 29096   2:129420  
    ##  Median :  2.000   27     : 29093   3:129531  
    ##  Mean   :  5.761   28     : 29093   4:129678  
    ##  3rd Qu.:  4.000   26     : 29084   5:122409  
    ##  Max.   :512.000   25     : 29061   6:122418  
    ##                    (Other):710626   7:122418

``` r
## not using pid, size, date, releaseDate for modelling
data_Jan <- train_Jan_format %>% filter(date < ymd("2018-01-01")) %>%
  select(-pid, -size, -date, -releaseDate)
data_Jan %>% glimpse
```

    ## Observations: 655,471
    ## Variables: 11
    ## $ price        <dbl> 133.31, 133.31, 133.31, 133.31, 133.31, 133.31, 1...
    ## $ units        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ color        <fctr> schwarz, schwarz, schwarz, schwarz, schwarz, sch...
    ## $ brand        <fctr> adidas, adidas, adidas, adidas, adidas, adidas, ...
    ## $ rrp          <dbl> 190.43, 190.43, 190.43, 190.43, 190.43, 190.43, 1...
    ## $ mainCategory <fctr> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ category     <fctr> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...
    ## $ subCategory  <fctr> 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, ...
    ## $ stock        <int> 1, 2, 3, 2, 9, 3, 2, 7, 2, 1, 1, 2, 1, 4, 6, 11, ...
    ## $ day          <fctr> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ weekday      <fctr> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...

``` r
data_Jan %>% summary
```

    ##      price            units              color       
    ##  Min.   :  2.47   Min.   :  0.0000   schwarz:242098  
    ##  1st Qu.: 22.46   1st Qu.:  0.0000   blau   :130389  
    ##  Median : 42.81   Median :  0.0000   rot    : 92576  
    ##  Mean   : 77.38   Mean   :  0.3258   weiss  : 83592  
    ##  3rd Qu.:101.53   3rd Qu.:  0.0000   grau   : 34078  
    ##  Max.   :419.03   Max.   :472.0000   gruen  : 23976  
    ##                                      (Other): 48762  
    ##           brand             rrp         mainCategory    category     
    ##  Nike        :346752   Min.   :  2.47   1 :517807    7      :304533  
    ##  adidas      :207017   1st Qu.: 31.68   9 : 68840    2      :192962  
    ##  PUMA        : 35900   Median : 63.43   15: 68824    10     : 34385  
    ##  Jako        : 31648   Mean   : 94.65                16     : 31436  
    ##  Under Armour:  7268   3rd Qu.:114.23                18     : 27387  
    ##  Jordan      :  4441   Max.   :444.43                33     : 24958  
    ##  (Other)     : 22445                                 (Other): 39810  
    ##   subCategory         stock              day         weekday  
    ##  3      :119272   Min.   :  1.000   30     : 21688   1:99640  
    ##  0      : 68824   1st Qu.:  1.000   29     : 21687   2:92375  
    ##  21     : 64070   Median :  2.000   27     : 21684   3:92486  
    ##  8      : 44686   Mean   :  5.781   28     : 21684   4:92633  
    ##  16     : 40083   3rd Qu.:  4.000   26     : 21675   5:92773  
    ##  32     : 33785   Max.   :512.000   25     : 21652   6:92782  
    ##  (Other):284751                     (Other):525401   7:92782

``` r
MLR <- lm(units~., data = data_Jan)

## prediction
test_Jan_format <- train_Jan_format %>% filter(date >= ymd("2018-01-01"))
pred.MLR <- MLR %>% predict(newdata = test_Jan_format) ## some negative values
## heads up! some negative predicted daily sales
summary(pred.MLR)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.75501  0.03247  0.20713  0.32853  0.45477 16.79871

``` r
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
```

    ## Observations: 6,672
    ## Variables: 3
    ## $ pid         <int> 10001, 10003, 10013, 10020, 10035, 10035, 10035, 1...
    ## $ size        <fctr> L, 5 ( 43-46 ), M, XL, L ( 152-158 ), XL ( 158-17...
    ## $ soldOutDate <date> 2018-01-10, 2018-01-11, 2018-01-06, 2018-01-04, 2...

``` r
pred_Jan %>% summary()
```

    ##       pid             size       soldOutDate        
    ##  Min.   :10001   L      : 727   Min.   :2018-01-02  
    ##  1st Qu.:13084   M      : 715   1st Qu.:2018-01-06  
    ##  Median :16297   XL     : 531   Median :2018-01-11  
    ##  Mean   :16366   S      : 496   Mean   :2018-01-14  
    ##  3rd Qu.:19594   2XL    : 271   3rd Qu.:2018-01-24  
    ##  Max.   :22881   44     : 259   Max.   :2018-01-31  
    ##                  (Other):3673

``` r
## error for MLR
## adjust predicted dates to be the end of the monthe for items with max(cumunits) < stock
comparison <- test_Jan %>% 
  full_join(pred_Jan, by = c("pid", "size"), suffix = c(".true", ".pred")) %>%
  mutate(soldOutDate.pred = replace(soldOutDate.pred, is.na(soldOutDate.pred), ymd("2018-01-31")))
comparison %>% glimpse
```

    ## Observations: 7,409
    ## Variables: 4
    ## $ pid              <int> 10001, 10003, 10008, 10013, 10020, 10035, 100...
    ## $ size             <fctr> L, 5 ( 43-46 ), XL, M, XL, L ( 152-158 ), XL...
    ## $ soldOutDate.true <date> 2018-01-23, 2018-01-30, 2018-01-13, 2018-01-...
    ## $ soldOutDate.pred <date> 2018-01-10, 2018-01-11, 2018-01-31, 2018-01-...

``` r
err.MLR <- (comparison$soldOutDate.true - comparison$soldOutDate.pred) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.MLR: %.3f, err.guess: %.3f", err.MLR, err.guess)
```

    ## [1] "err.MLR: 267.903, err.guess: 244.524"

Possible ways to reduce Err

``` r
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
```
