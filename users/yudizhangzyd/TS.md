``` r
## ---- input
library(lubridate)
library(tidyverse)
items <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/items.csv", sep = "|")
View(items)
prices <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/prices.csv", sep = "|")
train <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/train.csv", sep = "|")


test_Jan <- read.table("/Users/yudi/R projects/DMC/test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)

## ---- baseline

pred.guess <- ymd("2018-01-16")
err.guess <- (test_Jan$soldOutDate - pred.guess) %>% abs %>% sum %>% as.numeric %>% sqrt
sprintf("err.guess: %.3f", err.guess)
```

    ## [1] "err.guess: 244.524"

``` r
train_Jan <- read.table("/Users/yudi/R projects/DMC/train_Jan.txt", sep = "|", header = T)
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
    ## $ color        <fct> schwarz, schwarz, schwarz, schwarz, schwarz, schw...
    ## $ brand        <fct> adidas, adidas, adidas, adidas, adidas, adidas, a...
    ## $ rrp          <dbl> 190.43, 190.43, 190.43, 190.43, 190.43, 190.43, 1...
    ## $ mainCategory <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ category     <fct> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2...
    ## $ subCategory  <fct> 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5...
    ## $ stock        <int> 1, 2, 3, 2, 9, 3, 2, 7, 2, 1, 1, 2, 1, 4, 6, 11, ...
    ## $ day          <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ weekday      <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...

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
#date clean

train <- train %>% mutate(date = ymd(date))
items <- items %>% mutate(releaseDate = ymd(releaseDate))
prices_long <- prices %>% gather(date, price, -pid, -size) %>%
  mutate(date = gsub("X", "", date) %>% ymd())

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

##Analyze time seris by maincategory and category
train.ts <- train_Jan_format %>% group_by(mainCategory, category, date) %>% mutate(y = sum(units)) %>% filter (date < ymd("2018-01-01")) %>% select(mainCategory,category, date, y) %>% unique() 

names(train.ts) <- c("mainCategory","category", "ds", "y")
```

``` r
Jan.actual.sale <- subdata %>% full_join(stock_Jan, by = "key") %>%
select(-key) %>% group_by(mainCategory, category, date) %>% summarise(y = sum(units)) %>% filter (date >= ymd("2018-01-01")) %>% select(mainCategory,category, date, y) %>% unique() 
```

``` r
##TS based on xgboost
library(forecastxgb)
library(Metrics)

cat.fc <- function(maincat, cat){

     
 cat.train.ts <- subset(train.ts, category == cat & mainCategory == maincat)[,-c(1,2)]


model <- xgbar(ts(cat.train.ts[,2]), trend_method = "differencing", seas_method = "fourier")
cat.fc <- forecast(model, h = 31)

data.frame(mainCategory=maincat, category = cat, yhat = summary(cat.fc)$`Point Forecast`)
                   
}


ts.pre <- rbind(cat.fc(maincat = 1, cat = 2),cat.fc(maincat = 1, cat = 7),cat.fc(maincat = 1, cat = 37),cat.fc(maincat = 9, cat = 10),cat.fc(maincat = 9, cat = 18),cat.fc(maincat = 9, cat = 36),cat.fc(maincat = 9, cat = 37),cat.fc(maincat = 15, cat = 16),cat.fc(maincat = 15, cat = 24),cat.fc(maincat = 15, cat = 30),cat.fc(maincat = 15, cat = 33)) 
```

    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -210709.3 285702.1 210709.3 -133976.6 133976.6 1923.996
    ##                  ACF1
    ## Training set 0.962357
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93       150.2733
    ##  94       214.3086
    ##  95       141.2383
    ##  96       127.0896
    ##  97       179.0931
    ##  98       183.1036
    ##  99       141.2383
    ## 100       127.0896
    ## 101       179.0931
    ## 102       183.1036
    ## 103       141.2383
    ## 104       127.0896
    ## 105       179.0931
    ## 106       183.1036
    ## 107       141.2383
    ## 108       127.0896
    ## 109       179.0931
    ## 110       183.1036
    ## 111       141.2383
    ## 112       127.0896
    ## 113       179.0931
    ## 114       183.1036
    ## 115       141.2383
    ## 116       127.0896
    ## 117       179.0931
    ## 118       183.1036
    ## 119       141.2383
    ## 120       127.0896
    ## 121       179.0931
    ## 122       183.1036
    ## 123       141.2383
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                    ME    RMSE     MAE       MPE     MAPE     MASE
    ## Training set -1301794 1781051 1301794 -124494.7 124494.7 2147.474
    ##                   ACF1
    ## Training set 0.9612891
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      1034.2673
    ##  94       797.9021
    ##  95       856.2057
    ##  96      1653.7869
    ##  97      1759.4585
    ##  98      1813.0559
    ##  99      1682.3862
    ## 100      1073.8031
    ## 101       675.2297
    ## 102       561.3219
    ## 103       625.1984
    ## 104      1154.7666
    ## 105      1288.8142
    ## 106      1053.0253
    ## 107       991.2900
    ## 108       919.1949
    ## 109       878.6924
    ## 110      1168.8059
    ## 111      1101.6600
    ## 112      1035.2610
    ## 113       907.8019
    ## 114       819.7653
    ## 115       958.8712
    ## 116       987.8950
    ## 117      1035.2610
    ## 118      1035.2610
    ## 119       908.2640
    ## 120       812.2933
    ## 121       958.8712
    ## 122       860.4358
    ## 123      1199.6436
    ## 
    ## Forecast method: xgbar(8, 1, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -12.20938 15.65135 13.19735 -224.3849 228.3786 2.207644
    ##                  ACF1
    ## Training set 0.645791
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93       3.329730
    ##  94       3.659459
    ##  95       1.999459
    ##  96       2.329189
    ##  97       3.991689
    ##  98       4.321419
    ##  99       4.651149
    ## 100       5.011149
    ## 101       5.340878
    ## 102       5.670608
    ## 103       7.333108
    ## 104       7.662838
    ## 105       7.992568
    ## 106       8.322297
    ## 107       8.652027
    ## 108       8.981757
    ## 109       9.311487
    ## 110       9.641216
    ## 111       9.970946
    ## 112      10.300676
    ## 113      10.630405
    ## 114      10.960135
    ## 115      11.289865
    ## 116      11.619595
    ## 117      11.949324
    ## 118      12.279054
    ## 119      12.608784
    ## 120      12.938514
    ## 121      13.268243
    ## 122      13.597973
    ## 123      13.927703
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -66015.01 92168.16 66015.01 -99999.62 99999.62 883.3063
    ##                   ACF1
    ## Training set 0.9602435
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93       31.95707
    ##  94       17.17207
    ##  95       34.72763
    ##  96       32.18509
    ##  97       32.18509
    ##  98       32.18509
    ##  99       32.18509
    ## 100       26.69273
    ## 101       26.69273
    ## 102       26.69273
    ## 103       26.69273
    ## 104       26.69273
    ## 105       26.69273
    ## 106       26.69273
    ## 107       26.69273
    ## 108       26.69273
    ## 109       26.69273
    ## 110       26.69273
    ## 111       26.69273
    ## 112       26.69273
    ## 113       26.69273
    ## 114       26.69273
    ## 115       26.69273
    ## 116       26.69273
    ## 117       26.69273
    ## 118       26.69273
    ## 119       26.69273
    ## 120       26.69273
    ## 121       26.69273
    ## 122       26.69273
    ## 123       26.69273
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -11947.22 16255.95 11947.22 -84075.68 84075.68 723.3516
    ##                   ACF1
    ## Training set 0.9612932
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      12.498667
    ##  94       5.323061
    ##  95       3.257442
    ##  96       4.275136
    ##  97       5.356769
    ##  98       8.172153
    ##  99       8.172153
    ## 100       5.356769
    ## 101       3.257442
    ## 102       3.257442
    ## 103       5.356769
    ## 104       5.356769
    ## 105       5.356769
    ## 106       5.356769
    ## 107       5.356769
    ## 108       5.356769
    ## 109       5.356769
    ## 110       5.356769
    ## 111       5.356769
    ## 112       5.356769
    ## 113       5.356769
    ## 114       5.356769
    ## 115       5.356769
    ## 116       5.356769
    ## 117       5.356769
    ## 118       5.356769
    ## 119       5.356769
    ## 120       5.356769
    ## 121       5.356769
    ## 122       5.356769
    ## 123       5.356769
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE  MPE MAPE     MASE     ACF1
    ## Training set -2074.936 2761.904 2074.936 -Inf  Inf 1072.836 0.963592
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      1.3658862
    ##  94      1.9521132
    ##  95      2.9525499
    ##  96      0.5131343
    ##  97      1.7863603
    ##  98      1.2713326
    ##  99      1.0258274
    ## 100      2.6576993
    ## 101      2.1158862
    ## 102      2.4270444
    ## 103      1.0866102
    ## 104      0.9842783
    ## 105      1.1203811
    ## 106      1.9521132
    ## 107      2.4270444
    ## 108      1.3814609
    ## 109      1.4113603
    ## 110      1.0258274
    ## 111      1.0258274
    ## 112      2.4270444
    ## 113      1.7062110
    ## 114      1.7062110
    ## 115      1.4113603
    ## 116      1.0258274
    ## 117      1.0258274
    ## 118      1.9521132
    ## 119      2.4270444
    ## 120      1.7062110
    ## 121      1.4113603
    ## 122      1.0258274
    ## 123      1.0258274
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE  MPE MAPE     MASE      ACF1
    ## Training set -544.8133 725.4129 544.8133 -Inf  Inf 1077.783 0.9632139
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      0.5035661
    ##  94      0.4212132
    ##  95      0.6114956
    ##  96      0.6387500
    ##  97      0.3738235
    ##  98      0.2568055
    ##  99      0.2568055
    ## 100      0.6114956
    ## 101      0.2628125
    ## 102      0.6114956
    ## 103      0.6922449
    ## 104      0.2628125
    ## 105      0.2568055
    ## 106      0.2568055
    ## 107      0.6114956
    ## 108      0.2628125
    ## 109      0.6114956
    ## 110      0.6922449
    ## 111      0.2628125
    ## 112      0.2568055
    ## 113      0.2568055
    ## 114      0.6114956
    ## 115      0.2628125
    ## 116      0.6114956
    ## 117      0.6922449
    ## 118      0.2628125
    ## 119      0.2568055
    ## 120      0.2568055
    ## 121      0.6114956
    ## 122      0.2628125
    ## 123      0.6114956
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -250813.7 344897.3 250813.7 -179532.7 179532.7 1954.783
    ##                  ACF1
    ## Training set 0.962416
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93       79.99628
    ##  94       97.48672
    ##  95       74.46688
    ##  96       81.64406
    ##  97       74.46688
    ##  98       97.48672
    ##  99       97.48672
    ## 100       97.48672
    ## 101       97.48672
    ## 102       97.48672
    ## 103       97.48672
    ## 104       97.48672
    ## 105       97.48672
    ## 106       97.48672
    ## 107       97.48672
    ## 108       97.48672
    ## 109       97.48672
    ## 110       97.48672
    ## 111       97.48672
    ## 112       97.48672
    ## 113       97.48672
    ## 114       97.48672
    ## 115       97.48672
    ## 116       97.48672
    ## 117       97.48672
    ## 118       97.48672
    ## 119       97.48672
    ## 120       97.48672
    ## 121       97.48672
    ## 122       97.48672
    ## 123       97.48672
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -49628.87 66053.37 49628.87 -226945.6 226945.6 2384.492
    ##                  ACF1
    ## Training set 0.963283
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      44.536423
    ##  94      33.957249
    ##  95      44.655212
    ##  96      36.495907
    ##  97      33.376427
    ##  98      36.459614
    ##  99      39.517258
    ## 100      38.935200
    ## 101      33.293518
    ## 102      49.730984
    ## 103      33.859451
    ## 104      40.790020
    ## 105      51.117779
    ## 106      45.365372
    ## 107      17.024736
    ## 108      26.001173
    ## 109      36.984207
    ## 110      61.982567
    ## 111      51.620590
    ## 112      14.615943
    ## 113       9.530784
    ## 114      19.577217
    ## 115      53.561447
    ## 116      44.125919
    ## 117      29.769468
    ## 118      26.150532
    ## 119      24.179672
    ## 120      36.996201
    ## 121      35.704651
    ## 122      43.731571
    ## 123      37.718681
    ## 
    ## Forecast method: xgbar(8, 1, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE  MPE MAPE     MASE     ACF1
    ## Training set -11.37047 12.84998 11.37047 -Inf  Inf 19.89832 0.939946
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93      0.3508125
    ##  94      0.5134625
    ##  95      0.8642750
    ##  96      4.0005250
    ##  97      3.0725249
    ##  98      3.4233374
    ##  99      3.7741499
    ## 100      4.1249624
    ## 101      4.4757749
    ## 102      4.6384249
    ## 103      4.9892374
    ## 104      8.1254874
    ## 105      7.1974873
    ## 106      7.5482998
    ## 107      7.8991123
    ## 108      8.2499248
    ## 109      8.6007373
    ## 110      8.7633873
    ## 111      9.1141998
    ## 112     12.2504498
    ## 113     11.3224497
    ## 114     11.6732622
    ## 115     12.0240747
    ## 116     12.3748872
    ## 117     12.7256997
    ## 118     12.8883497
    ## 119     13.2391621
    ## 120     16.3754122
    ## 121     15.4474121
    ## 122     15.7982246
    ## 123     16.1490371
    ## 
    ## Forecast method: xgbar(8, 0, 'non-seasonal')
    ## 
    ## Model Information:
    ## NULL
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE
    ## Training set -188542.7 252549.4 188542.7 -231671.8 231671.8 1979.622
    ##                   ACF1
    ## Training set 0.9638007
    ## 
    ## Forecasts:
    ##     Point Forecast
    ##  93       31.24250
    ##  94       31.24250
    ##  95       25.48904
    ##  96       25.48904
    ##  97       25.48904
    ##  98       25.48904
    ##  99       25.48904
    ## 100       25.48904
    ## 101       25.48904
    ## 102       25.48904
    ## 103       25.48904
    ## 104       25.48904
    ## 105       25.48904
    ## 106       25.48904
    ## 107       25.48904
    ## 108       25.48904
    ## 109       25.48904
    ## 110       25.48904
    ## 111       25.48904
    ## 112       25.48904
    ## 113       25.48904
    ## 114       25.48904
    ## 115       25.48904
    ## 116       25.48904
    ## 117       25.48904
    ## 118       25.48904
    ## 119       25.48904
    ## 120       25.48904
    ## 121       25.48904
    ## 122       25.48904
    ## 123       25.48904

``` r
compare.Jan <- left_join(ts.pre, Jan.actual.sale, by = c("mainCategory", "category"))

mse(compare.Jan$y,compare.Jan$yhat)
```

    ## [1] 115204.4
