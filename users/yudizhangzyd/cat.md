# categroy.data
Yudi Zhang  
4/8/2018  



```r
items <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/items.csv", sep = "|")
View(items)
prices <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/prices.csv", sep = "|")
train <- read.csv("/Users/yudi/Downloads/DMC-2018/raw_data/train.csv", sep = "|")
```



##1. Data.Overview

```r
#Missing values in subCat
apply(items, 2, function(x) sum(is.na(x)))
```

```
##          pid         size        color        brand          rrp 
##            0            0            0            0            0 
## mainCategory     category  subCategory        stock  releaseDate 
##            0            0         1134            0            0
```

```r
#Cat.Overview
table(items$category) %>% pander()
```


------------------------------------------------------------
  2      7     10    16    18    24    30   33    36    37  
------ ------ ----- ----- ----- ----- ---- ----- ----- -----
 3705   5276   873   491   918   193   11   439   244   674 
------------------------------------------------------------

```r
table(items$mainCategory) %>% pander()
```


--------------------
  1      9      15  
------ ------ ------
 9605   2085   1134 
--------------------

```r
table(items$subCategory) %>% pander()
```


---------------------------------------------------------------------------
  3      4     5     6     8    11    12   13    14    16    17   19   20  
------ ----- ----- ----- ----- ----- ---- ----- ----- ----- ---- ---- -----
 2372   326   559   623   725   218   88   460   734   665   96   2    149 
---------------------------------------------------------------------------

Table: Table continues below

 
--------------------------------------------------------------------------------
  21    22    23    25    26   27   28    29   31     32    34   35    38   39  
------ ----- ----- ----- ---- ---- ----- ---- ----- ------ ---- ----- ---- -----
 1082   514   178   510   38   9    124   25   252   1164   4    210   46   434 
--------------------------------------------------------------------------------

Table: Table continues below

 
------------------------
 40   41   42   43   44 
---- ---- ---- ---- ----
 35   7    21   14   6  
------------------------

```r
#Only mainCat 1 and 9 have a same cat 37
table(items$category, items$mainCategory) %>% pander()
```


---------------------------
 &nbsp;    1      9    15  
-------- ------ ----- -----
 **2**    3705    0     0  

 **7**    5276    0     0  

 **10**    0     873    0  

 **16**    0      0    491 

 **18**    0     918    0  

 **24**    0      0    193 

 **30**    0      0    11  

 **33**    0      0    439 

 **36**    0     244    0  

 **37**   624    50     0  
---------------------------

```r
#mainCat 15 doesn't have subcat
table(items$subCategory, items$mainCategory) %>% pander()
```


---------------------------
 &nbsp;    1      9     15 
-------- ------ ------ ----
 **3**    2372    0     0  

 **4**    326     0     0  

 **5**    559     0     0  

 **6**    623     0     0  

 **8**    725     0     0  

 **11**    89    129    0  

 **12**    88     0     0  

 **13**   460     0     0  

 **14**   533    201    0  

 **16**   665     0     0  

 **17**    96     0     0  

 **19**    0      2     0  

 **20**   149     0     0  

 **21**   1082    0     0  

 **22**   385    129    0  

 **23**   173     5     0  

 **25**   347    163    0  

 **26**    38     0     0  

 **27**    9      0     0  

 **28**   124     0     0  

 **29**    10     15    0  

 **31**   252     0     0  

 **32**    0     1164   0  

 **34**    4      0     0  

 **35**    0     210    0  

 **38**    0      46    0  

 **39**   434     0     0  

 **40**    35     0     0  

 **41**    7      0     0  

 **42**    0      21    0  

 **43**    14     0     0  

 **44**    6      0     0  
---------------------------





```r
train %>% group_by(pid, size) %>% summarise(tot = sum(units)) -> totalsale
inner_join(items, totalsale) -> dat.for.ana
```

```
## Joining, by = c("pid", "size")
```

```r
#Sales units by maincat, subcat and categories
dat.for.ana %>% group_by(category, subCategory, mainCategory) %>% summarise(tot.sale=sum(tot)) %>% ggplot(aes(x = as.factor(subCategory),y = tot.sale,col = as.factor(category), shape = as.factor(mainCategory))) + geom_point() + labs(y='Sales Units in the Past Months', x="subCategory") + scale_colour_discrete(name  ="category") + scale_shape_discrete(name  ="mainCategory") 
```

![](1st_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


