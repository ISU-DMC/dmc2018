library(ggplot2)
library(caret)
library(lubridate)
library(reshape2)
library(data.table)

prices <- read.csv("prices.csv", sep = "|")
items <- read.csv("items.csv", sep = "|")
train <- read.csv("train.csv", sep = "|")

# separate dates (123 days, last date: 01/31/18)
train$year <- year(ymd(train$date))
train$month <- month(ymd(train$date))
train$day <- day(ymd(train$date))
train$weekday <- weekdays(ymd(train$date))

# translate to english
levels(items$color) <- c("beige", "blue", "brown", "yellow", "gold", 
                         "gray", "green", "khaki", "purple", "orange", 
                         "dark_pink", "pink", "red", "black", "silver", 
                         "turquoise", "white")


# chosen: Color 
color <- data.frame(table(items$color))
colnames(color) <- c("color", "frequency")


# 17 colors in total
# there are 4 major colors: black, blue, white and red
# 4 submajor: grey, green, gold and orange
ggplot(items, aes(color)) + geom_bar()
color[order(color$frequency, decreasing = TRUE),]



# merge datasets:
detailed_train <- merge(items, train, by = c("pid", "size"))

# extract_per_month <- function(m) {
#    m10 <- detailed_train[detailed_train$month == m,]
#    return(data.frame(sold_oct = tapply(m10$units, m10$color, sum)))
# }

m10 <- detailed_train[detailed_train$month == 10,]
s10 <- data.frame(sales_oct = tapply(m10$units, m10$color, sum))
m11 <- detailed_train[detailed_train$month == 11,]
s11 <- data.frame(sales_nov = tapply(m11$units, m11$color, sum))
m12 <- detailed_train[detailed_train$month == 12,]
s12 <- data.frame(sales_dec = tapply(m12$units, m12$color, sum))
m01 <- detailed_train[detailed_train$month == 01,]
s01 <- data.frame(sales_jan = tapply(m01$units, m01$color, sum))
sales_by_col <- cbind(s10, s11, s12, s01)


# relationship with sales
# sold units by color per month
sales_by_col[order(sales_by_col$sales_oct, decreasing = TRUE),]
 

# Items sold by color in different months 
# boxplots did not work because of the small values and outliers
# ggplot(m10, aes(x = color, y = units)) + geom_boxplot() 
ggplot(m10, aes(x = "", fill = color)) + geom_bar() + coord_polar("y") + 
    theme_bw() + ggtitle("Sales by color in October")
ggplot(m11, aes(x = "", fill = color)) + geom_bar() + coord_polar("y") +
    theme_bw() + ggtitle("Sales by color in September")
ggplot(m12, aes(x = "", fill = color)) + geom_bar() + coord_polar("y") +
    theme_bw() + ggtitle("Sales by color in December")
ggplot(m01, aes(x = "", fill = color)) + geom_bar() + coord_polar("y") +
    theme_bw() + ggtitle("Sales by color in January")


# relation with the other categorical variables:
table(items$color, items$brand)

## some brands have only one of the 4 major colors in stock
## (for unique product). Adidas has the largest number of color variations.
## Nike is second, then PUMA and Jako.






















