test_Jan <- read.table("data_clean/test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)

## error for random guess "2018-01-16"
(test_Jan$soldOutDate - ymd("2018-01-16")) %>% abs %>% sum %>% as.numeric %>% sqrt

## MLR modeling daily sale
train_Jan <- read.table("data_clean/train_Jan.txt", sep = "|", header = T)
items_Jan <- read.table("data_clean/items_Jan.txt", sep = "|", header = T)

train_Jan$date <- ymd(train_Jan$date)
data_Jan <- train_Jan %>% filter(date < ymd("2018-01-01"))
