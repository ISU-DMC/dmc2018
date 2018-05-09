Y=read.table('Response.txt')
set.seed(0505)
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/train_Jan.txt'
train<-read.table(filepath,sep='|',header=TRUE)
test <- train %>% filter(ymd(date) >= ymd("2018-01-01"))
train <- train[ymd(train$date)< ymd ('2018-01-01'),]
n=floor(dim(Y)[1]*0.4)
Index=sample(dim(Y)[1],n)
Historic=Y[-Index,]
filepath='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/data_clean/Historical_Jan.txt'
write.csv(Historic,'Historic01.csv', row.names = F)

Historic <- Historic %>% rename(date = Day)
Historic.X <- Historic %>% left_join(train, by = c("pid", "size", "date"))
train.X <- train %>% anti_join(Historic, by = c("pid", "size", "date"))
train.X <- train.X %>% mutate(Sold = NA) %>% select(pid, size, date, Sold, everything())
test <- test %>% mutate(Sold = NA) %>% select(pid, size, date, Sold, everything())

train.X %>% glimpse
test %>% glimpse

all_Jan <- rbind(cbind(Historic.X, group = "historic"),
                 cbind(train.X, group = "train"),
                 cbind(test, group = "test"))
all_Jan <- all_Jan %>% select(-units)
