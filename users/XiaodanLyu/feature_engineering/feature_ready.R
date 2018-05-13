rm(list = ls(all = T))
## repsonse = units ####
# filetonodummy <- "/vol/data/zhuz/lyux/feature_rds/all_features_may10.rds"

## response = waiting_times ####
filetonodummy <- "/vol/data/zhuz/lyux/feature_rds/WT_all_features_may12.rds"
## delete some categorical variables not to be dummied ####
alldata_expand_date <- read_rds(filetonodummy)
format.df <- sapply(alldata_expand_date, class) %>% data.frame
names.char <- format.df %>% mutate(var = rownames(format.df)) %>% rename(type = ".") %>%
  filter(type == "character") %>% dplyr::select(var) %>% unlist %>% unname
names.pick <- c("size", "size4", "size.shape", "size.body", "mainCategory", "category", "subCategory",
                "color", "color.coldorwarm", "pid.cut", "brand", "brand.NBA.team",
                "relweekday", "relmonthweek", "stock.cut",
                # "price.cut.FS29",
                "stock.units.cut", "date.wd", "date.wm")
names.out <- names.char[-match(names.pick, names.char)]

alldata_thin <- alldata_expand_date %>% dplyr::select(-one_of(names.out))
sapply(alldata_thin, class) %>% data.frame
alldata_thin %>% dim

## response = units ####
# alltrain <- data.frame(units = alldata_thin$units, 
#                        model.matrix(units~., data = alldata_thin %>% dplyr::select(-pid, -size, -date, -releaseDate)))
# filetoalltrain <- "/vol/data/zhuz/lyux/feature_rds/alltrain.rds"

label <- alldata_expand_date %>% dplyr::select(pid, size, startdate, Rcr) %>%
  filter(Rcr == 0) %>% select(-Rcr)
write_rds(label, "/vol/data/zhuz/lyux/feature_rds/WT_alllabel.rds")

## response = waiting_times ####
## version 1, no right-censored
train_nodummy <- alldata_thin %>% dplyr::select(-size, -startdate, -releaseDate) %>%
  filter(Rcr == 0) %>% select(-Rcr)
alltrain <- data.frame(time = train_nodummy$wait_time,
                       model.matrix(wait_time~., data = train_nodummy))
alltrain %>% dim
filetoalltrain <- "/vol/data/zhuz/lyux/feature_rds/WT_alltrain.rds"

readr::write_rds(alltrain, filetoalltrain)
