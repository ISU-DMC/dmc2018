source("feature_engineering/help.r")

library(MASS)
library(dplyr)
library(lubridate)
dec2frac <- function(chr){
  chr2 <- chr %>% as.numeric %>% fractions %>% as.character %>% 
    strsplit(split = "/") %>% unlist %>% as.numeric()
  frac <- paste0(chr2[1] %/% chr2[2], " ", chr2[1] %% chr2[2], "/", chr2[2])
  return(frac)
}

dec2frac(14/3)
dec2frac("4.666667")

items <- read.csv("../../data/items.csv", sep = "|")

library(readxl)
color_relabel <- read_excel("feature_engineering/fct_relabel.xlsx", sheet = "color")
brand_relabel <- read_excel("feature_engineering/fct_relabel.xlsx", sheet = "brand")
size_relabel <- read_excel("feature_engineering/fct_relabel.xlsx", sheet = "size")
id.dec <- grepl("\\.", size_relabel$old_levels)
size_relabel$old_levels[id.dec] <- sapply(size_relabel$old_levels[id.dec], dec2frac) %>% unname
## units used here!!!
product_relabel <- read.table("feature_engineering/stock_units_cut.txt", sep = "|", header = TRUE)
product_relabel <- product_relabel %>% mutate(size = gsub("\t", ",", size))

items_expand <- items %>%
  left_join(product_relabel, by = c("pid", "size")) %>%
  mutate(size = replace(size, size == "", "42"),
         subCategory = replace(subCategory, is.na(subCategory), category[is.na(subCategory)])) %>% 
  ## color
  left_join(color_relabel, by = c("color" = "old_levels")) %>% dplyr::select(-color.eng) %>% 
  ## brand
  left_join(brand_relabel, by = c("brand" = "old_levels")) %>%
  ## size
  left_join(size_relabel, by = c("size" = "old_levels")) %>%
  mutate_at(vars(brand.NBA.team:brand.competition),
            function(x) ifelse(x == 1, "Yes", "No")) %>%
  mutate(
    ## cut pid
    pid.cut = cut(as.numeric(as.character(pid)),
                  breaks = c(10000, 12938, 17662, 20896, 22881),
                  labels = c("[10000, 12938]", "(12938, 17662]", "(17662, 20896]", "(20896, 22881]"),
                  include.lowest = T),
    ## cut stock
    stock.cut = cut(stock,
                    breaks = c(0, 2.5, 4.5, 14.5, 38.5, 64.5, Inf),
                    include.lowest = T),
    ## repleaseDate
    releaseDate = ymd(releaseDate),
    NewRelease = ifelse(releaseDate==ymd("2017-10-01"), "No", "Yes"),
    relday = (releaseDate - ymd("2017-09-30")) %>% as.numeric,
    relweekday = weekdays(releaseDate),
    relmonthweek = ceiling((day(releaseDate)+first_day_of_month_wday(releaseDate)-1)/7) %>% as.character
    # main.subctg = paste(mainCategory, subCategory, sep = "-"),
    # ctg.subctg = paste(category, subCategory, sep = "-")
  )

which(apply(items_expand, 2, function(x) sum(is.na(x))) > 0)
items_expand %>% glimpse

items_expand_format <- items_expand %>% mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(pid, size), as.character) %>% 
  mutate_at(vars(mainCategory, category, subCategory), as.factor)

items_expand_format %>% summary
sapply(items_expand_format, class) %>% data.frame

id.fac <- sapply(items_expand_format, is.factor) %>% which
(names.color <- grep("color", colnames(items_expand_format)[id.fac], value = T))
(names.brand <- grep("brand", colnames(items_expand_format)[id.fac], value = T))
(names.category <- grep("cat", colnames(items_expand_format)[id.fac], value = T, ignore.case = TRUE))
(names.size <- grep("size", colnames(items_expand_format)[id.fac], value = T))
(names.rel <- grep("rel", colnames(items_expand_format)[id.fac], value = T))

names.all <- c(names.color, names.brand, names.category, names.size, names.rel)
names.twoway <- combn(names.all, 2) %>% t()
names.self <- rbind(expand.grid(names.color, names.color),
                    expand.grid(names.brand, names.brand),
                    expand.grid(names.category, names.category),
                    expand.grid(names.size, names.size),
                    expand.grid(names.rel, names.rel))
names2way <- data.frame(names.twoway) %>% 
  anti_join(names.self, by = c("X1" = "Var1", "X2" = "Var2")) %>%
  mutate_if(is.factor, as.character)

twoway_features <- data.frame(id = 1:nrow(items))
for(i in 1:nrow(names2way)){
  twoway_features <- cbind(twoway_features,
                           paste(items_expand_format[,names2way[i,1]],
                                 items_expand_format[,names2way[i,2]], sep = "&"))
  colnames(twoway_features)[i+1] <- paste(names2way[i,1], names2way[i,2], sep = "X")
}

items_Jumbo <- cbind(items_expand_format, twoway_features %>% dplyr::select(-id))
items_Jumbo %>% glimpse
any(is.na(items_Jumbo))

codebook_freq <- codebook(items_Jumbo, response = "")
codebook_freq$name.feature %>% unique
any(is.na(codebook_freq$value.feature))

freq_feature <- feature(items_Jumbo, codebook_freq)
any(is.na(freq_feature))
dim(freq_feature)

readr::write_rds(freq_feature, "/vol/data/zhuz/lyux/feature_rds/item_static_features_may10.rds")
