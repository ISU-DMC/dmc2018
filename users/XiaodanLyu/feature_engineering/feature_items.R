dec2frac <- function(chr){
  chr2 <- chr %>% as.numeric %>% as.fractions %>% as.character %>% 
    strsplit(split = "/") %>% unlist %>% as.numeric()
  frac <- paste0(chr2[1] %/% chr2[2], " ", chr2[1] %% chr2[2], "/", chr2[2])
  return(frac)
}

dec2frac(14/3)
dec2frac("4.666667")

library(readxl)
color_relabel <- read_excel("fct_relabel.xlsx", sheet = "color")
brand_relabel <- read_excel("fct_relabel.xlsx", sheet = "brand")
size_relabel <- read_excel("fct_relabel.xlsx", sheet = "size", col_types = "text")
id.dec <- grepl("\\.", size_relabel$old_levels)
size_relabel$old_levels[id.dec] <- sapply(size_relabel$old_levels[id.dec], dec2frac) %>% unname

items_expand <- items %>%
  mutate(size = replace(size, size == "", "42"),
         subCategory = replace(subCategory, is.na(subCategory), "0")) %>% 
  ## color
  left_join(color_relabel, by = c("color" = "old_levels")) %>% dplyr::select(-color.eng) %>% 
  ## brand
  left_join(brand_relabel, by = c("brand" = "old_levels")) %>%
  ## size
  left_join(size_relabel, by = c("size" = "old_levels")) %>%
  mutate_at(vars(color.rich, brand.NBA.team:brand.competition),
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
    relday = day(releaseDate),
    relweekday = weekdays(releaseDate),
    relmonthweek = ceiling((day(releaseDate) + first_day_of_month_wday(releaseDate) - 1) / 7),
    ## two-way interaction
    main.ctg = paste(mainCategory, category, sep = "-"),
    ctg.subctg = paste(category, subCategory, sep = "-")
  )

which(apply(items_expand, 2, function(x) sum(is.na(x))) > 0)
items_expand %>% glimpse

items_expand_format <- items_expand %>% mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(pid, size), as.character) %>% 
  mutate_at(vars(mainCategory, category, relday, relmonthweek), as.factor)

items_expand_format %>% summary
sapply(items_expand_format, class) %>% data.frame


codebook_freq <- codebook(items_expand_format, response = "")
codebook_freq$name.feature %>% unique
any(is.na(codebook_freq$value.feature))

freq_feature <- feature(items_expand_format, codebook_freq)
any(is.na(freq_feature))
glimpse(freq_feature)

write_rds(freq_feature, "item_static_features.rds")
