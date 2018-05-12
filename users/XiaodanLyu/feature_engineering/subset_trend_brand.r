trend_gl <- read.csv("feature_engineering/google trend_Guoliang.csv")
trend_gl %>% filter(category == "shopping", search.type == "web search"|search.type == "websearch") -> trend_gl_br
trend_gl_br %>% group_by(Day, category, search.type) %>% summarise(n = n()) %>% summary

trend_brand <- trend_gl_br %>% dplyr::select(-obs, -category, -search.type) %>% spread(variable, value)
names(trend_brand)[1] <- "date"
trend_brand[trend_brand == "<1"] <- 0
trend_brand_format <- trend_brand %>% mutate_at(vars(adidas:Under.Armour), funs(as.numeric(as.character(.))))
trend_brand_format %>% summary
trend_brand %>% glimpse

trend_brand_format <- trend_brand_format %>% mutate(date = mdy(date)) %>% arrange(date)
trend_brand_format %>% summary
plot(asics~date, type = "l", data = trend_brand_format)

trend_brand_sub <- trend_brand_format %>%
  dplyr::select(date, adidas,asics,Converse,Jako,Jordan,KangaROOS,
                Nike,PUMA,Reebok,Uhlsport,Under.Armour)
colnames(trend_brand_sub)[-1] <- paste0("trend.", colnames(trend_brand_sub)[-1])

trend_brand_sub %>% glimpse

write.table(trend_brand_sub,
            file = "trend_brand.txt", sep = "|", quote = F, row.names = F)
