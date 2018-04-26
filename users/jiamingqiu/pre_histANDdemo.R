# For Model --------history + demo info---------------------
# to prepare demo info for embedding,
# say, different variable get different set of indices.
# and I decided to feed every thing
# even NA is subCategory
# but not pid for the moment, it will inflate # of para

dat.info.recode = data.frame(pid = as.integer(as.character(dat.info$pid)))
range.recode = list(pid = range(dat.info.recode$pid))

tm = dat.info$size %>% as.character
length(unique(tm))
range.recode
range.recode$size = c(1,length(unique(tm)))
tm.idx = 0+(1:length(unique(tm)))
dat.info.recode$size = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))

tm = dat.info$color %>% as.character
length(unique(tm))
range.recode
tm.idx = 200+(1:length(unique(tm)))
range.recode$color = range(tm.idx)
range.recode
dat.info.recode$color = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
names(dat.info)

tm = dat.info$brand %>% as.character
length(unique(tm))
range.recode
tm.idx = 250+(1:length(unique(tm)))
range.recode$brand = range(tm.idx)
range.recode
dat.info.recode$brand = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)


tm = dat.info$mainCategory %>% as.character
length(unique(tm))
range.recode
tm.idx = 290+(1:length(unique(tm)))
range.recode$mainCategory = range(tm.idx)
range.recode
dat.info.recode$mainCategory = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$category)
length(unique(tm))
range.recode
tm.idx = 300+(1:length(unique(tm)))
range.recode$category = range(tm.idx)
range.recode
dat.info.recode$category = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$subCategory)
tm[is.na(tm)] = 1
length(unique(tm))
range.recode
tm.idx = 320+(1:length(unique(tm)))
range.recode$subCategory = range(tm.idx)
range.recode
dat.info.recode$subCategory = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)

tm = as.character(dat.info$releaseDate)
length(unique(tm))
range.recode
tm.idx = 400+(1:length(unique(tm)))
range.recode$releaseDate = range(tm.idx)
range.recode
dat.info.recode$releaseDate = as.integer(as.character(factor(tm, levels = sort(unique(as.character(tm))), labels = tm.idx)))
head(dat.info.recode)
names(dat.info)
dat.info.recode$rrp = dat.info$rrp
dat.info.recode$stock = dat.info$stock
dat.info.recode$key = dat.info$key
summary(dat.info.recode)

# select those in dat.sale.train
dat.info.recode.train = dat.info.recode[is.element(dat.info.recode$key, dat.sale.train$key),]
dim(dat.info.recode.train)
