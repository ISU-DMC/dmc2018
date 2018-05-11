myTimeControl <- trainControl(
                              method = "timeslice",
                              initialWindow = 30,
                              horizon = 28,
                              fixedWindow = TRUE)
alldata <- readRDS("data5_1.rds")
alldata_clean <- alldata %>% dplyr::select(-color.rich, -color.rich_freq,
                                           -pid, -size, -date, -releaseDate) %>%
  mutate(day = as.numeric(day))

library(doParallel)
library(caret)
t1 <- Sys.time()
cl <- makeCluster(4)
registerDoParallel(cl)
test <- train(units ~ ., 
              data = alldata_clean,
              method = 'glmnet', 
              family = 'poisson',
              preProc = c("center","scale"),
              trControl = trainControl(method = "none"),
              tuneGrid = data.frame(alpha=0.263333,lambda=1))
stopCluster(cl)
Sys.time()-t1
