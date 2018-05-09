data5.3$size1<-NULL
data5.3$size2<-NULL
data5.3$size3<-NULL

model.matrix(units~., data = data5.3 %>% select(-pid, -size, -date, -releaseDate)) -> matrix_x


matrix_x<-data.matrix(matrix_feature[,-c(1:4,10)])
xgb<-xgboost(data=matrix_x,label=matrix_feature$units,max_depth=40,eval_metric="rmse",
             subsample=0.6,eta=0.3,objective="count:poisson",nrounds = 80)

importance <- xgb.importance(colnames(matrix_x), model = xgb)  
head(importance)  
xgb.plot.importance(importance_matrix = importance,top_n = 20)  