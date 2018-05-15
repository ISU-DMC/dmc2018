units_wide <- read_rds("~/dmc_2018/units_wide.rds")
units_wide %>% mutate(total = rowSums(units_wide[,-1]),
                      id.order = rank(total)) %>%
  select(key, total) -> units_total
units_total %>% glimpse

rm(list = ls(all = T))
alltrain_freq4 <- read.table("/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_subfeatures_may14.txt",
                             sep = "|", header = T)

annie.hat.outlier.detect <- function(numCluster){
  ID.out <- c()
  for(i in 1:numCluster){
    alltrain_freq4_X <- alltrain_freq4 %>% filter(get(paste0("Cluster_", numCluster)) == i)
    # data_MLR <- alltrain_freq4_X %>% 
    #   select(units, price, id, discount, stock,
    #          trend.11teamsporting43, trend.amazon,
    #          trend.coupon, trend.clearance
    #          )
    ## principle components
    # library(pls)
    var.out <- names(alltrain_freq4_X)[apply(alltrain_freq4_X, 2, function(x) length(unique(x)) == 1)]
    alltrain_freq4_X <- alltrain_freq4_X %>% select(-one_of(var.out))
    pc.all <- princomp(alltrain_freq4_X %>% select(-pid, -size, -date, -units, -contains("Cluster_")), cor=TRUE)
    summary(pc.all)
    # loadings(pc.all)
    X <- data.frame(pc.all$scores[,1:30])
    data_MLR <- data.frame(units = alltrain_freq4_X$units, X)
    out <- lm(units~., data = data_MLR)
    sprintf("r.squared: %.5f", summary(out)$r.squared)
    sprintf("rmse: %.5f", summary(out)$sigma^2)
    # print(anova(out))
    # imI <- influence.measures(out)
    # sum(imI$is.inf)
    
    hat <- hatvalues(out)
    n <- nrow(data_MLR)
    k <- ncol(data_MLR)
    out.row <- which(hat > (3 * k)/n)
    
    ID.out <- rbind(ID.out, alltrain_freq4_X[out.row, c("pid", "size", "date", "units")])
  } 
  ID.out
}

table(ID.out$units)

ID.out <- cbind(annie.hat.outlier.detect(5), cluster = "Cluster_5")
# ID.out <- rbind(cbind(annie.hat.outlier.detect(5), cluster = "Cluster_5"),
#                 cbind(annie.hat.outlier.detect(9), cluster = "Cluster_9"))

ID.out %>% glimpse()
ID.out$cluster %>% table

write_rds(ID.out, "/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_cluster5_pcr_outlier_ID.rds")

write_rds(ID.out, "/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_outlier_ID.rds")
write.table(ID.out, "/vol/data/zhuz/lyux/feature_rds/alltrain_freq4_outlier_ID.txt",
            sep = "|", quote = F, row.names = F)
