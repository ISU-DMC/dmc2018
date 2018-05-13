## read cluster results ####
## hengfang_cluster_allproducts_group5
# cluster_hf <- read.table("cluster_distance/cluster_hengfang.txt", sep = "|", header = T)
cluster_hf <- read_rds("../hengfang/Cluster_Indicator_4_to_6_Specific.rds")
cluster_hf <- cluster_hf %>% mutate(size = replace(size, size == "", "42"))
cluster_hf %>% glimpse
table(cluster_hf$Cluster_5)

data5 <- cluster_hf %>% filter(Cluster_5 == 1) %>%
  left_join(alldata_expand_date, by = c("pid", "size")) %>%
  dplyr::select(-size1, -size2, -size3) %>%
  dplyr::select(-Cluster_4, -Cluster_5, -Cluster_6) %>% filter(date < ymd("2018-02-01"))
## check number of products in selected group
data5 %>% dplyr::select(pid, size) %>% unique %>% dim

## cluster_freq4_group9
# filename <- "cluster_distance/cluster_yan_freq4_group9.RDS"
# filename <- "cluster_distance/cluster_hengfang_freq4.rds"
# cluster <- read_rds(filename)
# cluster <- cluster %>% mutate(size = replace(size, size == "", "42"))
# names(cluster)[3] <- "group9"
# cluster %>% glimpse
# table(cluster$group9)
# train <- cluster %>% filter(group9 == 1) %>%
#   left_join(alldata, by = c("pid", "size")) %>%
#   mutate()
#   dplyr::select(-size1, -size2, -size3, -group9) %>% 
#   filter(date < ymd("2018-02-01"))
# ## check number of products in selected group
# train %>% dplyr::select(pid, size) %>% unique %>% dim
