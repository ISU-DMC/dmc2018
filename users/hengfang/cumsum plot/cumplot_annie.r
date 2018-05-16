setwd("~/my_git/dmc2018/users/hengfang/")
cluster_hf <- read_rds("Cluster_Indicator_4_to_6_Specific.rds")
cluster_hf <- cluster_hf %>%
  mutate(size = replace(size, size == "", "42")) %>%
  mutate(key = paste(pid, size, sep = " - "))
lagunits_wide <- read_rds("~/dmc_2018/lagunits_wide.rds")
lagunits_long <- lagunits_wide %>% gather(date, lagunits, -key) %>% 
  full_join(cluster_hf, by = "key")
lagunits_long %>% select(pid, size) %>% unique %>% dim
lagunits_long <- lagunits_long %>% mutate(date = ymd(date))

lagunits_long %>% glimpse

library(ggplot2)
ggplot(lagunits_long %>% filter(Cluster_4 == 1), aes(x = date, y = lagunits, color = key)) +
  geom_line() + guides(color = F) + labs(title = "Cluster4_1")

ggplot(lagunits_long %>% filter(Cluster_4 == 2), aes(x = date, y = lagunits, color = key)) +
  geom_line() + guides(color = F) + labs(title = "Cluster4_2")

ggplot(lagunits_long %>% filter(Cluster_4 == 3), aes(x = date, y = lagunits, color = key)) +
  geom_line() + guides(color = F) + labs(title = "Cluster4_3")

ggplot(lagunits_long %>% filter(Cluster_4 == 4), aes(x = date, y = lagunits, color = key)) +
  geom_line() + guides(color = F) + labs(title = "Cluster4_4")
