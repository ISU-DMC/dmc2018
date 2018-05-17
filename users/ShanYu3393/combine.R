filepath2='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/ConditionalMedian_C2.csv'
filepath1_3='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/ConditionalMedian_C3.csv'
filepath4='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/ConditionalMedian_C4.csv'

cluster2=read.csv(filepath2,sep='|') 
cluster1_3=read.csv(filepath1_3,sep='|')  
cluster4=read.csv(filepath4,sep='|') 

All=rbind(cluster2,cluster1_3,cluster4) 

filepath_yu='/Users/shanyu/Dropbox/DMC/dmc2018/users/XiaodanLyu/FinalResults/yuchen_predict.txt'
yuchen = read.table(filepath_yu,sep='|',header = TRUE)
yuchen2 <- yuchen %>% mutate(size = gsub("\\t", ",", size))
anti_join(yuchen2, All, by = c("pid", "size")) %>% dim

All2 <- All %>% left_join(yuchen2,by=c('pid','size')) %>% select(-stock)
anti_join(All2, item, by = c("pid", "size")) %>% dim

All2$soldOutDate[is.na(All2$soldOutDate)] <- All2$predictDate[is.na(All2$soldOutDate)]
All2$predictDate=NULL

plot(table(All2$soldOutDate))
sum(is.na(All2$soldOutDate))

write.table(All2,'/Users/shanyu/Dropbox/DMC/dmc2018/predictions/Uni_State_Iowa_1.csv',
            sep="|", row.names = F, quote = F)
