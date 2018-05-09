#load("C:/Users/Yan/Desktop/New folder/DMC.Data.Rda")
# DMC.Data <- data.frame(items,sell.freq,sell.history)为DMC.Data的构成,sell.freq为某物品的销售总次数，sell.history为每日累计销量
# sell.freq为第11列，sell.history为第12:134列，最后一列为该物品在四个月里的总累计销量

# ----- 1. 根据销售次数sell.freq选择出感兴趣的物品 -----
Data <- DMC.Data[DMC.Data$sell.freq >= 1 & DMC.Data$sell.freq < 2, 12:134]
#colnames(Data) <- NULL

# ----- 2. 用Wasserstein Distance衡量两个曲线之间的距离，越小越相似 -----
# 可以直接用包“transport”，版本0.9-4，之前的版本会有小问题
len <- dim(Data)[1]
dist <- matrix(rep(0, len*len), ncol = len) # 自己到自己的距离是0
library(transport)  #用Wasserstein距离的包
for(i in 1:(len-1)){
  si <- (Data[i,]/Data[i,123]) 
  for(j in (i+1):len){
    sj <- Data[j,]/Data[j,123]
    dist[j,i] <- wasserstein(pp(cbind(c(1:123),si)), pp(cbind(c(1:123),sj)), p = 1, #参数p代表范数，取1较常见
                             prob = FALSE) 
    dist[i,j] <- dist[j,i]
  }
}
#dist.ge02.lt04 <- dist # 根据给定条件把distance数据记录下来，留着以后用

# ----- 3. Hierarchical clustering，需要产生dissimilarity矩阵，必须用自带的dist()作用到上面算出的dist矩阵，注意重名 -----
library(cluster)
# 预处理distance数据
dist <- matrix(); dist <- dist.01 #导入感兴趣的distance数据
dist <- dist/max(dist); hist(dist) #distance直方图，单峰右偏
dist <- pnorm(dist,mean(dist),sd(dist)) #把distance进行非线性变换，目的为调整分布更均匀，分类时每类多分一些
#dist <- matrix(ecdf(dist)(dist), ncol = dim(dist)[1]) #绝对的均匀分布，但是分类效果不如上面
hist(dist,50) #调整完的distance直方图，双峰，右峰在1，但是中间相对平缓
# 调用hclust函数
curve.cluster <- hclust(dist(dist)) #第一个dist()是包里的函数
plot(hclust(dist(dist)))