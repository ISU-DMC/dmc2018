rm(list = ls())

source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Loss_function.R')
source('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/generate_soldoutday.R')

alltrain <- readRDS('/Users/shanyu/Dropbox/DMC/dmc2018/users/ShanYu3393/Binary_kNN_5_3.rds')

True <- Stock_Soldoutday(alltrain[,1:3],alltrain[,4])

Eror=sqrt(sum(abs(Loss_MAE(alltrain[,5],alltrain[,1:3],True$stock,
              True$SoldOutDay,'geom'))))
