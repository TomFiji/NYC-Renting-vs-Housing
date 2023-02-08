library("magrittr")
library("tidyverse")
library("data.table")
library("ggplot2")
rIncome.df <- read.csv("Renter Income.csv")
newIncome.df <- rIncome.df
row.names(newIncome.df)<- newIncome.df[,2]
newIncome.df <- newIncome.df[,-c(1:2)]
avg <- rowMeans(newIncome.df)
rFinal.df<-cbind(newIncome.df, avg)

rBurden.df<- read.csv("Renter Burden.csv")
newBurden.df<-rBurden.df
row.names(newBurden.df)<- newBurden.df[,2]
newBurden.df <- newBurden.df[,-c(1:2)]
avg2 <- rowMeans(newBurden.df)
rFinal.df<-cbind(newIncome.df, avg,avg2)
rFinal.df<-rFinal.df[,-c(1:14)]
colnames(rFinal.df)<-c('Avg_Renter_Income', 'Avg_Rent_Burden')
renterNorm.df<-data.frame(scale(rFinal.df))
kclusters<- kmeans(renterNorm.df, 3)
kclusters$cluster
rCluster.df<- data.frame(kclusters$cluster)
rFinal.df<- data.frame(rFinal.df, cluster=kclusters$cluster)
kclusters$centers
kclusters$withinss

