library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(rgl)

## 加载数据
load("thermoAnalyse.rdata")

#HZNU数据
load("2018-6-6.RData")#HZNU用电
rm(data.yx)
data.running <-
  as.data.table(read.table(
    "/Volumes/Windows 10/R分析_Demo/runningData.csv",
    header = TRUE,
    sep = ","
  ))#HZNU环境

data.running<-data.running[!duplicated(data.running),]

ggplot(data.running[month == "02"], aes(x = hour, y = real_temp))  +
  ylim(5, 35)+ geom_boxplot() # + geom_point(size = 1, position = "jitter",aes(color="blue",alpha=0.3))
ggplot(data.running[month == "02"], aes(x = real_temp))+xlim(30,45)+geom_histogram()
##季节分割
