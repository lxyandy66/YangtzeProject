library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(plyr)
library(timeDate)
library(rJava)
library(xlsx)
library(rgl)

####此处接已处理好使用模式的空调行为数据####
# raw.rawData
raw.rawData$pattern<-"NULL"
#####对不确定及认为错误的空调工况进行确定####
raw.rawData$modiState<-raw.rawData$state
raw.rawData[(month >= 5 &
           month<= 10) &
          (modiState == "other" |
             modiState == "auto" | modiState == "venti")]$modiState <- "cooling"
raw.rawData[(month %in% c(11, 12, 1, 2, 3, 4)) &
          (modiState == "other" |
             modiState == "auto" | modiState == "venti")]$modiState <- "heating"
raw.rawData[runtime == 0]$modiState<-"off"
# 仅用于查看各季节空调工况占比
nn<-raw.rawData[,.(useCount = length(label[runtime > 0]),
                   noneUseCount = length(label[runtime == 0]),
                   heatingCount = length(label[finalState == "heating"]),
                   coolingCount = length(label[finalState == "cooling"])
                   ),by=season]

raw.rawData$finalState<-raw.rawData$modiState
raw.rawData[season=="Summer"&finalState=="heating"]$finalState<-"cooling"
raw.rawData[season=="Winter"&finalState=="cooling"]$finalState<-"heating"

raw.noneOn <- raw.rawData[runtime == 0]
raw.fullOn <- raw.rawData[runtime == 15]
raw.periodOn <- raw.rawData[runtime != 15 & runtime != 0]


####一些预处理####
raw.fullOn$pattern<-"fullUse"#pattern对应聚类所得空调使用模式
raw.noneOn$pattern<-"noneUse"
raw.periodOn$pattern<-"NULL"

#?????需要确定聚类的分类，不同工况分开聚？不同季节分开聚？????
raw.periodOn$splitFactor<-paste(raw.periodOn$season,raw.periodOn$finalState,sep = "_")
list.behaviour.season<-split(raw.periodOn,as.factor(raw.periodOn$splitFactor))
# mapply(FUN = miningBehaviourPattern,list(data=list.behaviour.season,colRange=c(6:20),seasonCol=22))
# mapply(FUN = miningBehaviourPattern,list.behaviour.season,6:20,22)
#mapply有没有什么好方法，传参不对
for(s in unique(raw.periodOn$season)){
  raw.periodOn[season==s]$pattern<-pamk(raw.periodOn[season==s,6:20],
                                        krange = getkSizeBySeason(s),criterion = "ch",usepam = FALSE,critout = TRUE)
}

# list.behaviour.season 数据行为模式聚类已标记，全年模式统一

miningBehaviourPattern<-function(data,colRange,seasonCol){
  pamk.best<-pamk(data[,colRange],krange = getkSizeBySeason(unique(data[,seasonCol])[1]),criterion = "ch",
                  usepam = FALSE, critout = TRUE)
}

getkSizeBySeason<-function(season){
  kSizeBySeason<-array(c("Spring","Summer_warm","Summer","Autumn","Winter_warm","Winter",
                         6,6,4,4,5,3,6),dim = c(6,2))
  return(kSizeBySeason[season,2])
}