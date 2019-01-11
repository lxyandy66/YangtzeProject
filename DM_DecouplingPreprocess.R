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

########行为模式解耦预处理########
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

####开关状态统计为单个房间####
raw.rawData$roomCode<-substr(raw.rawData$ac_code,1,13)

raw.noneOn <- raw.rawData[runtime == 0]
raw.fullOn <- raw.rawData[runtime == 15]
raw.periodOn <- raw.rawData[runtime != 15 & runtime != 0]

####一些预处理####
raw.fullOn$pattern<-"fullUse"#pattern对应聚类所得空调使用模式
raw.noneOn$pattern<-"noneUse"
raw.periodOn$pattern<-"NULL"

raw.rawData$labelRoomDay<-paste(raw.rawData$roomCode,raw.rawData$date,sep = "_")
raw.rawData[runtime == 0]$pattern<-"noneUse"
raw.rawData[runtime == 15]$pattern<-"fullUse"
raw.rawData[runtime != 15 & runtime != 0]$pattern<-"periodUse"

####异常数据的去除####
#如夏季制热冬季制冷
raw.rawData$removeFlag<-FALSE
raw.rawData[(season=="Summer"|season=="Summer_warm")&finalState=="heating"]$removeFlag<-TRUE
raw.rawData[(season=="Winter"|season=="Winter_warm")&finalState=="cooling"]$removeFlag<-TRUE

####空调使用记录转为日内房间级使用记录####
data.hznu.use.room.day<-raw.rawData[removeFlag==FALSE,.(date=date[1],
                                       roomCode=roomCode[1],
                                       acCount=length(unique(ac_code)),
                                       finalState=ifelse(length(finalState[finalState!="off"])==0,"off",
                                                         getMode(finalState[finalState!="off"])),
                                       #注意需要特殊处理，否则用unique会存在finalState为off/heating 或者 cooling/heating的问题没有解决
                                       season=season[1],
                                       h8=ifelse(sum(h1)>0,1,0),
                                       h9=ifelse(sum(h2)>0,1,0),
                                       h10=ifelse(sum(h3)>0,1,0),
                                       h11=ifelse(sum(h4)>0,1,0),
                                       h12=ifelse(sum(h5)>0,1,0),
                                       h13=ifelse(sum(h6)>0,1,0),
                                       h14=ifelse(sum(h7)>0,1,0),
                                       h15=ifelse(sum(h8)>0,1,0),
                                       h16=ifelse(sum(h9)>0,1,0),
                                       h17=ifelse(sum(h10)>0,1,0),
                                       h18=ifelse(sum(h11)>0,1,0),
                                       h19=ifelse(sum(h12)>0,1,0),
                                       h20=ifelse(sum(h13)>0,1,0),
                                       h21=ifelse(sum(h14)>0,1,0),
                                       h22=ifelse(sum(h15)>0,1,0)
                                       ),by=labelRoomDay]
data.hznu.use.room.day$runtime<-apply(data.hznu.use.room.day[,c(7:21)],1,sum)#计算总使用时间
data.hznu.use.room.day$basePattern<-ifelse(data.hznu.use.room.day$runtime==0,"noneUse",
                                           ifelse(data.hznu.use.room.day$runtime==15,"fullUse","periodUse"))
data.hznu.use.room.day$month<-substr(data.hznu.use.room.day$date,6,7)
summary.room.finalUse<-data.hznu.use.room.day[,.(
  useCount = length(labelRoomDay[runtime > 0]),
  noneUseCount = length(labelRoomDay[runtime == 0]),
  heatingCount = length(labelRoomDay[finalState == "heating"]),
  coolingCount = length(labelRoomDay[finalState == "cooling"]),
  modiSeason=season[1]
),by=month]

#将春秋季合并为过渡季
data.hznu.use.room.day$modiSeason<-data.hznu.use.room.day$season
data.hznu.use.room.day[modiSeason=="Spring"|modiSeason=="Autumn"]$modiSeason<-"Transition"
data.hznu.use.room.day.period<-data.hznu.use.room.day[runtime>0&runtime<15]

#?????需要确定聚类的分类，不同工况分开聚？不同季节分开聚？????
list.hznu.room.use<-split(data.hznu.use.room.day.period,
                          f=as.factor(paste(data.hznu.use.room.day.period$finalState,
                                            data.hznu.use.room.day.period$modiSeason,sep = "_")))
####行为再聚类####
####试聚类####
#聚类评估
modeSelect<-"cooling_Summer"
data.use.room.tryCluster<-list.hznu.room.use[[modeSelect]]
wssClusterEvaluate(data = data.use.room.tryCluster[, 7:22],
                   maxIter = 1000,
                   maxK = 15)
pamkClusterEvaluate(
  data = data.use.room.tryCluster[, 7:22],#8-22时+runtime
  criter = "multiasw",
  startK = 2,
  endK = 10
)
#试聚类
kSize <- 3
pamk.best <-
  pamk(
    data.use.room.tryCluster[, 7:22],
    krange = kSize,
    criterion = "ch",
    usepam = FALSE,
    critout = TRUE
  )#注意有缺失值的聚类结果将会是NA
data.use.room.tryCluster$cluster<-pamk.best$pamobject$clustering
summary.use.room.cluster<-data.use.room.tryCluster[,.(
  count=length(labelRoomDay),
  runtime=mean(runtime),
  h8=mean(h8),
  h9=mean(h9),
  h10=mean(h10),
  h11=mean(h11),
  h12=mean(h12),
  h13=mean(h13),
  h14=mean(h14),
  h15=mean(h15),
  h16=mean(h16),
  h17=mean(h17),
  h18=mean(h18),
  h19=mean(h19),
  h20=mean(h20),
  h21=mean(h21),
  h22=mean(h22)
),by=cluster]
summary.use.plot.cluster<-melt(summary.use.room.cluster)
ggplot(data=summary.use.room.cluster,aes)
# list.behaviour.season 数据行为模式聚类已标记，全年模式统一



####热环境预处理数据处理####
#接data.analyse.all
data.hznu.thermo.room.day<-data.table(labelDay=data.analyse.all$labelDay,date_time=data.analyse.all$date_time,
                                room_code=data.analyse.all$room_code,season=data.analyse.all$season,
                                h8=(data.analyse.all$h08.00+data.analyse.all$h08.30)/2,
                                h9=(data.analyse.all$h09.00+data.analyse.all$h09.30)/2,
                                h10=(data.analyse.all$h10.00+data.analyse.all$h10.30)/2,
                                h11=(data.analyse.all$h11.00+data.analyse.all$h11.30)/2,
                                h12=(data.analyse.all$h12.00+data.analyse.all$h12.30)/2,
                                h13=(data.analyse.all$h13.00+data.analyse.all$h13.30)/2,
                                h14=(data.analyse.all$h14.00+data.analyse.all$h14.30)/2,
                                h15=(data.analyse.all$h15.00+data.analyse.all$h15.30)/2,
                                h16=(data.analyse.all$h16.00+data.analyse.all$h16.30)/2,
                                h17=(data.analyse.all$h17.00+data.analyse.all$h17.30)/2,
                                h18=(data.analyse.all$h18.00+data.analyse.all$h18.30)/2,
                                h19=(data.analyse.all$h19.00+data.analyse.all$h19.30)/2,
                                h20=(data.analyse.all$h20.00+data.analyse.all$h20.30)/2,
                                h21=(data.analyse.all$h21.00+data.analyse.all$h21.30)/2,
                                h22=(data.analyse.all$h22.00+data.analyse.all$h22.30)/2,
                                isWorkday=data.analyse.all$isWorkday)
data.hznu.thermo.room.day$date<-paste("20",substr(data.hznu.thermo.room.day$date_time,3,10),sep = "")
# data.hznu.thermo.room.day<-data.hznu.thermo.room.day[,-"labelDay"]
data.hznu.thermo.room.day$labelRoomDay<-paste(data.hznu.thermo.room.day$room_code,
                                              data.hznu.thermo.room.day$date,sep = "_")
rm(data.analyse.all)


#### 最终为 房间小时 行为/温度/能耗 #####
# 此处标签已统一，统一为labelRoomDay：房间号-年-月-日，如330100D251101_2017-03-27
#将使用行为标签合并至热环境数据集
data.hznu.thermo.room.day<-
  merge(x=data.hznu.thermo.room.day,y=data.hznu.use.room.day[,c("labelRoomDay","finalState","basePattern")],
        by = "labelRoomDay",all.x = TRUE)
# nn<-data.hznu.thermo.room.day[which(rowSums(is.na(data.hznu.thermo.room.day))>0),]#行为finalState及basePattern缺失43236条

data.hznu.energy.room.day<-
  merge(x=data.hznu.energy.room.day,y=data.hznu.use.room.day[,c("labelRoomDay","basePattern")],
        by = "labelRoomDay",all.x = TRUE)
data.hznu.energy.room.day[sumElec==0]$basePattern<-"noneUse"
data.hznu.energy.room.day$zeroCount<-apply(data.hznu.energy.room.day[,c(6:20)],1,function(x){sum(x==0)})
data.hznu.energy.room.day[is.na(basePattern)]$basePattern<-ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==15,"noneUse",
                                                                  ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==0,"fullUse","periodUse"))
nn<-data.hznu.energy.room.day[which(rowSums(is.na(data.hznu.energy.room.day))>0),]#行为finalState及basePattern缺失39758条




miningBehaviourPattern<-function(data,colRange,seasonCol){
  pamk.best<-pamk(data[,colRange],krange = getkSizeBySeason(unique(data[,seasonCol])[1]),criterion = "ch",
                  usepam = FALSE, critout = TRUE)
}

getkSizeBySeason<-function(season){
  kSizeBySeason<-array(c("Spring","Summer_warm","Summer","Autumn","Winter_warm","Winter",
                         6,6,4,4,5,3,6),dim = c(6,2))
  return(kSizeBySeason[season,2])
}