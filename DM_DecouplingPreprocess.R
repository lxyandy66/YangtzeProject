
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

####异常数据的去除####
#如夏季制热冬季制冷
raw.rawData$removeFlag<-FALSE
raw.rawData[(season=="Summer"|season=="Summer_warm")&finalState=="heating"]$finalState<-"cooling"
raw.rawData[(season=="Winter"|season=="Winter_warm")&finalState=="cooling"]$finalState<-"heating"

####开关状态统计为单个房间####
raw.rawData$roomCode<-substr(raw.rawData$ac_code,1,13)
raw.rawData$labelRoomDay<-paste(raw.rawData$roomCode,raw.rawData$date,sep = "_")

####空调使用记录转为日内房间级使用记录####
data.hznu.use.room.day<-raw.rawData[,.(date=date[1],
                                       roomCode=roomCode[1],
                                       acCount=length(unique(ac_code)),
                                       finalState=ifelse(length(finalState[finalState!="off"])==0,"off",
                                                         getMode(finalState[finalState!="off"])),
                                       #注意需要特殊处理，否则用unique会存在finalState为off/heating 或者 cooling/heating的问题没有解决
                                       season=season[1],
                                       h8=ifelse(sum(h1,na.rm = TRUE)>0,1,0),
                                       h9=ifelse(sum(h2,na.rm = TRUE)>0,1,0),
                                       h10=ifelse(sum(h3,na.rm = TRUE)>0,1,0),
                                       h11=ifelse(sum(h4,na.rm = TRUE)>0,1,0),
                                       h12=ifelse(sum(h5,na.rm = TRUE)>0,1,0),
                                       h13=ifelse(sum(h6,na.rm = TRUE)>0,1,0),
                                       h14=ifelse(sum(h7,na.rm = TRUE)>0,1,0),
                                       h15=ifelse(sum(h8,na.rm = TRUE)>0,1,0),
                                       h16=ifelse(sum(h9,na.rm = TRUE)>0,1,0),
                                       h17=ifelse(sum(h10,na.rm = TRUE)>0,1,0),
                                       h18=ifelse(sum(h11,na.rm = TRUE)>0,1,0),
                                       h19=ifelse(sum(h12,na.rm = TRUE)>0,1,0),
                                       h20=ifelse(sum(h13,na.rm = TRUE)>0,1,0),
                                       h21=ifelse(sum(h14,na.rm = TRUE)>0,1,0),
                                       h22=ifelse(sum(h15,na.rm = TRUE)>0,1,0)
                                       ),by=labelRoomDay]
data.hznu.use.room.day$runtime<-apply(data.hznu.use.room.day[,c(7:21)],1,function(x) {sum(x,na.rm = TRUE) } )#计算总使用时间
#切记sum如果不将na.rm设为TRUE则有缺失值结果即为NA
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

##存在一些使用状态缺失的，由于其runtime!=0，根据季节确定finalState
data.hznu.use.room.day.period[is.na(finalState)&(modiSeason=="Winter"|modiSeason=="Winter_warm")]$finalState<-"heating"
data.hznu.use.room.day.period[is.na(finalState)&(modiSeason=="Summer"|modiSeason=="Summer_warm")]$finalState<-"cooling"



#?????需要确定聚类的分类，不同工况分开聚？不同季节分开聚？????
# list.hznu.room.use<-split(data.hznu.use.room.day.period,
#                           f=as.factor(paste(data.hznu.use.room.day.period$finalState,
#                                             data.hznu.use.room.day.period$modiSeason,sep = "_")))

#更新一下仅教学
# data.hznu.teaching.use[runtime>0 & runtime<15] #符合该条件教学建筑全年仅20445条
list.hznu.room.use<-split(data.hznu.teaching.use[runtime>0 & runtime<15],
                          f=as.factor(paste(data.hznu.teaching.use[runtime>0 & runtime<15]$finalState,
                                            data.hznu.teaching.use[runtime>0 & runtime<15]$modiSeason,sep = "_")))

list.hznu.room.use[["NA_Transition"]]<-NULL#过渡季缺失不能用


##可接"HZNU_含追加_房间级_行为预处理完成.rdata"

####行为再聚类####
####试聚类####
#聚类评估
#names(list.hznu.room.use)
#"cooling_Summer"      "cooling_Summer_warm" "cooling_Transition"  "heating_Transition"  
#"heating_Winter"      "heating_Winter_warm"

modeSelect<-"cooling_Summer_warm"
data.use.room.tryCluster<-list.hznu.room.use[[modeSelect]]

wssClusterEvaluate(data = data.use.room.tryCluster[, 7:22],
                   maxIter = 1000,
                   maxK = 15)
pamkClusterEvaluate(
  data = data.use.room.tryCluster[, 7:22],#8-22时+runtime
  criter = "multiasw",
  startK = 1,
  endK = 10
)
#试聚类
kSize <- 5
for(kSize in c(3:9)){
  pamk.best <-pamk(
    data.use.room.tryCluster[, 7:21],
    krange = kSize,
    criterion = "ch",
    usepam = TRUE,
    critout = TRUE
  )#注意有缺失值的聚类结果将会是NA
data.use.room.tryCluster$cluster<-pamk.best$pamobject$clustering
data.use.room.tryCluster$isWorkday<-isWeekday(data.use.room.tryCluster$date)
stat.use.room.cluster<-data.use.room.tryCluster[,.(
  runtime=mean(runtime,na.rm = TRUE),
  count=length(labelRoomDay),
  workdayRatio=length(labelRoomDay[isWorkday==TRUE]),
  weekendRatio=length(labelRoomDay[isWorkday==FALSE]),
  h8=mean(h8,na.rm = TRUE),
  h9=mean(h9,na.rm = TRUE),
  h10=mean(h10,na.rm = TRUE),
  h11=mean(h11,na.rm = TRUE),
  h12=mean(h12,na.rm = TRUE),
  h13=mean(h13,na.rm = TRUE),
  h14=mean(h14,na.rm = TRUE),
  h15=mean(h15,na.rm = TRUE),
  h16=mean(h16,na.rm = TRUE),
  h17=mean(h17,na.rm = TRUE),
  h18=mean(h18,na.rm = TRUE),
  h19=mean(h19,na.rm = TRUE),
  h20=mean(h20,na.rm = TRUE),
  h21=mean(h21,na.rm = TRUE),
  h22=mean(h22,na.rm = TRUE)
),by=cluster]
setorder(stat.use.room.cluster,cluster)
write.xlsx(data.table(stat.use.room.cluster,pamk.best$pamobject$clusinfo,
                      pamk.best$pamobject$medoids),file = paste(modeSelect,kSize, "cluster_usage_stat.xlsx",sep = "_"))
stat.use.plot.cluster<-data.table(hour=(8:22),t(stat.use.room.cluster[,c(6:20)]))
stat.use.plot.cluster<-melt(stat.use.plot.cluster,id.vars ="hour")
ggsave(
  file = paste(modeSelect,kSize, "cluster.png", sep = "_"),
  height = 9,
  width = 16,
  dpi = 120,
  plot =
    ggplot(data=stat.use.plot.cluster,aes(x=hour,y=value,color=variable,shape=variable))+geom_line()+geom_point()
)
}

####加载聚类模式名称及编码对应表####
data.hznu.use.seasonMap<-as.data.table(read.xlsx2(file = "HZNU_ClusterMapping.xlsx",sheetIndex = 1))

# list.behaviour.season 数据行为模式聚类已标记，全年模式统一
####正式聚类####
#根据试聚类的聚类数统一对6个季节进行聚类
for(i in names(list.hznu.room.use)){
  list.hznu.room.use[[i]]$cluster<-pamk(
    list.hznu.room.use[[i]][,c(paste("h",c(8:22),sep = ""),"runtime")],
    krange = getkSizeBySeason(i),
    criterion = "ch",
    usepam = TRUE,
    critout = TRUE
  )$pamobject$clustering
  # list.hznu.room.use[[i]]$clusterName<-mapply(getUsePatternName,i,list.hznu.room.use[[i]]$cluster)
  
  #根据各季节聚类对应编码转换为统一名称
  list.hznu.room.use[[i]]$clusterName<-""
  for(j in unique(list.hznu.room.use[[i]]$cluster)){
    list.hznu.room.use[[i]][cluster==j,]$clusterName<-as.character(getUsePatternName(season= i,clusterNo = j))
  }
}#其实用lapply更好



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
  kSizeBySeason<-data.table(seasonMode=c("cooling_Summer","cooling_Summer_warm","cooling_Transition","heating_Transition",
                         "heating_Winter","heating_Winter_warm"),
                         kSize=c(3,6,6,4,3,6))
  return(kSizeBySeason[seasonMode==season,2])
}
getUsePatternName<-function(season,clusterNo){
  return(data.hznu.use.seasonMap[seasonMode==season & patternCode==clusterNo]$patternName)#参数名不要和变量名一样
}
