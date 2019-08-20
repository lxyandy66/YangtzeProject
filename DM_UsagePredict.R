#本脚本用于对行为进行预测
#主要预测总使用率和各模式占比
#接 data.hznu.teaching.use 数据集

####数据概况
# > table(data.hznu.teaching.use$basePattern)
# fullUse   noneUse periodUse 
# 1118    222183     20445 
# 
# > table(data.hznu.teaching.use$season)
# Autumn      Spring      Summer Summer_warm      Winter Winter_warm 
# 36434       37522       40300       39000       50354       40136 

####数据集初处理####
data.hznu.use.predict.raw<-data.hznu.teaching.use[,c("labelRoomDay","date","roomCode","buildingCode","acCount",
                                                 "finalState","modiSeason","runtime","basePattern")]
data.hznu.use.predict.raw[is.na(finalState)&modiSeason %in% c("Winter","Winter_warm")]$finalState<-"heating"
#合并使用强度
data.hznu.use.predict.raw<-merge(x=data.hznu.use.predict.raw,y = data.hznu.all.use[,c("labelRoomDay","acIntensity")],
                             all.x = TRUE,by="labelRoomDay")
data.hznu.use.predict.raw[is.nan(acIntensity)]$acIntensity<-0
#合并使用模式标签
data.hznu.use.predict.raw<-merge(x=data.hznu.use.predict.raw,y=data.hznu.use.final[,c("labelRoomDay","clusterName")],
                             all.x = TRUE,by="labelRoomDay")
data.hznu.use.predict.raw<-data.hznu.use.predict.raw[basePattern!="fullUse"]
data.hznu.use.predict.raw[basePattern=="noneUse"]$clusterName<-"noneUse"

####合并至建筑级别####
data.hznu.use.predict.raw$labelBuildingDay<-paste(data.hznu.use.predict.raw$buildingCode,
                                                  data.hznu.use.predict.raw$date,sep = "_")
data.hznu.use.predict.building<-data.hznu.use.predict.raw[,.(date=date[1],
                                                             buildingCode=buildingCode[1],
                                                             offCount=sum(length(labelRoomDay[finalState=="off"])),
                                                             
                                                             ),by=labelBuildingDay]