####整理成区域的数据集留作备用####

#接
# data.hznu.all.use（宽数据要处理成逐时的）
# data.hznu.teaching.energy.std（宽数据要处理成逐时的）

####能耗数据合并为建筑级别####
data.hznu.building.energy<-data.hznu.teaching.energy.std[,c("labelRoomDay","acCount","date","buildingCode","finalState",sprintf("h%d",8:22))]%>%
                           melt(.,id.var=c("labelRoomDay","acCount","date","buildingCode","finalState")) %>%
                           mutate(.,hour=substring(variable,2),labelBuildingHour=paste(buildingCode,date,hour,sep = "_")) %>%
                           .[,.(date=date[1],
                                buildingCode=buildingCode[1],
                                count=length(unique(labelRoomDay)),
                                hour=hour[1],
                                modiElec=sum(value,na.rm = TRUE)
                                ),by=labelBuildingHour]#这个数据也要保留着

####能耗数据合并为区域级别####
data.hznu.area.energy<-data.hznu.building.energy %>% 
                       mutate(.,datetime=as.POSIXct(paste(date,sprintf("%02s:00:00",hour)))) %>%
                       .[,.(date=date[1],
                            count=sum(count,na.rm=TRUE),
                            buildingCount=length(unique(buildingCode)),
                            modiElec=sum(modiElec,na.rm = TRUE)),by=datetime]

####行为合并至逐时建筑级长数据####
data.hznu.building.use<-data.hznu.all.use[,c("labelRoomDay","roomCode","date","finalState","acCount","runtime",sprintf("h%d",8:22))] %>% 
                    melt(.,id.var=c("labelRoomDay","roomCode","date","finalState","acCount","runtime")) %>% 
                    mutate(.,hour=substring(variable,2),onOff=ifelse(value!=0,1,0),#此value即当前小时当前房间使用的空调数
                           buildingCode=substr(roomCode,1,10),
                           labelBuildingHour=paste(buildingCode,date,hour,sep = "_")) %>% 
                    .[,.(date=date[1],
                         buildingCode=buildingCode[1],
                         count=length(labelRoomDay),#该小时该栋楼总记录数
                         hour=hour[1],
                         onCount=length(labelRoomDay[onOff!=0]),#当前小时总on记录数
                         offCount=length(labelRoomDay[onOff==0]),#当前小时总off记录数
                         dayOnCount=length(labelRoomDay[runtime!=0]),#当前小时记录属于acOnDay记录数
                         dayOffCount=length(labelRoomDay[runtime==0]),
                         acCount=sum(acCount,na.rm=TRUE),
                         acUsedCount=sum(value,na.rm=TRUE)
                         ),by=labelBuildingHour]

####行为合并至逐时区域级长数据####
data.hznu.area.use<-data.hznu.building.use[buildingCode %in% data.hznu.building.energy$buildingCode]%>%
                    mutate(.,datetime=as.POSIXct(paste(date,sprintf("%02s:00:00",hour)))) %>%
                    .[,.(date=date[1],
                         buildingCount=length(unique(buildingCode)),
                         count=sum(count,na.rm = TRUE),
                         onCount=sum(onCount,na.rm = TRUE),
                         offCount=sum(offCount,na.rm = TRUE),
                         dayOnCount=sum(dayOnCount,na.rm = TRUE),
                         dayOffCount=sum(dayOffCount,na.rm = TRUE),
                         acCount=sum(acCount,na.rm=TRUE),
                         acUsedCount=sum(acUsedCount,na.rm=TRUE)
                         ),by=datetime]

temp.hznu.area.energy<-data.hznu.area.energy %>% mutate(countEnergy=count,buildingCountEnergy=.$buildingCount)
data.hznu.area.predict.raw<-merge(x=data.hznu.area.use,y=temp.hznu.area.energy[,c("datetime","countEnergy","buildingCountEnergy","modiElec")],
                                  all.x = TRUE,by.x= "datetime",by.y= "datetime")#直接管道函数会warning，但是结果不影响#迷惑

####区域数据集汇总完成，不消去缺失项内容###
data.hznu.area.predict.raw<-merge(x=data.hznu.area.predict.raw,
                                  y=data.weather.airport.final[!duplicated(data.weather.airport.final[,"datetime"]),
                                                               c("datetime","outTemp","rhOut","windSpeed","weather")],
                                  all.x = TRUE,by.x = "datetime",by.y = "datetime")
####加一些辅助变量####
data.hznu.area.predict.raw<-data.hznu.area.predict.raw %>%
                            mutate(.,fullOnRatio=onCount/count,dayOnRatio=onCount/dayOnCount)

data.hznu.area.predict.raw$h1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d7_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                  FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=7*24*3600)



