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
data.hznu.building.use<-data.hznu.all.use[runtime!=15,c("labelRoomDay","roomCode","date","finalState","acCount","runtime",sprintf("h%d",8:22))] %>% 
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
rm(temp.hznu.area.energy)

####区域数据集汇总完成，不消去缺失项内容###
data.hznu.area.predict.raw<-merge(x=data.hznu.area.predict.raw,
                                  y=data.weather.airport.final[!duplicated(data.weather.airport.final[,"datetime"]),
                                                               c("datetime","outTemp","rhOut","windSpeed","weather")],
                                  all.x = TRUE,by.x = "datetime",by.y = "datetime")
####加一些辅助变量####
data.hznu.area.predict.raw<-data.hznu.area.predict.raw %>%
                            mutate(.,fullOnRatio=onCount/count,dayOnRatio=onCount/dayOnCount)

####单独取出数据集进行显著性测试####
data.hznu.area.signCheck<-data.hznu.area.predict.raw[,c("datetime","date","fullOnRatio","dayOnRatio","modiElec","outTemp","rhOut","windSpeed","dayOnCount")]
for(i in c(0,1,2,7)){#0天，1天，2天，7天前
  for(j in c(0,1,2)){#
    if(!(i==0&j==0)){#i,j即天和小时不同时为0
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_FullOnRatio",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1,
                                                                                   FUN = getIntervalData,
                                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="fullOnRatio",timeInvl=i*24*3600+j*3600)
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_DayOnRatio",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1, 
                                                                                  FUN = getIntervalData,
                                                                                  data=data.hznu.area.signCheck,timeColName="datetime",targetColName="dayOnRatio",timeInvl=i*24*3600+j*3600)
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_modiElec",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1, 
                                                                                FUN = getIntervalData,
                                                                                data=data.hznu.area.signCheck,timeColName="datetime",targetColName="modiElec",timeInvl=i*24*3600+j*3600)
    }
  }
}


####统计各使用模式日内占比####
data.hznu.area.signCheck<-data.hznu.use.predict.building.raw[,.(
                                      sumDayOnLogCount=sum(onCount,na.rm = TRUE),
                                      onDemandCount=sum(onDemandCount,na.rm = TRUE),
                                      forenoonCount=sum(forenoonCount,na.rm = TRUE),
                                      afternoonCount=sum(afternoonCount,na.rm = TRUE),
                                      daytimeCount=sum(daytimeCount,na.rm = TRUE),
                                      lateDaytimeCount=sum(lateDaytimeCount,na.rm = TRUE),
                                      allDayCount=sum(allDayCount,na.rm = TRUE)),by=date] %>%
                          merge(x=data.hznu.area.signCheck,y=.,all.x=TRUE,by.x="date",by.y="date")
# 计算一下统计正确性
nn<-data.hznu.area.signCheck[dayOnCount!=(onDemandCount+forenoonCount+afternoonCount+daytimeCount+lateDaytimeCount+allDayCount)]#这样可以，但是不能用dayOnCount
# #解决一下dayOnCount的错误
# nn1<-data.hznu.all.use[date=="2017-01-12"]
# nn1<-merge(x=nn1,y=data.hznu.use.predict.building.raw[,.(
#   sumDayOnLogCount=sum(onCount,na.rm = TRUE),
#   onDemandCount=sum(onDemandCount,na.rm = TRUE),
#   forenoonCount=sum(forenoonCount,na.rm = TRUE),
#   afternoonCount=sum(afternoonCount,na.rm = TRUE),
#   daytimeCount=sum(daytimeCount,na.rm = TRUE),
#   lateDaytimeCount=sum(lateDaytimeCount,na.rm = TRUE),
#   allDayCount=sum(allDayCount,na.rm = TRUE)),by=date],all.x=TRUE,by.x="date",by.y="date")
# ##由于区域行为中去除掉runtime==15的数据，而此类数据认为是异常数据，在行为模式分析中已筛去，且没有对应的能耗


#计算各模式所占比例
data.hznu.area.signCheck<-mutate(.data =data.hznu.area.signCheck,
                                 onDemandRatio=onDemandCount/sumDayOnLogCount,
                                 forenoonRatio=forenoonCount/sumDayOnLogCount,
                                 afternoonRatio=afternoonCount/sumDayOnLogCount,
                                 daytimeRatio=daytimeCount/sumDayOnLogCount,
                                 lateDaytimeRatio=lateDaytimeCount/sumDayOnLogCount,
                                 allDayRatio=allDayCount/sumDayOnLogCount)




data.hznu.area.predict.raw$h1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d7_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                  FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=7*24*3600)

####统计数据情况####
stat.hznu.area.completeCheck<-data.table(date=strptime("2016-12-07","%Y-%m-%d")+24*3600*0:850)
stat.hznu.area.completeCheck$useCount<-apply(stat.hznu.area.completeCheck[,"date"],MARGIN = 1,
                                             FUN = function(x){ return(nrow(data.hznu.area.use[date==x]))})
stat.hznu.area.completeCheck$isUseComplete<-(stat.hznu.area.completeCheck$useCount==15)
stat.hznu.area.completeCheck$energyCount<-apply(stat.hznu.area.completeCheck[,"date"],MARGIN = 1,
                                             FUN = function(x){ return(nrow(data.hznu.area.energy[date==x]))})
stat.hznu.area.completeCheck$isEnergyComplete<-(stat.hznu.area.completeCheck$energyCount==15)
nrow(stat.hznu.area.completeCheck[isUseComplete==TRUE]) #812
nrow(stat.hznu.area.completeCheck[isEnergyComplete==TRUE]) #579
nrow(stat.hznu.area.completeCheck[isEnergyComplete&isEnergyComplete]) #579
