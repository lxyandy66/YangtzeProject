####整理成区域的数据集留作备用####

#接
# data.hznu.all.use（宽数据要处理成逐时的）
# data.hznu.teaching.energy.std（宽数据要处理成逐时的）
# data.hznu.teaching.all 热环境、设定温度#注意这一部分预处理有点问题

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

####温度相关数据合并成区域级别####
# 非要加我能怎么办
data.hznu.area.thermal<-data.hznu.teaching.all[,.(date=date[1],
                                                  modiSeason=modiSeason[1],
                                                  count=length(unique(roomCode)),
                                                  modiElec=sum(modifyElec[on_off==1&finalState%in%c("cooling","heating")],na.rm = TRUE),
                                                  set_temp=mean(set_temp[set_temp>0&!is.nan(set_temp)&on_off==1&finalState%in%c("cooling","heating")],na.rm = TRUE),
                                                  modiTemp=mean(modiTemp[modiTemp>0&!is.nan(modiTemp)],na.rm = TRUE)
                                                  ),by=(datetime=time)]
data.hznu.area.thermal<-data.hznu.area.thermal%>%mutate_all(funs(ifelse(is.nan(.),NA, .))) %>% mutate(.,datetime=as.POSIXct(datetime))%>%data.table(.)


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
###还应合并热环境相关，目前直接在后面合并

####能耗基本缺失值处理####
# 切记最好应该统一处理！！
# 思路
# 1、对于能耗NA值，若该时刻的fullOnRatio==0，则置0
data.hznu.area.predict.raw[is.na(modiElec)&fullOnRatio==0]$modiElec<-0
data.hznu.area.predict.raw[is.na(modiElec)]

nn<-data.hznu.area.predict.raw[date %in% unique(data.hznu.area.predict.raw[is.na(modiElec)]$date)]
#还是有2017-01-28和2017-05-01缺失
#考虑直接按小时的找一天能耗和气候相近的有着相同使用率的进行补全
nn1<-data.hznu.area.predict.raw[fullOnRatio>0.00328&fullOnRatio<0.003315&month(datetime)%in% c(1,5)&year(datetime)=="2017",
                                c("date","datetime","fullOnRatio","modiElec")] %>% 
  mutate(.,hour=hour(datetime),month=month(datetime)) %>% as.data.table(.) %>%.[!duplicated(.)]

data.hznu.area.predict.raw[is.na(modiElec)&substr(date,1,4)=="2017"]$modiElec<-apply(X = data.hznu.area.predict.raw[is.na(modiElec)&substr(date,1,4)=="2017","datetime"],MARGIN = 1,
                                                            FUN = function(x){
                                                              mean(nn1[hour==hour(as.POSIXct(x))&month==month(as.POSIXct(x))]$modiElec,na.rm = TRUE)
                                                            })
data.hznu.area.predict.raw[rhOut>1]$rhOut<-NA


####加一些辅助变量####
data.hznu.area.predict.raw<-data.hznu.area.predict.raw %>%
                            mutate(.,fullOnRatio=onCount/count,dayOnRatio=onCount/dayOnCount)

####增加是否是工作日的判断####
info.hznu.holiday<-as.data.table(read.xlsx(file="HZNU_HolidayList.xlsx",sheetIndex = 1)) %>% mutate(.,date=as.character(date))
data.hznu.area.predict.raw$isWeekday<-isWeekday(data.hznu.area.predict.raw$date)
data.hznu.area.predict.raw<-merge(x=data.hznu.area.predict.raw,y=info.hznu.holiday[,c("date","isBizday")],all.x = TRUE,by.x="date",by.y="date") #逻辑操作
data.hznu.area.predict.raw[is.na(isBizday)]$isBizday<-data.hznu.area.predict.raw[is.na(isBizday)]$isWeekday

data.hznu.area.predict.raw$h1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d7_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=7*24*3600)


####单独取出数据集进行显著性测试####
data.hznu.area.signCheck<-data.hznu.area.predict.raw[,c("datetime","date","fullOnRatio","modiElec","outTemp","rhOut","windSpeed","dayOnCount","isBizday")]
data.hznu.area.signCheck$hour<-hour(data.hznu.area.signCheck$datetime)
data.hznu.area.signCheck$r1h0_FullOnRatio<-apply(X = data.hznu.area.signCheck[,c("datetime","isBizday")],MARGIN = 1,
                                                 FUN = function(x){
                                                   getPreviousDate(thisTime = x[1],expFlag = as.logical(gsub(" ","",x[2])),
                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="fullOnRatio",flagColName="isBizday",timeInvl= -24*3600)})
data.hznu.area.signCheck$r1h0_modiElec<-apply(X = data.hznu.area.signCheck[,c("datetime","isBizday")],MARGIN = 1,
                                                 FUN = function(x){
                                                   getPreviousDate(thisTime = x[1],expFlag = as.logical(gsub(" ","",x[2])),
                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="modiElec",flagColName="isBizday",timeInvl= -24*3600)})
#####切记！这玩意跑的贼慢
for(i in c(0,1,2,7)){#0天，1天，2天，7天前
  for(j in c(0,1,2)){#
    if(!(i==0&j==0)){#i,j即天和小时不同时为0
      cat(i,j,"\n")
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_FullOnRatio",sep = "")]<-
        apply(X=data.hznu.area.signCheck[,c("datetime","isBizday")], MARGIN = 1,
              FUN = function(x){
                getIntervalTimeData(thisTime = x[1],beforeDay = i,beforeHour = j,timeColName = "datetime",
                                    targetColName = "fullOnRatio",expFlag = gsub(" ","",x[2]),flagColName = "isBizday",data = data.hznu.area.signCheck)})
      
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_modiElec",sep = "")]<-
        apply(X=data.hznu.area.signCheck[,c("datetime","isBizday")], MARGIN = 1,
              FUN = function(x){
                getIntervalTimeData(thisTime = x[1],beforeDay = i,beforeHour = j,timeColName = "datetime",
                                    targetColName = "modiElec",expFlag = gsub(" ","",x[2]),flagColName = "isBizday",data = data.hznu.area.signCheck)})
    }
  }
}

#获取前一小时的参考时间
# 准备进行日起始时刻的处理
data.hznu.area.signCheck$refHour1<-apply(X=data.hznu.area.signCheck[,c("datetime","isBizday")], MARGIN = 1,
                                        FUN = function(x){
                                          getTargetTime(thisTime = x[1],beforeHour = 1,beforeDay = 0,data = data.hznu.area.signCheck,
                                                        timeColName = "datetime",expFlag = as.logical(gsub(" ","",x[2])),flagColName = "isBizday")
                                        }) %>% as.POSIXct(.)


#### 修正d0h1对于日起始小时的问题####
data.hznu.area.signCheck$back_d0h1fullOnRatio<-data.hznu.area.signCheck$d0h1_FullOnRatio
data.hznu.area.signCheck$back_d0h1ModiElec<-data.hznu.area.signCheck$d0h1_modiElec
data.hznu.area.signCheck[hour==8]$d0h1_FullOnRatio<-apply(data.hznu.area.signCheck[hour==8,c("refHour1","isBizday")],MARGIN = 1,
                                                          FUN = function(x){
                                                            if(anyNA(x))
                                                              return(NA)
                                                            if(as.logical(gsub(" ","",x[2]))){
                                                              #如果是工作日则取前一天均值
                                                              return(mean(data.hznu.area.signCheck[date==substr(x[1],1,10)]$fullOnRatio,na.rm = TRUE))
                                                            }else{
                                                              #如果是非工作日则取前一天8h
                                                              return(data.hznu.area.signCheck[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$fullOnRatio[1])
                                                            }
                                                          })
data.hznu.area.signCheck[hour==8]$d0h1_modiElec<-apply(data.hznu.area.signCheck[hour==8,c("refHour1","isBizday")],MARGIN = 1,
                                                          FUN = function(x){
                                                            if(anyNA(x))
                                                              return(NA)
                                                            if(as.logical(gsub(" ","",x[2]))){
                                                              #如果是工作日则取前一天均值
                                                              return(mean(data.hznu.area.signCheck[date==substr(x[1],1,10)]$modiElec,na.rm = TRUE))
                                                            }else{
                                                              #如果是非工作日则取前一天8h
                                                              return(data.hznu.area.signCheck[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$modiElec[1])
                                                            }
                                                          })


####加入各能耗模式占比####
temp.hznu.area.energyPattern<-data.hznu.teaching.energy.std[,.(count=length(labelRoomDay),
                                                               lowEnergyRatio=length(labelRoomDay[energyClusterName=="LowEnergy"])/length(labelRoomDay),
                                                               midEnergyRatio=length(labelRoomDay[energyClusterName=="MidEnergy"])/length(labelRoomDay),
                                                               ltMeRatio=length(labelRoomDay[energyClusterName=="LongTime_MidEnergy"])/length(labelRoomDay),
                                                               ltHeRatio=length(labelRoomDay[energyClusterName=="LongTime_HighEnergy"])/length(labelRoomDay)
                                                               ),by=date]#经检查无重复
data.hznu.area.signCheck<-merge(x=data.hznu.area.signCheck,y=temp.hznu.area.energyPattern[,-c("count")],
                                all.x=TRUE,by.x = "date",by.y = "date")


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

data.hznu.area.signCheck<-as.data.table(data.hznu.area.signCheck)#突然变成data.frame？？？
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

#还是差1-2左右，影响不大



#计算各模式所占比例
data.hznu.area.signCheck<-mutate(.data =data.hznu.area.signCheck,
                                 onDemandRatio=onDemandCount/sumDayOnLogCount,
                                 forenoonRatio=forenoonCount/sumDayOnLogCount,
                                 afternoonRatio=afternoonCount/sumDayOnLogCount,
                                 daytimeRatio=daytimeCount/sumDayOnLogCount,
                                 lateDaytimeRatio=lateDaytimeCount/sumDayOnLogCount,
                                 allDayRatio=allDayCount/sumDayOnLogCount)


for(i in names(data.hznu.area.signCheck)){
  data.hznu.area.signCheck[which(is.nan(as.matrix(data.hznu.area.signCheck[,..i]))) ,i] <- NA
  #很奇怪，理论来说应该是..i, 但是这里..i会提示找不到对象，直接i才行，提示是赋值时候出错
}

data.hznu.area.signCheck$modiSeason<- apply(data.hznu.area.signCheck[,"date"],MARGIN = 1,FUN = function(x){ getSeason(substr(x,6,7))}) 
data.hznu.area.signCheck[modiSeason %in% c("Spring","Autumn")]$modiSeason<-"Transition"
                         
####检查各因素显著性####
hstTimeInvl<-c("d0h1","d0h2","d1h0","d1h1","d1h2","d2h0","d2h1","d2h2","d7h0","d7h1","d7h2")#"r1h0",
patternRatioName<-c("onDemandRatio","forenoonRatio","afternoonRatio","daytimeRatio","lateDaytimeRatio","allDayRatio")
energyPatternRatioName<-c("lowEnergyRatio","midEnergyRatio","ltMeRatio","ltHeRatio")
signAttr<-list(weatherAttr=c("outTemp","rhOut","windSpeed","weekday","isBizday","hour","d0h1_modiTemp"),#"stdOutTemp","stdRhOut","stdWindSpeed",
               fullOnRatio=c(paste(hstTimeInvl,"FullOnRatio",sep = "_")),
               # dayOnRatio=c(paste(hstTimeInvl,"DayOnRatio",sep = "_")),
               stdModiElec=c(paste(hstTimeInvl,"modiElec",sep="_")),
               patternRatio=c(paste(c(rep("d1_",6),rep("d7_",6)),patternRatioName,sep = "")),#rep("r1_",6),
               energyPatternRatio=c(paste(c(rep("d1_",4),rep("d7_",4)),energyPatternRatioName,sep = ""))#rep("r1_",4),
               )#rep("r1_",6),

#需扣掉盛夏和严冬多余的两个季节中多余的模式
excessUsagePattern<-c("d1_forenoonRatio","d7_forenoonRatio","d1_afternoonRatio","d7_afternoonRatio",
                      "d1_lateDaytimeRatio","d7_lateDaytimeRatio")


data.hznu.area.signCheck[,c(paste(signAttr$patternRatio,"_org",sep = ""))]<-NULL

#取历史模式相关数据
for(i in unique(data.hznu.area.signCheck$date)){
  # ##取前一个参考天
  # targetTime<-as.POSIXct(getTargetDate(thisTime = i,data = data.hznu.area.signCheck,
  #                                      timeColName = "date",flagColName = "isBizday",
  #                                      expFlag = data.hznu.area.signCheck[date==i]$isBizday[1],timeInvl = -24*3600))
  # #取行为模式
  # data.hznu.area.signCheck[date==i,c(paste(rep("r1_",6),patternRatioName,sep = ""))]<-
  #   data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
  # #取能耗模式
  # data.hznu.area.signCheck[date==i,c(paste(rep("r1_",4),energyPatternRatioName,sep = ""))]<-
  #   data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..energyPatternRatioName][1]
  
  #取前一天
  targetTime<-as.POSIXct(getTargetTime(thisTime = i,data = data.hznu.area.signCheck,beforeHour = 0,beforeDay = 1,byDate = TRUE,
                                       timeColName = "date",flagColName = "isBizday",expFlag = data.hznu.area.signCheck[date==i]$isBizday[1]))
  data.hznu.area.signCheck[date==i,c(paste(rep("d1_",6),patternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
  data.hznu.area.signCheck[date==i,c(paste(rep("d1_",4),energyPatternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..energyPatternRatioName][1]
  
  #取前七天
  targetTime<-as.POSIXct(getTargetTime(thisTime = i,data = data.hznu.area.signCheck,beforeHour = 0,beforeDay = 7,byDate = TRUE,
                                       timeColName = "date",flagColName = "isBizday",expFlag = data.hznu.area.signCheck[date==i]$isBizday[1]))
  data.hznu.area.signCheck[date==i,c(paste(rep("d7_",6),patternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
  data.hznu.area.signCheck[date==i,c(paste(rep("d7_",4),energyPatternRatioName,sep = ""))]<-
      data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..energyPatternRatioName][1]
}
data.hznu.area.signCheck<-as.data.table(data.hznu.area.signCheck)
####对于模式缺失值的处理####
# 思路：
# 对于同一时刻的模式（例如d1行为模式）若均为缺失则不处理保留缺失
# 若部分缺失，例如间断、全天、白天有，其他模式缺失，则将其他模式缺失值改为0
#有没有聪明的办法 无需进一步处理
# for(i in c("d1_","d7_")){
#   selectCol<-paste(rep(i,6),patternRatioName,sep = "")
#   selectFlag<-data.hznu.area.signCheck[,..patternRatioName]%>%
#     .[,apply(.SD, MARGIN = 1,FUN = function(x){sum(x.na.rm=TRUE)})]#!=length(patternRatioName)
# }
# 
# nn<-data.table(a=c(1,2,3,NA),b=c(4,NA,6,NA),c=c(NA,7,8,NA))
# nn[,c("a","b")]<-nn[,c("a","b")]%>%.[.[,apply(.SD, MARGIN = 1,
#           FUN = function(x){sum(is.na(x))})!=2],]%>% mutate_all(funs(ifelse(is.na(.), 0, .)))
# nn[,c("a","b")]%>%.[,apply(.SD, MARGIN = 1,
#                              FUN = function(x){sum(is.na(x))})!=2]
# PERFECT!

nn<-replace(x=NA,nn, 0)
nn[,1:3]<-apply(X = nn[,1:3],MARGIN = 1,
                FUN = function(x){
                  if(sum(is.na(x))!=3){
                    x[is.na(x)]<-0
                  }
                  cat(x)
                  return(x)
                })



data.hznu.area.signCheck$weekday<-wday(data.hznu.area.signCheck$date,week_start = 1)
backup.hznu.area.signCheck<-data.hznu.area.signCheck
####加入室内温度、空调设定温度等相关数据####
# 她要加就加呗我能怎么办我也很无奈啊
# str(data.hznu.area.signCheck)
data.hznu.area.signCheck[,c("modiTemp","d0h1_modiTemp","set_temp","d0h1_setTemp")]<-NULL

data.hznu.area.signCheck<-merge(x=data.hznu.area.signCheck,y=data.hznu.area.thermal[,c("datetime","set_temp","modiTemp")],all.x = TRUE,by= "datetime")

data.hznu.area.signCheck$d0h1_modiTemp<-apply(data.hznu.area.signCheck[,c("refHour1")],MARGIN = 1,
                                              FUN = function(x){
                                                return(data.hznu.area.signCheck[datetime==as.POSIXct(x)]$modiTemp[1])
                                              })
data.hznu.area.signCheck[hour(datetime)==8]$d0h1_modiTemp<-apply(data.hznu.area.signCheck[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                FUN = function(x){
                                                                  if(anyNA(x))
                                                                    return(NA)
                                                                  if(as.logical(gsub(" ","",x[2]))){
                                                                    #如果是工作日则取前一天均值
                                                                    return(mean(data.hznu.area.signCheck[date==substr(x[1],1,10)]$d0h1_modiTemp,na.rm = TRUE))
                                                                  }else{
                                                                    #如果是非工作日则取前一天8h
                                                                    return(data.hznu.area.signCheck[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$d0h1_modiTemp[1])
                                                                  }
                                                                })

data.hznu.area.signCheck$d0h1_setTemp<-apply(data.hznu.area.signCheck[,c("refHour1")],MARGIN = 1,
                                              FUN = function(x){
                                                return(data.hznu.area.signCheck[datetime==as.POSIXct(x)]$set_temp[1])
                                              })
data.hznu.area.signCheck[hour(datetime)==8]$d0h1_setTemp<-apply(data.hznu.area.signCheck[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                 FUN = function(x){
                                                                   if(anyNA(x))
                                                                     return(NA)
                                                                   if(as.logical(gsub(" ","",x[2]))){
                                                                     #如果是工作日则取前一天均值
                                                                     return(mean(data.hznu.area.signCheck[date==substr(x[1],1,10)]$d0h1_setTemp,na.rm = TRUE))
                                                                   }else{
                                                                     #如果是非工作日则取前一天8h
                                                                     return(data.hznu.area.signCheck[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$d0h1_setTemp[1])
                                                                   }
                                                                 })


####按季节归一化####
data.hznu.area.signCheck.pickup<-data.hznu.area.signCheck[substr(date,1,4)=="2017"|substr(date,1,7)=="2018-01"]

data.hznu.area.signCheck.pickup$stdHour<-hour(data.hznu.area.signCheck.pickup$datetime) %>% normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.signCheck.pickup<-mutate(data.hznu.area.signCheck.pickup,stdOutTemp= -999,stdRhOut= -999,stdWindSpeed= -999,stdWeekday= -999)
data.hznu.area.signCheck.pickup$stdWeekday<-normalize(data.hznu.area.signCheck.pickup$weekday,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.signCheck.pickup<-as.data.table(data.hznu.area.signCheck.pickup)#很奇怪会莫名降级
data.hznu.area.signCheck.pickup[,c("stdModiElec","d0h1_modiTempStd","d0h1_setTempStd","stdOutTemp","stdRhOut","stdWindSpeed")]<- -999
for(i in unique(data.hznu.area.signCheck.pickup$modiSeason)){
  data.hznu.area.signCheck.pickup[modiSeason==i]$stdOutTemp<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i]$outTemp,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.signCheck.pickup[modiSeason==i]$stdRhOut<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i]$rhOut,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.signCheck.pickup[modiSeason==i]$stdWindSpeed<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i]$windSpeed,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.signCheck.pickup[modiSeason==i]$d0h1_modiTempStd<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i,"d0h1_modiTemp"],
                                                                        upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.signCheck.pickup[modiSeason==i]$d0h1_setTempStd<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i,"d0h1_setTemp"],
                                                                       upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.signCheck.pickup[modiSeason==i]$stdModiElec<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i,"modiElec"],upper = 0.9,lower = 0.1,intercept = 0.1)
}

####按季节归一化对象###



####按perason循环统计变量显著性####
rm(stat.hznu.area.cor)
for(i in c("fullOnRatio")){#,"stdModiElec"
  for(j in unique(data.hznu.area.signCheck.pickup$modiSeason)){
    for(k in c("weatherAttr","hst","patternRatio","energyPatternRatio","useHst")){
      #根据目前循环分组选取适合公式 #我觉得可以简化一下
      if(k=="useHst"){#useHst仅对能耗模式考虑，因此提前判断
        if(i=="stdModiElec"){
          attr<-c(signAttr[["fullOnRatio"]],"fullOnRatio")
        }#计算能耗显著性时考虑历史空调使用率时仍包括此刻的空调使用率，在实际模型中该值来源于预测
        else
          next#只针对能耗显著性才考虑行为的影响
      }
      else{
        if(k=="hst")
          attr<-signAttr[[i]]
        else
          attr<-signAttr[[k]]
      }
      #根据得到的显著性计算formula通过logistics计算显著性
      
      if(j %in% c("Summer","Winter")&k=="patternRatio")
        attr<-attr[!attr%in% excessUsagePattern]
    
      
      nn<-corr.test(y=data.hznu.area.signCheck.pickup[,..i],x=data.hznu.area.signCheck.pickup[,..attr],method = "spearman",use = "complete.obs")
      if(exists("stat.hznu.area.cor")){
        stat.hznu.area.cor<-rbind(stat.hznu.area.cor,data.table(target=i,modiSeason=j,attr=k,var=row.names(nn$r),r=as.numeric(nn$r),sign=nn$ci$p))
      }else{
        stat.hznu.area.cor<-data.table(target=i,modiSeason=j,attr=k,var=row.names(nn$r),r=as.numeric(nn$r),sign=nn$ci$p)
      }
    }
  }
}
write.xlsx(stat.hznu.area.cor,file = "HZNU_Area_变量相关性系数_含所有房间_小论文.xlsx")
#真的线性相关太不行了



####按logistics循环统计变量显著性####
rm(stat.hznu.area.predict.sign)
for(i in c("fullOnRatio")){#i表示显著性分析的因变量 #,"stdModiElec"
  for(j in unique(data.hznu.area.signCheck.pickup$modiSeason)){#j为季节分组
    for(k in c("weatherAttr","fullOnRatio","patternRatio")){#对应三组变量：天气，行为历史，行为模式历史#"energyPatternRatio"
      
      #对于部分季节剔除非典型的行为模式
      if(j %in% c("Summer","Winter"))
        seasonalAttr<-signAttr[[k]][!signAttr[[k]] %in% excessUsagePattern]
      else
        seasonalAttr<-signAttr[[k]]
      
      fit<-lm(as.formula(paste(i,"~",paste(seasonalAttr,collapse = "+"))),
            data=data.hznu.area.signCheck.pickup%>%.[modiSeason==j&complete.cases(.[,..seasonalAttr])],#family = binomial(),
            na.action = na.omit)
      cat(j,"\t",length(fit$residuals),"\n")
      stat.fit<-summary(fit)
      
      if(exists("stat.hznu.area.predict.sign")){
        stat.hznu.area.predict.sign<-rbind(stat.hznu.area.predict.sign,
                                           data.table(target=i,modiSeason=j,attr=k,var=row.names(stat.fit$coefficients),stat.fit$coefficients))
      }else{
        stat.hznu.area.predict.sign<-data.table(target=i,modiSeason=j,attr=k,var=row.names(stat.fit$coefficients),stat.fit$coefficients)
      }
    }
  }
}
write.xlsx(stat.hznu.area.predict.sign,file = "HZNU_AreaSelected_Use_withTemp_AttrSign_final_含所有房间_linear_小论文.xlsx")



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



ggplot(data=(data.hznu.area.predict.raw%>% mutate(.,isWeekday=isWeekday(datetime),year=as.factor(year(datetime)),
                                                  monthDay=format(datetime,format="%m-%d")) %>% 
               as.data.table(.) %>%.[substr(datetime,1,7) %in% c("2017-05","2017-06","2017-07","2017-08","2017-09","2017-10")]),
       aes(x=date,y=fullOnRatio,color=isBizday,group=date,shape=year))+geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))#+facet_wrap(~year,ncol=1)
# c("2018-11","2018-12","2019-01","2019-02")]#这一截不行


ggplot(data=(stat.hznu.area.completeCheck%>% mutate(.,date=as.Date(date))%>% mutate(.,isWeekday=isWeekday(date),year=as.factor(year(date)),monthDay=format(date,format="%m-%d"),sumCount=useCount+energyCount)),
       aes(x=monthDay,y=sumCount,color=isWeekday,shape=year,group=year))+geom_line()+geom_point()+facet_wrap(~year,ncol=1)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

####查看逐小时的能耗情况####
ggplot(data=data.hznu.area.predict.use[modiSeason%in%c("Summer"),#,"Summer_warm" date %in% c(paperTime$Summer,paperTime$Summer_warm),#& isBizday==TRUE,#,paperTime$Summer
                                       c("datetime","modiSeason","isBizday","modiElec")]%>% 
         .[complete.cases(.)]%>%melt(.,id.var=c("datetime","modiSeason","isBizday","modiElec")),aes(x=as.factor(hour(datetime)),y=modiElec))+
  geom_boxplot(width=0.5)+
  stat_summary(fun.y = "mean",geom = "point",size=2)+stat_summary(fun.y = "mean",geom = "line",group=1)+
  #facet_wrap(.~modiSeason,nrow=2)+
  theme_bw()+theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16))+ylim(0,600)

stat.hznu.area.hourlyEnergy<-data.hznu.area.predict.use[modiSeason%in%c("Summer","Summer_warm"),#date %in% c(paperTime$Summer,paperTime$Summer_warm),#& isBizday==TRUE,#,paperTime$Summer
                                                  c("datetime","modiSeason","isBizday","modiElec")]%>% 
  .[complete.cases(.)]%>%melt(.,id.var=c("datetime","modiSeason","isBizday","modiElec"))%>%
  aggregate(data=.,modiElec~modiSeason+as.factor(date(datetime)),range)#,stdModiElec

aggregate(data=stat.hznu.area.hourlyEnergy,modiElec~modiSeason,mean)
names(stat.hznu.area.hourlyEnergy)<-c("modiSeason","variable","hour","rlatErr")
ggplot(data=stat.hznu.area.predict.hourlyErr,aes(x=hour,y=rlatErr,color=variable,shape=variable,group=variable))+geom_point()+geom_line()+facet_wrap(.~modiSeason,nrow=2)



