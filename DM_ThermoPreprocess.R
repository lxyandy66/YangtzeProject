####本脚本用于清洗HZNU热环境数据####
#导入已清洗的数据
#接"HZNU_含追加_半小时_能耗_新清洗.rdata"
data.hznu.thermo.raw<-data.hznu.predict[,c("ac_code","time","total_elec","real_temp","set_temp",
                                      "state","roomCode","hour","on_off","modifyElec")]
#筛选仅教学的建筑
info.hznu.building<-as.data.table(read.csv(file = "HZNU_建筑功能及编码对应.csv"))
data.hznu.teaching.thermo.raw<-
  data.hznu.thermo.raw[substr(roomCode,1,10) %in% info.hznu.building[type=="teaching"]$buildingCode]
#筛选有空调使用的记录
data.hznu.teaching.thermo.raw$labelRoomDay<-paste(data.hznu.teaching.thermo.raw$roomCode,
                                                  data.hznu.teaching.thermo.raw$date,sep = "_")
data.hznu.teaching.thermo.raw<-
  data.hznu.teaching.thermo.raw[labelRoomDay %in% data.hznu.use.final$labelRoomDay]
data.hznu.teaching.thermo.raw<-
  data.hznu.teaching.thermo.raw[!duplicated(data.hznu.teaching.thermo.raw)]

####思路####
#0、条件：仅教学、房间级、有空调使用
#1、根据末端级数据，全天异常的数据（如全天温度无任何波动），此类数据无法修正
#2、根据季节特征，删去全天异常日数据（如冬季室内长时间大于40度）
#3、根据末端级数据所得全天序列，检查异常点，插值补齐异常点
#4、将末端整合至房间级

####去掉全天没有波动的数据####
data.hznu.teaching.thermo.raw<-data.hznu.teaching.thermo.raw[!is.na(real_temp)]
data.hznu.teaching.thermo.raw$date<-substr(data.hznu.teaching.thermo.raw$time,1,10)
data.hznu.teaching.thermo.raw$labelAcDay<-paste(data.hznu.teaching.thermo.raw$ac_code,
                                                data.hznu.teaching.thermo.raw$date,sep = "_")

####删去全局离群点####
data.hznu.teaching.thermo.raw$modiTemp<-data.hznu.teaching.thermo.raw$real_temp
data.hznu.teaching.thermo.raw[modiTemp>40|modiTemp< 5]<-NA

####删去单个不符合季节特征的数据点####
ggplot(data=data.hznu.teaching.thermo.raw[season %in% c("Winter","Winter_warm","Spring","Autumn")],aes(x=modiTemp))+geom_density()+xlim(0,10)
data.hznu.teaching.thermo.raw$season<- 
  sapply(as.numeric(substr(data.hznu.teaching.thermo.raw$date,6,7)), getSeason)
data.hznu.teaching.thermo.raw[(season %in% c("Winter","Winter_warm","Spring","Autumn") &(modiTemp>33))|
                                (season %in% c("Summer","Summer_warm") &(modiTemp<17.5|modiTemp>35)) ]$modiTemp<-NA

stat.hznu.thermo.stableCheck<-data.hznu.teaching.thermo.raw[,.(ac_code=ac_code[1],
                                                               count=length(time),
                                                               date=date[1],
                                                               mean=mean(modiTemp,na.rm =TRUE),
                                                               sd=sd(modiTemp,na.rm = TRUE),
                                                               month=substr(date[1],6,7),
                                                               maxTemp=max(modiTemp,na.rm = TRUE),
                                                               minTemp=min(modiTemp,na.rm = TRUE)
                                                               ),
                                                            by=labelAcDay]
stat.hznu.thermo.stableCheck$range<-stat.hznu.thermo.stableCheck$maxTemp-stat.hznu.thermo.stableCheck$minTemp
ggplot(data=data.hznu.teaching.thermo.raw[labelAcDay=="330100D258203m03_2017-10-20"]
       ,aes(x=hour,y=real_temp))+geom_point()
length(stat.hznu.thermo.stableCheck[sd==0]$labelAcDay)
#[1] 1363

#取样参考低水平sd值的数据是否可用
nn<-data.hznu.teaching.thermo.raw[labelAcDay=="330100D276063m01_2017-08-25"]
stat.hznu.thermo.stableCheck<-stat.hznu.thermo.stableCheck[sd>0.2 & range>1 & minTemp<40 &
                                                             complete.cases(stat.hznu.thermo.stableCheck)]

#删去全天无波动的记录
ggplot(data=stat.hznu.thermo.stableCheck,aes(x=count))+geom_density()#+xlim(0,5)
data.hznu.teaching.thermo.cleaned<-
  data.hznu.teaching.thermo.raw[labelAcDay %in% stat.hznu.thermo.stableCheck$labelAcDay]


####整合至小时级####
data.hznu.teaching.thermo.cleaned$datetime<-as.POSIXct(data.hznu.teaching.thermo.cleaned$time)
setorder(data.hznu.teaching.thermo.cleaned,ac_code,datetime)
