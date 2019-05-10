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
                                                               runtime=sum(on_off,na.rm = TRUE),
                                                               mean=mean(modiTemp,na.rm =TRUE),
                                                               sd=sd(modiTemp,na.rm = TRUE),
                                                               month=substr(date[1],6,7),
                                                               maxTemp=max(modiTemp,na.rm = TRUE),
                                                               minTemp=min(modiTemp,na.rm = TRUE)
                                                               ),
                                                            by=labelAcDay]
stat.hznu.thermo.stableCheck$range<-stat.hznu.thermo.stableCheck$maxTemp-stat.hznu.thermo.stableCheck$minTemp
stat.hznu.thermo.stableCheck$labelRoomDay<-paste(substr(stat.hznu.thermo.stableCheck$ac_code,1,13),stat.hznu.thermo.stableCheck$date,sep = "_")

ggplot(data=data.hznu.teaching.thermo.raw[labelAcDay=="330100D258203m03_2017-10-20"]
       ,aes(x=hour,y=real_temp))+geom_point()
length(stat.hznu.thermo.stableCheck[sd==0]$labelAcDay)
#[1] 1363

#取样参考低水平sd值的数据是否可用
nn<-data.hznu.teaching.thermo.raw[labelAcDay=="330100D276063m01_2017-08-25"]
stat.hznu.thermo.stableCheck<-stat.hznu.thermo.stableCheck[sd>0.2 & range>1 & minTemp<40 & range<20 &
                                                             complete.cases(stat.hznu.thermo.stableCheck)]

#删去全天无波动的记录
ggplot(data=stat.hznu.thermo.stableCheck,aes(x=range))+geom_density()+xlim(0,15)
data.hznu.teaching.thermo.cleaned<-
  data.hznu.teaching.thermo.raw[labelAcDay %in% stat.hznu.thermo.stableCheck$labelAcDay]


#参考单空调小时内温度变化情况
stat.hznu.thermo.acRange<-
  stat.hznu.thermo.stableCheck[length(unique(ac_code))>1,.(acCount=length(unique(ac_code)>1),
                                                           meanRange=max(mean,na.rm = TRUE)-min(mean,na.rm = TRUE)),by=labelRoomDay]
stat.hznu.thermo.acRange$logCount<-sapply(stat.hznu.thermo.acRange$labelRoomDay,
                                          function(x){length(!is.na(data.hznu.teaching.thermo.cleaned[labelRoomDay==x]$modiTemp))})

ggplot(data=stat.hznu.thermo.acRange,aes(x=meanRange))+geom_density()
nn<-data.hznu.teaching.thermo.cleaned[labelRoomDay=="330100D260201_2017-03-13"]
#ggplot(data=nn,aes(x=hour,y=modiTemp))+geom_boxplot()
ggplot(data=nn,aes(x=hour,y=modiTemp,color=ac_code,group=ac_code))+geom_point()+geom_line()
nn$hourMin<-substr(nn$time,12,16)
nn1<-dcast(nn[,c("ac_code","modiTemp","hourMin")],ac_code~hourMin,value.var = "modiTemp")
outlierModify(tempSeq = nn1[,c(2:49)],ac_code = nn1$ac_code)


# #异常值处理尝试
# temp.outlier.test<-data.hznu.teaching.thermo.cleaned[labelRoomDay=="330100D257209_2017-02-21"]
# temp.outlier.test$hourMin<-substr(temp.outlier.test$time,12,16)
# temp.outlier.test.wid<-dcast(temp.outlier.test[,c("ac_code","modiTemp","hourMin")],ac_code~hourMin,value.var = "modiTemp")
# #似乎宽数据来聚类效果比长数据好
# temp.outlier.test.wid$outlierCluster<-pamk(data=temp.outlier.test.wid[,c(2:49)],krange=2,criterion = "ch")$pamobject$clustering
# temp.outlier.test.wid$outlierCluster<-as.factor(temp.outlier.test.wid$outlierCluster)
# ggplot(data=temp.outlier.test,aes(x=hour,y=modiTemp,color=ac_code,group=ac_code))+geom_point()+geom_line()
# temp.stat.outlier<-temp.outlier.test[,.(cluster1=length(labelAcDay[outlierCluster==1]),
#                                         cluster2=length(labelAcDay[outlierCluster==2])
#                                         ),by=ac_code]
# outlierModify(temp.outlier.test.wid[,c(2:49)],temp.outlier.test.wid$ac_code)



