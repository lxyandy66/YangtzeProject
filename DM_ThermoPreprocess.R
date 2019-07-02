####本脚本用于清洗HZNU热环境数据####
#导入已清洗的数据
#接"HZNU_含追加_半小时_能耗_新清洗.rdata"
data.hznu.thermo.raw<-data.hznu.predict[,c("ac_code","time","total_elec","real_temp","set_temp",
                                      "state","roomCode","hour","on_off","modifyElec")]
#筛选仅教学的建筑
info.hznu.building<-as.data.table(read.csv(file = "HZNU_建筑功能及编码对应.csv"))
data.hznu.teaching.thermo.raw<-
  data.hznu.thermo.raw[substr(roomCode,1,10) %in% info.hznu.building[type=="teaching"]$buildingCode]


data.hznu.teaching.thermo.raw<-data.hznu.teaching.thermo.raw[!is.na(real_temp)]
data.hznu.teaching.thermo.raw$date<-substr(data.hznu.teaching.thermo.raw$time,1,10)
data.hznu.teaching.thermo.raw$labelAcDay<-paste(data.hznu.teaching.thermo.raw$ac_code,
                                                data.hznu.teaching.thermo.raw$date,sep = "_")


#筛选有空调使用的记录
data.hznu.teaching.thermo.raw$labelRoomDay<-paste(data.hznu.teaching.thermo.raw$roomCode,
                                                  data.hznu.teaching.thermo.raw$date,sep = "_")
# data.hznu.teaching.thermo.raw<-
#   data.hznu.teaching.thermo.raw[labelRoomDay %in% data.hznu.use.final$labelRoomDay]#这个应该放在最后
data.hznu.teaching.thermo.raw<-
  data.hznu.teaching.thermo.raw[!duplicated(data.hznu.teaching.thermo.raw)]

####思路####
#0、条件：仅教学、房间级、有空调使用
#1、根据末端级数据，全天异常的数据（如全天温度无任何波动），此类数据无法修正
#2、根据季节特征，删去全天异常日数据（如冬季室内长时间大于40度）
#3、根据末端级数据所得全天序列，检查异常点，插值补齐异常点
#4、将末端整合至房间级



####删去全局离群点####
data.hznu.teaching.thermo.raw$modiTemp<-data.hznu.teaching.thermo.raw$real_temp
data.hznu.teaching.thermo.raw[modiTemp>40|modiTemp< 5]$modiTemp<-NA

####删去单个不符合季节特征的数据点####
data.hznu.teaching.thermo.raw$month<-as.numeric(substr(data.hznu.teaching.thermo.raw$date,6,7))
data.hznu.teaching.thermo.raw$season<- 
  apply(data.hznu.teaching.thermo.raw[,"month"], MARGIN = 1,FUN = getSeason)
data.hznu.teaching.thermo.raw[(season %in% c("Winter","Winter_warm","Spring","Autumn") &(modiTemp>33))|
                                (season %in% c("Summer","Summer_warm") &(modiTemp<17.5|modiTemp>35)) ]$modiTemp<-NA
ggplot(data=data.hznu.teaching.thermo.raw[season %in% c("Winter","Winter_warm","Spring","Autumn")],aes(x=modiTemp))+geom_density()+xlim(0,10)


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

ggplot(data=data.hznu.teaching.thermo.raw[labelAcDay=="330100D255202m01_2017-11-09"]
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
  stat.hznu.thermo.stableCheck[,.(acCount=length(unique(ac_code)),
                                  meanRange=max(mean,na.rm = TRUE)-min(mean,na.rm = TRUE)),by=labelRoomDay]
stat.hznu.thermo.acRange<-stat.hznu.thermo.acRange[acCount>1]
stat.hznu.thermo.acRange$logCount<-sapply(stat.hznu.thermo.acRange$labelRoomDay,
                                          function(x){length(!is.na(data.hznu.teaching.thermo.cleaned[labelRoomDay==x]$modiTemp))})
ggplot(data=stat.hznu.thermo.acRange,aes(x=meanRange))+geom_density()
stat.hznu.thermo.needProcess<-stat.hznu.thermo.acRange[meanRange>5]


#异常值长数据处理尝试
temp.outlier.test<-(data.hznu.teaching.thermo.cleaned[labelRoomDay=="330100D258102_2017-12-21"])
#似乎长数据来聚类效果比宽数据好，鲁棒性较强
temp.outlier.test$isSelected<-"0"
temp.outlier.test[ac_code %in% outlierModify(temp.outlier.test$modiTemp,temp.outlier.test$ac_code)]$isSelected<-"1"
ggplot(data=temp.outlier.test,aes(x=hour,y=modiTemp,color=isSelected,group=ac_code,shape=ac_code))+geom_point()+geom_line()
# temp.outlier.test$outlierCluster<-pamk(data=temp.outlier.test$modiTemp,krange=2,criterion = "ch")$pamobject$clustering
# temp.outlier.test$outlierCluster<-as.factor(temp.outlier.test$outlierCluster)
# ggplot(data=temp.outlier.test,aes(x=hour,y=modiTemp,color=outlierCluster,group=ac_code))+geom_point()+geom_line()
# temp.stat.outlier<-temp.outlier.test[,.(cluster1=length(labelAcDay[outlierCluster==1]),
#                                         cluster2=length(labelAcDay[outlierCluster==2])
#                                         ),by=ac_code]


####转化为日内小时宽数据####
# 先将需要处理的数据区分，stat.hznu.thermo.needProcess提出处理后再合并
data.hznu.teaching.thermo.cleaned$labelRoomDayHour<-
  paste(data.hznu.teaching.thermo.cleaned$labelRoomDay,data.hznu.teaching.thermo.cleaned$hour,sep = "_")
setorder(data.hznu.teaching.thermo.cleaned,ac_code,time)
#尝试一个合并的简洁写法
data.hznu.teaching.thermo.day.long<-
  data.hznu.teaching.thermo.cleaned[,.(
    time=time[1],
    roomCode=roomCode[1],
    season=season[1],
    state=getMode(state[state!="off"]),
    labelRoomDay=labelRoomDay[1],
    total_elec=sum(total_elec,na.rm = TRUE),
    modifyElec=sum(modifyElec,na.rm = TRUE),
    hour=hour[1],
    on_off=ifelse(sum(on_off,na.rm = TRUE)>0,1,0),
    real_temp=mean(real_temp,na.rm = TRUE),
    set_temp=mean(set_temp[on_off==1],na.rm = TRUE),
    modiTemp=mean(modiTemp[ifelse(
      labelRoomDay %in% stat.hznu.thermo.needProcess$labelRoomDay,
      ac_code %in% outlierModify(data.hznu.teaching.thermo.cleaned[labelRoomDay==labelRoomDay[1]]$modiTemp,
                                 data.hznu.teaching.thermo.cleaned[labelRoomDay==labelRoomDay[1]]$ac_code),
      TRUE)],na.rm = TRUE)
  ),by=labelRoomDayHour]
data.hznu.teaching.thermo.day.long$date<-apply(data.hznu.teaching.thermo.day.long[,"labelRoomDay"],
                                               MARGIN = 1,FUN = getSplitMember,splitSimbol = "_",isLastOne = TRUE)

####对清洗了能耗及热环境的长数据进行输出####
data.hznu.teaching.thermo.day.long[modifyElec==0]$state<-"off"
data.hznu.teaching.thermo.day.long$modiSeason<-data.hznu.teaching.thermo.day.long$season
data.hznu.teaching.thermo.day.long[modiSeason %in% c("Autumn","Spring")]$modiSeason<-"Transition"

data.hznu.teaching.thermo.day.long$month<-as.numeric(substr(data.hznu.teaching.thermo.day.long$time,6,7))

data.hznu.teaching.thermo.day.long$finalState<-data.hznu.teaching.thermo.day.long$state
data.hznu.teaching.thermo.day.long[finalState %in% c("dehum","dehumi")]$finalState<-"cooling"
data.hznu.teaching.thermo.day.long[(month %in% c(6:9))& !is.na(finalState) & finalState!="off"]$finalState<-"cooling"
data.hznu.teaching.thermo.day.long[(month %in% c(12, 1, 2, 3))& !is.na(finalState) & finalState!="off"]$finalState <- "heating"

data.hznu.teaching.all<-
       data.hznu.teaching.thermo.day.long[,c("time","date","season","modiSeason","roomCode","on_off","state","finalState","total_elec","modifyElec","set_temp","real_temp","modiTemp")]
save(data.hznu.teaching.all,file = "HZNU_含追加_仅教学_能耗热环境已清洗_长数据.rdata")
data.hznu.teaching.all$on_off<-as.factor(data.hznu.teaching.all$on_off)
ggplot(data=data.hznu.teaching.all[finalState%in% c("cooling","heating","off")],aes(x=modiTemp,color=modiSeason,fill=on_off,linetype=on_off))+geom_density(alpha=0.3)#+facet_wrap(~finalState)


####整理为宽数据####
data.hznu.teaching.thermo.day.final<-dcast(
  data.hznu.teaching.thermo.day.long[hour %in% sprintf("%02d",8:22),c("labelRoomDay","date","roomCode","modiTemp","hour")],
  labelRoomDay+date+roomCode~hour,value.var = "modiTemp")
names(data.hznu.teaching.thermo.day.final)<-c("labelRoomDay","date","roomCode",sprintf("h%02d",8:22))
data.hznu.teaching.thermo.day.final<-as.data.table(data.hznu.teaching.thermo.day.final)

####增加一些统计量####
data.hznu.teaching.thermo.day.final$sd<-apply(data.hznu.teaching.thermo.day.final[,4:18],MARGIN = 1,FUN = sd,na.rm=TRUE)
data.hznu.teaching.thermo.day.final$meanTemp<-apply(data.hznu.teaching.thermo.day.final[,4:18],MARGIN = 1,FUN = mean,na.rm=TRUE)
data.hznu.teaching.thermo.day.final$naCount<-apply(data.hznu.teaching.thermo.day.final[,4:18],MARGIN = 1,FUN=function(x){sum(is.na(x))})
data.hznu.teaching.thermo.day.final$month<-as.numeric(substr(data.hznu.teaching.thermo.day.final$date,6,7))
data.hznu.teaching.thermo.day.final$season<-apply(data.hznu.teaching.thermo.day.final[,"month"],MARGIN = 1,FUN = getSeason)
data.hznu.teaching.thermo.day.final.modify<-data.table(data.hznu.teaching.thermo.day.final[,c(1:3,19:23)],apply)
ggplot(data=data.hznu.teaching.thermo.day.final[naCount!=0],aes(x=naCount))+geom_density()+scale_x_continuous(breaks = c(1:15))


#
tmp.temp.test<-data.hznu.teaching.thermo.day.final[labelRoomDay %in% c("330100D276101_2017-01-05","330100D255103_2017-01-12",
                                                                       "330100D272311_2017-07-02","330100D256302_2017-07-20")]



# tmp.temp.test$ccc<-apply(tmp.temp.test[,4:18],MARGIN = 1,FUN = function(x){
#   cat(class(x))
#   tmp.prep<-na.omit(data.table(t(x)))
#   return(NbClust(data=tmp.prep,min.nc = 2,max.nc = 5,method = "kmeans"))
# })

