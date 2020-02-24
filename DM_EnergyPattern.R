####能耗聚类模式####
#接能耗已处理清洗数据 data.hznu.predict(已清洗_末端半小时级)

data.hznu.predict$date<-substr(data.hznu.predict$time,1,10)

data.hznu.predict$labelRoomHour<-paste(data.hznu.predict$roomCode,data.hznu.predict$date,
                                       data.hznu.predict$hour,sep = "_")

#整理已清洗数据至末端-小时级
data.hznu.predict$labelAcHour<-paste(data.hznu.predict$ac_code,
                                     data.hznu.predict$date,data.hznu.predict$hour,sep = "_")
setorder(data.hznu.predict,time,ac_code)
data.hznu.energy.ac.hourly<-data.hznu.predict[,.(time=time[1],
                                                 ac_code=ac_code[1],
                                                 total_elec=sum(total_elec,na.rm = TRUE),
                                                 on_off=ifelse(sum(on_off,na.rm = TRUE)>0,1,0),
                                                 modifyElec=sum(modifyElec,na.rm = TRUE),
                                                 date=date[1]
                                                 ),by=labelAcHour]
data.hznu.energy.ac.hourly$labelAcDay<-paste(data.hznu.energy.ac.hourly$ac_code,
                                             data.hznu.energy.ac.hourly$date,sep = "_")
                                           # as.Date(data.hznu.energy.ac.hourly$time),sep = "_")
                                           ##!!!!切记如果直接用as.Date()会有时区问题，默认时区是UTC
#####整理末端-小时级数据至以天为单位的宽数据####
#统计各空调一天记录是否完整
data.hznu.energy.ac.hourly$time<-as.POSIXct(data.hznu.energy.ac.hourly$time,format="%Y-%m-%d %H:%M:%S")
data.hznu.energy.ac.hourly$hour<-format(data.hznu.energy.ac.hourly$time, format = "%H")
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly[hour %in% sprintf("%02d",c(8:22))]
data.hznu.energy.checkComplete<-data.hznu.energy.ac.hourly.pickup[,.(
  ac_code=ac_code[1],count=length(ac_code)),by=labelAcDay]
#未清洗仅修正
#记录小时    1       2       3       4       5       6       7       8       9      10      11      12      13      14      15 
#条数       2236    1794    1758    1909    2042    3251    5202    4497    3811    3087    3437    4280    4970    5208 1125372 
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly.pickup[
  labelAcDay %in% data.hznu.energy.checkComplete[count==15]$labelAcDay]#剔除不完整的数据
# rm(data.hznu.energy.ac.hourly)
#修正开关位标记
data.hznu.energy.ac.hourly.pickup[total_elec<0.2]$on_off<-0
data.hznu.energy.ac.hourly.pickup[total_elec>=0.2&on_off==0]$on_off<-1

setorder(data.hznu.energy.ac.hourly.pickup,time,ac_code)
data.hznu.energy.ac.day<-data.hznu.energy.ac.hourly.pickup[,.(date=date[1],
                                                       ac_code=ac_code[1],
                                                       runtime=sum(on_off,na.rm = TRUE),
                                                       sumElec=sum(modifyElec,na.rm = TRUE),
                                                       h8=modifyElec[1],
                                                       h9=modifyElec[2],
                                                       h10=modifyElec[3],
                                                       h11=modifyElec[4],
                                                       h12=modifyElec[5],
                                                       h13=modifyElec[6],
                                                       h14=modifyElec[7],
                                                       h15=modifyElec[8],
                                                       h16=modifyElec[9],
                                                       h17=modifyElec[10],
                                                       h18=modifyElec[11],
                                                       h19=modifyElec[12],
                                                       h20=modifyElec[13],
                                                       h21=modifyElec[14],
                                                       h22=modifyElec[15]
                                                       ),by=labelAcDay]
# rm(data.hznu.energy.ac.hourly.pickup)
####空调末端/天 宽数据整理成 房间/天 宽数据####
data.hznu.energy.ac.day$roomCode<-substr(data.hznu.energy.ac.day$ac_code,1,13)
data.hznu.energy.ac.day$labelRoomDay<-paste(data.hznu.energy.ac.day$roomCode,
                                            data.hznu.energy.ac.day$date,sep = "_")

setorder(data.hznu.energy.ac.day,date,ac_code)
data.hznu.energy.room.day<-data.hznu.energy.ac.day[,.(roomCode=roomCode[1],
                                                      acCount=length(unique(ac_code)),
                                                      date=date[1],
                                                      sumElec=sum(sumElec,na.rm = TRUE),
                                                      h8=sum(h8,na.rm = TRUE),
                                                      h9=sum(h9,na.rm = TRUE),
                                                      h10=sum(h10,na.rm = TRUE),
                                                      h11=sum(h11,na.rm = TRUE),
                                                      h12=sum(h12,na.rm = TRUE),
                                                      h13=sum(h13,na.rm = TRUE),
                                                      h14=sum(h14,na.rm = TRUE),
                                                      h15=sum(h15,na.rm = TRUE),
                                                      h16=sum(h16,na.rm = TRUE),
                                                      h17=sum(h17,na.rm = TRUE),
                                                      h18=sum(h18,na.rm = TRUE),
                                                      h19=sum(h19,na.rm = TRUE),
                                                      h20=sum(h20,na.rm = TRUE),
                                                      h21=sum(h21,na.rm = TRUE),
                                                      h22=sum(h22,na.rm = TRUE)
                                                      ),by=labelRoomDay]
ggplot(data=data.hznu.energy.room.day[sumElec!=0],aes(x=sumElec))+geom_density()+xlim(0,150)

####增加基本使用模式标签####
# basePattern={"noneUSe","periodUse","fullUse"}
data.hznu.energy.room.day<-
  merge(x=data.hznu.energy.room.day,y=data.hznu.use.room.day[,c("labelRoomDay","finalState","basePattern")],
        by = "labelRoomDay",all.x = TRUE)
data.hznu.energy.room.day[sumElec==0]$basePattern<-"noneUse"
data.hznu.energy.room.day$zeroCount<-apply(data.hznu.energy.room.day[,c(6:20)],1,function(x){sum(x==0)})
data.hznu.energy.room.day[is.na(basePattern)]$basePattern<-
  ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==15,"noneUse",
         ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==0,"fullUse","periodUse"))
nn<-data.hznu.energy.room.day[which(rowSums(is.na(data.hznu.energy.room.day))>0),]#行为finalState及basePattern缺失83781条

# save(data.hznu.energy.room.day,data.hznu.energy.ac.hourly,
#      file = "HZNU_能耗聚类预处理结果_仅工作时间_按原始修正能耗.rdata")


####接仅教学能耗数据集####
####增加基本使用模式标签####
# basePattern={"noneUSe","periodUse","fullUse"}
data.hznu.teaching.energy.final<-
  merge(x=data.hznu.teaching.energy,y=data.hznu.use.final[,c("labelRoomDay","finalState","basePattern","clusterName","runtime")],
        by = "labelRoomDay",all.x = TRUE)
data.hznu.teaching.energy.final<-data.hznu.teaching.energy.final[sumElec!=0]
nn<-data.hznu.teaching.energy.final[which(rowSums(is.na(data.hznu.teaching.energy.final))>0),]#行为finalState及basePattern缺失1787条
ggplot(data = data.hznu.teaching.energy.final[!labelRoomDay %in% nn$labelRoomDay],aes(x=sumElec))+geom_density()+xlim(0,150)

####计算得单位空调能耗####
tmp.mean<-data.table(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22),"acCount")])
tmp.mean[,c(sprintf("euiH%d",8:22))]<-tmp.mean[,c(sprintf("h%d",8:22))]/tmp.mean$acCount
data.hznu.teaching.energy.final<-cbind(data.hznu.teaching.energy.final,tmp.mean[,c(sprintf("euiH%d",8:22))])

#不加all前缀说明仅考虑使用时
data.hznu.teaching.energy.final$sumUiElec<-
  data.hznu.teaching.energy.final$sumElec/data.hznu.teaching.energy.final$acCount
data.hznu.teaching.energy.final$sdElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22))],
                                              MARGIN = 1,FUN = function(x){ sd(x[x>0.2],na.rm = TRUE)})
data.hznu.teaching.energy.final$sdAllElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22))],
                                              MARGIN = 1,FUN = sd,na.rm=TRUE)
data.hznu.teaching.energy.final$sdUiElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("euiH%d",8:22))],
                                              MARGIN = 1,FUN = function(x){ sd(x[x>0.2],na.rm = TRUE)})#sapply为啥不对
data.hznu.teaching.energy.final$sdUiAllElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("euiH%d",8:22))],
                                                 MARGIN = 1,FUN = sd,na.rm=TRUE)#sapply为啥不对
#乱改变量含义干嘛！？！？！meanElec变成台均时均能耗了
data.hznu.teaching.energy.final$meanUiElec<-
  data.hznu.teaching.energy.final$sumUiElec/data.hznu.teaching.energy.final$runtime

#增加统计变量：房间空调使用时间电耗均值
data.hznu.teaching.energy.final$meanUseRoomElec<-data.hznu.teaching.energy.final$sumElec/data.hznu.teaching.energy.final$runtime
data.hznu.teaching.energy.final$meanAcElec<-data.hznu.teaching.energy.final$sumElec/
  (data.hznu.teaching.energy.final$runtime*data.hznu.teaching.energy.final$acCount)
data.hznu.teaching.energy.final$meanAllElec<-data.hznu.teaching.energy.final$sumElec/15

#对于单台空调能耗设上限
ecLim<-mean(data.hznu.teaching.energy.final$meanAcElec,na.rm = TRUE)+
  3*sd(data.hznu.teaching.energy.final$meanAcElec,na.rm = TRUE)
#apply(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22))],MARGIN = 1,mean,na.rm=TRUE)#不应该用这个
data.hznu.teaching.energy.final<-data.hznu.teaching.energy.final[!is.na(runtime)]
ggplot(data=data.hznu.teaching.energy.final,aes(x=meanAcElec))+geom_density()+xlim(0,ecLim)



####能耗数据的归一化处理####
#使用z-score算法零-均值标准化
data.hznu.teaching.energy.std<-data.hznu.teaching.energy.final[sumElec<=150&meanAcElec<=ecLim]

temp.std<-data.table(scale(data.hznu.teaching.energy.std[,c(sprintf("euiH%d",8:22))],center = FALSE))
names(temp.std)<-sprintf("stdH%d",8:22)
temp.std$stdSumEuiElec<-scale(data.hznu.teaching.energy.std$sumUiElec,center = FALSE)#日内房间使用时刻台均总能耗
temp.std$stdSumElec<-scale(data.hznu.teaching.energy.std$sumElec,center = FALSE)#日内房间使用时刻房间总能耗
temp.std$stdRuntime<-scale(data.hznu.teaching.energy.std$runtime,center = FALSE)#使用时长
temp.std$stdAcCount<-scale(data.hznu.teaching.energy.std$acCount,center = FALSE)#空调数
#temp.std$stdMeanElec<-scale(data.hznu.teaching.energy.std$meanElec,center = FALSE)#这个变量改名了
temp.std$stdMeanAcElec<-scale(data.hznu.teaching.energy.std$meanAcElec,center = FALSE)#日内房间所有时刻台均时均能耗
temp.std$stdSdElec<-scale(data.hznu.teaching.energy.std$sdElec,center = FALSE)#使用时房间逐时能耗标准差
temp.std$stdSdAllElec<-scale(data.hznu.teaching.energy.std$sdAllElec,center = FALSE)#日内房间所有时刻能耗标准差
temp.std$stdMeanUseRoomElec<-scale(data.hznu.teaching.energy.std$meanUseRoomElec,center = FALSE)#使用时能耗时均值
data.hznu.teaching.energy.std<-cbind(data.hznu.teaching.energy.std,temp.std)

ggplot(data=data.hznu.teaching.energy.std,aes(x=runtime,color=clusterName))+geom_density()
nn<-boxplot(data =data.hznu.teaching.energy.std,runtime~clusterName,outline = FALSE)
boxplot.stats(x=data.hznu.teaching.energy.std,runtime~clusterName)
cor(data.hznu.energy.tryCluster[,c("stdSumElec","stdSdAllElec","stdRuntime")])

# data.hznu.energy.tryCluster<-data.hznu.teaching.energy.final[finalState=="cooling"&sumElec<=150,c("sdElec","meanElec","sumElec","runtime")]
#,"meanElec","sumElec","runtime")]

####聚类对象选择####
modeSelect<-"cooling"
usePAM<-TRUE
clusterAttr<-c("stdSumElec","stdSdAllElec","stdRuntime")
data.hznu.energy.tryCluster<-data.hznu.teaching.energy.std[finalState==modeSelect]

ggplot(data=data.hznu.energy.tryCluster,aes(x=runtime))+geom_density()+scale_x_continuous(breaks = c(1:16))#+xlim(0,100)

wssClusterEvaluate(data = data.hznu.energy.tryCluster[,..clusterAttr],
                   maxIter = 1000,maxK = 10)
pamkClusterEvaluate(
  data = data.hznu.energy.tryCluster[,..clusterAttr],
  criter = "asw",startK = 2,endK = 10)
multiplyClusterEvaluate(data = data.hznu.energy.tryCluster)
# stat.energy.bestK<-NbClust(data=data.hznu.teaching.energy.final[,c("sdElec","meanElec","sumElec","runtime")],
#                            min.nc = 2,max.nc = 4,method = "kmeans")#内存不够
#3或4类
####分类法一####
#直接按照总体特征进行分类
for(i in 2){
energy.pamk<-pamk(data = data.hznu.energy.tryCluster[,..clusterAttr],
                  krange = i,criterion = "ch",critout = TRUE,usepam = usePAM)
# energy.pamk
data.hznu.energy.tryCluster$energyCluster<-energy.pamk$pamobject$clustering
# data.hznu.energy.tryCluster$sdElec<-apply(data.hznu.energy.tryCluster[,c(sprintf("h%d",8:22))],
#                                               MARGIN = 1,FUN = function(x){ sd(x>0.2,na.rm = TRUE)})#sapply为啥不对
# data.hznu.energy.tryCluster$sumElec<-apply(data.hznu.energy.tryCluster[,c(sprintf("h%d",8:22))],MARGIN = 1,FUN = sum,na.rm=TRUE)
stat.hznu.energy.tryCluster.descr<-data.hznu.energy.tryCluster[,.(
  count=length(labelRoomDay),
  runtime=mean(runtime,na.rm = TRUE),
  sumUiElec=mean(sumUiElec,na.rm = TRUE),
  sumElec=mean(sumElec,na.rm = TRUE),
  sdSumElec=sd(sumElec,na.rm = TRUE),
  meanAcCount=mean(acCount,na.rm = TRUE),
  meanSd=mean(sdAllElec,na.rm = TRUE),
  meanElec=mean(meanElec,na.rm = TRUE),#台均
  meanUseRoomElec=mean(meanUseRoomElec,na.rm = TRUE),#室内空调使用时均
  onDemandUsage=length(labelRoomDay[clusterName=="OnDemand"]),
  forenoonUsage=length(labelRoomDay[clusterName=="Forenoon"]),
  afternoonUsage=length(labelRoomDay[clusterName=="Afternoon"]),
  daytimeUsage=length(labelRoomDay[clusterName=="Daytime"]),
  laterDaytimeUsage=length(labelRoomDay[clusterName=="LateDayTime"]),
  allDayUsage=length(labelRoomDay[clusterName=="All-Day"])
),by=energyCluster]
# stat.hznu.energy.tryCluster<-describeBy(x = data.hznu.energy.tryCluster[,c("meanAcElec","meanElec","sumElec")],
#                                         group = list(usageCluster=data.hznu.energy.tryCluster$clusterName,
#                                                      energyCluster=data.hznu.energy.tryCluster$energyCluster,
#                                                      acMode=data.hznu.energy.tryCluster$finalState),mat=TRUE)
write.xlsx(x=stat.hznu.energy.tryCluster.descr,
           file=paste(i,modeSelect,"3var_sumElec_sdAll_runtime",ifelse(usePAM,"PAM","noPAM"),"descr","EnergyPattern.xlsx",sep = "_"))
# write.xlsx(x=stat.hznu.energy.tryCluster,file=paste(i,modeSelect,"3var",ifelse(usePAM,"PAM","noPAM"),"EnergyPattern.xlsx",sep = "_"))
ggsave(file=paste(i,modeSelect,"3var_sumElec_sdAll_runtime",ifelse(usePAM,"PAM","noPAM"),"EnergyPattern_dist.png",sep = "_"),
       plot = ggplot(data=stat.hznu.energy.tryCluster.descr,
                     aes(x=runtime,y=sumElec,size=meanSd,color=meanUseRoomElec))+geom_point(),
       width=8,height = 6,dpi = 100
       )
}

ggplot(data.hznu.energy.tryCluster,aes(x=stdSdAllElec,color=clusterName))+geom_density()#+xlim(0,4)

####集合聚类####
info.energy.clusterName<-as.data.table(read.xlsx(file = "HZNU_EnergyClusterMapping.xlsx",sheetName = "cooling=4"))
data.hznu.teaching.energy.std$energyCluster<- -1
data.hznu.teaching.energy.std$energyClusterName<-""
for(i in unique(data.hznu.teaching.energy.std$finalState)){
  data.hznu.teaching.energy.std[finalState==i]$energyCluster<-pamk(
    data.hznu.teaching.energy.std[finalState==i,..clusterAttr],
    krange = nrow(info.energy.clusterName[finalState==i]),
    criterion = "ch",
    usepam = TRUE,
    critout = TRUE
  )$pamobject$clustering
  for(j in unique(data.hznu.teaching.energy.std[finalState==i]$energyCluster)){
    data.hznu.teaching.energy.std[finalState==i & energyCluster==j]$energyClusterName<-
      info.energy.clusterName[finalState==i & patternCode==j]$patternName
  }
}

####统一能耗参数命名####
#此处注意无需重复执行
data.hznu.teaching.energy.std[energyClusterName=="ShortTime_LowEnergy"]$energyClusterName<-"LowEnergy"
data.hznu.teaching.energy.std[energyClusterName=="MidTime_MidEnergy"]$energyClusterName<-"MidEnergy"
data.hznu.teaching.energy.std[energyClusterName=="LongTime_LowEnergy"]$energyClusterName<-"LongTime_MidEnergy"
data.hznu.teaching.energy.std[energyClusterName=="LongTime_HighEnergy"]$energyClusterName<-"LongTime_HighEnergy"
ggplot(data.hznu.teaching.energy.std,aes(x=energyClusterName,y=sumElec))+geom_boxplot()


####加载房间使用强度指标####
#接"HZNU_含追加_全部类型_房间级_含使用强度_行为模式预处理完成.rdata"
# data.hznu.all.use 数据集
# temp.stat.energy.room
data.hznu.teaching.energy.std<-merge(x=data.hznu.teaching.energy.std,
                                     y=data.hznu.all.use[,c("labelRoomDay","acIntensity","acUsedRate","meanAcUsed")],
                                     all.x = TRUE,by.x="labelRoomDay",by.y="labelRoomDay")
data.hznu.teaching.energy.std<-merge(x = data.hznu.teaching.energy.std,y=temp.stat.energy.room[,c("labelRoomDay","meanDailyAcElec")],
                                     all.x = TRUE,by="labelRoomDay")

stat.hznu.energy.tryCluster.descr<-data.hznu.teaching.energy.std[,.(
  count=length(labelRoomDay),
  finalState=unique(finalState),
  energyClusterName=energyClusterName[1],
  runtime=mean(runtime,na.rm = TRUE),
  sumElec=mean(sumElec,na.rm = TRUE),
  sumUiElec=mean(sumUiElec,na.rm = TRUE),
  sdElec=sd(sumElec,na.rm = TRUE),
  meanSdHourlyElec=mean(sdAllElec,na.rm = TRUE),
  meanElec=mean(meanElec,na.rm = TRUE),
  meanAcElec=mean(meanAcElec,na.rm = TRUE),
  meanDailyAcElec=mean(meanDailyAcElec,na.rm = TRUE),
  meanAcUseIntensity=mean(acIntensity[acCount!=1],na.rm = TRUE),
  meanAcUsedRate=mean(acUsedRate[acCount!=1],na.rm = TRUE),
  meanAcCount=mean(acCount,na.rm = TRUE),
  meanAcUsedCount=mean(meanAcUsed,na.rm = TRUE),
  meanMulitiAcCount=mean(acCount[acCount!=1],na.rm = TRUE),
  meanMulitiAcUsedCount=mean(meanAcUsed[acCount!=1],na.rm = TRUE),
  onDemandUsage=length(labelRoomDay[clusterName=="OnDemand"]),
  forenoonUsage=length(labelRoomDay[clusterName=="Forenoon"]),
  afternoonUsage=length(labelRoomDay[clusterName=="Afternoon"]),
  daytimeUsage=length(labelRoomDay[clusterName=="Daytime"]),
  laterDaytimeUsage=length(labelRoomDay[clusterName=="LateDayTime"]),
  allDayUsage=length(labelRoomDay[clusterName=="All-Day"])
),by=paste(energyClusterName,finalState,sep = "_")]

write.xlsx(x=stat.hznu.energy.tryCluster.descr,file = "HZNU_teaching_EnergyPattern_Evaluate.xlsx")

nn<-boxplot(acIntensity~energyClusterName,data=data.hznu.teaching.energy.std[finalState=="cooling"])


data.hznu.teaching.energy.std$meanRuntime<-apply(data.hznu.teaching.energy.std[,c("finalState","energyClusterName")],MARGIN = 1,
                                                 FUN = function(x){ return(stat.hznu.energy.tryCluster.descr[paste==paste(x[2],x[1],sep = "_")]$runtime)})

#合并房间面积数据，计算EUI
# data.hznu.teaching.energy.std[,c("area","modiArea","seatCount","modiSeat")]<-NULL
data.hznu.teaching.energy.std<-merge(x=data.hznu.teaching.energy.std,y=info.hznu.teaching.room[,c("roomCode","area","modiArea","seatCount","modiSeat")],
                                     by.x="roomCode",by.y="roomCode",all.x = TRUE)
data.hznu.teaching.energy.std$areaEUI<-data.hznu.teaching.energy.std$sumElec/data.hznu.teaching.energy.std$modiArea
stat.hznu.teaching.energy.bySeat<-data.hznu.teaching.energy.std[,.(meanSumElec=mean(sumElec,na.rm = TRUE),
                                                                   meanAreaEUI=mean(areaEUI,na.rm = TRUE),
                                                                   modiSeat=modiSeat[1],
                                                                   count=length(labelRoomDay),
                                                                   finalState=finalState[1]),by=paste(finalState,modiSeat,sep = "_")]


data.hznu.teaching.energy.std$modiSeason<-apply(X = data.hznu.teaching.energy.std[,"date"],MARGIN = 1,
                                                FUN = function(x){
                                                  season<-getSeason(as.numeric(substr(x,6,7)))
                                                  if(season %in% c("Spring","Autumn"))
                                                    return("Transition")
                                                  else
                                                    return(season)
                                                })

ggplot(data=stat.hznu.energy.tryCluster.descr,
       aes(x=runtime,y=sumElec,size=sdElec,color=energyClusterName))+
  geom_point()+facet_wrap(~finalState)+theme_bw()#+scale_size_area(range=c(0,50))


ggplot(data=data.hznu.teaching.energy.std,aes(x=runtime,y=sumElec,color=energyClusterName))+facet_wrap(~finalState)+geom_density2d(size=1,binwidth=0.002)+
  geom_point(alpha=0.3,position = "jitter")+ylim(c(0,100))+#xlim(c(0,5))+#stat_density_2d(aes(fill=..level..,color=energyClusterName),geom="polygon")+
  theme_bw()

#检查教室容量与EUI或绝对能耗分布关系
ggplot(data=data.hznu.teaching.energy.std[!is.na(modiSeat)&modiSeat!=0] %>% mutate(.,modiSeat=as.factor(modiSeat)),
       aes(x=modiSeat,y=areaEUI))+#ylim(0,2.5)+ color=energyClusterName
  geom_boxplot(outlier.colour = NA,width=0.5)+facet_wrap(~finalState)+  
  geom_line(data=stat.hznu.teaching.energy.bySeat[modiSeat!=0],aes(x=factor(modiSeat),y=meanAreaEUI,group=finalState))+
  geom_point(data=stat.hznu.teaching.energy.bySeat[modiSeat!=0],aes(x=factor(modiSeat),y=meanAreaEUI,group=finalState))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,2.5)+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),strip.text =element_text(size=14),
        legend.text = element_text(size=14))


#检查各能耗模式对应的教室EUI或能耗
ggplot(data=data.hznu.teaching.energy.std[modiSeat!=0],
       aes(y=areaEUI,x=factor(energyClusterName,order=TRUE,levels=c("LowEnergy","MidEnergy","LongTime_MidEnergy","LongTime_HighEnergy")),color=energyClusterName))+
  geom_boxplot(width=0.5)+facet_wrap(~as.factor(modiSeat),nrow=2)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),strip.text =element_text(size=14),
        legend.text = element_text(size=14))

#检查各教室的空调使用情况，使用强度与使用台数和面积的关系
ggplot(data=data.hznu.teaching.energy.std[!is.na(modiSeat)&modiSeat!=0&acIntensity<=1],aes(x=as.factor(modiSeat),y=acIntensity))+geom_boxplot(width=0.5)+
  stat_summary(aes(x=as.factor(modiSeat),y=meanAcUsed/16,color="red",group=1,size=0.09),fun.y=mean,geom="point")+
  stat_summary(aes(x=as.factor(modiSeat),y=meanAcUsed/16,color="red",group=1,size=0.08),fun.y=mean,geom="line")+
  # stat_summary(aes(x=as.factor(modiSeat),y=acCount/16,color="green",group=1,size=0.09),fun.y=mean,geom="point")+#这个空调数不要最好，相同规模的acCount在制冷制热中曲线不同
  # stat_summary(aes(x=as.factor(modiSeat),y=acCount/16,color="green",group=1,size=0.08),fun.y=mean,geom="line")+
  scale_y_continuous(sec.axis = sec_axis(~.*16,name = "meanAcUsed"))+facet_wrap(~finalState)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"),strip.text =element_text(size=16),
        legend.text = element_text(size=16))

#检查各行为模式下空调使用强度
ggplot(data = data.hznu.teaching.energy.std[acCount>1&acIntensity<=1],aes(x=clusterName,y=acIntensity))+
  geom_boxplot()+stat_summary(fun.y = "mean",geom = "point")+facet_wrap(~finalState)
ggplot(data=data.hznu.teaching.energy.std[clusterName %in% c("Forenoon","Afternoon")&finalState=="cooling"&modiSeat!=0&acCount>1&acIntensity<=1],aes(x=clusterName,y=meanAcUsed))+
  geom_boxplot(width=0.5)+stat_summary(fun.y = "mean",geom = "point")+facet_wrap(~finalState)

#各行为下教室使用占比
data.hznu.teaching.energy.std$areaScale<-apply(X = data.hznu.teaching.energy.std[,"acCount"],MARGIN = 1,FUN = getAreaLevel)
ggplot(data=data.hznu.teaching.energy.std[clusterName %in% c("Forenoon","Afternoon")&finalState=="cooling"&modiSeat!=0],
       aes(fill=clusterName,x=as.factor(modiSeat)))+geom_bar()#真没啥区别

#不同规模教室各季节空调使用时长
ggplot(data=data.hznu.teaching.decoupling,aes(x=areaScale,y=runtime))+geom_boxplot(width=0.5)+
  stat_summary(fun.y = "mean",geom = "point",size=2)+stat_summary(fun.y = "mean",geom = "line",group=1)+scale_y_continuous(breaks=seq(0,18,2))+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))+
  facet_wrap(~modiSeason,nrow=1)#

#不同规模教室各季节空调使用模式分布
ggplot(data=data.hznu.teaching.decoupling,aes(fill=clusterName,x=areaScale))+geom_bar(position = "fill",width=0.75)+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))+
  facet_wrap(~modiSeason,nrow=1)+
  scale_fill_brewer(palette="Greys")+scale_color_manual(values=rep("grey",6))

ggplot(data=data.hznu.teaching.energy.std[clusterName %in% c("Forenoon","Afternoon")&finalState=="cooling"&modiSeat!=0],
       aes(x=as.factor(modiSeat),y=acIntensity,color=clusterName,group=clusterName))+stat_summary(fun.y = "mean",geom = "point")+stat_summary(fun.y = "mean",geom = "line")

#不同教室空调使用率区别

nn<-data.hznu.teaching.use[runtime!=15] %>% .[,.(roomCode=roomCode[1],
                                             date=date[1],
                                             modiSeason=modiSeason[1],
                                             modiSeat=modiSeat[1],
                                             areaScale=areaScale[1],
                                             meanRuntime=mean(runtime[runtime!=0],na.rm = TRUE),
                                             useRate=length(labelRoomDay[runtime!=0])/length(labelRoomDay),
                                             useCount=length(labelRoomDay[runtime!=0])),
                                          by=(labelRoomSeason=paste(roomCode,modiSeason,sep = "_"))]
# write.xlsx(nn,"HZNU_原始数据_教室空调日使用率.xlsx")
# =IF(F2>90,"90人以上",F2)
for(i in unique(nn$areaScale)){
  cat(i,"\t",mean(nn[areaScale==i&modiSeason=="Summer_warm"]$useRate),"\n")
}

ggplot(data=nn[useRate<0.4],aes(x=areaScale,y=useRate))+geom_boxplot(width=0.5)+
  stat_summary(fun.y = "mean",geom = "point",size=2)+stat_summary(fun.y = "mean",geom = "line",group=1)+#scale_y_continuous(breaks=seq(0,18,2))+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))+
  facet_wrap(~modiSeason,nrow=1)#

#各行为模式下空调设定温度分布
ggplot(data=data.hznu.teaching.decoupling[finalState=="cooling"],aes(x=clusterName,y=setTemp))+geom_boxplot()+stat_summary(fun.y = "mean",geom = "point",color="red")#+facet_wrap(~finalState)

#不同季节下行为模式和能耗
ggplot(data=data.hznu.teaching.energy.std[clusterName %in% c("Forenoon","Afternoon")&finalState=="cooling"&modiSeat!=0],
       aes(x=substr(date,6,7),fill=energyClusterName))+geom_bar(position = "fill")+facet_wrap(~clusterName+areaScale,nrow=2)#真没啥区别

#检查16台空调的教室的空调使用情况
nn<-data.hznu.teaching.energy.std[acCount==16]
ggplot(data = data.hznu.teaching.energy.std[acCount==16],aes(x=meanAcUsed,color=roomCode))+geom_density()

#检查各能耗模式的EUI分布情况
ggplot(data=data.hznu.teaching.energy.std,aes(x=energyClusterName,y=areaEUI))+geom_boxplot(outlier.colour = NA,width=0.5)+
  stat_summary(fun.y =mean,geom = "point")+
  facet_wrap(~finalState)+ylim(0,2.5)+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),strip.text =element_text(size=14),
        legend.text = element_text(size=14),legend.position = c(0.1,0.9))

