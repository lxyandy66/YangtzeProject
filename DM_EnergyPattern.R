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
  merge(x=data.hznu.teaching.energy,y=data.hznu.use.final[,c("labelRoomDay","finalState","basePattern","clusterName")],
        by = "labelRoomDay",all.x = TRUE)
data.hznu.teaching.energy.final<-data.hznu.teaching.energy.final[sumElec!=0]
nn<-data.hznu.teaching.energy.final[which(rowSums(is.na(data.hznu.teaching.energy.final))>0),]#行为finalState及basePattern缺失1787条
ggplot(data = data.hznu.teaching.energy.final[!labelRoomDay %in% nn$labelRoomDay],aes(x=sumElec))+geom_density()
data.hznu.teaching.energy.final$sdElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22))],
                                              MARGIN = 1,sd,na.rm=TRUE)#sapply为啥不对
data.hznu.teaching.energy.final$meanElec<-apply(data.hznu.teaching.energy.final[,c(sprintf("h%d",8:22))],
                                                MARGIN = 1,mean,na.rm=TRUE)
wssClusterEvaluate(data = data.hznu.teaching.energy.final[,c("sdElec","meanElec","sumElec")],
                   maxIter = 1000,maxK = 8)
pamkClusterEvaluate(
  data = data.hznu.teaching.energy.final[,c("sdElec","meanElec","sumElec")],
  criter = "ch",startK = 2,endK = 8)







