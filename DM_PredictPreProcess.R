#接最原始数据集####

data.yx$labelMerge<-paste(data.yx$ac_code,data.yx$time)
data.yd$labelMerge<-paste(data.yd$ac_code,data.yd$time)
data.hznu.raw <-
  data.table(
    ac_code=data.yx$ac_code,
    time=data.yx$time,
    on_off=data.yx$on_off,
    run_mode=data.yx$run_mode,
    wind=data.yx$wind,
    set_temp=data.yx$set_temp,
    real_temp=data.yx$real_temp,
    total_elec=data.yd$total_elec,
    outdoor_elec=data.yd$outdoor_elec,
    system_elec=data.yd$system_elec,
    indoor_elec=data.yd$indoor_elec,
    ac_code=data.yd$ac_code,
    time_yd=data.yd$time
  )
##有一部分对不上，共90703

# data.hznu.raw$set_temp<-data.yx[labelMerge==data.hznu.raw$labelMerge]$set_temp
# data.hznu.merge<-data.hznu.raw[,c("ac_code.x","time.x","total_elec","on_off","set_temp","real_temp")] #这么合并会多
data.hznu.raw<-data.hznu.raw[,c("ac_code","time","total_elec","on_off","set_temp","real_temp")] 
data.hznu.raw<-data.hznu.raw[! ac_code%in%unique(nn$ac_code)]#把对不上的空调记录删去

#开关机阈值清洗
data.hznu.raw$on_off<-ifelse(data.hznu.raw$on_off=="开机",1,0)
data.hznu.raw$on_off<-ifelse(data.hznu.raw$total_elec<=0.2,0,1)
data.hznu.raw[on_off==0]$total_elec<-0

####天气合并####
#天气数据处理
weather.HZ$time <- as.POSIXct(weather.HZ$时间)
weather.HZ <- data.table(weather.HZ)
weather.HZ$year <- format(weather.HZ$time, "%Y")
# weather.HZ$month <- format(weather.HZ$time, "%m")
weather.HZ <-
  weather.HZ[year %in% c("2016", "2017")]
weather.HZ$year <- NULL
# weather.HZ$month <- NULL#这样就能删除列了？！？！？！
weather.HZ$温度 <- as.numeric(weather.HZ$温度)
weather.HZ$风速 <- as.numeric(weather.HZ$风速)

#
weather.HZ <- weather.HZ[, .(city=unique(城市),weather=天气[1],w_temp = mean(温度),
                              w_hum = mean(相对湿度),
                              w_winds = mean(风速),windDirection=风向[1]),by = time]
# save(weather.HZ,file="杭州2016_2017气象数据.rdata")
data.hznu.raw<- merge(x=data.hznu.raw, y=weather.HZ,by = "time",all.x = TRUE)#这种merge要比设标签方便多了
#以后可以直接加载
rm(data.yx)
rm(data.yd)
rm(weather.HZ)
rm(nn)
rm(data.hznu.merge)
gc()


###前接用能模式的数据集和热环境模式已清洗和过滤的数据集
data.hznu.predict<-data.hznu.raw[!duplicated(data.hznu.raw)]
data.hznu.predict$buildingCode<-substr(data.hznu.predict$ac_code,1,10)
data.hznu.predict$roomCode<-substr(data.hznu.predict$ac_code,1,13)
data.hznu.predict$hour<-format(data.hznu.predict$time,"%H")
data.hznu.predict$labelBuildingHour <-
  paste(data.hznu.predict$buildingCode, as.Date(data.hznu.predict$time), data.hznu.predict$hour,sep = "-")#标签：建筑号-年月日-小时

ggplot(data=data.hznu.predict)+geom_histogram(aes(y=total_elec))#+geom_boxplot(aes(y=total_elec))+geom_boxplot(aes(y=modifyElec))+ylim(0,10)# +xlim(0,10)+ylim(0,50000)

####与热环境过滤的数据集合并####
#将预测数据除去热环境中过滤掉的异常值
setorder(data.hznu.final,time,ac_code)
data.hznu.final$time<-as.POSIXct(data.hznu.final$time)
data.hznu.predict$labelAcHour<-paste(data.hznu.predict$ac_code,data.hznu.predict$time)
data.hznu.final$labelAcHour<-paste(data.hznu.final$ac_code,data.hznu.final$time)
data.hznu.predict<-data.hznu.predict[labelAcHour%in% data.hznu.final$labelAcHour]

#能耗分布
ggplot(data=data.hznu.predict[on_off==1],aes(x=total_elec))+geom_density()

####能耗修正####
#统计开机状态下的能耗值，选上限为
data.hznu.predict.raw<-data.hznu.predict#原始能耗数据备份
data.hznu.predict<-data.hznu.predict[!is.na(total_elec)]
data.hznu.predict[total_elec<0]$total_elec<-0
data.hznu.predict$on_off<-ifelse(data.hznu.predict$total_elec<=0.2,0,1)
data.hznu.predict[on_off!=0&total_elec>0.2]$on_off<-1
ecLim<-mean(data.hznu.predict[on_off==1]$total_elec,na.rm = TRUE)+3*sd(data.hznu.predict[on_off==1]$total_elec,na.rm = TRUE)
# > ecLim
# [1] 4.883569

length(data.hznu.predict[total_elec>ecLim&on_off==1]$time)/length(data.hznu.predict[on_off==1]$time)
#[1] 0.01942678


#该方法效果不佳
data.hznu.predict.modify<-data.hznu.predict[total_elec>ecLim]
data.hznu.predict.modify$modifyElec<-ecLim+((data.hznu.predict.modify$total_elec-ecLim)/10)
data.hznu.predict$modifyElec<- data.hznu.predict$total_elec
data.hznu.predict[labelAcHour %in% data.hznu.predict.modify$labelAcHour]$modifyElec<- 
  data.hznu.predict.modify$modifyElec
rm(data.hznu.predict.modify)


data.hznu.predict$modifyElec<-data.hznu.predict$total_elec
data.hznu.predict.normal<-data.hznu.predict[total_elec<=ecLim]
data.hznu.predict.modify<-data.hznu.predict[total_elec>ecLim]
data.hznu.predict.modify$modifyElec<-ecLim+((data.hznu.predict.modify$total_elec-ecLim)/10)
data.hznu.predict<-rbind(data.hznu.predict.modify,data.hznu.predict.normal)
rm(data.hznu.predict.modify)
rm(data.hznu.predict.normal)

##如果效果不佳考虑直接用均值代替


####可视化能耗修正####
plot(data.hznu.predict[on_off==1]$total_elec,type="l")
lines(data.hznu.predict[on_off==1]$modifyElec,col="red")#绘图太慢
plot(data.hznu.predict[total_elec<10]$total_elec)

ggplot(data=data.hznu.predict[on_off==1])+geom_density(aes(x=total_elec))+geom_density(aes(x=modifyElec,color="red"))

####将半小时数据转化为一小时,即原始末端小时级数据####
data.hznu.predict$labelAcHour<-paste(data.hznu.predict$ac_code,format(data.hznu.predict$time,format = "%Y-%m-%d-%H"),sep = "-")
setorder(data.hznu.predict,time,ac_code)
data.hznu.predict.hour<-data.hznu.predict[,.(time=time[1],ac_code=ac_code[1],
                                             total_elec=sum(total_elec,na.rm=TRUE),
                                             on_off=ifelse(sum(on_off,na.rm=TRUE)>=1,1,0),
                                             set_temp=mean(set_temp[on_off==1]),
                                             real_temp = mean(real_temp[on_off == 1]),
                                             temp_diff = mean(real_temp[on_off == 1] - set_temp[on_off == 1]),
                                             w_temp = mean(w_temp),
                                             w_winds = mean(w_winds),
                                             w_hum = mean(w_hum)
                                             ),by=labelAcHour]

save(data.hznu.predict,file = "HZNU_含追加_半小时_能耗_已清洗.rdata")

####将数据转换为建筑小时级数据####
#data.hznu.predict为原始末端数据，已过滤及清洗
data.hznu.predict$date<-as.Date(data.hznu.predict$time)
data.hznu.predict<-data.hznu.predict[!duplicated(data.hznu.predict)]
data.hznu.predict[on_off==0]$modifyElec<-0
data.hznu.predict[on_off==0]$total_elec<-0
#对空调加入行为模式属性
raw.periodOn$modifyCluster<-raw.periodOn$cluster
raw.periodOn[cluster==5|cluster==6]$modifyCluster<-2
raw.periodOn[cluster==4]$modifyCluster<-3
data.hznu.predict$labelDay<-paste(data.hznu.predict$ac_code,data.hznu.predict$date,sep = "-")
data.hznu.predict<-merge(x=data.hznu.predict,y=raw.periodOn[,c("label","modifyCluster")],by.x="labelDay",by.y="label",all.x=TRUE)

data.regress.total <- data.hznu.predict[, .(
  buildingCode = unique(buildingCode),
  date = unique(date),
  time = unique(paste(date, hour)),
  ac_num = length(unique(ac_code)),
  room_num = length(unique(roomCode)),
  temp_diff = mean(real_temp[on_off == 1] - set_temp[on_off == 1],na.rm=TRUE),
  temp_diffRatio = mean(abs(real_temp[on_off == 1] - set_temp[on_off == 1])/set_temp[on_off == 1],na.rm=TRUE),
  on_ratio = sum(on_off == 1,na.rm=TRUE) / length(on_off),
  set_temp = mean(set_temp[on_off == 1],na.rm=TRUE),
  real_temp = mean(real_temp[on_off == 1],na.rm=TRUE),
  total_elec = sum(modifyElec,na.rm=TRUE),
  w_temp = mean(w_temp,na.rm=TRUE),
  w_winds = mean(w_winds,na.rm=TRUE),
  w_hum = mean(w_hum,na.rm=TRUE),
  c1Ratio=length(unique(ac_code[on_off==1&modifyCluster==1&!is.na(modifyCluster)]))/length(unique(ac_code[on_off==1&!is.na(modifyCluster)])),
  c2Ratio=length(unique(ac_code[on_off==1&modifyCluster==2&!is.na(modifyCluster)]))/length(unique(ac_code[on_off==1&!is.na(modifyCluster)])),
  c3Ratio=length(unique(ac_code[on_off==1&modifyCluster==3&!is.na(modifyCluster)]))/length(unique(ac_code[on_off==1&!is.na(modifyCluster)])),
  c4Ratio=length(unique(ac_code[on_off==1&modifyCluster==7&!is.na(modifyCluster)]))/length(unique(ac_code[on_off==1&!is.na(modifyCluster)]))
), by = labelBuildingHour]
data.regress.total$time<-as.POSIXct(data.regress.total$time,format="%Y-%m-%d %H")

####行为聚类影响因素加入####
#行为聚类的预处理
# raw.periodOn$labelDay<-paste(substr(raw.periodOn$ac_code,1,10),raw.periodOn$date,sep = "-")
# data.regress.behavior.raw<-melt(raw.periodOn[,-c("label","runtime","isWorkday")],id=c("labelDay","cluster","date","ac_code"))
# names(data.regress.behavior.raw)[6]<-"on_off"
# names(data.regress.behavior.raw)[5]<-"hour"
# data.regress.behavior.raw$hour<-as.character(data.regress.behavior.raw$hour)
# data.regress.behavior.raw$hour<-gsub("h","",data.regress.behavior.raw$hour)
# data.regress.behavior.raw$hour<-as.numeric(data.regress.behavior.raw$hour)
# data.regress.behavior.raw$hour<-data.regress.behavior.raw$hour+7
# data.regress.behavior.raw$labelBuildingHour<-paste(data.regress.behaviorCluster$labelDay,
#                                                    sprintf("%02d",data.regress.behaviorCluster$hour),sep = "-")
# data.regress.behavior.raw$modifyCluster<-data.regress.behavior.raw$cluster
# data.regress.behavior.raw[cluster==5|cluster==6]$modifyCluster<-2
# data.regress.behavior.raw[cluster==4]$modifyCluster<-3
# data.regress.behaviorCluster<-data.regress.behavior.raw[,.(
#   buildingCode=substr(ac_code[1],1,10),
#   date=date[1],
#   hour=hour[1],
#   activeAcSum=length(unique(ac_code)),
#   onCount=length(unique(ac_code[on_off==1])),
#   activeAcRatio=sum(on_off)/length(ac_code),
#   c1Ratio=length(unique(ac_code[modifyCluster==1]))/length(unique(ac_code)),
#   c2Ratio=length(unique(ac_code[modifyCluster==2]))/length(unique(ac_code)),
#   c3Ratio=length(unique(ac_code[modifyCluster==3]))/length(unique(ac_code)),
#   c4Ratio=length(unique(ac_code[modifyCluster==7]))/length(unique(ac_code))
# ),by=labelBuildingHour]
# # nn<-data.table(data.regress.behaviorCluster$c1Ratio+data.regress.behaviorCluster$c2Ratio+
# #                  data.regress.behaviorCluster$c3Ratio+data.regress.behaviorCluster$c4Ratio)
# # rm(data.regress.behavior.raw)
# 
# ####行为聚类结果合并至预测数据####
# data.regress.full.total<-merge(x=data.regress.total,y=data.regress.behaviorCluster[,c("labelBuildingHour","c1Ratio","c2Ratio","c3Ratio","c4Ratio")],all.x = TRUE,by = "labelBuildingHour")
# data.regress.full.total$cFullOff<-0
# data.regress.full.total$cFullOn<-0
# data.regress.full.total[is.na(c1Ratio)]$cFullOff<-1
# data.regress.full.total[cFullOff==1,c("c1Ratio","c2Ratio","c3Ratio","c4Ratio")]<-0
# ggplot(data=data.regress.total,aes(x=on_ratio,y=total_elec))+geom_point(aes(color=buildingCode))

rm(newdata)
gc()
