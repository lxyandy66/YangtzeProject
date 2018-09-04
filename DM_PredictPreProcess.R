library(data.table)
library(forecast)
library(ggplot2)
library(car)
library(gvlma)
library(pls)
library(kernlab)

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

ggplot(data=data.hznu.predict,aes)+geom_boxplot(aes(y=total_elec))+geom_boxplot(aes(y=modifyElec))+ylim(0,10)# geom_histogram()+xlim(0,10)+ylim(0,50000)

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
data.hznu.predict[total_elec<0]$total_elec<-0
ecLim<-mean(data.hznu.predict[on_off==1]$total_elec)+1.96*sd(data.hznu.predict[on_off==1]$total_elec)
# > ecLim
# [1] 3.05254
#> boxplot.stats(data.hznu.predict[on_off==1]$total_elec)
# $stats
# [1] 0.2000000 0.5428571 0.9333333 1.5333333 3.0185966
# $n
# [1] 587462
# $conf
# [1] 0.9312915 0.9353751
length(data.hznu.predict[total_elec>ecLim]$time)/length(data.hznu.predict[on_off==1]$time)
#[1] 0.04784139

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

####将半小时数据转化为一小时,即原始末端小时级数据####
data.hznu.predict$labelAcHour<-paste(data.hznu.predict$ac_code,format(data.hznu.predict$time,format = "%Y-%m-%d-%H"),sep = "-")
setorder(data.hznu.predict,time,ac_code)
data.hznu.predict.hour<-data.hznu.predict[,.(time=time[1],ac_code=ac_code[1],
                                             total_elec=sum(total_elec),
                                             on_off=ifelse(sum(on_off)>=1,1,0),
                                             set_temp=mean(set_temp[on_off==1]),
                                             real_temp = mean(real_temp[on_off == 1]),
                                             temp_diff = mean(real_temp[on_off == 1] - set_temp[on_off == 1]),
                                             w_temp = mean(w_temp),
                                             w_winds = mean(w_winds),
                                             w_hum = mean(w_hum)
                                             ),by=labelAcHour]

save(data.hznu.predict,file = "HZNU_预测用原始数据_半小时_已清洗能耗及热环境数据.rdata")

####将数据转换为建筑小时级数据####
#data.hznu.predict为原始末端数据，已过滤及清洗
data.hznu.predict$date<-as.Date(data.hznu.predict$time)
data.hznu.predict<-data.hznu.predict[!duplicated(data.hznu.predict)]
data.hznu.predict[on_off==0]$modifyElec<-0
data.hznu.predict[on_off==0]$total_elec<-0
data.regress.total <- data.hznu.predict[, .(
  buildingCode = unique(buildingCode),
  date = unique(date),
  time = unique(paste(date, hour)),
  ac_num = length(unique(ac_code)),
  room_num = length(unique(roomCode)),
  temp_diff = mean(real_temp[on_off == 1] - set_temp[on_off == 1]),
  temp_diffRatio = mean(abs(real_temp[on_off == 1] - set_temp[on_off == 1])/set_temp[on_off == 1]),
  on_ratio = sum(on_off == 1) / length(on_off),
  set_temp = mean(set_temp[on_off == 1]),
  real_temp = mean(real_temp[on_off == 1]),
  total_elec = sum(modifyElec),
  w_temp = mean(w_temp),
  w_winds = mean(w_winds),
  w_hum = mean(w_hum)
), by = labelBuildingHour]
data.regress.total$time<-as.POSIXct(data.regress.total$time,format="%Y-%m-%d %H")

####行为聚类影响因素加入####
#行为聚类的预处理
raw.periodOn$labelDay<-paste(substr(raw.periodOn$ac_code,1,10),raw.periodOn$date,sep = "-")
data.regress.behavior.raw<-melt(raw.periodOn[,-c("label","runtime","isWorkday")],id=c("labelDay","cluster","date","ac_code"))
names(data.regress.behavior.raw)[6]<-"on_off"
names(data.regress.behavior.raw)[5]<-"hour"
data.regress.behavior.raw$hour<-as.character(data.regress.behavior.raw$hour)
data.regress.behavior.raw$hour<-gsub("h","",data.regress.behavior.raw$hour)
data.regress.behavior.raw$hour<-as.numeric(data.regress.behavior.raw$hour)
data.regress.behavior.raw$labelBuildingHour<-paste(data.regress.behaviorCluster$labelDay,sprintf("%02d",data.regress.behaviorCluster$hour),sep = "-")
data.regress.behavior.raw$modifyCluster<-data.regress.behavior.raw$cluster
data.regress.behavior.raw[cluster==5|cluster==6]$modifyCluster<-2
data.regress.behavior.raw[cluster==4]$modifyCluster<-3
data.regress.behaviorCluster<-data.regress.behavior.raw[,.(
  buildingCode=substr(ac_code[1],1,10),
  date=date[1],
  sum=length(ac_code),
  c1Ratio=length(cluster=="1")/length(ac_code)
),by=labelBuildingHour]

ggplot(data=data.regress.total,aes(x=on_ratio,y=total_elec))+geom_point(aes(color=buildingCode))

rm(newdata)
gc()
