library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(plyr)
library(timeDate)
library(rJava)
library(xlsx)

####能耗聚类模式####
#接能耗已处理清洗数据 data.hznu.predict
data.hznu.predict$labelRoomHour<-paste(data.hznu.predict$roomCode,data.hznu.predict$date,data.hznu.predict$hour,sep = "_")
setorder(data.hznu.predict,time,roomCode)
data.hznu.energy.ac.hourly<-data.hznu.predict[,.(time=time[1],
                                                 ac_code=unique(ac_code),
                                                 total_elec=sum(total_elec),
                                                 on_off=ifelse(sum(on_off)>0,1,0),
                                                 modifyElec=sum(modifyElec)
                                                 ),by=labelAcHour]
data.hznu.energy.ac.hourly$labelDay<-paste(data.hznu.energy.ac.hourly$ac_code,datedata.hznu.energy.ac.hourly$time)
data.hznu.energy.ac.day<-data.hznu.energy.ac.hourly[,.(),by=label]