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

data.hznu.predict$date<-format(data.hznu.predict$time,"%Y-%m-%d")

data.hznu.predict$labelRoomHour<-paste(data.hznu.predict$roomCode,data.hznu.predict$date,
                                       data.hznu.predict$hour,sep = "_")

#整理已清洗数据至末端-小时级
data.hznu.predict$labelAcHour<-paste(data.hznu.predict$ac_code,
                                     data.hznu.predict$date,data.hznu.predict$hour,sep = "_")
setorder(data.hznu.predict,time,ac_code)
data.hznu.energy.ac.hourly<-data.hznu.predict[,.(time=time[1],
                                                 ac_code=ac_code[1],
                                                 total_elec=sum(total_elec),
                                                 on_off=ifelse(sum(on_off)>0,1,0),
                                                 modifyElec=sum(modifyElec),
                                                 date=date[1]
                                                 ),by=labelAcHour]
data.hznu.energy.ac.hourly$labelAcDay<-paste(data.hznu.energy.ac.hourly$ac_code,
                                             data.hznu.energy.ac.hourly$date,sep = "_")
                                           # as.Date(data.hznu.energy.ac.hourly$time),sep = "_")
                                           ##!!!!切记如果直接用as.Date()会有时区问题，默认时区是UTC
#####整理末端-小时级数据至以天为单位的宽数据####
#统计各空调一天记录是否完整
data.hznu.energy.ac.hourly$hour<-as.numeric(format(data.hznu.energy.ac.hourly$time,"%H"))
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly[hour>=8&hour<=22]
data.hznu.energy.checkComplete<-data.hznu.energy.ac.hourly.pickup[,.(
  ac_code=ac_code[1],count=length(ac_code)),by=labelAcDay]
#按热环境清洗数据
#记录小时  2      4      5      6      7      8      9     10     11     12     13     14     15 
#条数     12     41      1    195     22    128    177     16     12      1     65     31 257905 
#未清洗仅修正
#记录小时 1      2      3      4      5      6      7      8      9     10     11     12     13     14     15 
#条数 7     13      1     50      9    307     96    167    266     38     54      4    127     37 470656 
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly.pickup[
  labelAcDay %in% data.hznu.energy.checkComplete[count==15]$labelAcDay]#剔除不完整的数据
# rm(data.hznu.energy.ac.hourly)
setorder(data.hznu.energy.ac.hourly.pickup,time,ac_code)
data.hznu.energy.ac.day<-data.hznu.energy.ac.hourly.pickup[,.(date=date[1],
                                                       ac_code=ac_code[1],
                                                       sumElec=sum(modifyElec),
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
data.hznu.energy.ac.day$date<-as.Date(data.hznu.energy.ac.day$date)
setorder(data.hznu.energy.ac.day,date,ac_code)
data.hznu.energy.room.day<-data.hznu.energy.ac.day[,.(roomCode=roomCode[1],
                                                      acCount=length(unique(ac_code)),
                                                      date=date[1],
                                                      sumElec=sum(sumElec),
                                                      h8=sum(h8),
                                                      h9=sum(h9),
                                                      h10=sum(h10),
                                                      h11=sum(h11),
                                                      h12=sum(h12),
                                                      h13=sum(h13),
                                                      h14=sum(h14),
                                                      h15=sum(h15),
                                                      h16=sum(h16),
                                                      h17=sum(h17),
                                                      h18=sum(h18),
                                                      h19=sum(h19),
                                                      h20=sum(h20),
                                                      h21=sum(h21),
                                                      h22=sum(h22)
                                                      ),by=labelRoomDay]

####增加基本使用模式标签####
# basePattern={"noneUSe","periodUse","fullUse"}
data.hznu.energy.room.day<-
  merge(x=data.hznu.energy.room.day,y=data.hznu.use.room.day[,c("labelRoomDay","finalState","basePattern")],
        by = "labelRoomDay",all.x = TRUE)
data.hznu.energy.room.day[sumElec==0]$basePattern<-"noneUse"
data.hznu.energy.room.day$zeroCount<-apply(data.hznu.energy.room.day[,c(6:20)],1,function(x){sum(x==0)})
data.hznu.energy.room.day[is.na(basePattern)]$basePattern<-ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==15,"noneUse",
                                                                  ifelse(data.hznu.energy.room.day[is.na(basePattern)]$zeroCount==0,"fullUse","periodUse"))
nn<-data.hznu.energy.room.day[which(rowSums(is.na(data.hznu.energy.room.day))>0),]#行为finalState及basePattern缺失83781条

# save(data.hznu.energy.room.day,data.hznu.energy.ac.hourly,
#      file = "HZNU_能耗聚类预处理结果_仅工作时间_按原始修正能耗.rdata")


