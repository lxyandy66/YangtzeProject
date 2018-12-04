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

####�ܺľ���ģʽ####
#���ܺ��Ѵ�����ϴ���� data.hznu.predict

# data.hznu.predict$date<-format(data.hznu.predict$time,"%Y-%m-%d")

data.hznu.predict$labelRoomHour<-paste(data.hznu.predict$roomCode,data.hznu.predict$date,
                                       data.hznu.predict$hour,sep = "_")

#��������ϴ������ĩ��-Сʱ��
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
                                           ##!!!!�м����ֱ����as.Date()����ʱ�����⣬Ĭ��ʱ����UTC
#####����ĩ��-Сʱ������������Ϊ��λ�Ŀ�����####
#ͳ�Ƹ��յ�һ���¼�Ƿ�����
data.hznu.energy.ac.hourly$hour<-as.numeric(format(data.hznu.energy.ac.hourly$time,"%H"))
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly[hour>=8&hour<=22]
data.hznu.energy.checkComplete<-data.hznu.energy.ac.hourly.pickup[,.(
  ac_code=ac_code[1],count=length(ac_code)),by=labelAcDay]
#��¼Сʱ  2      4      5      6      7      8      9     10     11     12     13     14     15 
#����     12     41      1    195     22    128    177     16     12      1     65     31 257905 
data.hznu.energy.ac.hourly.pickup<-data.hznu.energy.ac.hourly.pickup[
  labelAcDay %in% data.hznu.energy.checkComplete[count==15]$labelAcDay]#�޳�������������
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
####�յ�ĩ��/�� ������������ ����/�� ������####
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
# save(data.hznu.energy.room.day,data.hznu.energy.ac.hourly.pickup,
#      file = "HZNU_�ܺľ���Ԥ�������_������ʱ��.rdata")

####���յ�ʹ��ģʽ��ǩ�����ܺ����ݼ�####
data.hznu.energy.room.day$pattern<-"NULL"
