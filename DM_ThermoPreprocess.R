####���ű�������ϴHZNU�Ȼ�������####
#��������ϴ������
#��"HZNU_��׷��_��Сʱ_�ܺ�_����ϴ.rdata"
data.hznu.thermo.raw<-data.hznu.predict[,c("ac_code","time","total_elec","real_temp","set_temp",
                                      "state","roomCode","hour","on_off","modifyElec")]
#ɸѡ����ѧ�Ľ���
info.hznu.building<-as.data.table(read.csv(file = "HZNU_�������ܼ������Ӧ.csv"))
data.hznu.teaching.thermo.raw<-
  data.hznu.thermo.raw[substr(roomCode,1,10) %in% info.hznu.building[type=="teaching"]$buildingCode]
#ɸѡ�пյ�ʹ�õļ�¼
data.hznu.teaching.thermo.raw$labelRoomDay<-paste(data.hznu.teaching.thermo.raw$roomCode,
                                                  data.hznu.teaching.thermo.raw$date,sep = "_")
data.hznu.teaching.thermo.raw<-
  data.hznu.teaching.thermo.raw[labelRoomDay %in% data.hznu.use.final$labelRoomDay]
data.hznu.teaching.thermo.raw<-
  data.hznu.teaching.thermo.raw[!duplicated(data.hznu.teaching.thermo.raw)]

####˼·####
#0������������ѧ�����伶���пյ�ʹ��
#1������ĩ�˼����ݣ�ȫ���쳣�����ݣ���ȫ���¶����κβ����������������޷�����
#2�����ݼ���������ɾȥȫ���쳣�����ݣ��綬�����ڳ�ʱ�����40�ȣ�
#3������ĩ�˼���������ȫ�����У�����쳣�㣬��ֵ�����쳣��
#4����ĩ�����������伶

####ȥ��ȫ��û�в���������####
data.hznu.teaching.thermo.raw<-data.hznu.teaching.thermo.raw[!is.na(real_temp)]
data.hznu.teaching.thermo.raw$date<-substr(data.hznu.teaching.thermo.raw$time,1,10)
data.hznu.teaching.thermo.raw$labelAcDay<-paste(data.hznu.teaching.thermo.raw$ac_code,
                                                data.hznu.teaching.thermo.raw$date,sep = "_")

####ɾȥȫ����Ⱥ��####
data.hznu.teaching.thermo.raw$modiTemp<-data.hznu.teaching.thermo.raw$real_temp
data.hznu.teaching.thermo.raw[modiTemp>40|modiTemp< 5]<-NA

####ɾȥ���������ϼ������������ݵ�####
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

#ȡ���ο���ˮƽsdֵ�������Ƿ����
nn<-data.hznu.teaching.thermo.raw[labelAcDay=="330100D276063m01_2017-08-25"]
stat.hznu.thermo.stableCheck<-stat.hznu.thermo.stableCheck[sd>0.2 & range>1 & minTemp<40 & range<20 &
                                                             complete.cases(stat.hznu.thermo.stableCheck)]

#ɾȥȫ���޲����ļ�¼
ggplot(data=stat.hznu.thermo.stableCheck,aes(x=range))+geom_density()+xlim(0,15)
data.hznu.teaching.thermo.cleaned<-
  data.hznu.teaching.thermo.raw[labelAcDay %in% stat.hznu.thermo.stableCheck$labelAcDay]


#�ο����յ�Сʱ���¶ȱ仯���
stat.hznu.thermo.acRange<-
  stat.hznu.thermo.stableCheck[,.(acCount=length(unique(ac_code)),
                                  meanRange=max(mean,na.rm = TRUE)-min(mean,na.rm = TRUE)),by=labelRoomDay]
stat.hznu.thermo.acRange<-stat.hznu.thermo.acRange[acCount>1]
stat.hznu.thermo.acRange$logCount<-sapply(stat.hznu.thermo.acRange$labelRoomDay,
                                          function(x){length(!is.na(data.hznu.teaching.thermo.cleaned[labelRoomDay==x]$modiTemp))})
ggplot(data=stat.hznu.thermo.acRange,aes(x=meanRange))+geom_density()
stat.hznu.thermo.needProcess<-stat.hznu.thermo.acRange[meanRange>5]


#�쳣ֵ�����ݴ�������
temp.outlier.test<-(data.hznu.teaching.thermo.cleaned[labelRoomDay=="330100D258102_2017-12-21"])
#�ƺ�������������Ч���ȿ����ݺã�³���Խ�ǿ
temp.outlier.test$isSelected<-"0"
temp.outlier.test[ac_code %in% outlierModify(temp.outlier.test$modiTemp,temp.outlier.test$ac_code)]$isSelected<-"1"
ggplot(data=temp.outlier.test,aes(x=hour,y=modiTemp,color=isSelected,group=ac_code,shape=ac_code))+geom_point()+geom_line()
# temp.outlier.test$outlierCluster<-pamk(data=temp.outlier.test$modiTemp,krange=2,criterion = "ch")$pamobject$clustering
# temp.outlier.test$outlierCluster<-as.factor(temp.outlier.test$outlierCluster)
# ggplot(data=temp.outlier.test,aes(x=hour,y=modiTemp,color=outlierCluster,group=ac_code))+geom_point()+geom_line()
# temp.stat.outlier<-temp.outlier.test[,.(cluster1=length(labelAcDay[outlierCluster==1]),
#                                         cluster2=length(labelAcDay[outlierCluster==2])
#                                         ),by=ac_code]


####ת��Ϊ����Сʱ������####
# �Ƚ���Ҫ�������������֣�stat.hznu.thermo.needProcess����������ٺϲ�
data.hznu.teaching.thermo.cleaned$labelRoomDayHour<-
  paste(data.hznu.teaching.thermo.cleaned$labelRoomDay,data.hznu.teaching.thermo.cleaned$hour,sep = "_")
setorder(data.hznu.teaching.thermo.cleaned,ac_code,time)
#����һ���ϲ��ļ��д��
data.hznu.teaching.thermo.day.long<-
  data.hznu.teaching.thermo.cleaned[,.(
    time=time[1],
    roomCode=roomCode[1],
    labelRoomDay=labelRoomDay[1],
    hour=hour[1],
    real_temp=mean(real_temp,na.rm = TRUE),
    set_temp=mean(set_temp[on_off==1],na.rm = TRUE),
    modiTemp=mean(modiTemp[ifelse(
      labelRoomDay %in% stat.hznu.thermo.needProcess,
      ac_code %in% outlierModify(data.hznu.teaching.thermo.cleaned[labelRoomDay==labelRoomDay[1]]$modiTemp,
                                 data.hznu.teaching.thermo.cleaned[labelRoomDay==labelRoomDay[1]]$ac_code),
      TRUE)],na.rm = TRUE)
  ),by=labelRoomDayHour]
data.hznu.teaching.thermo.day.long$date<-apply(data.hznu.teaching.thermo.day.long[,"labelRoomDay"],
                                               MARGIN = 1,FUN = getSplitMember,splitSimbol = "_",isLastOne = TRUE)
data.hznu.teaching.thermo.day.final<-dcast(
  data.hznu.teaching.thermo.day.long[hour %in% sprintf("%02d",8:22),c("labelRoomDay","date","roomCode","modiTemp","hour")],
  labelRoomDay+date+roomCode~hour,value.var = "modiTemp")
names(data.hznu.teaching.thermo.day.final)<-c("labelRoomDay","date","roomCode",sprintf("h%02d",8:22))
data.hznu.teaching.thermo.day.final<-as.data.table(data.hznu.teaching.thermo.day.final)

####����һЩͳ����####
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
