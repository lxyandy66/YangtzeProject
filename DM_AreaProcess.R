####��������������ݼ���������####

#��
# data.hznu.all.use��������Ҫ��������ʱ�ģ�
# data.hznu.teaching.energy.std��������Ҫ��������ʱ�ģ�

####�ܺ����ݺϲ�Ϊ��������####
data.hznu.building.energy<-data.hznu.teaching.energy.std[,c("labelRoomDay","acCount","date","buildingCode","finalState",sprintf("h%d",8:22))]%>%
                           melt(.,id.var=c("labelRoomDay","acCount","date","buildingCode","finalState")) %>%
                           mutate(.,hour=substring(variable,2),labelBuildingHour=paste(buildingCode,date,hour,sep = "_")) %>%
                           .[,.(date=date[1],
                                buildingCode=buildingCode[1],
                                count=length(unique(labelRoomDay)),
                                hour=hour[1],
                                modiElec=sum(value,na.rm = TRUE)
                                ),by=labelBuildingHour]#�������ҲҪ������

####�ܺ����ݺϲ�Ϊ���򼶱�####
data.hznu.area.energy<-data.hznu.building.energy %>% 
                       mutate(.,datetime=as.POSIXct(paste(date,sprintf("%02s:00:00",hour)))) %>%
                       .[,.(date=date[1],
                            count=sum(count,na.rm=TRUE),
                            buildingCount=length(unique(buildingCode)),
                            modiElec=sum(modiElec,na.rm = TRUE)),by=datetime]

####��Ϊ�ϲ�����ʱ������������####
data.hznu.building.use<-data.hznu.all.use[runtime!=15,c("labelRoomDay","roomCode","date","finalState","acCount","runtime",sprintf("h%d",8:22))] %>% 
                    melt(.,id.var=c("labelRoomDay","roomCode","date","finalState","acCount","runtime")) %>% 
                    mutate(.,hour=substring(variable,2),onOff=ifelse(value!=0,1,0),#��value����ǰСʱ��ǰ����ʹ�õĿյ���
                           buildingCode=substr(roomCode,1,10),
                           labelBuildingHour=paste(buildingCode,date,hour,sep = "_")) %>% 
                    .[,.(date=date[1],
                         buildingCode=buildingCode[1],
                         count=length(labelRoomDay),#��Сʱ�ö�¥�ܼ�¼��
                         hour=hour[1],
                         onCount=length(labelRoomDay[onOff!=0]),#��ǰСʱ��on��¼��
                         offCount=length(labelRoomDay[onOff==0]),#��ǰСʱ��off��¼��
                         dayOnCount=length(labelRoomDay[runtime!=0]),#��ǰСʱ��¼����acOnDay��¼��
                         dayOffCount=length(labelRoomDay[runtime==0]),
                         acCount=sum(acCount,na.rm=TRUE),
                         acUsedCount=sum(value,na.rm=TRUE)
                         ),by=labelBuildingHour]

####��Ϊ�ϲ�����ʱ���򼶳�����####
data.hznu.area.use<-data.hznu.building.use[buildingCode %in% data.hznu.building.energy$buildingCode]%>%
                    mutate(.,datetime=as.POSIXct(paste(date,sprintf("%02s:00:00",hour)))) %>%
                    .[,.(date=date[1],
                         buildingCount=length(unique(buildingCode)),
                         count=sum(count,na.rm = TRUE),
                         onCount=sum(onCount,na.rm = TRUE),
                         offCount=sum(offCount,na.rm = TRUE),
                         dayOnCount=sum(dayOnCount,na.rm = TRUE),
                         dayOffCount=sum(dayOffCount,na.rm = TRUE),
                         acCount=sum(acCount,na.rm=TRUE),
                         acUsedCount=sum(acUsedCount,na.rm=TRUE)
                         ),by=datetime]

temp.hznu.area.energy<-data.hznu.area.energy %>% mutate(countEnergy=count,buildingCountEnergy=.$buildingCount)
data.hznu.area.predict.raw<-merge(x=data.hznu.area.use,y=temp.hznu.area.energy[,c("datetime","countEnergy","buildingCountEnergy","modiElec")],
                                  all.x = TRUE,by.x= "datetime",by.y= "datetime")#ֱ�ӹܵ�������warning�����ǽ����Ӱ��#�Ի�
rm(temp.hznu.area.energy)

####�������ݼ�������ɣ�����ȥȱʧ������###
data.hznu.area.predict.raw<-merge(x=data.hznu.area.predict.raw,
                                  y=data.weather.airport.final[!duplicated(data.weather.airport.final[,"datetime"]),
                                                               c("datetime","outTemp","rhOut","windSpeed","weather")],
                                  all.x = TRUE,by.x = "datetime",by.y = "datetime")
####��һЩ��������####
data.hznu.area.predict.raw<-data.hznu.area.predict.raw %>%
                            mutate(.,fullOnRatio=onCount/count,dayOnRatio=onCount/dayOnCount)

####�����Ƿ��ǹ����յ��ж�####
info.hznu.holiday<-as.data.table(read.xlsx(file="HZNU_HolidayList.xlsx",sheetIndex = 1)) %>% mutate(.,date=as.character(date))
data.hznu.area.predict.raw$isWeekday<-isWeekday(data.hznu.area.predict.raw$date)
data.hznu.area.predict.raw<-merge(x=data.hznu.area.predict.raw,y=info.hznu.holiday[,c("date","isBizday")],all.x = TRUE,by.x="date",by.y="date") #�߼�����
data.hznu.area.predict.raw[is.na(isBizday)]$isBizday<-data.hznu.area.predict.raw[is.na(isBizday)]$isWeekday


####����ȡ�����ݼ����������Բ���####
data.hznu.area.signCheck<-data.hznu.area.predict.raw[,c("datetime","date","fullOnRatio","modiElec","outTemp","rhOut","windSpeed","dayOnCount","isBizday")]
data.hznu.area.signCheck$r1h0_FullOnRatio<-apply(X = data.hznu.area.signCheck[,c("datetime","isBizday")],MARGIN = 1,
                                                 FUN = function(x){
                                                   getPreviousDate(thisTime = x[1],expFlag = as.logical(gsub(" ","",x[2])),
                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="fullOnRatio",flagColName="isBizday",timeInvl= -24*3600)})
data.hznu.area.signCheck$r1h0_modiElec<-apply(X = data.hznu.area.signCheck[,c("datetime","isBizday")],MARGIN = 1,
                                                 FUN = function(x){
                                                   getPreviousDate(thisTime = x[1],expFlag = as.logical(gsub(" ","",x[2])),
                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="modiElec",flagColName="isBizday",timeInvl= -24*3600)})
for(i in c(0,1,2,7)){#0�죬1�죬2�죬7��ǰ
  for(j in c(0,1,2)){#
    if(!(i==0&j==0)){#i,j�����Сʱ��ͬʱΪ0
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_FullOnRatio",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1,
                                                                                   FUN = getIntervalData,
                                                                                   data=data.hznu.area.signCheck,timeColName="datetime",targetColName="fullOnRatio",timeInvl=-i*24*3600-j*3600)
      # data.hznu.area.signCheck[,paste("d",i,"h",j,"_DayOnRatio",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1, 
      #                                                                             FUN = getIntervalData,
      #                                                                             data=data.hznu.area.signCheck,timeColName="datetime",targetColName="dayOnRatio",timeInvl=-i*24*3600-j*3600)
      data.hznu.area.signCheck[,paste("d",i,"h",j,"_modiElec",sep = "")]<-apply(X=data.hznu.area.signCheck[,"datetime"], MARGIN = 1, 
                                                                                FUN = getIntervalData,
                                                                                data=data.hznu.area.signCheck,timeColName="datetime",targetColName="modiElec",timeInvl=-i*24*3600-j*3600)
    }
  }
}

####ͳ�Ƹ�ʹ��ģʽ����ռ��####
data.hznu.area.signCheck<-data.hznu.use.predict.building.raw[,.(
                                      sumDayOnLogCount=sum(onCount,na.rm = TRUE),
                                      onDemandCount=sum(onDemandCount,na.rm = TRUE),
                                      forenoonCount=sum(forenoonCount,na.rm = TRUE),
                                      afternoonCount=sum(afternoonCount,na.rm = TRUE),
                                      daytimeCount=sum(daytimeCount,na.rm = TRUE),
                                      lateDaytimeCount=sum(lateDaytimeCount,na.rm = TRUE),
                                      allDayCount=sum(allDayCount,na.rm = TRUE)),by=date] %>%
                          merge(x=data.hznu.area.signCheck,y=.,all.x=TRUE,by.x="date",by.y="date")

data.hznu.area.signCheck<-as.data.table(data.hznu.area.signCheck)#ͻȻ���data.frame������
# ����һ��ͳ����ȷ��
nn<-data.hznu.area.signCheck[dayOnCount!=(onDemandCount+forenoonCount+afternoonCount+daytimeCount+lateDaytimeCount+allDayCount)]#�������ԣ����ǲ�����dayOnCount
# #���һ��dayOnCount�Ĵ���
# nn1<-data.hznu.all.use[date=="2017-01-12"]
# nn1<-merge(x=nn1,y=data.hznu.use.predict.building.raw[,.(
#   sumDayOnLogCount=sum(onCount,na.rm = TRUE),
#   onDemandCount=sum(onDemandCount,na.rm = TRUE),
#   forenoonCount=sum(forenoonCount,na.rm = TRUE),
#   afternoonCount=sum(afternoonCount,na.rm = TRUE),
#   daytimeCount=sum(daytimeCount,na.rm = TRUE),
#   lateDaytimeCount=sum(lateDaytimeCount,na.rm = TRUE),
#   allDayCount=sum(allDayCount,na.rm = TRUE)),by=date],all.x=TRUE,by.x="date",by.y="date")
# ##����������Ϊ��ȥ����runtime==15�����ݣ�������������Ϊ���쳣���ݣ�����Ϊģʽ��������ɸȥ����û�ж�Ӧ���ܺ�

#���ǲ�1-2���ң�Ӱ�첻��



#�����ģʽ��ռ����
data.hznu.area.signCheck<-mutate(.data =data.hznu.area.signCheck,
                                 onDemandRatio=onDemandCount/sumDayOnLogCount,
                                 forenoonRatio=forenoonCount/sumDayOnLogCount,
                                 afternoonRatio=afternoonCount/sumDayOnLogCount,
                                 daytimeRatio=daytimeCount/sumDayOnLogCount,
                                 lateDaytimeRatio=lateDaytimeCount/sumDayOnLogCount,
                                 allDayRatio=allDayCount/sumDayOnLogCount)


for(i in names(data.hznu.area.signCheck)){
  data.hznu.area.signCheck[which(is.nan(as.matrix(data.hznu.area.signCheck[,..i]))) ,i] <- NA
  #����֣�������˵Ӧ����..i, ��������..i����ʾ�Ҳ�������ֱ��i���У���ʾ�Ǹ�ֵʱ�����
}

data.hznu.area.signCheck$modiSeason<- apply(data.hznu.area.signCheck[,"date"],MARGIN = 1,FUN = function(x){ getSeason(substr(x,6,7))}) 
data.hznu.area.signCheck[modiSeason %in% c("Spring","Autumn")]$modiSeason<-"Transition"
                         
####��������������####
hstTimeInvl<-c("d0h1","d0h2","d1h0","d1h1","d1h2","d2h0","d2h1","d2h2","d7h0","d7h1","d7h2")#"r1h0",
patternRatioName<-c("onDemandRatio","forenoonRatio","afternoonRatio","daytimeRatio","lateDaytimeRatio","allDayRatio")
signAttr<-list(weatherAttr=c("outTemp","rhOut","windSpeed","weekday","isBizday"),#
               fullOnRatio=c(paste(hstTimeInvl,"FullOnRatio",sep = "_")),
               # dayOnRatio=c(paste(hstTimeInvl,"DayOnRatio",sep = "_")),
               stdModiElec=c(paste(hstTimeInvl,"modiElec",sep="_")),
               patternRatio=c(paste(c(rep("d1_",6),rep("d7_",6)),patternRatioName,sep = "")))#rep("r1_",6),

for(i in signAttr$patternRatio){
  data.hznu.area.signCheck[,c(paste(i,"_org",sep = ""))]<-apply(data.hznu.area.signCheck[,"datetime"], MARGIN = 1,
                                                                FUN = getIntervalData,data=data.hznu.area.signCheck,timeColName="datetime",targetColName=i,timeInvl= -24*3600)
  data.hznu.area.signCheck[,c(paste(i,"_org",sep = ""))]<-apply(data.hznu.area.signCheck[,"datetime"], MARGIN = 1,
                                                                FUN = getIntervalData,data=data.hznu.area.signCheck,timeColName="datetime",targetColName=i,timeInvl= -7*24*3600)
}#��һ���㷨���ã�̫���ˣ��϶�����ֱ�Ӱ���������ֱ�Ӱ�������õö�
#���Ǹ�ɵ*��...������Բ��ԵĶ���
data.hznu.area.signCheck[,c(paste(signAttr$patternRatio,"_org",sep = ""))]<-NULL


for(i in unique(data.hznu.area.signCheck$date)){
  #ȡǰһ���ο���
  targetTime<-as.POSIXct(getTargetDate(thisTime = i,data = data.hznu.area.signCheck,
                                       timeColName = "date",flagColName = "isBizday",
                                       expFlag = data.hznu.area.signCheck[date==i]$isBizday[1],timeInvl = -24*3600))
  # cat("thisTime: ",i,"\tTargetTime: ",,"\n")
  data.hznu.area.signCheck[date==i,c(paste(rep("r1_",6),patternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
  
  #ȡǰһ��
  targetTime<-as.POSIXct(getTargetDate(thisTime = i,data = data.hznu.area.signCheck,
                                       timeColName = "date",timeInvl = -24*3600))
  data.hznu.area.signCheck[date==i,c(paste(rep("d1_",6),patternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
  #ȡǰ����
  targetTime<-as.POSIXct(getTargetDate(thisTime = i,data = data.hznu.area.signCheck,
                                       timeColName = "date",timeInvl = -7*24*3600))
  data.hznu.area.signCheck[date==i,c(paste(rep("d7_",6),patternRatioName,sep = ""))]<-
    data.hznu.area.signCheck[date==format(targetTime,format="%Y-%m-%d"),..patternRatioName][1]
}

nn<-data.hznu.area.signCheck[,c("date","isBizday","onDemandRatio","d1_onDemandRatio","d1_onDemandRatio_org")]

data.hznu.area.signCheck$weekday<-wday(data.hznu.area.signCheck$date,week_start = 1)

####�����ڹ�һ��####
data.hznu.area.signCheck.pickup<-data.hznu.area.signCheck[substr(date,1,4)=="2017"|substr(date,1,7)=="2018-01"]
data.hznu.area.signCheck.pickup$stdModiElec<- -9999
for(i in unique(data.hznu.area.signCheck.pickup$modiSeason)){
  data.hznu.area.signCheck.pickup[modiSeason==i]$stdModiElec<-normalize(data.hznu.area.signCheck.pickup[modiSeason==i,"modiElec"],upper = 0.9,lower = 0.1,intercept = 0.1)
}
rm(stat.hznu.area.predict.sign)
for(i in c("stdModiElec","fullOnRatio")){
  for(j in unique(data.hznu.area.signCheck.pickup$modiSeason)){
    for(k in c("weatherAttr","hst","patternRatio")){
      if(k=="hst"){
        fmla.area.sign<-as.formula(paste(i,"~",paste(signAttr[[i]],collapse = "+")))
      }else{
        fmla.area.sign<-as.formula(paste(i,"~",paste(signAttr[[k]],collapse = "+")))
      }
      fit<-glm(fmla.area.sign,
               data=data.hznu.area.signCheck.pickup[modiSeason==j],family = binomial(),na.action = na.omit)
      stat.fit<-summary(fit)
      
      if(exists("stat.hznu.area.predict.sign")){
        stat.hznu.area.predict.sign<-rbind(stat.hznu.area.predict.sign,data.table(target=i,modiSeason=j,attr=k,var=row.names(stat.fit$coefficients),stat.fit$coefficients))
      }else{
        stat.hznu.area.predict.sign<-data.table(target=i,modiSeason=j,attr=k,var=row.names(stat.fit$coefficients),stat.fit$coefficients)
      }
    }
  }
}
write.xlsx(stat.hznu.area.predict.sign,file = "HZNU_AreaSelected_AttrSign_origin.xlsx")


data.hznu.area.predict.raw$h1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d1_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                          FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=1*24*3600)
data.hznu.area.predict.raw$d7_Elec<-apply(X=data.hznu.area.predict.raw[,"datetime"], MARGIN = 1, 
                                  FUN = getIntervalData,data=data.hznu.area.predict.raw,timeColName="datetime",targetColName="modiElec",timeInvl=7*24*3600)

####ͳ���������####
stat.hznu.area.completeCheck<-data.table(date=strptime("2016-12-07","%Y-%m-%d")+24*3600*0:850)
stat.hznu.area.completeCheck$useCount<-apply(stat.hznu.area.completeCheck[,"date"],MARGIN = 1,
                                             FUN = function(x){ return(nrow(data.hznu.area.use[date==x]))})
stat.hznu.area.completeCheck$isUseComplete<-(stat.hznu.area.completeCheck$useCount==15)
stat.hznu.area.completeCheck$energyCount<-apply(stat.hznu.area.completeCheck[,"date"],MARGIN = 1,
                                             FUN = function(x){ return(nrow(data.hznu.area.energy[date==x]))})
stat.hznu.area.completeCheck$isEnergyComplete<-(stat.hznu.area.completeCheck$energyCount==15)
nrow(stat.hznu.area.completeCheck[isUseComplete==TRUE]) #812
nrow(stat.hznu.area.completeCheck[isEnergyComplete==TRUE]) #579
nrow(stat.hznu.area.completeCheck[isEnergyComplete&isEnergyComplete]) #579



ggplot(data=(data.hznu.area.predict.raw%>% mutate(.,isWeekday=isWeekday(datetime),year=as.factor(year(datetime)),
                                                  monthDay=format(datetime,format="%m-%d")) %>% .[substr(datetime,1,7) %in% c("2017-05","2017-06","2017-07","2017-08","2017-09","2017-10")]),
       aes(x=date,y=fullOnRatio,color=isBizday,group=date,shape=year))+geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))#+facet_wrap(~year,ncol=1)
# c("2018-11","2018-12","2019-01","2019-02")]#��һ�ز���


ggplot(data=(stat.hznu.area.completeCheck%>% mutate(.,date=as.Date(date))%>% mutate(.,isWeekday=isWeekday(date),year=as.factor(year(date)),monthDay=format(date,format="%m-%d"),sumCount=useCount+energyCount)),
       aes(x=monthDay,y=sumCount,color=isWeekday,shape=year,group=year))+geom_line()+geom_point()+facet_wrap(~year,ncol=1)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

