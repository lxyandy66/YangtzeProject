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
library(rgl)

########��Ϊģʽ����Ԥ����########
####�˴����Ѵ�����ʹ��ģʽ�Ŀյ���Ϊ����####
# raw.rawData
raw.rawData$pattern<-"NULL"
#####�Բ�ȷ������Ϊ����Ŀյ���������ȷ��####
raw.rawData$modiState<-raw.rawData$state
raw.rawData[(month >= 5 &
           month<= 10) &
          (modiState == "other" |
             modiState == "auto" | modiState == "venti")]$modiState <- "cooling"
raw.rawData[(month %in% c(11, 12, 1, 2, 3, 4)) &
          (modiState == "other" |
             modiState == "auto" | modiState == "venti")]$modiState <- "heating"
raw.rawData[runtime == 0]$modiState<-"off"

# �����ڲ鿴�����ڿյ�����ռ��
nn<-raw.rawData[,.(useCount = length(label[runtime > 0]),
                   noneUseCount = length(label[runtime == 0]),
                   heatingCount = length(label[finalState == "heating"]),
                   coolingCount = length(label[finalState == "cooling"])
                   ),by=season]

raw.rawData$finalState<-raw.rawData$modiState
raw.rawData[season=="Summer"&finalState=="heating"]$finalState<-"cooling"
raw.rawData[season=="Winter"&finalState=="cooling"]$finalState<-"heating"

####����״̬ͳ��Ϊ��������####
raw.rawData$roomCode<-substr(raw.rawData$ac_code,1,13)

raw.noneOn <- raw.rawData[runtime == 0]
raw.fullOn <- raw.rawData[runtime == 15]
raw.periodOn <- raw.rawData[runtime != 15 & runtime != 0]

####һЩԤ����####
raw.fullOn$pattern<-"fullUse"#pattern��Ӧ�������ÿյ�ʹ��ģʽ
raw.noneOn$pattern<-"noneUse"
raw.periodOn$pattern<-"NULL"

raw.rawData$labelRoomDay<-paste(raw.rawData$roomCode,raw.rawData$date,sep = "_")
raw.rawData[runtime == 0]$pattern<-"noneUse"
raw.rawData[runtime == 15]$pattern<-"fullUse"
raw.rawData[runtime != 15 & runtime != 0]$pattern<-"periodUse"

####�յ�ʹ�ü�¼תΪ���ڷ��伶ʹ�ü�¼####
data.hznu.use.room.day<-raw.rawData[,.(date=date[1],
                                       roomCode=roomCode[1],
                                       acCount=length(unique(ac_code)),
                                       finalState=ifelse(length(finalState[finalState!="off"])==0,"off",
                                                         getMode(finalState[finalState!="off"])),
                                       #ע����Ҫ���⴦����������unique�����finalStateΪoff/heating ���� cooling/heating������û�н��
                                       season=season[1],
                                       h8=ifelse(sum(h1)>0,1,0),
                                       h9=ifelse(sum(h2)>0,1,0),
                                       h10=ifelse(sum(h3)>0,1,0),
                                       h11=ifelse(sum(h4)>0,1,0),
                                       h12=ifelse(sum(h5)>0,1,0),
                                       h13=ifelse(sum(h6)>0,1,0),
                                       h14=ifelse(sum(h7)>0,1,0),
                                       h15=ifelse(sum(h8)>0,1,0),
                                       h16=ifelse(sum(h9)>0,1,0),
                                       h17=ifelse(sum(h10)>0,1,0),
                                       h18=ifelse(sum(h11)>0,1,0),
                                       h19=ifelse(sum(h12)>0,1,0),
                                       h20=ifelse(sum(h13)>0,1,0),
                                       h21=ifelse(sum(h14)>0,1,0),
                                       h22=ifelse(sum(h15)>0,1,0)
                                       ),by=labelRoomDay]
data.hznu.use.room.day$runtime<-apply(data.hznu.use.room.day[,c(7:21)],1,sum)#������ʹ��ʱ��
data.hznu.use.room.day$basePattern<-ifelse(data.hznu.use.room.day$runtime==0,"noneUse",
                                           ifelse(data.hznu.use.room.day$runtime==15,"fullUse","periodUse"))


#?????��Ҫȷ������ķ��࣬��ͬ�����ֿ��ۣ���ͬ���ڷֿ��ۣ�????
raw.periodOn$splitFactor<-paste(raw.periodOn$season,raw.periodOn$finalState,sep = "_")
list.behaviour.season<-split(raw.periodOn,as.factor(raw.periodOn$splitFactor))
# mapply(FUN = miningBehaviourPattern,list(data=list.behaviour.season,colRange=c(6:20),seasonCol=22))
# mapply(FUN = miningBehaviourPattern,list.behaviour.season,6:20,22)
#mapply��û��ʲô�÷��������β���


for(s in unique(raw.periodOn$season)){
  raw.periodOn[season==s]$pattern<-pamk(raw.periodOn[season==s,6:20],
                                        krange = getkSizeBySeason(s),criterion = "ch",usepam = FALSE,critout = TRUE)
}



# list.behaviour.season ������Ϊģʽ�����ѱ�ǣ�ȫ��ģʽͳһ



####�Ȼ���Ԥ�������ݴ���####
#��data.analyse.all
data.hznu.thermo.room.day<-data.table(labelDay=data.analyse.all$labelDay,date_time=data.analyse.all$date_time,
                                room_code=data.analyse.all$room_code,season=data.analyse.all$season,
                                h8=(data.analyse.all$h08.00+data.analyse.all$h08.30)/2,
                                h9=(data.analyse.all$h09.00+data.analyse.all$h09.30)/2,
                                h10=(data.analyse.all$h10.00+data.analyse.all$h10.30)/2,
                                h11=(data.analyse.all$h11.00+data.analyse.all$h11.30)/2,
                                h12=(data.analyse.all$h12.00+data.analyse.all$h12.30)/2,
                                h13=(data.analyse.all$h13.00+data.analyse.all$h13.30)/2,
                                h14=(data.analyse.all$h14.00+data.analyse.all$h14.30)/2,
                                h15=(data.analyse.all$h15.00+data.analyse.all$h15.30)/2,
                                h16=(data.analyse.all$h16.00+data.analyse.all$h16.30)/2,
                                h17=(data.analyse.all$h17.00+data.analyse.all$h17.30)/2,
                                h18=(data.analyse.all$h18.00+data.analyse.all$h18.30)/2,
                                h19=(data.analyse.all$h19.00+data.analyse.all$h19.30)/2,
                                h20=(data.analyse.all$h20.00+data.analyse.all$h20.30)/2,
                                h21=(data.analyse.all$h21.00+data.analyse.all$h21.30)/2,
                                h22=(data.analyse.all$h22.00+data.analyse.all$h22.30)/2,
                                isWorkday=data.analyse.all$isWorkday)
data.hznu.thermo.room.day$date<-paste("20",substr(data.hznu.thermo.room.day$date_time,3,10),sep = "")
# data.hznu.thermo.room.day<-data.hznu.thermo.room.day[,-"labelDay"]
data.hznu.thermo.room.day$labelRoomDay<-paste(data.hznu.thermo.room.day$room_code,
                                              data.hznu.thermo.room.day$date,sep = "_")
rm(data.analyse.all)


####����Ϊ ����Сʱ ��Ϊ/�¶�/�ܺ� #####

miningBehaviourPattern<-function(data,colRange,seasonCol){
  pamk.best<-pamk(data[,colRange],krange = getkSizeBySeason(unique(data[,seasonCol])[1]),criterion = "ch",
                  usepam = FALSE, critout = TRUE)
}

getkSizeBySeason<-function(season){
  kSizeBySeason<-array(c("Spring","Summer_warm","Summer","Autumn","Winter_warm","Winter",
                         6,6,4,4,5,3,6),dim = c(6,2))
  return(kSizeBySeason[season,2])
}