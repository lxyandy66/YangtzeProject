#���ű�����ͳ�ƿյ�������̨�����Լ�Сʱ�ܺĵȣ������ܺ�ģʽ�ھ�ͽ���
#��"HZNU_��׷��_ĩ�˼�_��ΪģʽԤ�������.rdata"
#�� raw.rawData ���ݼ�

raw.rawData$roomCode<-substr(raw.rawData$ac_code,1,13)
raw.rawData$labelRoomDay<-paste(raw.rawData$roomCode,raw.rawData$date,sep = "_")

####����������-��####
data.hznu.all.use<-raw.rawData[,.(roomCode=roomCode[1],
                                   date=date[1],
                                   season=season[1],
                                   finalState=ifelse(is.na(getMode(finalState[finalState!="off"])),"off",
                                                     getMode(finalState[finalState!="off"])),
                                   acCount=length(unique(ac_code)),
                                   runtime=max(runtime,na.rm = TRUE),
                                   h8=sum(h1,na.rm = TRUE),
                                   h9=sum(h2,na.rm = TRUE),
                                   h10=sum(h3,na.rm = TRUE),
                                   h11=sum(h4,na.rm = TRUE),
                                   h12=sum(h5,na.rm = TRUE),
                                   h13=sum(h6,na.rm = TRUE),
                                   h14=sum(h7,na.rm = TRUE),
                                   h15=sum(h8,na.rm = TRUE),
                                   h16=sum(h9,na.rm = TRUE),
                                   h17=sum(h10,na.rm = TRUE),
                                   h18=sum(h11,na.rm = TRUE),
                                   h19=sum(h12,na.rm = TRUE),
                                   h20=sum(h13,na.rm = TRUE),
                                   h21=sum(h14,na.rm = TRUE),
                                   h22=sum(h15,na.rm = TRUE)
                                   ),by=labelRoomDay]#ע��˴��в���finalState��NA����runtime����
####�����ۼƿյ�ʹ��Сʱ����####
#����ʵ��ʹ��Сʱ*�յ���
data.hznu.all.use$sumAllAcRuntime<-apply(X = data.hznu.all.use[,c(sprintf("h%d",8:22))],MARGIN = 1,FUN = sum,na.rm=TRUE)
#�����ܿյ�Сʱ������
data.hznu.all.use$capAcHour<-15*data.hznu.all.use$acCount
#����ʵ�ʿյ�Сʱ������
data.hznu.all.use$realCapAcHour<-data.hznu.all.use$runtime*data.hznu.all.use$acCount
#�յ�ʹ��ǿ��=ʵ�ʿյ�Сʱ�� / ʵ�ʿյ�Сʱ������
data.hznu.all.use$acIntensity<-data.hznu.all.use$sumAllAcRuntime/data.hznu.all.use$realCapAcHour
#ƽ���յ�ʹ��̨��
data.hznu.all.use$meanAcUsed<-apply(X = data.hznu.all.use[,c(sprintf("h%d",8:22))],MARGIN = 1,
                                    FUN = function(x){mean(x[x!=0],na.rm = TRUE)})
#����ƽ���յ�ʹ����
data.hznu.all.use$acUsedRate<-data.hznu.all.use$meanAcUsed/data.hznu.all.use$acCount


save(data.hznu.all.use,file = "HZNU_��׷��_ȫ������_���伶_��ʹ��ǿ��_��ΪģʽԤ�������.rdata")


####ͳ�ƿյ�Сʱ���####
#�� HZNU_��׷��_ĩ�˼�_�ܺ�ģʽԤ�������_����ϴ.rdata HZNU_��ʱ_����ѧ_��ʱ�����ܺ�.rdata
#���ݼ� data.hznu.energy.ac.day; temp.hznu.energy.room

# #���㵥̨�յ�ƽ��Сʱ���
# #����ô��
# data.hznu.energy.ac.day$elecIntensity<-apply(X=data.hznu.energy.ac.day[,c(sprintf("h%d",8:22))],MARGIN = 1,
#                                              FUN = function(x){ mean(x[x>0.2],na.rm = TRUE)})
# nn<-data.hznu.energy.ac.day[!is.nan(data.hznu.energy.ac.day$elecIntensity),]

####��ʱͳ�Ʒ���յ�ƽ������####
temp.hznu.energy.room<-melt(data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date","sumElec","runtime",sprintf("h%d",8:22))],
                            id.var=c("labelRoomDay","roomCode","date","sumElec","runtime"))
temp.hznu.energy.room<-temp.hznu.energy.room[!duplicated(temp.hznu.energy.room)]
names(temp.hznu.energy.room)<-c("labelRoomDay","roomCode","date","sumElec","runtime","hour","roomElec")
temp.hznu.energy.room$labelRoomHour<-paste(temp.hznu.energy.room$labelRoomDay,temp.hznu.energy.room$hour,sep = "_")
# save(temp.hznu.energy.room,file="HZNU_��ʱ_����ѧ_��ʱ�����ܺ�.rdata")

temp.hznu.acUse.room<-melt(data.hznu.all.use[,c("labelRoomDay","roomCode","date",sprintf("h%d",8:22))],id.var=c("labelRoomDay","roomCode","date"))
names(temp.hznu.acUse.room)<-c("labelRoomDay","roomCode","date","hour","acUsedCount")
temp.hznu.acUse.room$labelRoomHour<-paste(temp.hznu.acUse.room$labelRoomDay,temp.hznu.acUse.room$hour,sep = "_")

#�ϲ�Сʱ�յ�ʹ������
temp.hznu.energy.room<-merge(x=temp.hznu.energy.room,y=temp.hznu.acUse.room[,c("labelRoomHour","acUsedCount")],
                             all.x = TRUE,by="labelRoomHour")
temp.hznu.energy.room$meanPower<-temp.hznu.energy.room$roomElec/temp.hznu.energy.room$acUsedCount
#���������⣬��Ϊ���������ܺ������ֵ���ܺ���ϴ��û��
temp.hznu.energy.room[is.nan(meanPower)|is.infinite(meanPower)]$meanPower<-NA
#�򵥵��
temp.stat.energy.room<-temp.hznu.energy.room[,.(roomCode=roomCode[1],
                                                date=date[1],
                                                runtime=runtime[1],
                                                countRuntime=sum(!is.na(meanPower)),
                                                meanDailyAcElec=mean(meanPower[meanPower!=0],na.rm = TRUE)),by=labelRoomDay]
save(temp.stat.energy.room,file = "HZNU_����ѧ_ʱ���յ���̨���ͳ��.rdata")

