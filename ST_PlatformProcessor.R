####�������ٽ���Ǹ�ƽ̨���ӻ����ֵ����ݴ���####

data.cplx.monthly<-data.raw.final %>% mutate(.,date=paste(year,month,sep = "-")) %>% 
                   .[typeCode=="M",c("BuildingCode","typeCode","area","date","value","eui")] %>% 
                   mutate(.,dataType="1")#������Ϊ1

data.cplx.annual<-data.all.annualSum %>% mutate(.,date=paste(year,"01",sep="-")) %>%
                  .[typeCode=="M",c("BuildingCode","typeCode","area","date","annualEnergy","annualEUI")]%>% 
                  mutate(.,dataType="2")#������Ϊ2

write.xlsx(data.cplx.monthly,file = "CPLX_MonthlyData.xlsx")
write.xlsx(data.cplx.annual,file = "CPLX_AnnualData.xlsx")


####HZNU�¶�����####
#������
#����HZNU����������
info.hznu.teaching.position<-read.csv(file = "HZNU_��ѧ�������뼰�����Ӧ��.csv",encoding = "UTF-8")
data.plfm.hznu.thermo.daily<-data.hznu.teaching.thermo.final[,c("date","roomCode",sprintf("modH%02d",8:22))] %>%
                             mutate(.,buildingCode=substr(roomCode,1,10),labelBuildingDay=paste(buildingCode,date,sep="_"),
                                    meanDailyTemp=apply(.[,c(sprintf("modH%02d",8:22))], MARGIN = 1, FUN = mean,na.rm=TRUE)) %>%
                             .[,.(date=date[1],
                                  buildingCode=buildingCode[1],
                                  count=length(roomCode),
                                  meanTemp=mean(meanDailyTemp,na.rm=TRUE),
                                  timeType="0"),by=labelBuildingDay] %>%#������Ϊ0
                              merge(x=.,y=info.hznu.teaching.position[,c("buildingName","buildingCode","lat","lng")],
                                    all.x=TRUE,by.x="buildingCode",by.y="buildingCode")
data.plfm.hznu.use.daily<-data.hznu.building.use[buildingCode %in% unique(data.plfm.hznu.thermo.daily$buildingCode),
                                                 c("date","hour","buildingCode","count","onCount","acCount","acUsedCount")] %>% 
                          mutate(.,labelBuildingDay=paste(buildingCode,date,sep = "_")) %>%
                          .[,.(date=date[1],
                               buildingCode=buildingCode[1],
                               runtime=length(hour[onCount!=0]),
                               acCount=sum(acCount,na.rm = TRUE),
                               acUsedCount=sum(acUsedCount,na.rm = TRUE)
                               ),by=labelBuildingDay] %>% 
                          mutate(.,acUsedIntensity=acUsedCount/acCount,timeType="0")%>%#������Ϊ0
                          merge(x=.,y=info.hznu.teaching.position[,c("buildingName","buildingCode","lat","lng")],
                                all.x=TRUE,by.x="buildingCode",by.y="buildingCode")
data.plfm.hznu.energy.daily<-data.hznu.building.energy %>% mutate(.,labelBuildingDay=paste(buildingCode,date,sep = "_")) %>%
                             .[,.(date=date[1],
                                  buildingCode=buildingCode[1],
                                  timeType="0",
                                  modiElec=sum(modiElec,na.rm = TRUE)
                                  ),by=labelBuildingDay]%>%#������Ϊ0
                              merge(x=.,y=info.hznu.teaching.position[,c("buildingName","buildingCode","lat","lng")],
                                    all.x=TRUE,by.x="buildingCode",by.y="buildingCode")
write.xlsx(data.plfm.hznu.thermo.daily,file="PLFM_HZNU_Thermo_daily.xlsx")
write.xlsx(data.plfm.hznu.use.daily,file="PLFM_HZNU_Use_daily.xlsx")
write.xlsx(data.plfm.hznu.energy.daily,file="PLFM_HZNU_Energy_daily.xlsx")

#�ܺĵ��ꡢ�����ݴ���
data.plfm.hznu.energy.monthly<-data.plfm.hznu.energy.daily %>% 
                               mutate(.,labelBuildingMonth=substr(labelBuildingDay,1,18),date=paste(substr(date,1,8),"01",sep = "")) %>%
                               .[,.(date=date[1],
                                    buildingCode=buildingCode[1],
                                    buildingName=buildingName[1],
                                    lat=lat[1],
                                    lng=lng[1],
                                    timeType="1",
                                    modiElec=sum(modiElec,na.rm = TRUE)
                                    ),by=labelBuildingMonth]
data.plfm.hznu.energy.annual<-data.plfm.hznu.energy.monthly %>% 
                              mutate(.,labelBuildingYear=substr(labelBuildingMonth,1,15),date=paste(substr(date,1,5),"01-01",sep = "")) %>%
                              .[,.(date=date[1],
                                   buildingCode=buildingCode[1],
                                   buildingName=buildingName[1],
                                   lat=lat[1],
                                   lng=lng[1],
                                   timeType="2",
                                   modiElec=sum(modiElec,na.rm = TRUE)
                              ),by=labelBuildingYear]


write.xlsx(data.plfm.hznu.energy.monthly,file="PLFM_HZNU_Energy_monthly.xlsx")
write.xlsx(data.plfm.hznu.energy.annual,file="PLFM_HZNU_Energy_annual.xlsx")



length(unique(data.plfm.hznu.thermo$buildingCode))
length(unique(data.plfm.hznu.use$buildingCode))


####����סլ������####
#�ϲ�����Ϊ��
data.plfm.rsdt.location<-as.data.table(read.xlsx(file = "PLFM_RSDT_location.xlsx",sheetIndex = 1)) %>% mutate(.,id=1:nrow(.))

set.seed(711)
data.plfm.rsdt.window<-as.data.table(read.xlsx(file = "PLFM_RSDT_Window.xlsx",sheetIndex = 1)) %>% 
                       mutate(.,locationID=sample(1:150,nrow(.),replace = TRUE)) %>%
                       merge(x=.,y=data.plfm.rsdt.location,all.x=TRUE,by.x="locationID",by.y="id")
data.plfm.rsdt.ac<-as.data.table(read.xlsx(file = "PLFM_RSDT_AC.xlsx",sheetIndex = 1))%>% 
                   mutate(.,locationID=sample(1:150,nrow(.),replace = TRUE))%>%
                   merge(x=.,y=data.plfm.rsdt.location,all.x=TRUE,by.x="locationID",by.y="id")

write.xlsx(data.plfm.rsdt.window,file="PLFM_RSDT_Window_final.xlsx")
write.xlsx(data.plfm.rsdt.ac,file="PLFM_RSDT_AC_final.xlsx")
