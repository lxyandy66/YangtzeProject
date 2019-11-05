####用于淑琴姐姐那个平台可视化部分的数据处理####

data.cplx.monthly<-data.raw.final %>% mutate(.,date=paste(year,month,sep = "-")) %>% 
                   .[typeCode=="M",c("BuildingCode","typeCode","area","date","value","eui")] %>% 
                   mutate(.,dataType="1")#月数据为1

data.cplx.annual<-data.all.annualSum %>% mutate(.,date=paste(year,"01",sep="-")) %>%
                  .[typeCode=="M",c("BuildingCode","typeCode","area","date","annualEnergy","annualEUI")]%>% 
                  mutate(.,dataType="2")#年数据为2

write.xlsx(data.cplx.monthly,file = "CPLX_MonthlyData.xlsx")
write.xlsx(data.cplx.annual,file = "CPLX_AnnualData.xlsx")


####HZNU温度数据####
#日数据
data.plfm.hznu.thermo.daily<-data.hznu.teaching.thermo.final[,c("date","roomCode",sprintf("modH%02d",8:22))] %>%
                       mutate(.,buildingCode=substr(roomCode,1,10),labelBuildingDay=paste(buildingCode,date,sep="_"),
                              meanDailyTemp=apply(.[,c(sprintf("modH%02d",8:22))], MARGIN = 1, FUN = mean,na.rm=TRUE)) %>%
                       .[,.(date=date[1],
                            buildingCode=buildingCode[1],
                            count=length(roomCode),
                            meanTemp=mean(meanDailyTemp,na.rm=TRUE),
                            timeType="0"),by=labelBuildingDay]#日数据为0
data.plfm.hznu.use.daily<-data.hznu.building.use[,c("date","hour","buildingCode","count","onCount","acCount","acUsedCount")] %>% 
                    mutate(.,labelBuildingDay=paste(buildingCode,date,sep = "_")) %>%
                    .[,.(date=date[1],
                         buildingCode=buildingCode[1],
                         runtime=length(hour[onCount!=0]),
                         acCount=sum(acCount,na.rm = TRUE),
                         acUsedCount=sum(acUsedCount,na.rm = TRUE)
                         ),by=labelBuildingDay] %>% 
                    mutate(.,acUsedIntensity=acUsedCount/acCount,timeType="0")#日数据为0
data.plfm.hznu.energy.daily<-data.hznu.building.energy %>% mutate(.,labelBuildingDay=paste(buildingCode,date,sep = "_")) %>%
                             .[,.(date=date[1],
                                  buildingCode=buildingCode[1],
                                  timeType="0",
                                  modiElec=sum(modiElec,na.rm = TRUE)
                                  ),by=labelBuildingDay]
write.xlsx(data.plfm.hznu.thermo.daily,file="PLFM_HZNU_Thermo_daily.xlsx")
write.xlsx(data.plfm.hznu.use.daily,file="PLFM_HZNU_Use_daily.xlsx")
write.xlsx(data.plfm.hznu.energy.daily,file="PLFM_HZNU_Energy_daily.xlsx")


length(unique(data.plfm.hznu.thermo$buildingCode))
length(unique(data.plfm.hznu.use$buildingCode))


####处理住宅的数据####
#合并坐标为主
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

