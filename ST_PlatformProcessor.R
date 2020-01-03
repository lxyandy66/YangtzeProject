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
#加载HZNU的坐标数据
info.hznu.teaching.position<-read.csv(file = "HZNU_教学建筑编码及坐标对应表.csv",encoding = "UTF-8")
data.plfm.hznu.thermo.daily<-data.hznu.teaching.thermo.final[,c("date","roomCode",sprintf("modH%02d",8:22))] %>%
                             mutate(.,buildingCode=substr(roomCode,1,10),labelBuildingDay=paste(buildingCode,date,sep="_"),
                                    meanDailyTemp=apply(.[,c(sprintf("modH%02d",8:22))], MARGIN = 1, FUN = mean,na.rm=TRUE)) %>%
                             .[,.(date=date[1],
                                  buildingCode=buildingCode[1],
                                  count=length(roomCode),
                                  meanTemp=mean(meanDailyTemp,na.rm=TRUE),
                                  timeType="0"),by=labelBuildingDay] %>%#日数据为0
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
                          mutate(.,acUsedIntensity=acUsedCount/acCount,timeType="0")%>%#日数据为0
                          merge(x=.,y=info.hznu.teaching.position[,c("buildingName","buildingCode","lat","lng")],
                                all.x=TRUE,by.x="buildingCode",by.y="buildingCode")
data.plfm.hznu.energy.daily<-data.hznu.building.energy %>% mutate(.,labelBuildingDay=paste(buildingCode,date,sep = "_")) %>%
                             .[,.(date=date[1],
                                  buildingCode=buildingCode[1],
                                  timeType="0",
                                  modiElec=sum(modiElec,na.rm = TRUE)
                                  ),by=labelBuildingDay]%>%#日数据为0
                              merge(x=.,y=info.hznu.teaching.position[,c("buildingName","buildingCode","lat","lng")],
                                    all.x=TRUE,by.x="buildingCode",by.y="buildingCode")
write.xlsx(data.plfm.hznu.thermo.daily,file="PLFM_HZNU_Thermo_daily.xlsx")
write.xlsx(data.plfm.hznu.use.daily,file="PLFM_HZNU_Use_daily.xlsx")
write.xlsx(data.plfm.hznu.energy.daily,file="PLFM_HZNU_Energy_daily.xlsx")

#能耗的年、月数据处理
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

#住宅开窗和空调使用数据去重
fileName<-c("RSDT_Window_final_upload_LVRM.xlsx","RSDT_Window_final_upload_BDRM.xlsx",
            "RSDT_AC_final_upload_LVRM.xlsx","RSDT_AC_final_upload_BDRM.xlsx")
for(i in 1:length(fileName)){
  read.xlsx(file = fileName[i],sheetIndex = 1) %>% as.data.table(.) %>% 
    .[!duplicated(.[,c("buildingCode","time")])] %>% write.xlsx(x=.,file = fileName[i])
}

length(unique(data.plfm.rsdt.window$locationID))


####WJJ办公数据处理####
#逐时办公空调使用概率
nn<-read.xlsx(file = "WJJ_office_Window.xlsx",sheetIndex = 1) %>% as.data.table(.)
nn<-melt(nn[,c("hour","dataType",paste("X",c(1:12),sep = ""))],id.var=c("hour","dataType"))
nn$modiSeason<-apply(nn[,"variable"],MARGIN = 1,function(x){ getSeason(as.numeric(substring(x,2)))})
nn[modiSeason %in% c("Spring","Autumn")]$modiSeason<-"Transition"
nn<-nn[,.(hour=hour[1],
          modiSeason=modiSeason[1],
          usedRate=1-(sum(value[dataType=="SUM"],na.rm = TRUE)/sum(value[dataType=="COUNT"],na.rm = TRUE))
          ),by=(seasonHour=paste(modiSeason,hour,sep = "_"))]
nn<-dcast(nn[,-c("seasonHour")],modiSeason~hour)
write.xlsx(x=nn,file="WJJ_office_hourlyWindow.xlsx")

####HZNU教学逐时空调使用概率处理####
melt(data.hznu.teaching.use[,c("modiSeason",sprintf("h%d",8:22))],id.vars = "modiSeason")%>%
  dcast(.,modiSeason~variable,mean) %>%write.xlsx(x=.,file="HZNU_HourlyACUsedRate.xlsx")
