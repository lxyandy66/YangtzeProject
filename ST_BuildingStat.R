#本脚本用于建筑编码及功能的对应，包括其他的简单统计

####读取映射文件####
data.mapping.buildingType<-as.data.table(read.csv(file = "HZNU_建筑功能及编码对应.csv"))
data.mapping.buildingType<-data.mapping.buildingType[,c("buildingCode","type")]


##简单情况
# 总末端1492台，教学建筑末端709台
# 总房间数1162间，教学建筑房间数385间

####对于行为/能耗/热环境数据集中教学建筑单独区分####
#行为拆分
data.hznu.use.room.day$buildingCode<-substr(data.hznu.use.room.day$roomCode,1,10)
data.hznu.teaching.use<-data.hznu.use.room.day[buildingCode %in% 
                                  data.mapping.buildingType[type=="teaching"]$buildingCode]
#能耗拆分
data.hznu.energy.room.day$buildingCode<-substr(data.hznu.energy.room.day$roomCode,1,10)
data.hznu.teaching.energy<-data.hznu.energy.room.day[buildingCode %in% 
                                                 data.mapping.buildingType[type=="teaching"]$buildingCode]


####简单统计####
#房间空调数统计
# stat.hznu.roomAc<-data.all[!is.na(ac_code),.(acCount=length(unique(ac_code))),by=(roomCode=substr(ac_code,1,grep("m",ac_code)))]

data.all$roomCode<-apply(data.all[,"ac_code"],MARGIN=1,FUN=getSplitMember,splitSimbol="m")

data.all<-data.all[!is.na(ac_code)]
data.all$roomCode<-as.character(strsplit(data.all$ac_code,split = "m")[[1]][1])
# stat.hznu.roomAc<-as.data.table(read.xlsx(file = "HZNU_AcStat.xlsx",sheetIndex = 1))
stat.hznu.roomAc<-data.all[!is.na(ac_code),.(acCount=length(unique(ac_code))),by=roomCode]
stat.hznu.roomAc$buildingCode<-substr(stat.hznu.roomAc$roomCode,1,10)
stat.hznu.roomAc<-merge(x=stat.hznu.roomAc,y=data.mapping.buildingType[,c("buildingCode","type")],
                        by.x = "buildingCode",by.y="buildingCode",all.x = TRUE)
write.xlsx(x = stat.hznu.roomAc,file="HZNU_AcStat.xlsx")

####HZNU建筑面积调研列表####
stat.hznu.roomAc<-merge(x=stat.hznu.roomAc,
                        y=info.hznu.teaching.position[,c("buildingName","buildingCode")],
                        all.x = TRUE,by.x = "buildingCode",by.y="buildingCode")
stat.hznu.roomAc.teaching<-stat.hznu.roomAc[type=="teaching"] %>% 
                           mutate(.,NA.=NULL,roomCode=as.character(roomCode)) %>% as.data.table(.)
stat.hznu.roomAc.teaching$roomNo<-apply(X = stat.hznu.roomAc.teaching[,"roomCode"],MARGIN = 1,
                                        FUN = function(x){
                                          return(substr(x,nchar(x)-2,nchar(x)))
                                        })

####导入实际调研数据####
info.hznu.teaching.room<-read.xlsx(file="HZNU_Teaching _RoomInfo_Export.xlsx",sheetIndex = 1) %>% 
                         mutate(.,roomCode=as.character(.$roomCode)) %>% as.data.table(.) 
# str(info.hznu.teaching.room)
#检查一下教室分布情况
ggplot(data=info.hznu.teaching.room %>% mutate(.,modiSeat=as.factor(modiSeat)),aes(x=modiSeat,y=acCount))+geom_boxplot()
ggplot(data=info.hznu.teaching.room %>% mutate(.,acCount=as.factor(acCount)),aes(x=acCount,y=modiArea))+geom_boxplot()
describeBy(info.hznu.teaching.room[,c("modiArea","area")],list(modiSeat=info.hznu.teaching.room$modiSeat),na.)

