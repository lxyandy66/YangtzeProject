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
stat.hznu.roomAc<-data.all[!is.na(ac_code),.(acCount=length(unique(ac_code))),by=roomCode]
