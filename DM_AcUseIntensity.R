#本脚本用于统计空调开启的台数，以及小时能耗等，用于能耗模式挖掘和解耦
#接"HZNU_含追加_末端级_行为模式预处理完成.rdata"
#接 raw.rawData 数据集

raw.rawData$roomCode<-substr(raw.rawData$ac_code,1,13)
raw.rawData$labelRoomDay<-paste(raw.rawData$roomCode,raw.rawData$date,sep = "_")

####汇总至房间-日####
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
                                   ),by=labelRoomDay]#注意此处有部分finalState是NA但是runtime非零
####汇总累计空调使用小时数等####
#日内实际使用小时*空调数
data.hznu.all.use$sumAllAcRuntime<-apply(X = data.hznu.all.use[,c(sprintf("h%d",8:22))],MARGIN = 1,FUN = sum,na.rm=TRUE)
#日内总空调小时数容量
data.hznu.all.use$capAcHour<-15*data.hznu.all.use$acCount
#日内实际空调小时数容量
data.hznu.all.use$realCapAcHour<-data.hznu.all.use$runtime*data.hznu.all.use$acCount
#空调使用强度=实际空调小时数 / 实际空调小时数容量
data.hznu.all.use$acIntensity<-data.hznu.all.use$sumAllAcRuntime/data.hznu.all.use$realCapAcHour
#平均空调使用台数
data.hznu.all.use$meanAcUsed<-apply(X = data.hznu.all.use[,c(sprintf("h%d",8:22))],MARGIN = 1,
                                    FUN = function(x){mean(x[x!=0],na.rm = TRUE)})
#日内平均空调使用率
data.hznu.all.use$acUsedRate<-data.hznu.all.use$meanAcUsed/data.hznu.all.use$acCount


save(data.hznu.all.use,file = "HZNU_含追加_全部类型_房间级_含使用强度_行为模式预处理完成.rdata")


####统计空调小时电耗####
#接 HZNU_含追加_末端级_能耗模式预处理完成_新清洗.rdata HZNU_临时_仅教学_逐时房间能耗.rdata
#数据集 data.hznu.energy.ac.day; temp.hznu.energy.room

# #计算单台空调平均小时电耗
# #不这么算
# data.hznu.energy.ac.day$elecIntensity<-apply(X=data.hznu.energy.ac.day[,c(sprintf("h%d",8:22))],MARGIN = 1,
#                                              FUN = function(x){ mean(x[x>0.2],na.rm = TRUE)})
# nn<-data.hznu.energy.ac.day[!is.nan(data.hznu.energy.ac.day$elecIntensity),]

####临时统计房间空调平均功率####
temp.hznu.energy.room<-melt(data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date","sumElec","runtime",sprintf("h%d",8:22))],
                            id.var=c("labelRoomDay","roomCode","date","sumElec","runtime"))
temp.hznu.energy.room<-temp.hznu.energy.room[!duplicated(temp.hznu.energy.room)]
names(temp.hznu.energy.room)<-c("labelRoomDay","roomCode","date","sumElec","runtime","hour","roomElec")
temp.hznu.energy.room$labelRoomHour<-paste(temp.hznu.energy.room$labelRoomDay,temp.hznu.energy.room$hour,sep = "_")
# save(temp.hznu.energy.room,file="HZNU_临时_仅教学_逐时房间能耗.rdata")

temp.hznu.acUse.room<-melt(data.hznu.all.use[,c("labelRoomDay","roomCode","date",sprintf("h%d",8:22))],id.var=c("labelRoomDay","roomCode","date"))
names(temp.hznu.acUse.room)<-c("labelRoomDay","roomCode","date","hour","acUsedCount")
temp.hznu.acUse.room$labelRoomHour<-paste(temp.hznu.acUse.room$labelRoomDay,temp.hznu.acUse.room$hour,sep = "_")

#合并小时空调使用数量
temp.hznu.energy.room<-merge(x=temp.hznu.energy.room,y=temp.hznu.acUse.room[,c("labelRoomHour","acUsedCount")],
                             all.x = TRUE,by="labelRoomHour")
temp.hznu.energy.room$meanPower<-temp.hznu.energy.room$roomElec/temp.hznu.energy.room$acUsedCount
#这里有问题，行为中有设置能耗最低阈值，能耗清洗中没有
temp.hznu.energy.room[is.nan(meanPower)|is.infinite(meanPower)]$meanPower<-NA
#简单点吧
temp.stat.energy.room<-temp.hznu.energy.room[,.(roomCode=roomCode[1],
                                                date=date[1],
                                                runtime=runtime[1],
                                                countRuntime=sum(!is.na(meanPower)),
                                                meanDailyAcElec=mean(meanPower[meanPower!=0],na.rm = TRUE)),by=labelRoomDay]
save(temp.stat.energy.room,file = "HZNU_仅教学_时均空调单台电耗统计.rdata")


