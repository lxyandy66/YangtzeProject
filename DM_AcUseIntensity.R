#本脚本用于统计空调开启的台数，用于能耗模式挖掘和解耦
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

save(data.hznu.all.use,file = "HZNU_含追加_全部类型_房间级_含使用强度_行为模式预处理完成.rdata")
