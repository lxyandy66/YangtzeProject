####本脚本用于HZNU追加数据的处理####

####从数据库中获取数据####
#MySQL中编码为utf8，ODBC接口需要将编码同样设置为utf8否则会乱码
connect<-odbcConnect("db_hznu",uid = "root",pwd = "",DBMSencoding="utf8")
data.append.raw<-as.data.table(sqlFetch(connect,"hznu_append_cn"))

####统一状态值####

####统一on_off
#> unique(data.append.raw$on_off)
#[1] <NA> 关机 开机 停止 制热 制冷 送风 自动
#Levels: 关机 开机 送风 停止 制冷 制热 自动
data.append.raw[innerEnergy<=0]$on_off<-"0"
data.append.raw[innerEnergy<=0]$status<-"off"
data.append.raw[on_off=="关机"|on_off=="停止"]$on_off<-"0"
data.append.raw[on_off=="关机"|on_off=="停止"]$status<-"off"

####统一status
unique(data.append.raw$status)
data.append.raw[status=="停止"]$on_off<-"0"
data.append.raw[status=="停止"]$status<-"off"
data.append.raw[status=="抽湿"|status=="除湿"]$status<-"dehumi"
data.append.raw[status=="送风"|status=="通风"]$status<-"venti"
data.append.raw[status=="制冷"]$status<-"cooling"
data.append.raw[status=="制热"]$status<-"heating"
data.append.raw[status=="自动"]$status<-"auto"

####统一风机能耗
#> table(data.append.raw$fanSpeed)
#1        2        3        4        5   低风档   高风档       弱   中风档 自动风档     <NA> 
# 1389     3831     3831    11381   436714  7301820  9702561   686819  2559105 10513503  4149548 
data.append.raw[on_off=="0"]$fanSpeed<-"off"
data.append.raw[fanSpeed=="1"]$fanSpeed<-"off"
data.append.raw[fanSpeed=="2"|fanSpeed=="低风档"|fanSpeed=="弱"]$fanSpeed<-"low"
data.append.raw[fanSpeed=="3"|fanSpeed=="中风档"]$fanSpeed<-"mid"
data.append.raw[fanSpeed=="4"|fanSpeed=="高风档"]$fanSpeed<-"high"
data.append.raw[fanSpeed=="5"|fanSpeed=="自动风档"]$fanSpeed<-"auto"

####能耗置零
#清空待机能耗
nn<-data.append.raw[status=="off"&on_off!="0"]
data.append.raw[on_off=="0"&innerEnergy!=0]$roomEnergy<-
  data.append.raw[on_off=="0"&innerEnergy!=0]$roomEnergy-
  data.append.raw[on_off=="0"&innerEnergy!=0]$innerEnergy
data.append.raw[on_off=="0"]$innerEnergy<-0

#能耗为0状态置0
data.append.raw[innerEnergy<=0]$on_off<-"0"
data.append.raw[innerEnergy<=0]$status<-"off"

####房间编码对应####
data.mapping.roomCode.old<-as.data.table(read.csv(file = "杭师大完成_房间属性-教学.csv"))
names(data.mapping.roomCode.old)<-c("roomName","roomCode","roomType")
data.mapping.roomCode.old$roomType<-NULL

data.mapping.roomCode.append<-read.csv(file = "HZNU_AppendRoomName.csv",encoding = "gbk")
data.mapping.roomCode.all<-as.data.table(rbind(data.mapping.roomCode.old,data.mapping.roomCode.append[,c("roomName","roomCode")]))

data.append.raw<-merge(data.append.raw,data.mapping.roomCode.all,all.x = TRUE,by.x = "roomName",by.y = "roomName")

####空调编码对应####
data.mapping.acCode<-data.append.raw[,.(roomName=unique(roomName),
                                        acName=unique(innerName),
                                        acAppendCode=c(1:length(unique(innerName)))
                                        ),by=roomCode]
data.mapping.acCode$acCode<-paste(data.mapping.acCode$roomCode,"m",sprintf("%02d",data.mapping.acCode$acAppendCode),sep = "")
data.mapping.acCode$acFullName<-paste(data.mapping.acCode$roomName,data.mapping.acCode$acName,sep = "_")
data.append.raw$acFullName<-paste(data.append.raw$roomName,data.append.raw$innerName,sep = "_")
data.append.raw<-merge(x=data.append.raw,y=data.mapping.acCode[,c("acFullName","acCode")],all.x = TRUE,by.x = "acFullName",by.y = "acFullName")

####完成追加数据初步处理，数据导出####
data.append.final<-data.append.raw[,c("datetime","roomCode","acCode","innerTemp","setTemp","status","on_off","fanSpeed","innerEnergy","roomEnergy")]
data.append.final$status<-as.character(data.append.final$status)
data.append.final$on_off<-as.character(data.append.final$on_off)


####统一新数据和旧数据变量名，汇总数据成新数据集####
data.raw.complete<-data.table(time=data.append.final$datetime,ac_code=data.append.final$acCode,total_elec=data.append.final$innerEnergy,
                              real_temp=data.append.final$innerTemp,set_temp=data.append.final$setTemp,state=data.append.final$status)

data.raw.complete$time<-as.character(data.raw.complete$time)
data.all$time<-as.character(data.all$time)
data.all$state<-as.character(data.all$state)
data.all$ac_code<-as.character(data.all$ac_code)
data.raw.complete<-rbind(data.raw.complete,data.all[,c("time","ac_code","total_elec","real_temp","set_temp","state")])

