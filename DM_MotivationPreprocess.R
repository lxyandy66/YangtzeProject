####本脚本用于分析教学建筑空调使用的驱动因素####
# 根据部分教室的空调电量情况，结合课表、气象条件分析空调使用的驱动因素

####读取校历信息####
data.calendar.raw<-read.xlsx(file = "2016_Spring_Summer_Semester.xlsx",sheetIndex = 1)
data.calendar.raw<-rbind(data.calendar.raw,read.xlsx(file = "2017_Spring_Summer_Semester.xlsx",sheetIndex = 1))
data.calendar.raw<-as.data.table(rbind(data.calendar.raw,read.xlsx(file = "2017_Autumn_Winter_Semester.xlsx",sheetIndex = 1)))
data.calendar.raw$isWeekend<-isWeekend(data.calendar.raw$date)

####读取课表信息####
data.curriculum.raw<-as.data.table(read.csv("课表_可导入.csv",encoding = "UTF-8"))
# 一个预处理，已经不需要了
# data.curriculum.raw$labelYearSemeType<-paste(data.curriculum.raw$year,
#   substring(data.curriculum.raw$semester,1,3),substring(data.curriculum.raw$weekType,1,1),sep = "_")
# data.curriculum.raw$lessonID<- -1
# for(i in unique(data.curriculum.raw$labelYearSemeType)){
#   data.curriculum.raw[labelYearSemeType==i]$lessonID<-c(1:length(data.curriculum.raw[labelYearSemeType==i]$lessonID))
# }

####处理上课周数和上课时间####
#数据中所有的上课时间都是连续的，即1-4节课，不存在如1、3、4节课情况
#数据中所有上课周数均是连续，即6-8周
data.curriculum.raw$week<-as.character(data.curriculum.raw$week)
data.curriculum.raw$time<-as.character(data.curriculum.raw$time)
data.curriculum.raw$startWeek<-"-1"
data.curriculum.raw$endWeek<-"-1"
data.curriculum.raw$startTime<-"-1"
data.curriculum.raw$endTime<-"-1"
data.curriculum.raw$startWeek<-sapply(data.curriculum.raw$week,getSplitMember,splitSimbol="-",index=1,simplify = TRUE)
data.curriculum.raw$endWeek<-sapply(data.curriculum.raw$week,getSplitMember,splitSimbol="-",isLastOne=TRUE,simplify = TRUE)
data.curriculum.raw$startTime<-sapply(data.curriculum.raw$time,getSplitMember,splitSimbol=",",index=1,simplify = TRUE)
data.curriculum.raw$endTime<-sapply(data.curriculum.raw$time,getSplitMember,splitSimbol=",",isLastOne=TRUE,simplify = TRUE)



####获取数据库中教室内热环境数据和空调用电数据####
conn<-dbConnect(MySQL(),dbname="zdacp-01",user="root",password="Kamarov",host="localhost")
dbListTables(conn)#查看table
dbSendQuery(conn,'SET NAMES utf8')#设置数据库读取编码格式为utf-8
# connect<-odbcConnect("zdacp-01",uid = "root",pwd = "",DBMSencoding="utf8")#mac下就不用odbc了
data.zju.energy.raw<-as.data.table(dbReadTable(conn,"zelement_ac_elec_data"))
data.zju.env.raw<-as.data.table(dbReadTable(conn,"zelement_room_realdata"))
dbDisconnect(conn)#断开当前连接

data.zju.energy.raw<-data.zju.energy.raw[,c("begin","end","ac_code","build_code","total_elec")]
data.zju.env.raw<-data.zju.env.raw[,c("date_time","build_code","room_code","temp","humidity")]  

data.zju.env.raw$date<-substr(data.zju.env.raw$date_time,1,10)
# ggplot(data=data.zju.env.raw[date=="2018-04-14"& room_code=="330100D284305"],aes(x=date_time,y=temp,group=date))+geom_line()  
  
####基于教室空调使用数据，合并其他参数####
# baseLabel: 时间_房间号
####对已经清洗的数据再进行预处理####
#去重复，有部分时间点相同但有两个记录的情况
# 同一时刻两点温度相差2度以内取平均值，否则取小值，似乎整体偏大
data.zju.thermo.rawProcess$baseLabel<-paste(data.zju.thermo.rawProcess$date_time,
                                            data.zju.thermo.rawProcess$room_code,sep = "_")
data.zju.thermo.rawProcess<-data.zju.thermo.rawProcess[!duplicated(data.zju.thermo.rawProcess)]
length(unique(data.zju.thermo.rawProcess$baseLabel))#438958,总长442199
nn<-data.zju.thermo.rawProcess[!duplicated(data.zju.thermo.rawProcess$baseLabel)]
nn1<-data.zju.thermo.rawProcess[baseLabel %in% nn$baseLabel]#看看重复情况
data.zju.thermo<-data.zju.thermo.rawProcess[,.(
  baseLabel=baseLabel[1],
  datetime=date_time[1],
  roomCode=room_code[1],
  temp=ifelse((max(temp,na.rm = TRUE)-min(temp,na.rm = TRUE))>=2,
              min(temp,na.rm = TRUE),mean(temp,na.rm = TRUE)),
  humidity=ifelse((max(humidity,na.rm = TRUE)-min(humidity,na.rm = TRUE))>=2,
                  min(humidity,na.rm = TRUE),mean(humidity,na.rm = TRUE))
),by=baseLabel]
data.zju.thermo$modiTemp<-data.zju.thermo$temp

data.zju.env.raw$datetime<-as.POSIXct(data.zju.env.raw$date_time,formate="%Y-%m-%d %H:%M:%OS")
data.zju.env.raw$baseLabel<-paste(data.zju.env.raw$datetime,data.zju.env.raw$room_code,sep = "_")
# 合并原预处理（预处理对吗...）到data.zju.env.raw

# nn<-data.zju.env.raw[duplicated(data.zju.env.raw$baseLabel)]
# nn1<-data.zju.env.raw[baseLabel%in% nn$baseLabel]#从数据库里拉出来的没毛病
length(unique(data.zju.env.raw$baseLabel))#290911
data.zju.env.raw<-merge(x=data.zju.env.raw,y=data.zju.thermo[,c("baseLabel","modiTemp")],all.x = TRUE,by.x = "baseLabel",by.y = "baseLabel")
data.zju.env.raw<-data.zju.env.raw[!duplicated(data.zju.env.raw)]

####处理能耗数据####
data.zju.energy.raw$datetime<-as.POSIXct(data.zju.energy.raw$begin,formate="%Y-%m-%d %H:%M:%OS")
#噫这张表居然还是到末端的
data.zju.energy.raw$roomCode<-substr(data.zju.energy.raw$ac_code,1,13)
data.zju.energy.raw$total_elec<-abs(as.numeric(data.zju.energy.raw$total_elec))
data.zju.energy.raw$baseLabel<-paste(data.zju.energy.raw$datetime,data.zju.energy.raw$roomCode,sep = "_")
#清洗重复数据
data.zju.energy.raw$labelTimeAc<-paste(data.zju.energy.raw$datetime,data.zju.energy.raw$ac_code,sep = "_")
data.zju.energy.raw<-data.zju.energy.raw[!duplicated(data.zju.energy.raw)]
length(unique(data.zju.energy.raw$labelTimeAc))#617680 总长637369
nn<-data.zju.energy.raw[duplicated(data.zju.energy.raw$labelTimeAc)]
nn1<-data.zju.energy.raw[labelTimeAc %in% nn$labelTimeAc]#看看重复情况
#nn2对应原始数据中不存在重复的记录点
nn2<-data.zju.energy.raw[!labelTimeAc %in% nn$labelTimeAc,.(
  baseLabel=baseLabel[1],
  datetime=datetime[1],
  total_elec=total_elec[1]
  # range=range(total_elec)[2]-range(total_elec)[1]
),by=labelTimeAc]#分析一下重复的情况
ggplot(nn2[total_elec!=0],aes(x=total_elec))+geom_density()+xlim(0,1)#597991条仅有39条大于1
boxplot(x=nn2[!is.na(total_elec)&total_elec!=0]$total_elec)
#nn3对应有重复记录中，同一时刻的重复记录的极差
nn3<-nn1[,.(baseLabel=baseLabel[1],
         datetime=datetime[1],
         range=range(total_elec)[2]-range(total_elec)[1]
         ),by=labelTimeAc]
ggplot(nn3[range!=0],aes(x=range))+geom_density()+xlim(0,2)#有重复记录的电耗记录差距太大
boxplot(nn3$range)
data.zju.energy.cleaned<-data.zju.energy.raw[,.(
  baseLabel=baseLabel[1],
  datetime=datetime[1],
  ac_code=ac_code[1],
  roomCode=roomCode[1],
  total_elec=ifelse((range(total_elec,na.rm = TRUE)[2]-range(total_elec,na.rm = TRUE)[1])>=1,
                    range(total_elec,na.rm = TRUE)[1],mean(total_elec,na.rm = TRUE))
),by=labelTimeAc]
ecLim<-mean(data.zju.energy.cleaned[total_elec!=0]$total_elec,na.rm = TRUE)+
  3*sd(data.zju.energy.cleaned[total_elec!=0]$total_elec,na.rm = TRUE)
# 大于3sigma仅占113/617680
ggplot(data = data.zju.energy.cleaned[total_elec!=0],aes(x=total_elec))+geom_density()#+xlim(0,ecLim)
boxplot(data.zju.energy.cleaned[total_elec!=0]$total_elec,outline = FALSE)
# 再对大于2的放缩一下（？
data.zju.energy.cleaned$modiElec<-data.zju.energy.cleaned$total_elec
data.zju.energy.cleaned[total_elec>ecLim]$modiElec<-ecLim+(data.zju.energy.cleaned[total_elec>ecLim]$total_elec-ecLim)/10

data.zju.energy.room.raw<-data.zju.energy.cleaned[,.(
  datetime=datetime[1],
  roomCode=roomCode[1],
  acCount=length(unique(ac_code)),
  ac_code=ac_code[1],
  total_elec=sum(total_elec,na.rm = TRUE),
  modiElec=sum(modiElec,na.rm = TRUE)
),by=baseLabel]
ggplot(data = data.zju.energy.room.raw[modiElec!=0],aes(x=modiElec))+geom_density()
boxplot(x=data.zju.energy.room.raw[modiElec>0]$modiElec)

####合并热环境到电耗表####
data.zju.combine.raw<-merge(x=data.zju.energy.room.raw[,c("baseLabel","datetime","roomCode","total_elec","modiElec")],
                            y=data.zju.env.raw[,c("baseLabel","temp","humidity","modiTemp")],
                            all.x = TRUE,by.x ="baseLabel",by.y ="baseLabel")



