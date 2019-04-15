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
conn<-dbConnect(MySQL(),dbname="zdacp-01",user="root",password="",host="localhost")
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
  

####时间轴处理，统一为10分钟间隔####
data.zju.energy.raw$datetime<-as.POSIXct(data.zju.energy.raw$begin,formate="%Y-%m-%d %H:%M:%OS")
data.zju.env.raw$datetime<-as.POSIXct(data.zju.env.raw$date_time,formate="%Y-%m-%d %H:%M:%OS")
nn<-data.zju.env.raw[,.(
  count=length(build_code)
),by=substr(data.zju.env.raw$modiTime,11,16)]#时间轴都存在有问题的
#标签统一为 baseLabel: 时间）_房间号
data.zju.energy.raw$modiTime<-paste(substr(data.zju.energy.raw$datetime,1,15),"0:00",sep = "")
data.zju.env.raw$modiTime<-paste(substr(data.zju.env.raw$datetime,1,15),"0:00",sep = "")
data.zju.thermo.rawProcess$modiTime<-paste(substr(data.zju.thermo.rawProcess$date_time,1,15),"0:00",sep = "")

data.zju.env.raw$baseLabel<-paste(data.zju.env.raw$modiTime,data.zju.env.raw$room_code,sep = "_")
data.zju.thermo.rawProcess$baseLabel<-paste(data.zju.thermo.rawProcess$modiTime,
                                            data.zju.thermo.rawProcess$room_code,sep = "_")
data.zju.energy.raw$roomCode<-substr(data.zju.energy.raw$ac_code,1,13)
data.zju.energy.raw$total_elec<-abs(as.numeric(data.zju.energy.raw$total_elec))
data.zju.energy.raw[total_elec>100]$total_elec<-1#只有14条
data.zju.energy.raw$baseLabel<-paste(data.zju.energy.raw$modiTime,data.zju.energy.raw$roomCode,sep = "_")
data.zju.energy.raw$labelTimeAc<-paste(data.zju.energy.raw$modiTime,data.zju.energy.raw$ac_code,sep = "_")

####基于教室空调使用数据，合并其他参数####
#
####对已经清洗的数据再进行预处理####
#去重复，有部分时间点相同但有两个记录的情况
# 同一时刻两点温度相差2度以内取平均值，否则取小值，似乎整体偏大

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
length(unique(data.zju.thermo$baseLabel))


# 合并原预处理（预处理对吗...）到data.zju.env.raw
# nn<-data.zju.env.raw[duplicated(data.zju.env.raw$baseLabel)]
# nn1<-data.zju.env.raw[baseLabel%in% nn$baseLabel]#从数据库里拉出来的没毛病
length(unique(data.zju.env.raw$baseLabel))#290901
data.zju.env.raw<-merge(x=data.zju.env.raw,y=data.zju.thermo[,c("baseLabel","modiTemp")],
                        all.x = TRUE,by.x = "baseLabel",by.y = "baseLabel")
data.zju.env.raw<-data.zju.env.raw[!duplicated(data.zju.env.raw)]
nn<-data.zju.env.raw[duplicated(data.zju.env.raw$baseLabel)]
nn1<-data.zju.env.raw[baseLabel %in% nn$baseLabel]#看看重复情况：多出来10条，温度湿度都为0
####处理能耗数据####
#噫这张表居然还是到末端的

#清洗重复数据
data.zju.energy.raw<-data.zju.energy.raw[!duplicated(data.zju.energy.raw)]
length(unique(data.zju.energy.raw$labelTimeAc))#617635 总长637369
nn<-data.zju.energy.raw[duplicated(data.zju.energy.raw$labelTimeAc)]
nn1<-data.zju.energy.raw[labelTimeAc %in% nn$labelTimeAc]#看看重复情况
#nn2对应原始数据中不存在重复的记录点
nn2<-data.zju.energy.raw[!labelTimeAc %in% nn$labelTimeAc,.(
  baseLabel=baseLabel[1],
  datetime=datetime[1],
  total_elec=total_elec[1]
  # range=range(total_elec)[2]-range(total_elec)[1]
),by=labelTimeAc]#分析一下重复的情况
ggplot(nn2[total_elec!=0],aes(x=total_elec))+geom_density()+xlim(0,1)#597909条仅有53条大于1
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
  total_elec=mean(total_elec,na.rm = TRUE),
  modiElec=ifelse((range(total_elec,na.rm = TRUE)[2]-range(total_elec,na.rm = TRUE)[1])>=1,
                    range(total_elec,na.rm = TRUE)[1],mean(total_elec,na.rm = TRUE))
),by=labelTimeAc]
ecLim<-mean(data.zju.energy.cleaned[modiElec!=0]$modiElec,na.rm = TRUE)+
  3*sd(data.zju.energy.cleaned[modiElec!=0]$modiElec,na.rm = TRUE)
# 大于3sigma仅占113/617680
ggplot(data = data.zju.energy.cleaned[modiElec!=0],aes(x=modiElec))+geom_density()#+xlim(0,ecLim)
boxplot(data.zju.energy.cleaned[total_elec!=0]$total_elec,outline = FALSE)
data.zju.energy.cleaned[total_elec>ecLim]$modiElec<-ecLim+(data.zju.energy.cleaned[total_elec>ecLim]$total_elec-ecLim)/10

data.zju.energy.room.raw<-data.zju.energy.cleaned[,.(
  datetime=datetime[1],
  roomCode=roomCode[1],
  acCount=length(unique(ac_code)),
  ac_code=ac_code[1],
  total_elec=sum(total_elec,na.rm = TRUE),
  modiElec=sum(modiElec,na.rm = TRUE)
),by=baseLabel]
ggplot(data = data.zju.energy.room.raw[modiElec!=0],aes(x=modiElec))+geom_density()+xlim(0,0.025)#+scale_x_continuous(breaks = seq(from=0,to=0.03,by=0.005))
boxplot(x=data.zju.energy.room.raw[modiElec>0]$modiElec)

####合并热环境到电耗表####
data.zju.combine.raw<-merge(x=data.zju.energy.room.raw[,c("baseLabel","datetime","roomCode","total_elec","modiElec")],
                            y=data.zju.env.raw[,c("baseLabel","temp","humidity","modiTemp")],
                            all.x = TRUE,by.x ="baseLabel",by.y ="baseLabel")

####增加学期、课程等标签####
data.zju.combine.raw$date<-substr(data.zju.combine.raw$baseLabel,1,10)
data.calendar.raw$date<-as.character(data.calendar.raw$date)
data.zju.combine.raw<-merge(x=data.zju.combine.raw,y=data.calendar.raw,all.x = TRUE,by= "date")
data.zju.combine.raw$time<-format(data.zju.combine.raw$datetime,format = "%H:%M")

#课程、周、学期等标签的添加
data.zju.schedule.raw<-read.xlsx(file="ZJU_CourseSchedule.xlsx",sheetIndex = 1)
data.zju.schedule.raw$originTime<-data.zju.schedule.raw$time
data.zju.schedule.raw$time<-format(data.zju.schedule.raw$originTime,format = "%H:%M")
data.zju.combine.raw<-merge(x=data.zju.combine.raw,y=data.zju.schedule.raw[,c("time","hasClass","detail","classNo")],
                            all.x = TRUE,by.x="time",by.y="time")
data.zju.combine.raw$detail<-as.character(data.zju.combine.raw$detail)
data.zju.combine.raw$dateStr<-format(data.zju.combine.raw$datetime,format = "%Y-%m-%d")
data.zju.combine.raw<-merge(x=data.zju.combine.raw,y=data.calendar.raw,all.x=TRUE,by.x = "dateStr",by.y = "date")

#添加对应课程属性
data.curriculum.raw$fullSemester<-paste(data.curriculum.raw$year,data.curriculum.raw$semester,sep = "_")
#不用for用sapply还有什么别的呢

#将课程标签对应到逐时的时间轴中
data.zju.combine.raw$lessonCode<-""
for(i in 1:length(data.curriculum.raw$lessonCode)){
  #1709,还好 #不行还是慢了
  #定义每周的序列
  weekVector<-data.curriculum.raw[i]$startWeek:data.curriculum.raw[i]$endWeek
  if(data.curriculum.raw[i]$weekType %in% c("full","temp")){
    weekTypeVector<-weekVector
  }else if(data.curriculum.raw[i]$weekType=="single"){
    weekTypeVector<-weekVector[weekVector %%2==0]-1
  }else if(data.curriculum.raw[i]$weekType=="double"){
    weekTypeVector<-weekVector[weekVector %%2==0]
  }
  # weekTypeVector<-ifelse(FALSE,weekVector[],weekVector[]+11)#为啥不行
  
  #定义课程序列
  lessonVector<-c(data.curriculum.raw[i]$startTime:data.curriculum.raw[i]$endTime)
  data.zju.combine.raw[semester==data.curriculum.raw[i]$fullSemester&
                         week %in% weekTypeVector&
                         weekday==data.curriculum.raw[i]$weekday&
                         classNo %in% lessonVector]$lessonCode<-data.curriculum.raw[i]$lessonCode
}

####根据课程编码合并课程相关信息####
data.zju.combine.final<-merge(x=data.zju.combine.raw,
                              y=data.curriculum.raw[,c("lessonCode","studentType","studentNum","weekType")],
                              all.x = TRUE,by.x="lessonCode",by.y="lessonCode")

####开关机状态清洗####
length(data.zju.combine.final[total_elec!=0&total_elec<=0.015]$total_elec)/
  length(data.zju.combine.final[total_elec!=0]$total_elec)
ecLim<-0.015
# > length(data.zju.combine.final[total_elec!=0&total_elec<=0.015]$total_elec)/
#   +   length(data.zju.combine.final[total_elec!=0]$total_elec)
# [1] 0.3274574
# > length(data.zju.combine.final[total_elec!=0&total_elec<=0.05]$total_elec)/
#   +   length(data.zju.combine.final[total_elec!=0]$total_elec)
# [1] 0.4016175

data.zju.combine.final$on_off<- 0
data.zju.combine.final[total_elec>=ecLim]$on_off<-1

#预处理完成


