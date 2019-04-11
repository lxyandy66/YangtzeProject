####���ű����ڷ�����ѧ�����յ�ʹ�õ���������####
# ���ݲ��ֽ��ҵĿյ������������Ͽα����������������յ�ʹ�õ���������

####��ȡУ����Ϣ####
data.calendar.raw<-read.xlsx(file = "2016_Spring_Summer_Semester.xlsx",sheetIndex = 1)
data.calendar.raw<-rbind(data.calendar.raw,read.xlsx(file = "2017_Spring_Summer_Semester.xlsx",sheetIndex = 1))
data.calendar.raw<-as.data.table(rbind(data.calendar.raw,read.xlsx(file = "2017_Autumn_Winter_Semester.xlsx",sheetIndex = 1)))
data.calendar.raw$isWeekend<-isWeekend(data.calendar.raw$date)

####��ȡ�α���Ϣ####
data.curriculum.raw<-as.data.table(read.csv("�α�_�ɵ���.csv",encoding = "UTF-8"))
# һ��Ԥ�������Ѿ�����Ҫ��
# data.curriculum.raw$labelYearSemeType<-paste(data.curriculum.raw$year,
#   substring(data.curriculum.raw$semester,1,3),substring(data.curriculum.raw$weekType,1,1),sep = "_")
# data.curriculum.raw$lessonID<- -1
# for(i in unique(data.curriculum.raw$labelYearSemeType)){
#   data.curriculum.raw[labelYearSemeType==i]$lessonID<-c(1:length(data.curriculum.raw[labelYearSemeType==i]$lessonID))
# }

####�����Ͽ��������Ͽ�ʱ��####
#���������е��Ͽ�ʱ�䶼�������ģ���1-4�ڿΣ���������1��3��4�ڿ����
#�����������Ͽ�����������������6-8��
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



####��ȡ���ݿ��н������Ȼ������ݺͿյ��õ�����####
conn<-dbConnect(MySQL(),dbname="zdacp-01",user="root",password="Kamarov",host="localhost")
dbListTables(conn)#�鿴table
dbSendQuery(conn,'SET NAMES utf8')#�������ݿ��ȡ�����ʽΪutf-8
# connect<-odbcConnect("zdacp-01",uid = "root",pwd = "",DBMSencoding="utf8")#mac�¾Ͳ���odbc��
data.zju.energy.raw<-as.data.table(dbReadTable(conn,"zelement_ac_elec_data"))
data.zju.env.raw<-as.data.table(dbReadTable(conn,"zelement_room_realdata"))
dbDisconnect(conn)#�Ͽ���ǰ����

data.zju.energy.raw<-data.zju.energy.raw[,c("begin","end","ac_code","build_code","total_elec")]
data.zju.env.raw<-data.zju.env.raw[,c("date_time","build_code","room_code","temp","humidity")]  

data.zju.env.raw$date<-substr(data.zju.env.raw$date_time,1,10)
# ggplot(data=data.zju.env.raw[date=="2018-04-14"& room_code=="330100D284305"],aes(x=date_time,y=temp,group=date))+geom_line()  
  
####���ڽ��ҿյ�ʹ�����ݣ��ϲ���������####
# baseLabel: ʱ��_�����
####���Ѿ���ϴ�������ٽ���Ԥ����####
#ȥ�ظ����в���ʱ�����ͬ����������¼�����
# ͬһʱ�������¶����2������ȡƽ��ֵ������ȡСֵ���ƺ�����ƫ��
data.zju.thermo.rawProcess$baseLabel<-paste(data.zju.thermo.rawProcess$date_time,
                                            data.zju.thermo.rawProcess$room_code,sep = "_")
data.zju.thermo.rawProcess<-data.zju.thermo.rawProcess[!duplicated(data.zju.thermo.rawProcess)]
length(unique(data.zju.thermo.rawProcess$baseLabel))#438958,�ܳ�442199
nn<-data.zju.thermo.rawProcess[!duplicated(data.zju.thermo.rawProcess$baseLabel)]
nn1<-data.zju.thermo.rawProcess[baseLabel %in% nn$baseLabel]#�����ظ����
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
# �ϲ�ԭԤ������Ԥ��������...����data.zju.env.raw

# nn<-data.zju.env.raw[duplicated(data.zju.env.raw$baseLabel)]
# nn1<-data.zju.env.raw[baseLabel%in% nn$baseLabel]#�����ݿ�����������ûë��
length(unique(data.zju.env.raw$baseLabel))#290911
data.zju.env.raw<-merge(x=data.zju.env.raw,y=data.zju.thermo[,c("baseLabel","modiTemp")],all.x = TRUE,by.x = "baseLabel",by.y = "baseLabel")
data.zju.env.raw<-data.zju.env.raw[!duplicated(data.zju.env.raw)]

####�����ܺ�����####
data.zju.energy.raw$datetime<-as.POSIXct(data.zju.energy.raw$begin,formate="%Y-%m-%d %H:%M:%OS")
#�����ű���Ȼ���ǵ�ĩ�˵�
data.zju.energy.raw$roomCode<-substr(data.zju.energy.raw$ac_code,1,13)
data.zju.energy.raw$total_elec<-abs(as.numeric(data.zju.energy.raw$total_elec))
data.zju.energy.raw$baseLabel<-paste(data.zju.energy.raw$datetime,data.zju.energy.raw$roomCode,sep = "_")
#��ϴ�ظ�����
data.zju.energy.raw$labelTimeAc<-paste(data.zju.energy.raw$datetime,data.zju.energy.raw$ac_code,sep = "_")
data.zju.energy.raw<-data.zju.energy.raw[!duplicated(data.zju.energy.raw)]
length(unique(data.zju.energy.raw$labelTimeAc))#617680 �ܳ�637369
nn<-data.zju.energy.raw[duplicated(data.zju.energy.raw$labelTimeAc)]
nn1<-data.zju.energy.raw[labelTimeAc %in% nn$labelTimeAc]#�����ظ����
#nn2��Ӧԭʼ�����в������ظ��ļ�¼��
nn2<-data.zju.energy.raw[!labelTimeAc %in% nn$labelTimeAc,.(
  baseLabel=baseLabel[1],
  datetime=datetime[1],
  total_elec=total_elec[1]
  # range=range(total_elec)[2]-range(total_elec)[1]
),by=labelTimeAc]#����һ���ظ������
ggplot(nn2[total_elec!=0],aes(x=total_elec))+geom_density()+xlim(0,1)#597991������39������1
boxplot(x=nn2[!is.na(total_elec)&total_elec!=0]$total_elec)
#nn3��Ӧ���ظ���¼�У�ͬһʱ�̵��ظ���¼�ļ���
nn3<-nn1[,.(baseLabel=baseLabel[1],
         datetime=datetime[1],
         range=range(total_elec)[2]-range(total_elec)[1]
         ),by=labelTimeAc]
ggplot(nn3[range!=0],aes(x=range))+geom_density()+xlim(0,2)#���ظ���¼�ĵ�ļ�¼���̫��
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
# ����3sigma��ռ113/617680
ggplot(data = data.zju.energy.cleaned[total_elec!=0],aes(x=total_elec))+geom_density()#+xlim(0,ecLim)
boxplot(data.zju.energy.cleaned[total_elec!=0]$total_elec,outline = FALSE)
# �ٶԴ���2�ķ���һ�£���
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

####�ϲ��Ȼ�������ı�####
data.zju.combine.raw<-merge(x=data.zju.energy.room.raw[,c("baseLabel","datetime","roomCode","total_elec","modiElec")],
                            y=data.zju.env.raw[,c("baseLabel","temp","humidity","modiTemp")],
                            all.x = TRUE,by.x ="baseLabel",by.y ="baseLabel")


