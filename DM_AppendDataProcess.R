####���ű�����HZNU׷�����ݵĴ���####

####�����ݿ��л�ȡ����####
#MySQL�б���Ϊutf8��ODBC�ӿ���Ҫ������ͬ������Ϊutf8���������
connect<-odbcConnect("db_hznu",uid = "root",pwd = "",DBMSencoding="utf8")
data.append.raw<-as.data.table(sqlFetch(connect,"hznu_append_cn"))

####ͳһ״ֵ̬####

####ͳһon_off
#> unique(data.append.raw$on_off)
#[1] <NA> �ػ� ���� ֹͣ ���� ���� �ͷ� �Զ�
#Levels: �ػ� ���� �ͷ� ֹͣ ���� ���� �Զ�
data.append.raw[innerEnergy<=0]$on_off<-"0"
data.append.raw[innerEnergy<=0]$status<-"off"
data.append.raw[on_off=="�ػ�"|on_off=="ֹͣ"]$on_off<-"0"
data.append.raw[on_off=="�ػ�"|on_off=="ֹͣ"]$status<-"off"

####ͳһstatus
unique(data.append.raw$status)
data.append.raw[status=="ֹͣ"]$on_off<-"0"
data.append.raw[status=="ֹͣ"]$status<-"off"
data.append.raw[status=="��ʪ"|status=="��ʪ"]$status<-"dehumi"
data.append.raw[status=="�ͷ�"|status=="ͨ��"]$status<-"venti"
data.append.raw[status=="����"]$status<-"cooling"
data.append.raw[status=="����"]$status<-"heating"
data.append.raw[status=="�Զ�"]$status<-"auto"

####ͳһ����ܺ�
#> table(data.append.raw$fanSpeed)
#1        2        3        4        5   �ͷ絵   �߷絵       ��   �з絵 �Զ��絵     <NA> 
# 1389     3831     3831    11381   436714  7301820  9702561   686819  2559105 10513503  4149548 
data.append.raw[on_off=="0"]$fanSpeed<-"off"
data.append.raw[fanSpeed=="1"]$fanSpeed<-"off"
data.append.raw[fanSpeed=="2"|fanSpeed=="�ͷ絵"|fanSpeed=="��"]$fanSpeed<-"low"
data.append.raw[fanSpeed=="3"|fanSpeed=="�з絵"]$fanSpeed<-"mid"
data.append.raw[fanSpeed=="4"|fanSpeed=="�߷絵"]$fanSpeed<-"high"
data.append.raw[fanSpeed=="5"|fanSpeed=="�Զ��絵"]$fanSpeed<-"auto"

####�ܺ�����
#��մ����ܺ�
nn<-data.append.raw[status=="off"&on_off!="0"]
data.append.raw[on_off=="0"&innerEnergy!=0]$roomEnergy<-
  data.append.raw[on_off=="0"&innerEnergy!=0]$roomEnergy-
  data.append.raw[on_off=="0"&innerEnergy!=0]$innerEnergy
data.append.raw[on_off=="0"]$innerEnergy<-0

#�ܺ�Ϊ0״̬��0
data.append.raw[innerEnergy<=0]$on_off<-"0"
data.append.raw[innerEnergy<=0]$status<-"off"

####��������Ӧ####
data.mapping.roomCode.old<-as.data.table(read.csv(file = "��ʦ�����_��������-��ѧ.csv"))
names(data.mapping.roomCode.old)<-c("roomName","roomCode","roomType")
data.mapping.roomCode.old$roomType<-NULL

data.mapping.roomCode.append<-read.csv(file = "HZNU_AppendRoomName.csv",encoding = "gbk")
data.mapping.roomCode.all<-as.data.table(rbind(data.mapping.roomCode.old,data.mapping.roomCode.append[,c("roomName","roomCode")]))

data.append.raw<-merge(data.append.raw,data.mapping.roomCode.all,all.x = TRUE,by.x = "roomName",by.y = "roomName")

####�յ������Ӧ####
data.mapping.acCode<-data.append.raw[,.(roomName=unique(roomName),
                                        acName=unique(innerName),
                                        acAppendCode=c(1:length(unique(innerName)))
                                        ),by=roomCode]
data.mapping.acCode$acCode<-paste(data.mapping.acCode$roomCode,"m",sprintf("%02d",data.mapping.acCode$acAppendCode),sep = "")
data.mapping.acCode$acFullName<-paste(data.mapping.acCode$roomName,data.mapping.acCode$acName,sep = "_")
data.append.raw$acFullName<-paste(data.append.raw$roomName,data.append.raw$innerName,sep = "_")
data.append.raw<-merge(x=data.append.raw,y=data.mapping.acCode[,c("acFullName","acCode")],all.x = TRUE,by.x = "acFullName",by.y = "acFullName")

####���׷�����ݳ������������ݵ���####
data.append.final<-data.append.raw[,c("datetime","roomCode","acCode","innerTemp","setTemp","status","on_off","fanSpeed","innerEnergy","roomEnergy")]
data.append.final$status<-as.character(data.append.final$status)
data.append.final$on_off<-as.character(data.append.final$on_off)


####ͳһ�����ݺ;����ݱ��������������ݳ������ݼ�####
data.raw.complete<-data.table(time=data.append.final$datetime,ac_code=data.append.final$acCode,total_elec=data.append.final$innerEnergy,
                              real_temp=data.append.final$innerTemp,set_temp=data.append.final$setTemp,state=data.append.final$status)

data.raw.complete$time<-as.character(data.raw.complete$time)
data.all$time<-as.character(data.all$time)
data.all$state<-as.character(data.all$state)
data.all$ac_code<-as.character(data.all$ac_code)
data.raw.complete<-rbind(data.raw.complete,data.all[,c("time","ac_code","total_elec","real_temp","set_temp","state")])
