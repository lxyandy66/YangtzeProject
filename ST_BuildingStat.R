#���ű����ڽ������뼰���ܵĶ�Ӧ�����������ļ�ͳ��

####��ȡӳ���ļ�####
data.mapping.buildingType<-as.data.table(read.csv(file = "HZNU_�������ܼ������Ӧ.csv"))
data.mapping.buildingType<-data.mapping.buildingType[,c("buildingCode","type")]


##�����
# ��ĩ��1492̨����ѧ����ĩ��709̨
# �ܷ�����1162�䣬��ѧ����������385��

####������Ϊ/�ܺ�/�Ȼ������ݼ��н�ѧ������������####
#��Ϊ���
data.hznu.use.room.day$buildingCode<-substr(data.hznu.use.room.day$roomCode,1,10)
data.hznu.teaching.use<-data.hznu.use.room.day[buildingCode %in% 
                                  data.mapping.buildingType[type=="teaching"]$buildingCode]
#�ܺĲ��
data.hznu.energy.room.day$buildingCode<-substr(data.hznu.energy.room.day$roomCode,1,10)
data.hznu.teaching.energy<-data.hznu.energy.room.day[buildingCode %in% 
                                                 data.mapping.buildingType[type=="teaching"]$buildingCode]


####��ͳ��####
#����Ҫʱ