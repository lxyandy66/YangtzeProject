####���ű����ڷ�����ѧ�����յ�ʹ�õ���������####
# ���ݲ��ֽ��ҵĿյ������������Ͽα����������������յ�ʹ�õ���������

####��ȡУ����Ϣ####
data.raw.calendar<-read.xlsx(file = "2016_Spring_Summer_Semester.xlsx",sheetIndex = 1)
data.raw.calendar<-rbind(data.raw.calendar,read.xlsx(file = "2017_Spring_Summer_Semester.xlsx",sheetIndex = 1))
data.raw.calendar<-as.data.table(rbind(data.raw.calendar,read.xlsx(file = "2017_Autumn_Winter_Semester.xlsx",sheetIndex = 1)))
data.raw.calendar$isWeekend<-isWeekend(data.raw.calendar$date)
