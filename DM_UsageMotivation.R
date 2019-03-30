####本脚本用于分析教学建筑空调使用的驱动因素####
# 根据部分教室的空调电量情况，结合课表、气象条件分析空调使用的驱动因素

####读取校历信息####
data.raw.calendar<-read.xlsx(file = "2016_Spring_Summer_Semester.xlsx",sheetIndex = 1)
data.raw.calendar<-rbind(data.raw.calendar,read.xlsx(file = "2017_Spring_Summer_Semester.xlsx",sheetIndex = 1))
data.raw.calendar<-as.data.table(rbind(data.raw.calendar,read.xlsx(file = "2017_Autumn_Winter_Semester.xlsx",sheetIndex = 1)))
data.raw.calendar$isWeekend<-isWeekend(data.raw.calendar$date)

