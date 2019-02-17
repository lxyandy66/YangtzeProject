library(data.table)
library(reshape2)
library(ggplot2)
library(xlsx)

data.raw.campus<-data.table(read.csv(file = "“CPLX_已处理_Campus”的副本.csv",encoding = "GBK"))
data.raw.complex<-data.table(read.csv(file="CPLX_已处理_二期_excel.csv"))
data.raw.complex$DataSource<-"CPLX"

data.raw.pick<-data.raw.campus
data.raw.pick<-rbind(data.raw.pick,data.raw.complex)
data.raw.pick<-data.table(DataSource=data.raw.pick$DataSource,
                             name=data.raw.pick$建筑名称,
                             type=data.raw.pick$建筑类型,
                             area=data.raw.pick$建筑面积,
                             data.raw.pick[,c(23:34,36:47,49:60)])
data.raw.pick$name<-as.character(data.raw.pick$name)
data.raw.pick$BuildingCode<-""
data.raw.pick[DataSource=="Campus"]$BuildingCode<-matrix(unlist((strsplit(data.raw.pick[DataSource=="Campus"]$name,"_"))),byrow = TRUE,ncol = 2)[,1]
data.raw.pick[BuildingCode==""]$BuildingCode<-"CPLX"

#转换为建筑代码形式，便于跨平台 
data.hashmap.type<-data.raw.pick[,.(DataSource=unique(DataSource),
                                    type=unique(type)
                                    ),by=paste(DataSource,type,sep = "_")]
data.hashmap.type$typeCode<-LETTERS[1:length(data.hashmap.type$type)]
data.hashmap.building<-data.raw.pick[,.(DataSource=DataSource,
                                        name=name,
                                        type=type,
                                        no=sprintf("%03d",c(1:length(unique(name))))
                                        ),by=BuildingCode]
data.hashmap.building$paste<-paste(data.hashmap.building$DataSource,data.hashmap.building$type,sep = "_")
data.hashmap.building<-merge(x=data.hashmap.building,y=data.hashmap.type[,c("paste","typeCode")],all.x = TRUE,
     by = "paste")
data.hashmap.building$BuildingCode<-paste(data.hashmap.building$BuildingCode,data.hashmap.building$typeCode,data.hashmap.building$no,sep = "_")
#组合编码
data.raw.pick$BuildingCode<-NULL
data.raw.pick<-merge(x=data.raw.pick,y=data.hashmap.building[,c("name","BuildingCode","typeCode")],all.x = TRUE,by.x="name",by.y = "name")
#重构数据
data.raw.final<-melt(data = data.raw.pick[,c(2,4:42)],id.vars = c("DataSource","BuildingCode","typeCode","area"))
names(data.raw.final)[5]<-"time"
data.raw.final$time<-substr(data.raw.final$time,2,8)
data.raw.final$year<-substr(data.raw.final$time,1,4)
data.raw.final$month<-substr(data.raw.final$time,6,7)
data.raw.final$eui<-data.raw.final$value/data.raw.final$area
boxplot(data=data.raw.final[DataSource=="Campus"],eui~month+year,outline=TRUE)
ggplot(data=data.raw.final[DataSource=="Campus"&eui!=0&eui<3&typeCode%in% c("A","D","I")],
         aes(x=month,y=eui,fill=typeCode))+geom_boxplot()+theme_bw()

####箱形图统计####
nn<-boxplot(data = data.raw.final[eui!=0&typeCode=="D"],eui~month+typeCode,outline=FALSE)
stat.raw<-data.table(nn$stats)
names(stat.raw)<-as.character(nn$names)
stat.raw$name<-c("lowerWhisker","lowerHinge","median","upperHinge","upperWhisker")
stat.monthly.final<-melt(data=stat.raw,id.vars = "name")
stat.monthly.final$variable<-as.character(stat.monthly.final$variable)
nn<-matrix(unlist(strsplit(stat.monthly.final$variable,"\\.")),ncol = 2,byrow = TRUE)
stat.monthly.final$month<-nn[,1]
stat.monthly.final$typeCode<-nn[,2]
stat.monthly.final$variable<-NULL
write.xlsx(x=stat.monthly.final,file="RAW_MonthlyStat.xlsx")

####箱形图输出####
par(mfrow = c(2, 2))
for(i in LETTERS[c(1:20)]){
  boxplot(data = data.raw.final[typeCode==i & eui!=0],eui~month,outline=FALSE,xlab="Month",ylab="EUI (kWh/ m^2)")
  title(paste("Type:",i," n=",nrow(data.raw.final[typeCode==i & eui!=0])))
}
