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
ggplot(data=data.raw.final[DataSource=="Campus"&eui!=0&eui<3&typeCode%in% c("D")],
         aes(x=month,y=eui,fill=year))+geom_boxplot()+theme_bw()

####对于不同高校分类统计####
data.hashmap.schoolType<-read.xlsx(file = "schoolTypeMapping.xlsx",sheetIndex = 1)
data.raw.final$SchoolName<-matrix(data = unlist(strsplit(data.raw.final$BuildingCode,split = "_")),ncol = 3,byrow = TRUE)[,1]
data.raw.final<-merge(x=data.raw.final,y=data.hashmap.schoolType[,c("SchoolCode","SchoolType")],all.x = TRUE,by.x = "SchoolName",by.y = "SchoolCode")
ggplot(data = data.raw.final[DataSource=="Campus"&eui!=0],aes(x=month,y=eui,color=SchoolType))+
  ylim(0,10)+geom_boxplot()+facet_wrap(~typeCode,nrow = 3)



####建筑信息情况统计####
####公建按20000平方米划分，办公建筑以党政机关划分
data.building.info<-data.raw.final[,.(DataSource=DataSource[1],
                                      SchoolName=SchoolName[1],
                                      area=area[1],
                                      typeCode=typeCode[1]
                                      ),by=BuildingCode]
#按面积分类大型公建
data.building.info$isLarge<- NA#-1表示不适用，此处用于如高校等数据源的建筑
data.building.info[DataSource=="CPLX"&area>=20000]$isLarge<-TRUE
data.building.info[DataSource=="CPLX"&area<20000]$isLarge<-FALSE
data.raw.final<-merge(x=data.raw.final,y=data.building.info[,c("BuildingCode","isLarge")],all.x = TRUE,
                      by.x = "BuildingCode",by.y = "BuildingCode")
data.raw.final$isLarge<-as.factor(data.raw.final$isLarge)
ggplot(data = data.raw.final[DataSource=="CPLX"&eui!=0],aes(x=month,y=eui,color=isLarge))+
  ylim(0,10)+geom_boxplot()+facet_wrap(~typeCode,nrow = 3)

###根据面积统计公建能耗情况
data.raw.final$labelTypeLargeMonth<-paste(data.raw.final$typeCode,data.raw.final$isLarge,data.raw.final$month,sep = ".")
nn<-boxplot(data=data.raw.final[DataSource=="CPLX"&eui>0],eui~typeCode+isLarge+month,outline=FALSE)
stat.cplx.box<-data.table(nn$stats)
names(stat.cplx.box)<-as.character(nn$names)
stat.cplx.box$name<-c("lowerWhisker","lowerHinge","median","upperHinge","upperWhisker")
stat.cplx.box<-melt(data=stat.cplx.box,id.vars = "name")
stat.cplx.box$typeCode<-substr(stat.cplx.box$variable,1,1)
stat.cplx.box.final<-stat.cplx.box[,.(
  typeCode=typeCode[1],
  sd=sd(data.raw.final[eui!=0&labelTypeLargeMonth==variable]$eui,na.rm = TRUE),
  mean=mean(data.raw.final[eui!=0&labelTypeLargeMonth==variable]$eui,na.rm = TRUE),
  #切记统计的是原始数据的平均值方差不是箱形图的！！！
  lowerWhisker=value[name=="lowerWhisker"],
  lowerHinge=value[name=="lowerHinge"],
  median=value[name=="median"],
  upperHinge=value[name=="upperHinge"],
  upperWhisker=value[name=="upperWhisker"]
),by=variable]
#提取月份
stat.cplx.box.final$variable<-as.character(stat.cplx.box.final$variable)
stat.cplx.box.final$month<-matrix(data=unlist(strsplit(stat.cplx.box.final$variable,"\\.")),ncol = 3,byrow = TRUE)[,3]
write.xlsx(x=stat.cplx.box.final,file="EUI_Monthly_CPLX_withDetail.xlsx")

#按党政机关分类大型公建办公
data.hashmap.party<-as.data.table(read.xlsx("partyBuildingHashMap.xlsx",sheetIndex = 1))
data.raw.final$isParty<-NA
data.raw.final[typeCode=="M"]$isParty<-FALSE
data.raw.final[BuildingCode %in% data.hashmap.party$BuildingCode]$isParty<-TRUE
ggplot(data = data.raw.final[typeCode=="M"&eui!=0],aes(x=month,y=eui))+geom_boxplot(aes(color=isParty))+ylim(0,10)
ggplot(data = data.raw.final[typeCode=="M"&eui!=0],aes(x=month,y=eui))+geom_boxplot(aes(color=isLarge))+ylim(0,10)

data.raw.final$labelTypeDetail<-NA
data.raw.final[DataSource=="CPLX"]$labelTypeDetail<-
  paste(data.raw.final[DataSource=="CPLX"]$typeCode,data.raw.final[DataSource=="CPLX"]$isLarge,sep = "_")
data.raw.final[DataSource=="Campus"]$labelTypeDetail<-
  paste(data.raw.final[DataSource=="Campus"]$typeCode,data.raw.final[DataSource=="Campus"]$SchoolType,sep = "_")

stat.building.summary<-data.raw.final[,.(typeCode=typeCode[1],
                                        count=length(unique(BuildingCode)),
                                        meanArea=mean(area,na.rm = TRUE),
                                        maxArea=max(area,na.rm = TRUE),
                                        minArea=min(area,na.rm = TRUE)
                                                     ),by=labelTypeDetail]
write.xlsx(x=stat.building.summary,file = "All_BuildingSummary.xlsx")

####全年能耗情况统计####
#按年统计能耗情况
data.raw.final$labelBuildingYearDetail<-paste(data.raw.final$BuildingCode,data.raw.final$year,data.raw.final$labelTypeDetail,sep = "_")
data.building.selection<-data.raw.final[eui>0,.(count=length(BuildingCode)),by=labelBuildingYearDetail]
data.raw.pickFullYear<-data.raw.final[labelBuildingYearDetail %in% unique(data.building.selection[count==12]$labelBuildingYearDetail)]
data.all.annualSum<-data.raw.pickFullYear[eui!=0,.(BuildingCode=BuildingCode[1],
                                                   DataSource=DataSource[1],
                                                   labelTypeDetail=labelTypeDetail[1],
                                                   typeCode=typeCode[1],
                                                   year=year[1],
                                                   annualEnergy=sum(value,na.rm = TRUE),
                                                   area=area[1]),by=labelBuildingYearDetail]
data.all.annualSum$annualEUI<-data.all.annualSum$annualEnergy/data.all.annualSum$area
write.xlsx(x = data.all.annualSum,file = "EUI_Annual_BuildingWithDetail.xlsx")#原始数据，各栋情况
#箱形图统计，整体分布情况
nn<-boxplot(data=data.all.annualSum[DataSource=="CPLX"],annualEUI~labelTypeDetail,outline=FALSE,xlab="Type Code",ylab="Annual EUI (kWh/ m^2)")
stat.cplx.annual.box<-data.table(nn$stats)
names(stat.cplx.annual.box)<-as.character(nn$names)
stat.cplx.annual.box$name<-c("lowerWhisker","lowerHinge","median","upperHinge","upperWhisker")
stat.cplx.annual.box<-melt(data=stat.cplx.annual.box,id.vars = "name")
stat.cplx.annual.box$typeCode<-substr(stat.cplx.annual.box$variable,1,1)
stat.cplx.annual.box.final<-stat.cplx.annual.box[,.(
  typeCode=typeCode[1],
  count=length(unique(data.all.annualSum[labelTypeDetail==variable]$BuildingCode)),
  sd=sd(data.all.annualSum[labelTypeDetail==variable]$annualEUI,na.rm = TRUE),
  mean=mean(data.all.annualSum[labelTypeDetail==variable]$annualEUI,na.rm = TRUE),
  #切记统计的是原始数据的平均值方差不是箱形图的！！！
  lowerWhisker=value[name=="lowerWhisker"],
  lowerHinge=value[name=="lowerHinge"],
  median=value[name=="median"],
  upperHinge=value[name=="upperHinge"],
  upperWhisker=value[name=="upperWhisker"]
),by=variable]
write.xlsx(x = stat.cplx.annual.box.final,file = "EUI_Annual_AllTypeWithDetail.xlsx")
#


####逐月箱形图统计####
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

####逐月箱形图输出####
par(mfrow = c(2, 2))
for(i in LETTERS[c(1:20)]){
  boxplot(data = data.raw.final[typeCode==i & eui!=0],eui~month,outline=FALSE,xlab="Month",ylab="EUI (kWh/ m^2)")
  title(paste("Type:",i," n=",nrow(data.raw.final[typeCode==i & eui!=0])))
}
ggplot(data=data.raw.final[typeCode==""])

####简单统计量####
#获取全年箱形图统计参数
nn<-boxplot(data = data.raw.final[eui!=0],eui~typeCode,outline=FALSE)
stat.all.box.raw<-data.table(nn$stats)
names(stat.all.box.raw)<-as.character(nn$names)
stat.all.box.raw$name<-c("lowerWhisker","lowerHinge","median","upperHinge","upperWhisker")
stat.all.box.final<-melt(data=stat.all.box.raw,id.vars = "name")

#统计全年方差均值等
stat.all.simple<-data.table(typeCode=unique(data.raw.final$typeCode))
stat.all.simple$std<--1
stat.all.simple$mean<--1
for(i in unique(stat.all.simple$typeCode)){
  stat.all.simple[typeCode==i]$std<-sd(data.raw.final[typeCode==i&eui>0]$eui)
  stat.all.simple[typeCode==i]$mean<-mean(data.raw.final[typeCode==i&eui>0]$eui)
}
# eui<stat.all.box.final[name=="upperWhisker"&variable==i]$value &
#未清洗
write.xlsx(x=stat.all.simple,file = "RAW_SimpleStat.xlsx")
