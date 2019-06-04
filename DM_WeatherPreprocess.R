####接气象站数据####
fileName<-c("日期2016-10-27???2016-10-31.xls",
            "日期2016-11-01???2016-11-04.xls",
            "日期2016-11-04???2016-11-16.xls",
            "日期2016-11-16???2016-11-30.xls",
            "日期2016-11-30.xls",
            "日期2017-04-01???2017-04-05.xls",
            "日期2017-04-05???2017-04-30.xls",
            "日期2017-05-01???2017-05-05.xls",
            "日期2017-05-05???2017-05-15.xls",
            "日期2017-05-15???2017-05-31.xls",
            "日期2017-06-01???2017-06-07.xls",
            "日期2017-06-07???2017-06-28.xls",
            "日期2017-09-01???2017-09-11.xls",
            "日期2017-09-11???2017-09-30.xls",
            "日期2017-10-01???2017-10-18.xls")
data.weather.raw<-read.xlsx2(sheetIndex = 1,colClasses=c("Date","character"),
                             file = i)
data.weather.raw<-data.weather.raw[0,]
for(i in paste("数据-气象站数据\\",fileName,sep="")){
  temp.weather.raw<-data.table(
    read.xlsx2(sheetIndex = 1,colClasses=c("Date","character"),file = i))
  tryCatch({
    temp.weather.raw$naSum<-apply(temp.weather.raw, MARGIN = 1, 
                                  FUN = function(x) sum(as.character(x)=="",na.rm = TRUE))
    data.weather.raw<-rbind(data.weather.raw,temp.weather.raw[naSum<5,1:23])
  },error=function(e){cat("file = ",i,e)} )
}
#合并后日期与时间处理正常

####中文变量替换####
names(data.weather.raw)<-c( "date","time","temp1","temp2","temp3","temp4","temp5",
                            "outTemp","rhOut","dewTemp","pressure","lat","windDir","WindSpeed",
                            "meanWind2Min","meanWind10Min","rainSum","light","curRad1","sumRad1","curRad2",  
                            "sumRad2","sunTime")
#除去一些变量
data.weather.raw[,c("temp1","temp2","temp3","temp4","temp5")]<-NULL
data.weather.station.raw<-data.weather.raw
#暂存
# save(data.weather.station.raw,file="HZ_气象站数据.rdata")
data.weather.station.raw[,c("light","lat","rainSum","pressure")]<-NULL
#变量格式转换
data.weather.station.raw.merge<-data.weather.station.raw
# data.weather.station.raw.merge<-cbind(data.weather.station.raw[,1:2],as.matrix(data.weather.station.raw[,3:14],ncol=length(3:14)))#这个也可以转成char
# data.weather.station.raw.merge<-as.data.table(unfactor(as.data.frame(data.weather.station.raw.merge)))#????用这个包让我宛如一个智障🌚
data.weather.station.raw.merge[,3:14]<-lapply(data.weather.station.raw.merge[,3:14], FUN = function(x){ as.numeric(as.character(x))} )#lapply???怎么可以 apply不行


ggplot(data=melt(data.weather.station.raw.merge[curRad1!=0&curRad2!=0,c("date","time","curRad1","curRad2")],
                 id.vars = c("date","time")),aes(x=value,color=variable))+geom_density()
ggplot(data=data.weather.station.raw.merge[curRad1!=0],aes(x=curRad1))+geom_density()

# > table(data.weather.station.raw.merge$curRad2)#这个数据没什么好要的了🌚
# 0    26    40    63   177   235   256   273   350   410   447   455   488   548   555   606   623   687   901   941   982  1009  1034  1053  1060 
# 1805     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1 
# 1155  1181  1197  1245  1247  1250  1285  1312  1355  1381  1433  1469  1497  1515  1541  1599  1631  1756  1764  1784  1841  1865  1894  1917  1928 
# 1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1     1 
# 2197  2208  2228  2239  2262  2265  2296  2312  2349 
# 1     1     1     1     1     1     1     1   20213 
#再去除变量
data.weather.station.raw.merge$curRad2<-NULL

#### 时间轴整理####
data.weather.station.raw.merge$originDatetime<-as.POSIXct(paste(data.weather.station.raw.merge$date,
                                                          data.weather.station.raw.merge$time,sep = " "))
data.weather.station.raw.merge$datetime<-lapply(X = data.weather.station.raw.merge[,c("originDatetime")],FUN = fixTimeInterval,invl=600)#切记apply要看结果

####合并至小时级别####
setorder(data.weather.station.raw.merge,datetime)
data.weather.station.raw.merge$labelHour<-format(data.weather.station.raw.merge$datetime,format = "%Y-%m-%d_%H")
data.weather.station.final<-data.weather.station.raw.merge[,.(datetime=datetime[1],
                                                              outTemp=mean(outTemp,na.rm = TRUE),
                                                              rhOut=mean(rhOut,na.rm = TRUE),
                                                              dewTemp=mean(dewTemp,na.rm = TRUE),
                                                              windDir=mean(windDir,na.rm = TRUE),
                                                              windSpeed=mean(meanWind10Min,na.rm = TRUE),
                                                              curRad=mean(curRad1,na.rm = TRUE),
                                                              sumRad1=max(sumRad1,na.rm = TRUE),
                                                              sumRad2=max(sumRad2,na.rm = TRUE),
                                                              sunTime=max(sunTime,na.rm = TRUE)
                                                              ),by=labelHour]
data.weather.station.final$labelHour<-NULL
#ZJU气象站最终：data.weather.station.final

####处理萧山机场气象数据集####
#从MySQL导入
conn<-dbConnect(MySQL(),dbname="EnergyData",user="root",password="",host="localhost")
dbListTables(conn)
dbSendQuery(conn,'SET NAMES utf8')
data.weather.airport.raw<-as.data.table(dbReadTable(conn,"WD_AirportWeather"))
dbDisconnect(conn)

data.weather.airport.raw$outTemp<-apply(data.weather.airport.raw[,"TMP"],MARGIN = 1,
                                        FUN = function(x){
                                              return(as.numeric( getSplitMember(x,splitSimbol=","))/10)
                                          })
data.weather.airport.raw$dewTemp<-apply(data.weather.airport.raw[,"DEW"],MARGIN = 1,
                                        FUN = function(x){
                                          return(as.numeric( getSplitMember(x,splitSimbol=","))/10)
                                        })
#其实这个数据集很规整可以直接用substr
data.weather.airport.raw$windDir<-as.numeric(substr(data.weather.airport.raw$WND,1,3))
data.weather.airport.raw$windSpeed<-as.numeric(substr(data.weather.airport.raw$WND,9,12))/10

#天气处理
data.weather.airport.raw$weather1<-apply(data.weather.airport.raw[,"MW1"],MARGIN = 1,
                                         FUN = function(x){
                                           if(is.na(x)) {
                                             return("NULL")
                                           }
                                           x<-as.numeric(substr(x, 1, 2))
                                           ifelse(x <11, "Sunny",
                                              ifelse(x < 20, "Cloudy",
                                                  ifelse(x <30, "Rainy",
                                                      ifelse(x < 36, "Cloudy",
                                                          ifelse(x < 40, "Snowy",
                                                              ifelse(x < 50, "Cloudy",
                                                                  ifelse(x <70, "Rainy",
                                                                      ifelse(x < 80, "Snowy",
                                                                          ifelse(x <93, "Rainy",
                                                                             ifelse(x < 95, "Snowy",
                                                                                  ifelse(x < 100, "Rainy", "ERROR")
                                                                                  )
                                                                             )
                                                                          )
                                                                      )
                                                                  )
                                                              )
                                                          )
                                                      )
                                                  )
                                              )
                                         })
data.weather.airport.raw$weather2<-apply(data.weather.airport.raw[,"AY1"],MARGIN = 1,
                                         FUN = function(x){
                                           if(is.na(x)){
                                             return("NULL")
                                           }
                                           x<-as.numeric(substr(x,1,1))
                                           ifelse(x==0,"Sunny",
                                                  ifelse(x<=4,"Cloudy",
                                                         ifelse(x<=6,"Rainy",
                                                                ifelse(x==7,"Snowy",
                                                                       ifelse(x<=9,"Rainy","ERROR")
                                                                       )
                                                                )
                                                         )
                                                  )
                                         }
                                         )
####计算相对湿度####
# 实际温度对应的饱和水蒸气分压力（即该温度为露点温度时的水蒸气分压力）
# 露点温度对应当前实际的水蒸气分压力
data.weather.airport.raw$realSatWaterVp<-getWaterVp(data.weather.airport.raw$outTemp)
data.weather.airport.raw$realWaterVp<-getWaterVp(data.weather.airport.raw$dewTemp)
data.weather.airport.raw$rhOut<-data.weather.airport.raw$realWaterVp/data.weather.airport.raw$realSatWaterVp

####时间轴及重复数据处理####
#时区处理
data.weather.airport.raw$datetime<-as.POSIXct(data.weather.airport.raw$DATE,format="%Y-%m-%dT%H:%M:%S")+28800
#REPORT_TYPE处理
#同时存在FM-15 FM-12格式
# FM-12 = SYNOP Report of surface observation form a fixed land station
# FM-12每3小时采集一次数据
# FM-15 = METAR(Meteorological Routine Weather Report) Aviation routine weather report

#FM-12复制成每小时采样
nn<-data.weather.airport.raw[REPORT_TYPE=="FM-12"]
nn$datetime<-nn$datetime+3600
nn1<-nn
nn1$datetime<-nn1$datetime+3600
nn2<-nn1
nn2$datetime<-nn2$datetime+3600
data.weather.airport.raw.merge<-rbind(data.weather.airport.raw,nn,nn1)
# nn<-data.weather.airport.raw.merge[REPORT_TYPE=="FM-12"]
# rm(data.weather.airport.raw.merge)

####处理完成，数据合并及导出####
data.weather.airport.raw.merge<-
  data.weather.airport.raw.merge[,c("datetime","DATE","REPORT_TYPE","outTemp","dewTemp","rhOut","windDir","windSpeed","weather1","weather2")]
#缺失值的设置
data.weather.airport.raw.merge[windSpeed==999.9]$windSpeed<-NA
data.weather.airport.raw.merge[weather1=="NULL"]$weather1<-NA
data.weather.airport.raw.merge[weather2=="NULL"]$weather2<-NA
data.weather.airport.raw.merge[outTemp==999.9]$outTemp<-NA
data.weather.airport.raw.merge[dewTemp==999.9]$dewTemp<-NA
data.weather.airport.raw.merge[is.na(outTemp)]$rhOut<-NA
#合并至小时值
data.weather.airport.raw.merge$labelHour<-format(data.weather.airport.raw.merge$datetime,format = "%Y-%m-%d_%H")
setorder(data.weather.airport.raw.merge,datetime,REPORT_TYPE)
#统计一下
stat.hour<-data.weather.airport.raw.merge[,.(count=length(datetime)),by=labelHour]
#合并至小时并导出
data.weather.airport.final<-data.weather.airport.raw.merge[,.(
  DATE=DATE[1],
  datetime=datetime[1],
  outTemp=mean(outTemp,na.rm = TRUE),
  dewTemp=mean(dewTemp,na.rm = TRUE),
  rhOut=mean(rhOut,na.rm = TRUE),
  windDir=mean(windDir[windDir!=999],na.rm = TRUE),
  windSpeed=mean(windSpeed,na.rm = TRUE),
  weather=getMode(c(weather1[!is.na(weather1)],weather2[!is.na(weather2)]))
),by=labelHour]
data.weather.airport.final[,c("datetime","outTemp","dewTemp","rhOut","windDir","windSpeed","weather")]
save(data.weather.airport.final,data.weather.airport.raw.merge,file="HZ_2016-2019机场逐时气象.rdata")
#全球气象数据机场最终：data.weather.airport.final


####ZYP大棚辐射数据####
data.weather.zyp.raw<-data.table(read.csv(file = "粗处理_2018太阳辐射（截至6.14）.csv"))
#删去一些变量
data.weather.zyp.raw[,c("temp1","temp2","temp3","temp4","temp5")]<-NULL
data.weather.zyp.raw[,c("rhEarth","obs","obsMean10Min","CO2","compass","windSpeed","WindDir","meanWind2Min","meanWind10Min","lat","pressure","dewTemp")]<-NULL
data.weather.zyp.raw[,c("rhOut","outTemp","rainSum","elec")]<-NULL

####时间轴的处理####
#其部分数据按月存在误差
data.weather.zyp.raw$datetime<-as.POSIXct(data.weather.zyp.raw$X...datetime)#原始居然是一分钟为时间间隔
data.weather.zyp.raw$month<-format(data.weather.zyp.raw$datetime,format = "%m")
data.weather.zyp.raw$rangeRatio<-apply(X=data.weather.zyp.raw[,c("curRad1","curRad2")],MARGIN = 1,FUN = rangeRatio)
ggplot(data=data.weather.zyp.raw[rangeRatio>0.85],aes(x=rangeRatio))+geom_density()

#将异常值设为缺失
ggplot(data = melt(data.weather.zyp.raw[curRad1!=0&curRad2!=0,c("X...datetime","curRad1","curRad2")],
                   id.vars = c("X...datetime")),aes(x=value,color=variable))+geom_density()#+xlim(1000,3000)
data.weather.zyp.raw[curRad1==2500]$curRad1<-NA
data.weather.zyp.raw[curRad2==2205]$curRad2<-NA
data.weather.zyp.raw[curRad2>1000]$curRad2<-NA #大于1000就4个值
# curRad2值相对较好

#辐射数据清洗
data.weather.zyp.raw$curRad<-apply(data.weather.zyp.raw[,c("curRad1","curRad2","rangeRatio")],
                                   MARGIN = 1,FUN = function(x){
                                     if(is.nan(x[3])|is.na(x[3])){
                                       return(NA)
                                     }
                                     if(x[3]>0.9){
                                        return(max(c(x[1],x[2]),na.rm = TRUE))
                                     }
                                     return(mean(c(x[1],x[2]),na.rm = TRUE))
                                  })
ggplot(data.weather.zyp.raw,aes(x=curRad,color=month))+geom_density()
data.weather.zyp.raw[month %in% c("01","02") & curRad>1000]$curRad<-NA#很少，ZYP说是异常

#检查一下数据量
nn<- (data.weather.zyp.raw %>% cbind(.,date=substr(.$labelHour,1,10)))[,.(hourCount=length(unique(labelHour[!is.na(curRad)]))),by=date]
####整合至小时级别####
setorder(data.weather.zyp.raw,datetime)
data.weather.zyp.raw$labelHour<-format(data.weather.zyp.raw$datetime,format = "%Y-%m-%d_%H")
data.weather.zyp.final<-data.weather.zyp.raw[,.(sumRad1=max(sumRad1,na.rm = TRUE),
                                                sumRad2=max(sumRad2,na.rm = TRUE),
                                                curRad=mean(curRad,na.rm = TRUE)
                                                ),by=labelHour]
data.weather.zyp.final$datetime<-as.POSIXct(paste(data.weather.zyp.final$labelHour,":00:00",sep = ""),
                                        format="%Y-%m-%d_%H:%M:%S")
data.weather.zyp.final<-data.weather.zyp.final[,c("datetime","sumRad1","sumRad2","curRad")]
data.weather.zyp.final[is.nan(curRad)]$curRad<-NA
data.weather.zyp.final[is.na(curRad)&format(datetime,format="%H") %in% sprintf("%02d",c(17:23,00:06))]$curRad<-0
#ZYP最终：data.weather.zyp.final

####数据完整度统计####
stat.weather.complete<-data.weather.zyp.final %>% 
                       cbind(date=format(.$datetime,format="%Y-%m-%d"),
                            hour=format(.$datetime,format="%H")) %>% 
                       .[,.(fullCount=length(datetime[!is.na(curRad)]),
                            workHourCount=length(datetime[!is.na(curRad)& hour %in% sprintf("%02d",8:22)]),
                            month=unique(format(datetime,format="%Y-%m")),
                            source="ZYP"),by=date]%>% 
                       rbind(.,
                             data.weather.station.final%>% 
                               cbind(date=format(.$datetime,format="%Y-%m-%d"),
                                     hour=format(.$datetime,format="%H")) %>% 
                               .[,.(fullCount=length(datetime[!is.na(curRad)]),
                                    workHourCount=length(datetime[!is.na(curRad)& hour %in% sprintf("%02d",8:22)]),
                                    month=unique(format(datetime,format="%Y-%m")),
                                    source="STATION"
                                    ),by=date])
write.xlsx(x=stat.weather.complete,file = "Weather_checkComplete.xlsx")
ggplot(data = stat.weather.complete,
       aes(x=date,y=workHourCount,color=source,shape=source,group=source))+geom_point()+geom_line()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
