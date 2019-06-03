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
save(data.weather.station.raw,file="HZ_气象站数据.rdata")


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



