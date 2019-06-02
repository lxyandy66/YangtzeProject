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
