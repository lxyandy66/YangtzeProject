library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(rgl)

## 加载数据
load("thermoAnalyse.rdata")

#HZNU数据
load("2018-6-6.RData")#HZNU用电
rm(data.yx)
data.running <-
  as.data.table(read.table(
    "/Volumes/Windows 10/R分析_Demo/runningData.csv",
    header = TRUE,
    sep = ","
  ))#HZNU环境



data.zju.consumption <-
  read.csv("/Users/Mr_Li/Desktop/新增_空调用电数据_玉1.csv")
data.zju.consumption <-
  rbind(data.zju.consumption,
        read.csv("/Users/Mr_Li/Desktop/新增_空调用电数据_玉2.csv"))

data.zju.consumption <- as.data.table(data.zju.consumption)

data.running <- as.data.table(data.running)

#不相关变量删除
data.zju.consumption <-
  data.zju.consumption[, c("begin", "end", "ac_code", "total_elec")]#仅保留开始、结束、空调编码、空调总能耗
data.yd <-
  data.yd[, c("ac_code", "time", "total_elec")]#HZNU用电数据仅保留空调编码、时间、用电量

#数据格式等转换
data.running$date <-
  as.POSIXct(data.running$time, format = "%Y-%m-%d %H:%M:%S")
data.yd$time <-
  as.POSIXct(data.yd$time, format = "%Y-%m-%d %H:%M:%S")
data.zju.consumption$begin <-
  as.POSIXct(data.zju.consumption$begin, format = "%Y-%m-%d %H:%M:%S")
data.zju.consumption$end <-
  as.POSIXct(data.zju.consumption$end, format = "%Y-%m-%d %H:%M:%S")
data.zju.thermo$date_time <-
  as.POSIXct(data.zju.thermo$date_time, format = "%Y-%m-%d %H:%M:%S")

#时间对象分割
data.running$year <- format(data.running$date, format = "%y")
data.running$month <- format(data.running$date, format = "%m")
data.running$day <- format(data.running$date, format = "%d")

data.running$hour <- format(data.running$date, format = "%H")
data.running$minute <- format(data.running$date, format = "%M")


data.running[on_off == "开机"]$on_off = "1"
data.running[on_off == "关机"]$on_off = "0"
data.running$room_code <- substr(data.running$ac_code, 1, 13)
setorder(data.zju.thermo, date_time, room_code)
#将数据化为小时级
data.zju.thermo.hourly <-
  data.zju.thermo[, .(
    room_code = unique(room_code),
    date_time = date_time[1],
    temp = mean(temp),
    humidity = mean(humidity),
    year = unique(year),
    month = unique(month),
    day = unique(day),
    hour = unique(hour)
  ), by = paste(room_code, year, month, day, hour, sep = "-")]
#将小时级数
data.zju.thermo.hourly.plot <- data.zju.thermo.hourly[]


#数据清洗，头疼
data.zju.thermo <- data.zju.thermo[!duplicated(data.zju.thermo),]
data.running <- data.running[!duplicated(data.running),]
#length(data.zju.thermo[temp<=0&humidity<10]$temp)  #565881个数据119537条异常的
data.zju.thermo$labelErr <-
  data.zju.thermo <- data.zju.thermo[temp > 2,]#室内温度低于2度认为异常
data.zju.errMark <- data.zju.thermo[month >= 10 & temp > 30]
data.err <-
  data.zju.thermo[room_code == "330100D284305"  &
                    month == "12"]
# data.plot<-melt(data.err,id.vars = "hour")
setorder(data.err, room_code, date_time)
# data.err.plot<-data.err[,.(room_code=unique(room_code),temp[]),by=paste(room_code,day,sep = "-")]
data.err$temp <- as.numeric(data.err$temp)
data.err$hour <- as.numeric(data.err$hour)
ggplot(data.err, aes(x = factor(hour), y = temp), group = data.err$day)  +
  ylim(10, 45) + geom_boxplot() + geom_point(size = 2, position = "jitter", aes(color =
                                                                                  day))

setorder(data.err, date_time)
data.err <-
  data.err[, .(
    room_code = unique(room_code),
    day = unique(day),
    hour = unique(hour),
    temp = mean(temp)
  ), by = paste(day, hour, sep = "-")]
#######还没完

#加标签
data.zju.thermo$labelHalfHour <-
  paste(
    data.zju.thermo$room_code,
    data.zju.thermo$year,
    data.zju.thermo$month,
    data.zju.thermo$day,
    data.zju.thermo$hour,
    data.zju.thermo$halfHour,
    sep = "-"
  )#这个标签用于将各不同时间段的记录统一到半小时
setorder(data.zju.thermo, room_code, date_time)

data.zju.thermo.halfHour <-
  data.zju.thermo[, .(room_code = unique(room_code)), by =
                    label]


##HZNU数据规范化
# unique(data.running$on_off)# "开机"/"关机"
# unique(data.running$run_mode)# 关机 制冷 其他 除湿 送风 制热 自动
# unique(data.running$wind) #低 高 关机 其他 中 自动




##将房间和日期作为一个标签
data.running$label <-
  paste(data.running$roomCode, data.running$date, sep = "-")#注意date是什么
