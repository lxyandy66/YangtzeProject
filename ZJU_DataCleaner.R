##浙大室内环境数据清理脚本
library(data.table)

##浙大数据导入
data.zju.thermo <-
  read.csv("/Users/Mr_Li/Desktop/房间环境参数-仪器记录-教学_西2.csv")
data.zju.thermo <-
  rbind(data.zju.thermo,
        read.csv("/Users/Mr_Li/Desktop/房间环境参数-仪器记录-教学_西.csv"))
data.zju.thermo <-
  rbind(data.zju.thermo,
        read.csv("/Users/Mr_Li/Desktop/房间环境参数-仪器记录-教学_玉1.csv"))
data.zju.thermo <-
  rbind(data.zju.thermo,
        read.csv("/Users/Mr_Li/Desktop/房间环境参数-仪器记录-教学_玉2.csv"))
data.zju.thermo <- as.data.table(data.zju.thermo)

##不相关变量删除
data.zju.thermo <-
  data.zju.thermo[, c("date_time", "room_code", "temp", "humidity")]#仅保留时间、房间编码、温度、湿度

##格式转换及时间变量分割
##分割各时间变量
data.zju.thermo$date_time <-
  as.POSIXct(data.zju.thermo$date_time, format = "%Y-%m-%d %H:%M:%S")
data.zju.thermo$year <-
  format(data.zju.thermo$date_time, format = "%y")
data.zju.thermo$month <-
  format(data.zju.thermo$date_time, format = "%m")
data.zju.thermo$day <-
  format(data.zju.thermo$date_time, format = "%d")
sagbdata.zju.thermo$hour <-
  format(data.zju.thermo$date_time, format = "%H")
data.zju.thermo$minute <-
  format(data.zju.thermo$date_time, format = "%M")

data.zju.thermo$minute <- as.numeric(data.zju.thermo$minute)
data.zju.thermo$halfHour <- "00"
data.zju.thermo[minute >= 30]$halfHour <- 30

######可以从这里开始
load("thermoAnalyse.rdata")

##过滤数据
##目前已知有问题的数据
# 见手稿，过滤部分夜间温度过高的数据，行为异常而产生，影响聚类结果
# 无法调整，直接删除
data.zju.thermo$deleteFlag <- 0
data.zju.thermo[room_code == "330100D016405" &
                  month == "12" &
                  (day == "11" | day == "17")]$deleteFlag <- 1
data.zju.thermo[room_code == "330100D284305" &
                  month == "02"]$deleteFlag <- 1
data.zju.thermo[room_code == "330100D284305" &
                  month == "12" & day == "20"]$deleteFlag <- 1
data.zju.thermo[room_code == "330100D284305" &
                  month == "01" &
                  (day == "04" | day == "05" |
                     day == "28" |
                     day == "29" | day == "31")]$deleteFlag <- 1
data.zju.thermo <- data.zju.thermo[deleteFlag == 0]

##mice法进行插补
#多重插补法处理缺失
# library(lattice) #调入函数包
# library(MASS)
# library(nnet)
# library(mice) #前三个包是mice的基础
#效果不好

##平均值偏移对数据进行修正
data.zju.thermo$outerFlag <- "0"
data.zju.thermo$modiTemp<-data.zju.thermo$temp
#考虑开机情况修正
for (i in unique(data.zju.thermo$room_code)) {
  for (j in unique(data.zju.thermo[room_code == i]$month)) {
    #逐房间逐月进行计算
    data.modify <- data.zju.thermo[room_code == i & month == j]
    data.zju.thermo[room_code == i & month == j]$deleteFlag <- 1
    for (k in sprintf("%02d",0:23)) {
      #逐小时计算
      boxStats <- boxplot.stats(data.modify[hour == k]$temp)
      originTarget <- boxStats$stats[4]#用箱型图的箱上界作为偏移量的基准
      data.modify[temp %in% boxStats$out &
                    temp > originTarget]$outerFlag <-
        "1"
      data.modify.outer <- data.modify[hour == k & outerFlag == "1"]
      outerMean <- mean(data.modify.outer$temp)
      # if (i == 8) {
      #   print(c(outerMean - originTarget, outerMean - mean(data.err[hour == i]$temp)))
      # }#对于8点两者效果是 8.853846 8.959957
      data.modify.outer$temp <-
        data.modify.outer$temp - (outerMean - originTarget)
      data.modify.outer$outerFlag <- "0"
      data.modify <-
        rbind(data.modify[outerFlag == "0"], data.modify.outer)
      data.zju.thermo <-
        rbind(data.zju.thermo[deleteFlag != 1], data.modify)
    }
  }
}#有问题,40多万条数据跑出来200多万...产生有大量重复的数据

##1-12月逐月图形输出
for (i in c(1:12)) {
  thisMonth <- str_pad(i, 2, side = "left", pad = "0")
  ggplot(data.zju.thermo[month == thisMonth], aes(x = factor(hour), y = temp))  +
    ylim(5, 35) + geom_boxplot() + geom_point(size = 3.5, position = "jitter", aes(color =
                                                                                     room_code))
  ggsave(
    filename = paste(thisMonth, "_temp_modify", ".png", sep = ""),
    width = 25,
    height = 16,
    dpi = 100
  )
}

# ##备份一个
# data.zju.thermo.rawProcess<-data.zju.thermo
# save(data.zju.thermo.rawProcess,file = "浙大室内热环境数据_已清洗修正.rdata")

##将数据化为半小时级

data.zju.thermo$label <-
  paste(
    data.zju.thermo$room_code,
    data.zju.thermo$year,
    data.zju.thermo$month,
    data.zju.thermo$day,
    data.zju.thermo$hour,
    data.zju.thermo$halfHour,
    sep = "-"
  )#标签为：房间号-年-月-日-小时-半小时

setorder(data.zju.thermo, room_code, date_time)
data.zju.thermo.halfHourly <-
  data.zju.thermo[, .(
    room_code = unique(room_code),
    date_time = date_time[1],
    temp = mean(temp),
    humidity = mean(humidity),
    year = unique(year),
    month = unique(month),
    day = unique(day),
    hour = unique(hour),
    halfHour = unique(halfHour)
  ), by =label]
ggplot(data.zju.thermo.halfHourly[month == ""], aes(x = factor(hour), y = temp))  +
  ylim(5, 35) + geom_boxplot() + geom_point(size = 3.5, position = "jitter", aes(color =
                                                                                   room_code))

#大概...原始数据就差不多了？
#return 0  ##just for fun