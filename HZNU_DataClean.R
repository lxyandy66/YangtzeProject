library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)


unique(data.running[month == "02"]$ac_code)
# "330100D261022" |变化正常，个别天数行为异常
# "330100D260201"|无变化，温度均约1-2度
# "330100D268323" |变化正常，零散大于32.5度
# "330100D276063"|有变化，变化趋势异常，整体温度偏高，均大于30度

data.running <- data.running[, -1]
data.running <- data.running[!duplicated(data.running), ]
#按月和房间查看逐时温度
ggplot(data.running[month == "02" &
                      room_code %in% unique(data.running[month == "02" &
                                                           real_temp >= 35]$room_code)], aes(x = hour, y = real_temp))  +
  ylim(5, 45) + geom_boxplot()  + geom_point(size = 1,
                                             position = "jitter",
                                             aes(color = room_code, alpha = 0.3))
ggplot(data.running[month == "02"], aes(x = real_temp)) + xlim(30, 45) +
  geom_histogram()

ggplot(data.running[room_code == "330100D262402" &
                      month == "09" &
                      day == "21"], aes(x = hour, y = real_temp)) + geom_boxplot()  + geom_point(size = 1.5, position = "jitter", #alpha=0.5,
                                                                                                 aes(color = ac_code))#+geom_line(aes(x=as.numeric(hour),y=mean(real_temp),group=day,color=day))#+ylim(0,30)

##########先按月进行过滤
data.running$labelMonth <- paste(data.running$ac_code,
                                 data.running$year,
                                 data.running$month,
                                 sep = "-")
setorder(data.running, ac_code, time)
data.running.checkMonth <- data.running[, .(
  year = unique(year),
  month = unique(month),
  ac_code = unique(ac_code),
  room_code = unique(room_code),
  meanTemp = mean(real_temp, na.rm = TRUE),
  sd = sd(real_temp, na.rm = TRUE),
  range = max(real_temp) - min(real_temp),
  maxTemp = max(real_temp),
  minTemp = min(real_temp)
), by = labelMonth]
##按月粗过滤
data.running.checkMonth <-
  data.running.checkMonth[meanTemp < 40 &
                            meanTemp >= 1]#月平均温度大于50小于等于1全部过滤
data.running.checkMonth <-
  data.running.checkMonth[sd >= 1]#月温度方差大于10全部过滤
data.running.checkMonth <-
  data.running.checkMonth[range > 3]#月温度方差大于10全部过滤
setorder(data.running.checkMonth, range, sd)
##将数据清理的备份
data.running.processed <-
  data.running[labelMonth %in% data.running.checkMonth$labelMonth]

#######再按天清洗数据
##统计各房间每天的平均值和方差
setorder(data.running.processed, ac_code, time)
data.running.processed$labelDay <-
  paste(
    data.running.processed$ac_code,
    data.running.processed$year,
    data.running.processed$month,
    data.running.processed$day,
    sep = "-"
  )
data.running.checkDay <- data.running.processed[, .(
  year = unique(year),
  month = unique(month),
  day = unique(day),
  ac_code = unique(ac_code),
  room_code = unique(room_code),
  runtime = sum(on_off == "1"),
  runRatio = sum(on_off == "1") / length(on_off),
  meanTemp = mean(real_temp, na.rm = TRUE),
  sd = sd(real_temp, na.rm = TRUE),
  range = max(real_temp) - min(real_temp),
  maxTemp = max(real_temp),
  minTemp = min(real_temp)
), by = labelDay]
##按天粗过滤
data.running.checkDay <-
  data.running.checkDay[range < 20 & range > 0.5]#单天内温度波动超过25度的过滤
data.running.checkDay <-
  data.running.checkDay[meanTemp < 45 &
                          meanTemp >= 1]#单天平均温度大于50小于等于1全部过滤
data.running.checkDay <-
  data.running.checkDay[sd > 0.05]#单天逐时温度方差大于10全部过滤

##按季节对应温度过滤
data.running.checkDay$season <-lapply(data.running.checkDay$month,getSeason)
#checkDay 功德圆满

data.running.checkSeason <-
  data.running.checkDay[season == "Winter" &
                          meanTemp < 30 &
                          meanTemp > 5 & minTemp > 5 & maxTemp < 35]
data.running.checkSeason <-
  rbind(data.running.checkSeason, data.running.checkDay[season == "Spring" &
                                                          meanTemp < 30 &
                                                          meanTemp > 10 &
                                                          minTemp > 5 &
                                                          maxTemp < 35])
data.running.checkSeason <-
  rbind(data.running.checkSeason, data.running.checkDay[season == "Summer_warm" &
                                                          meanTemp < 35 &
                                                          meanTemp > 15 &
                                                          minTemp > 10 &
                                                          maxTemp < 40])
data.running.checkSeason <-
  rbind(data.running.checkSeason, data.running.checkDay[season == "Summer" &
                                                          meanTemp < 35 &
                                                          meanTemp > 15 &
                                                          minTemp > 10 &
                                                          maxTemp < 40])
data.running.checkSeason <-
  rbind(data.running.checkSeason, data.running.checkDay[season == "Autumn" &
                                                          meanTemp < 30 &
                                                          meanTemp > 10 &
                                                          minTemp > 5 &
                                                          maxTemp < 35])
data.running.checkSeason <-
  rbind(data.running.checkSeason, data.running.checkDay[season == "Winter_warm" &
                                                          meanTemp < 30 &
                                                          meanTemp > 5 &
                                                          minTemp > 5 &
                                                          maxTemp < 35])
setorder(data.running.checkSeason, runtime, sd)

data.running.processed <-
  data.running.processed[labelDay %in% data.running.checkSeason$labelDay]


##对夏季及夏季温和的异常高温值进行修正
#对单天最高温度超过35度观测做压缩操作
data.running.checkSeason$moveFlag <- 0
data.running.checkSeason[maxTemp > 35]$moveFlag <- 1
data.running.processed$moveFlag <- 0
data.running.processed[labelDay %in% data.running.checkSeason[maxTemp >
                                                                35]$labelDay]$moveFlag <-1
data.running.modifyLog <-
  data.running.processed[labelDay %in% data.running.checkSeason[maxTemp >
                                                                  35]$labelDay]

data.running.modifyLog$currentMean <- 0
data.running.modifyLog$targetMean <- 0
data.running.modifyLog$dateStr <-
  substr(data.running.modifyLog$labelDay, 18, 25)

##初始化当前记录对应label的当日平均温度
for (i in unique(data.running.modifyLog$labelDay)) {
  data.running.modifyLog[labelDay == i]$currentMean <-
    data.running.checkSeason[labelDay == i]$meanTemp
}#for循环真是要命...
data.running.meanTemp <-
  data.running.checkSeason[, .(meanTemp = mean(meanTemp, na.rm = TRUE)), by = paste(
    data.running.checkSeason$year,
    data.running.checkSeason$month,
    data.running.checkSeason$day,
    sep = "-"
  )]
##初始化当前记录对应所有记录的当日平均温度
for (i in unique(data.running.modifyLog$dateStr)) {
  #在循环体内要用i不能用substr(labelDay, 18, 25)，因为内存及处理速度data.table会报错
  data.running.modifyLog[dateStr == i]$targetMean <-
    data.running.meanTemp[paste == i]$meanTemp
}
data.running.modifyLog$real_temp <-
  data.running.modifyLog$real_temp - abs(data.running.modifyLog$currentMean -
                                           data.running.modifyLog$targetMean)

data.hznu.final<-rbind(data.running.processed[moveFlag==0,1:16],data.running.modifyLog[,1:16])
