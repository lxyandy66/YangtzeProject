library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(plyr)
library(rgl)

## 加载数据
load("HZNU_室内热环境数据_已清洗修正.rdata")
load("ZJU_室内热环境数据_已清洗修正.rdata")

#########删去多余的数据并将两者数据格式统一为按房间逐半小时记录
data.zju.thermo.rawProcess <-
  data.zju.thermo.rawProcess[, c("date_time",
                                 "room_code",
                                 "temp",
                                 "year",
                                 "month",
                                 "day",
                                 "hour",
                                 "halfHour")]
data.hznu.final <-
  data.hznu.final[, c("time",
                      "ac_code",
                      "room_code",
                      "real_temp",
                      "year",
                      "month",
                      "day",
                      "hour",
                      "minute")]
data.hznu.final$label <-
  paste(
    data.hznu.final$room_code,
    data.hznu.final$year,
    data.hznu.final$month,
    data.hznu.final$day,
    data.hznu.final$hour,
    data.hznu.final$minute,
    sep = "-"
  )#标签为： 房间号-年-月-日-小时-半小时
data.zju.thermo.rawProcess$label <-
  paste(
    data.zju.thermo.rawProcess$room_code,
    data.zju.thermo.rawProcess$year,
    data.zju.thermo.rawProcess$month,
    data.zju.thermo.rawProcess$day,
    data.zju.thermo.rawProcess$hour,
    data.zju.thermo.rawProcess$halfHour,
    sep = "-"
  )

##增加季节标签
data.hznu.final$season <- "NULL"
data.hznu.final[month == "01" | month == "02"]$season <- "Winter"
data.hznu.final[month == "03" | month == "04"]$season <- "Spring"
data.hznu.final[month == "05" |
                  month == "06"]$season <- "Summer_warm"
data.hznu.final[month == "07" | month == "08"]$season <- "Summer"
data.hznu.final[month == "09" | month == "10"]$season <- "Autumn"
data.hznu.final[month == "11" |
                  month == "12"]$season <- "Winter_warm"

data.zju.thermo.rawProcess$season <- "NULL"
data.zju.thermo.rawProcess[month == "01" |
                             month == "02"]$season <- "Winter"
data.zju.thermo.rawProcess[month == "03" |
                             month == "04"]$season <- "Spring"
data.zju.thermo.rawProcess[month == "05" |
                             month == "06"]$season <- "Summer_warm"
data.zju.thermo.rawProcess[month == "07" |
                             month == "08"]$season <- "Summer"
data.zju.thermo.rawProcess[month == "09" |
                             month == "10"]$season <- "Autumn"
data.zju.thermo.rawProcess[month == "11" |
                             month == "12"]$season <- "Winter_warm"

###按季节把各月单独提取，否则太慢了
###先将数据按房间及半小时汇总
setorder(data.hznu.final, time, room_code)
setorder(data.zju.thermo.rawProcess, date_time, room_code)
data.hznu.room <-
  data.hznu.final[, .(
    date_time = time[1],
    #date_time这里会是factor类型
    room_code = room_code[1],
    temp = mean(real_temp),
    year = year[1],
    month = month[1],
    day = day[1],
    hour = hour[1],
    halfHour = minute[1],
    season = season[1]
  ), by = label]
data.zju.room <- data.zju.thermo.rawProcess[, .(
  date_time = date_time[1],
  room_code = room_code[1],
  temp = mean(temp),
  year = year[1],
  month = month[1],
  day = day[1],
  hour = hour[1],
  halfHour = unique(halfHour),
  season = season[1]
), by = label]
data.hznu.room$date_time <- as.POSIXct(data.hznu.room$date_time)
data.zju.room$date_time <- as.POSIXct(data.zju.room$date_time)

###将所有数据合并
data.raw.all <-
  rbind(data.zju.room, data.hznu.room)[hour %in% c("08",
                                                   "09",
                                                   "10",
                                                   "11",
                                                   "12",
                                                   "13",
                                                   "14",
                                                   "15",
                                                   "16",
                                                   "17",
                                                   "18",
                                                   "19",
                                                   "20",
                                                   "21",
                                                   "22")]
data.raw.all$labelDay <-
  paste(
    data.raw.all$room_code,
    data.raw.all$year,
    data.raw.all$month,
    data.raw.all$day,
    sep = "-"
  )
data.raw.all <- data.raw.all[!duplicated(data.raw.all)]
###清洗日内长度不为30的数据，即8：00-22:00中间有缺失或多余的数据
data.labelSelect <-
  data.raw.all[, .(num = length(temp)), by = labelDay]
#这里要看一下不为30的数据有多少
data.raw.all <-
  data.raw.all[labelDay %in% data.labelSelect[num == 30]$labelDay]
rm(data.hznu.room)
rm(data.zju.room)


######设置日期标签，将数据变成一天的记录


data.analyse.all <- data.raw.all[, .(
  date_time = paste(unique(year), unique(month), unique(day), sep = "-"),
  #小心这么弄出来是char啊
  room_code = unique(room_code),
  season = unique(season),
  h08.00 = temp[1],
  h08.30 = temp[2],
  h09.00 = temp[3],
  h09.30 = temp[4],
  h10.00 = temp[5],
  h10.30 = temp[6],
  h11.00 = temp[7],
  h11.30 = temp[8],
  h12.00 = temp[9],
  h12.30 = temp[10],
  h13.00 = temp[11],
  h13.30 = temp[12],
  h14.00 = temp[13],
  h14.30 = temp[14],
  h15.00 = temp[15],
  h15.30 = temp[16],
  h16.00 = temp[17],
  h16.30 = temp[18],
  h17.00 = temp[19],
  h17.30 = temp[20],
  h18.00 = temp[21],
  h18.30 = temp[22],
  h19.00 = temp[23],
  h19.30 = temp[24],
  h20.00 = temp[25],
  h20.30 = temp[26],
  h21.00 = temp[27],
  h21.30 = temp[28],
  h22.00 = temp[29],
  h22.30 = temp[30]
), by = labelDay]#好吧涉及变量都要用unique或者取头一个，否则会重读
data.analyse.all <- data.analyse.all[!duplicated(data.analyse.all)]
data.analyse.all$date_time <- as.Date(data.analyse.all$date_time)
data.analyse.all$isWorkday <-
  isWeekday(timeDate(data.analyse.all$date_time))


#####开始聚类分析
seasonSelect <- "Summer_warm"
data.analyse.season <-
  data.analyse.all[season == seasonSelect, c(1, 5:35)]

# wssClusterEvaluate(data.analyse.season)
# pamkClusterEvaluate(data.analyse.season, startK = 2, endK = 15)
for (i in c(3:7)) {
  kSize <- i
  pamk.best <-
    pamk(
      data.analyse.season[, 2:31],
      krange = kSize,
      criterion = "ch",
      usepam = FALSE,
      critout = TRUE
    )
  data.analyse.season$cluster <- pamk.best$pamobject$clustering
  data.analyse.tempPlot <-
    melt(data.analyse.season,
         id = c("labelDay", "cluster", "isWorkday"))
  data.analyse.tempPlot$cluster <-
    as.factor(data.analyse.tempPlot$cluster)
  
  #工作日影响汇总
  sumWorkday <-
    length(data.analyse.season[isWorkday == TRUE]$labelDay)
  sumWeekend <-
    length(data.analyse.season[isWorkday == FALSE]$labelDay)
  
  data.analyse.workday <-
    data.analyse.season[, .(
      cluster = unique(cluster),
      isWorkday = unique(isWorkday),
      count = length(labelDay),
      ratio = length(labelDay) / ifelse(unique(isWorkday), sumWorkday, sumWeekend)
    ), by = paste(cluster, isWorkday, sep = "_")]#这样才对
  
  ggsave(
    filename = paste(kSize, seasonSelect, "cluster.png", sep = "_"),
    width = 12,
    height = 18,
    dpi = 100,
    plot = (
      ggplot(
        data.analyse.tempPlot,
        aes(
          x = factor(variable),
          y = value,
          color = cluster
        )
      ) + geom_boxplot(outlier.colour = NA) + ylim(20, 35) + stat_summary(
        fun.y = "mean",
        geom = "point",
        shape = 23
      ) + facet_wrap(~ cluster, nrow = kSize) + scale_x_discrete(breaks =
                                                                   data.analyse.tempPlot[substr(variable, 5, 6) != "30"]$variable)
    )
  )
  ##逐半小时平均温度处理，统计各小时平均温度
  data.analyse.meanPlot <-
    data.analyse.tempPlot[, .(
      hour = unique(as.numeric(substr(unique(variable),2,3))+ifelse(substr(unique(variable),5,6)=="00",0,0.5)),
      meanTemp = mean(value),
      cluster = unique(cluster)
    ), by = paste(cluster, variable, sep = "_")]

  ##统计各聚类的平均值，标准差，方差等
  data.analyse.cluster <-
    data.analyse.meanPlot[, .(
      meanTemp = mean(meanTemp),
      stdDiv = sd(meanTemp),
      pointRange = max(meanTemp) - min(meanTemp),
      lowHinge = unique(boxplot.stats(meanTemp)$stats[2]),
      highHinge = unique(boxplot.stats(meanTemp)$stats[4])
    ), by = cluster]
  data.analyse.cluster$quarterRange <-
    data.analyse.cluster$highHinge - data.analyse.cluster$lowHinge
  
  
  write.csv(
    data.table(
      data.analyse.workday,
      data.analyse.cluster,
      cluster_count = pamk.best$pamobject$i.med,
      pamk.best$pamobject$clusinfo
    ),
    paste(kSize, seasonSelect, "cluster.csv", sep = "_")
  )
  ggsave(
    file = paste(kSize, seasonSelect, "cluster_meanTemp.png", sep = "_"),
    height = 9,
    width = 16,
    dpi = 120,
    plot =
      ggplot(data.analyse.meanPlot, aes(
        x = hour, y = meanTemp, shape = cluster
      )) + geom_line(aes(color =
                           cluster)) + geom_point(size = 5) + ylim(20, 35)
  )
}




ggplot(data.running[month == "02"], aes(x = hour, y = real_temp))  +
  ylim(5, 35) + geom_boxplot() # + geom_point(size = 1, position = "jitter",aes(color="blue",alpha=0.3))
ggplot(data.running[month == "02"], aes(x = real_temp)) + xlim(30, 45) +
  geom_histogram()
##季节分割
