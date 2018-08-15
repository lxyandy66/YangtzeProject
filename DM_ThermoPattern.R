library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
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

setorder(data.hznu.final, time, ac_code)
data.hznu.thermo <-
  data.hznu.final[, .(
    date_time = unique(time),
    room_code = unique(room_code),
    temp = mean(real_temp),
    year = unique(year),
    month = unique(month),
    day = unique(day),
    hour = unique(hour),
    halfHour = unique(minute)
  ), by = label]#一句话可以跑到下班了...

###将所有数据合并
data.all.thermo <-
  rbind(data.zju.thermo.rawProcess, data.hznu.thermo)[hour %in% c("08",
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
###清洗日内长度不为30的数据，即8：00-22:00中间有缺失或多余的数据
data.labelSelect <- data.all.thermo[, .(num = length(temp)), by = label]
#这里要看一下不为30的数据有多少
data.labelSelect <- data.labelSelect[num == 30]
data.all.thermo <- data.all.thermo[label %in% data.labelSelect$label]



######设置日期标签，将数据变成一天的记录
data.all.thermo$labelDay <-
  paste(
    data.all.thermo$room_code,
    data.all.thermo$year,
    data.all.thermo$month,
    data.all.thermo$day,
    sep = "-"
  )#标签为：房间号-年-月-日

data.analyse <- data.all.thermo[, .(
  date_time = paste(year, month, day, sep = "-"),
  room_code = unique(room_code),
  h8.00 = temp[1],
  h8.30 = temp[2],
  h9.00 = temp[3],
  h9.30 = temp[4],
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
), by = labelDay]

#####开始聚类分析



ggplot(data.running[month == "02"], aes(x = hour, y = real_temp))  +
  ylim(5, 35) + geom_boxplot() # + geom_point(size = 1, position = "jitter",aes(color="blue",alpha=0.3))
ggplot(data.running[month == "02"], aes(x = real_temp)) + xlim(30, 45) +
  geom_histogram()
##季节分割
