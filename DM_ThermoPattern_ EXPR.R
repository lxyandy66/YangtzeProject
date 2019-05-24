library(data.table)
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)
library(psych)
library(plyr)
library(timeDate)
library(rJava)
library(xlsx)
library(rgl)

## ��������
load("HZNU_�����Ȼ�������_����ϴ����.rdata")
load("ZJU_�����Ȼ�������_����ϴ����.rdata")

#########ɾȥ��������ݲ����������ݸ�ʽͳһΪ���������Сʱ��¼
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
  )#��ǩΪ�� �����-��-��-��-Сʱ-��Сʱ
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

##���Ӽ��ڱ�ǩ
data.hznu.final$season <- lapply(data.hnzu.final$month,getSeason)

data.zju.thermo.rawProcess$season <- lapply(data.zju.thermo.rawProcess$month,getSeason)

###�����ڰѸ��µ�����ȡ������̫����
###�Ƚ����ݰ����估��Сʱ����
setorder(data.hznu.final, time, room_code)
setorder(data.zju.thermo.rawProcess, date_time, room_code)
data.hznu.room <-
  data.hznu.final[, .(
    date_time = time[1],
    #date_time�������factor����
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

###���������ݺϲ�
data.raw.all <-
  rbind(data.zju.room, data.hznu.room)[hour %in% sprintf("%02d",c(8:22))]
data.raw.all$labelDay <-
  paste(
    data.raw.all$room_code,
    data.raw.all$year,
    data.raw.all$month,
    data.raw.all$day,
    sep = "-"
  )
data.raw.all <- data.raw.all[!duplicated(data.raw.all)]
###��ϴ���ڳ��Ȳ�Ϊ30�����ݣ���8��00-22:00�м���ȱʧ����������
data.labelSelect <-
  data.raw.all[, .(num = length(temp)), by = labelDay]
#����Ҫ��һ�²�Ϊ30�������ж���
# data.err<-data.raw.all[labelDay %in% data.labelSelect[num != 30]$labelDay] #10565
data.raw.all <-
  data.raw.all[labelDay %in% data.labelSelect[num == 30]$labelDay]
rm(data.hznu.room)
rm(data.zju.room)
rm(data.err)


######�������ڱ�ǩ�������ݱ��һ��ļ�¼
data.analyse.all <- data.raw.all[, .(
  date_time = paste(unique(year), unique(month), unique(day), sep = "-"),
  #С����ôŪ������char��
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
), by = labelDay]#�ð��漰������Ҫ��unique����ȡͷһ����������ض�
data.analyse.all <- data.analyse.all[!duplicated(data.analyse.all)]
data.analyse.all$date_time <- as.Date(data.analyse.all$date_time)
data.analyse.all$isWorkday <-
  isWeekday(timeDate(data.analyse.all$date_time))


#####��ʼ�������
seasonSelect <- "Spring"
data.analyse.season <-
  data.analyse.all[season == seasonSelect, c(1, 5:35)]

wssClusterEvaluate(data.analyse.season[,2:31])
pamkClusterEvaluate(data.analyse.season[,2:31], startK = 2, endK = 10,criter = "multiasw")
for (i in c(3:10)) {
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
  
  #������Ӱ�����
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
    ), by = paste(cluster, isWorkday, sep = "_")]#�����Ŷ�
  
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
      ) + geom_boxplot(outlier.colour = NA) + ylim(10, 30) + stat_summary(
        fun.y = "mean",
        geom = "point",
        shape = 23
      ) + facet_wrap(~ cluster, nrow = kSize) + scale_x_discrete(breaks =
                                              data.analyse.tempPlot[substr(variable, 5, 6) != "30"]$variable)
    )
  )
  ##���Сʱƽ���¶ȴ�����ͳ�Ƹ�Сʱƽ���¶�
  data.analyse.meanPlot <-
    data.analyse.tempPlot[, .(
      hour = unique(as.numeric(substr(unique(variable),2,3))+ifelse(substr(unique(variable),5,6)=="00",0,0.5)),
      meanTemp = mean(value),
      cluster = unique(cluster)
    ), by = paste(cluster, variable, sep = "_")]

  ##ͳ�Ƹ������ƽ��ֵ����׼������
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
  
  
  write.xlsx(
    data.table(
      data.analyse.workday,
      data.analyse.cluster,
      cluster_count = pamk.best$pamobject$i.med,
      pamk.best$pamobject$clusinfo
    ),
    paste(kSize, seasonSelect, "cluster.xlsx", sep = "_")
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
                           cluster)) + geom_point(size = 5) + ylim(10, 30)
  )
}




ggplot(data.running[month == "02"], aes(x = hour, y = real_temp))  +
  ylim(5, 35) + geom_boxplot() # + geom_point(size = 1, position = "jitter",aes(color="blue",alpha=0.3))
ggplot(data.running[month == "02"], aes(x = real_temp)) + xlim(30, 45) +
  geom_histogram()
##���ڷָ�