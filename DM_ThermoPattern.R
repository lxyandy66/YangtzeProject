####本脚本用于热环境聚类####
#更新重写，原脚本停止使用
#接已处理热环境数据"HZNU_含追加_房间级_热环境模式预处理完成.rdata"

#####开始聚类分析
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
      ) + geom_boxplot(outlier.colour = NA) + ylim(10, 30) + stat_summary(
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
##季节分割
