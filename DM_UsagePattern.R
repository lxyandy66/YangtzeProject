####分析教学建筑的空调使用模式的分析脚本####
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


load("2018-6-6.RData")

setwd("/Users/Mr_Li/Downloads/R分析_改")

####空调运行状态乱码对应####
data.yx$state<-data.yx$run_mode
# > unique(data.yx$run_mode)
# [1] \271\330\273\372 \326\306\300\344 \306\344\313\373 \263\375\312\252 \313\315\267\347 \326\306\310\310
# [7] \327\324\266\257
stateCode<-array(c(as.vector(unique(data.yx$run_mode)),"off","cooling","other","dehum","venti","heating","auto"),dim = c(7,2))
for(i in c(1:length(stateCode[,1]))){
  data.yx[state==stateCode[i,1]]$state<-stateCode[i,2]
}
data.summary.state<-data.yx[,.(count=length(time)),by=state]#统计原始数据中各状态总数
write.xlsx(x=data.summary.state,file = "使用模式统计.xlsx")
#对不确定的运行状态进行定值
data.yx$modiState<-data.yx$state
data.yx[state=="dehum"]$modiState<-"cooling"
#按典型月份对不确定空调状态进行处理
data.yx[(month(time)>=5&month(time)<=10)&(modiState=="other"|modiState=="auto"|modiState=="venti")]$modiState<-"cooling"
data.yx[(month(time)%in%c(11,12,1,2,3,4))&(modiState=="other"|modiState=="auto"|modiState=="venti")]$modiState<-"heating"

####基本标签设定####
#基本标签：空调编码_年月日小时分钟秒
data.yx$baseLabel<-paste(data.yx$ac_code,data.yx$time,sep = "_")
data.yd$baseLabel<-paste(data.yd$ac_code,data.yd$time,sep = "_")

data.all <- data.yd#将用电数据作为实际运行模式的分析数据
data.state<-na.omit(data.yx[,c("baseLabel","modiState")])
data.state<-data.state[!duplicated(data.state$baseLabel)]#一定一定Key不要有重复才不会增加数据！！！
data.all<-merge(x=data.all,y=data.state,all.x = TRUE,by.x = "baseLabel",by.y = "baseLabel")

#重复10万多？？！
data.all <- data.all[, c("ac_code", "time", "total_elec","modiState")]
data.all <- data.all[!duplicated(data.all),]
#????


###自己玉泉的数据加了还不如不加...不能加不能加不是不如不加！
data.zju.consumption <-
  as.data.table(read.csv("/Users/Mr_Li/Desktop/新增_空调用电数据_玉1.csv"))
data.zju.consumption <-
  rbind(data.zju.consumption,
        read.csv("/Users/Mr_Li/Desktop/新增_空调用电数据_玉2.csv"))
data.zju.consumption <-
  data.table(
    ac_code = data.zju.consumption$ac_code,
    time = data.zju.consumption$begin,
    total_elec = data.zju.consumption$total_elec
  )
data.zju.consumption$time <- as.POSIXct(data.zju.consumption$time)
data.zju.consumption <-
  data.zju.consumption[!duplicated(data.zju.consumption), ]
data.all <- rbind(data.all, data.zju.consumption)

data.zju.consumption$hour <- format(data.zju.consumption$time, "%H")
data.zju.distri <-
  data.zju.consumption[, .(
    sum = length(total_elec),
    onCount = sum(total_elec>=0.2),
    offCount = sum(total_elec < 0.2)
  ), by = hour]#仅用来统计分布
# data.all <- data.zju.consumption

##对时间进行处理
data.all$year <- format(data.all$time, "%Y")
data.all$month <- format(data.all$time, "%m")
data.all$day <- format(data.all$time, "%d")
data.all$hour <- format(data.all$time, "%H")
data.all$date <-
  paste(data.all$year, data.all$month, data.all$day, sep = "-")
#data.all <- data.all[year %in% c("2016", "2017")]#仅处理2016与2017年的数据
data.all$label <- paste(data.all$ac_code, data.all$date, sep = "-")#label=空调编码-年-月-日

#开关机情况处理
#?????????为什么不可以直接用循环设置data.all的on_off值
data.all[total_elec>=0.2&modiState=="off"]$modiState<-
  ifelse(data.all[total_elec>=0.2&modiState=="off"]$month>=5&data.all[total_elec>=0.2&modiState=="off"]$month<=10,"cooling","heating")
data_onLog <- data.all[total_elec >= 0.2]#数据清洗阈值还需要再考虑
data_offLog <- data.all[total_elec < 0.2]
data_onLog$on_off <- "1"
data_offLog$on_off <- "0"
#.()为list()的一个别名。如果使用.(),返回的为一个data.table对象。如果不使用.()，结果为返回一个向量。

data.all <- rbind(data_onLog, data_offLog)
setorder(data.all, ac_code, time)
data.all$label1 <-
  paste(data.all$ac_code, data.all$date, data.all$hour, sep = "-")#label1 空调名-日期-小时
data.all$on_off <- as.numeric(data.all$on_off)

all_on_off <-
  data.all[, .(runtime = sum(on_off)), by = ac_code]#对单台空调计算开关机情况
acOn <- all_on_off[runtime >= 10]#将全年开机次数大于10次的算在有使用的记录中，阈值10次？？？
new.data.all0 <- data.all[ac_code %in% acOn$ac_code]
rm(data.all)

new.data.all1 <-
  new.data.all0[, .(
    ac_code = ac_code[1],
    time = time[1],
    date = date[1],
    on_off = sum(on_off),
    total_elec = sum(total_elec),
    year = year[1],
    month = month[1],
    day = day[1],
    label = unique(label),
    hour = hour[1],
    # state=
  ), by = label1]
#将半个小时的数据化为一个小时的数据

newdata_0 <- new.data.all1[on_off == 0]
newdata_1 <- new.data.all1[on_off >= 1]
newdata_1$on_off <- 1
newdata <- rbind(newdata_0, newdata_1)
newdata <- newdata[hour %in% c("08",
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
                               "22")]#过滤只取8-22点的数据
setorder(newdata, label, hour)
newdata <- newdata[!duplicated(newdata)]
rm(newdata.all1)

#筛选除去长度不为15（即8:00-22:00不完整的数据）
data.labelSelect<-newdata[,.(num=length(on_off)),by=label]
newdata<-newdata[label%in%data.labelSelect[num==15]$label]

raw.rawData <- newdata[, .(
  runtime = sum(on_off),
  date = unique(date),
  ac_code = unique(ac_code),
  h1 = on_off[1],
  h2 = on_off[2],
  h3 = on_off[3],
  h4 = on_off[4],
  h5 = on_off[5],
  h6 = on_off[6],
  h7 = on_off[7],
  h8 = on_off[8],
  h9 = on_off[9],
  h10 = on_off[10],
  h11 = on_off[11],
  h12 = on_off[12],
  h13 = on_off[13],
  h14 = on_off[14],
  h15 = on_off[15]
), by = label]


raw.noneOn <- raw.rawData[runtime == 0]
raw.fullOn <- raw.rawData[runtime == 15]
raw.periodOn <- raw.rawData[runtime != 15 & runtime != 0]

if (anyNA(raw.periodOn)) {
  raw.periodOn <- na.omit(raw.periodOn)
}

#emmmm这个真是无话可说...
#加了玉泉的数据之后会多聚出来一个晚上开的类型，仅HZNU的数据会有上午下午开的类型
# raw.periodOn.full <- raw.periodOn
# raw.periodOn <-
#   raw.periodOn[!ac_code %in% unique(data.zju.consumption$ac_code)]

rm(newdata)
rm(newdata_0)
rm(newdata_1)
rm(new.data.all0)
rm(data_offLog)
rm(data_onLog)
gc()


####聚类部分#########
library(fpc)
library(cluster)
library(ggplot2)
library(ggradar)
library(knitr)

data.behavior.full<-raw.periodOn

raw.periodOn<-data.behavior.full[month(data.behavior.full$date) > 2 & month(data.behavior.full$date) <= 4]

wssClusterEvaluate(data = raw.periodOn[, 5:19],
                   maxIter = 1000,
                   maxK = 15)
pamkClusterEvaluate(data = raw.periodOn[, 5:19],criter ="ch",
                    startK = 2,
                    endK = 10)



#实际聚类操作，k-medoids

kSize <- 8
pamk.best <-
  pamk(
    raw.periodOn[, 5:19],
    krange = kSize,
    criterion = "ch",
    usepam = FALSE,
    critout = TRUE
  )#注意有缺失值的聚类结果将会是NA


raw.periodOn$cluster <- pamk.best$pamobject$clustering#将聚类的值赋给原始数据
raw.periodOn$date <- as.Date(raw.periodOn$date)
raw.fullOn$cluster <- 4#全开作为第四类

#工作日及非工作日的标签区分
library(timeDate)
raw.periodOn$isWorkday<-isWeekday(timeDate(raw.periodOn$date))

## 逐时开机概率进行处理，并输出到文件及绘图
nn <- data.table()
for (i in 1:kSize) {
  nn <-
    rbind(
      nn,
      data.table(
        mean(raw.periodOn[cluster == i]$h1),
        mean(raw.periodOn[cluster == i]$h2),
        mean(raw.periodOn[cluster == i]$h3),
        mean(raw.periodOn[cluster == i]$h4),
        mean(raw.periodOn[cluster == i]$h5),
        mean(raw.periodOn[cluster == i]$h6),
        mean(raw.periodOn[cluster == i]$h7),
        mean(raw.periodOn[cluster == i]$h8),
        mean(raw.periodOn[cluster == i]$h9),
        mean(raw.periodOn[cluster == i]$h10),
        mean(raw.periodOn[cluster == i]$h11),
        mean(raw.periodOn[cluster == i]$h12),
        mean(raw.periodOn[cluster == i]$h13),
        mean(raw.periodOn[cluster == i]$h14),
        mean(raw.periodOn[cluster == i]$h15)
      )
    )
}
#计算每一种开启模式的平均使用时长
raw.meanRuntime<-raw.periodOn[,.(meanRuntime=mean(runtime)),by=cluster]
setorder(raw.meanRuntime,cluster)
write.csv(
   data.table( pamk.best$pamobject$clusinfo,
     nn,raw.meanRuntime,
   pamk.best$pamobject$medoids
   ),
   paste(kSize, "_cluster", ".csv")
 )
nn <- data.table(hour = (8:22), t(nn))
dataPlot <- melt(nn, id.vars = "hour")
ggplot(dataPlot, aes(x = hour, y = value, shape = variable)) + geom_line(aes(color =
                                                                               variable)) + geom_point(size = 5)

##加入季节标签
postProcessData <-
  data.table(raw.periodOn, month = as.numeric(format(raw.periodOn$date, "%m")))

postProcessData.Winter <- postProcessData[month <= 2]
postProcessData.Winter$season <- "Winter"
postProcessData.Spring <- postProcessData[month > 2 & month <= 4]
postProcessData.Spring$season <- "Spring"
postProcessData.Summer_warm <-
  postProcessData[month > 4 & month <= 6]
postProcessData.Summer_warm$season <- "Summer_warm"
postProcessData.Summer <- postProcessData[month > 6 & month <= 8]
postProcessData.Summer$season <- "Summer"
postProcessData.Autumn <- postProcessData[month > 8 & month <= 10]
postProcessData.Autumn$season <- "Autumn"
postProcessData.Winter_warm <- postProcessData[month > 10 &
                                                 month <= 12]
postProcessData.Winter_warm$season <- "Winter_warm"
postProcessData <-
  rbind(
    postProcessData.Spring,
    postProcessData.Summer_warm,
    postProcessData.Summer,
    postProcessData.Autumn,
    postProcessData.Winter_warm,
    postProcessData.Winter
  )
rm(postProcessData.Autumn)
rm(postProcessData.Summer)
rm(postProcessData.Summer_warm)
rm(postProcessData.Winter_warm)
rm(postProcessData.Spring)
rm(postProcessData.Winter)
#这样比for循环快多了...


postProcessData$clusterSeasonLabel <-
  paste(postProcessData$cluster, postProcessData$season,postProcessData$isWorkday, sep = "_")
#clusterSeasonLabel：聚类_季节_是否为工作日

postProcessData$seasonWorkdayLabel<-paste(postProcessData$season,postProcessData$isWorkday,sep = "_")
#seasonWorkdayLabel：季节_是否为工作日

##聚类统计
seasonSum <-
  postProcessData[, .(sum = length(month), seasonInfo = unique(season)), by=seasonWorkdayLabel]#中间变量，区分季节中工作/非工作日
                  #by = season]#中间变量，计算各季节工作日与非工作日的总数

#按季节统计聚类
clusterEvaluate <- postProcessData[, .(
  cluster = unique(cluster),
  season = unique(season),
  count = length(cluster),
  isWorkday=unique(isWorkday)
), by = clusterSeasonLabel]


#计算各种聚类在不同季节所占百分比
for (i in 1:nrow(clusterEvaluate)) {
  clusterEvaluate$ratio[i] =
    clusterEvaluate$count[i] / seasonSum[seasonWorkdayLabel == paste(clusterEvaluate$season[i],clusterEvaluate$isWorkday[i],sep = "_")]$sum
}#有没有更简单的办法

clusterEvaluate$type<-paste(clusterEvaluate$cluster,clusterEvaluate$isWorkday,sep = "_")

clusterMapping <- clusterEvaluate[, .(
  Spring = ratio[season == "Spring"],
  Summer_warm = ratio[season == "Summer_warm"],
  Summer = ratio[season == "Summer"],
  Autumn = ratio[season == "Autumn"],
  Winter_warm = ratio[season == "Winter_warm"],
  Winter = ratio[season == "Winter"]
), by = type]

write.csv(cbind(clusterEvaluate,clusterMapping),
          file = paste(kSize, "clusterEvaluate.csv", sep = "_"))


####函数加载####

##  拐点法求最佳聚类数
wssClusterEvaluate <- function(data,
                               maxIter = 1000,
                               maxK = 20) {
  wss <-
    (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 1:maxK)
    wss[i] <-
      sum(kmeans(data, centers = i, iter.max = maxIter)$withinss)
  plot(1:maxK,
       wss,
       type = "o",
       xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
  return(wss)
}

##  分割算法求最佳聚类数，使用pamk方法
pamkClusterEvaluate <- function(data, startK = 2, endK = 10,criter="ch") {
  pamk.best <-
    pamk(data,
         usepam = FALSE,critout = TRUE,criterion = criter,
         krange = min(startK, endK):max(startK, endK))
  return(pamk.best)
}

#Calinsky标准
# calinskyClusterEvaluate(data) {
#   require(vegan)
#   fit <-
#     cascadeKM(scale(calinsky, center = TRUE,  scale = TRUE),
#               1,
#               10,
#               iter = 1000)
#   plot(fit, sortg = TRUE, grpmts.plot = TRUE)
#   calinski.best <- as.numeric(which.max(fit$results[2,]))
#   cat("Calinski criterion optimal number of clusters:",
#       calinski.best,
#       "\n")
# }
#Gap Statistic
gapClusterEvaluate <- function(data, kMax = 15, b = 10) {
  library(cluster)
  gap_cluster <-
    clusGap(data, kmeans, K.max = kMax, B = b)
  fviz_gap_stat(gap_cluster)
}
#多指标法
multiplyClusterEvaluate <- function(data) {
  library(factoextra)
  library(ggplot2)
  fviz_nbclust(
    data,
    kmeans,
    method = "silhouette",
    k.max = 15,
    diss = dist(data, method = "binary")
  )
}
# 错误: 矢量内存用完了(达到了极限?)，这个方法不行
