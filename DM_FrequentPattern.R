####该脚本仅用于JCQU发表期刊作图用####
library(data.table)
library(ggplot2)

####加载Excel处理完成报表####
data.hznu.apriori <- as.data.table(read.csv("HZNU_Apriori.csv"))

####天气数据处理####
weather.HZ$year <- format(weather.HZ$time, "%Y")
weather.HZ$month <- as.numeric(format(weather.HZ$time, "%m"))
weather.HZ$day <- as.numeric(format(weather.HZ$time, "%d"))
weather.HZ$periodMark <- "last"
weather.HZ[day <= 10]$periodMark <- "first"
weather.HZ[day <= 20 & day > 10]$periodMark <- "mid"
weather.HZ[day > 20]$periodMark <- "last"
weather.HZ$labelMonth <-
  paste(weather.HZ$year,
        weather.HZ$month,
        weather.HZ$periodMark,
        sep = "_")
data.hz.weather.monthly <-
  weather.HZ[, .(aveTemp = mean(w_temp, na.rm = TRUE),
                 aveHumid = mean(w_hum, na.rm = TRUE)), by = labelMonth]
data.hz.weather.monthly$year<-substr(data.hz.weather.monthly$labelMonth,1,4)
data.hz.weather.monthly$timePeriod<-substring(data.hz.weather.monthly$labelMonth,first = 6)


ggplot(data = data.hznu.apriori, aes(x = as.factor(timePeriod), y = setTemp)) + geom_boxplot() +
  scale_x_discrete(breaks = c("6_first","6_mid","6_last",
                                "7_first","7_mid","7_last",
                                "8_first","8_mid","8_last",
                                "9_first","9_mid","9_last")
                     # labels =  c("6月上","6月中","6月下",
                     #               "7月上","7月中","7月下",
                     #               "8月上","8月中","8月下",
                     #               "9月上","9月中","9月下")
                     )#+geom_line(data = data.hz.weather.monthly[year=="2017"],aes(x=timePeriod,y = aveTemp))
