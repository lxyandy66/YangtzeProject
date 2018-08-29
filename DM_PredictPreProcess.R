###前接用能模式的数据集和热环境模式已清洗和过滤的数据集
data.hznu.thermo.final <-
  data.hznu.final[, .(
    ac_code = ac_code[1],
    time = time[1],
    set_temp = mean(set_temp),
    real_temp = mean(real_temp)
  ), by = paste(labelDay, hour)]
data.hznu.predict <-
  merge(data.hznu.elec.raw, data.hznu.thermo.final[, c("set_temp", "real_temp", "labelHour")], by = "labelHour")

data.hznu.predict<-data.hznu.predict[,-c("year","month","day","hour","labelHour")]

#天气数据的预处理及合并####
weather.HZ$time <- as.POSIXct(weather.HZ$时间)
weather.HZ <- data.table(weather.HZ)
weather.HZ$year <- format(weather.HZ$time, "%Y")
weather.HZ$month <- format(weather.HZ$time, "%m")
weather.HZ <-
  weather.HZ[year %in% c("2016", "2017") & month %in% c("05", "06", "07")]
weather.HZ$year <- NULL
weather.HZ$month <- NULL#这样就能删除列了？！？！？！
weather.HZ$温度 <- as.numeric(weather.HZ$温度)
weather.HZ$风速 <- as.numeric(weather.HZ$风速)

weather.HZ <- weather.HZ[, .(w_temp = mean(温度),
                              w_hum = mean(相对湿度),
                              w_winds = mean(风速)), by = time]

data.hznu.predict <- merge(data.hznu.predict, weather.HZ.processed,by = "time")#这种merge要比设标签方便多了


data.hznu.predict<-data.hznu.predict[!duplicated(data.hznu.predict)]
data.hznu.predict$buildingCode<-substr(data.hznu.predict$ac_code,1,10)
data.hznu.predict$roomCode<-substr(data.hznu.predict$ac_code,1,13)
data.hznu.predict$hour<-format(data.hznu.predict$time,"%H")
data.hznu.predict$labelBuildingHour <-
  paste(data.hznu.predict$buildingCode, data.hznu.predict$date, data.hznu.predict$hour,sep = "-")#标签：建筑号-年月日-小时

ggplot(data=data.hznu.predict,aes(x=total_elec))+geom_histogram(binwidth  = 0.1)+xlim(0,10)

data.regress.total <- data.hznu.predict[, .(
  buildingCode = unique(buildingCode),
  date = unique(date),
  time = unique(paste(date, hour)),
  ac_num = length(unique(ac_code)),
  room_num = length(unique(roomCode)),
  temp_diff = mean(real_temp[on_off == 1] - set_temp[on_off == 1]),
  on_ratio = sum(on_off == 1) / length(on_off),
  set_temp = mean(set_temp[on_off == 1]),
  real_temp = mean(real_temp[on_off == 1]),
  total_elec = sum(total_elec),
  w_temp = mean(w_temp),
  w_winds = mean(w_winds),
  w_hum = mean(w_hum)
), by = lable0]


rm(newdata)
gc()
