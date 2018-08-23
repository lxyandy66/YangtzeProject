load("D:/R/kt/dxgj/hsd/2018-6-6.RData")
library(data.table)
setwd("D:/R/kt/dxgj/hsd")

data.all<-data.table(data.yx$ac_code,data.yx$time,data.yx$on_off,data.yx$run_mode,data.yx$wind,data.yx$set_temp,data.yx$real_temp,
                     data.yd$total_elec,data.yd$outdoor_elec,data.yd$system_elec,data.yd$indoor_elec,data.yd$ac_code,data.yd$time)
setnames(data.all,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13"),
         c("ac_code","time","on_off","run_mode","wind","set_temp","real_temp","total_elec","outdoor_elec","system_elec","indoor_elec","ac_code_yd","time_yd"))

data.all<-data.all[!duplicated(data.all),] ###去除重复数据

rm(data.yx)
rm(data.yd)
gc()

data.all[1:10]
str(data.all)
data.all$build_code<-substr(data.all$ac_code,1,10)

data.all<-data.all[,c("build_code","ac_code","time","on_off","set_temp","real_temp","total_elec")]
data.all$year<-format(data.all$time,"%Y")
data.all$month<-format(data.all$time,"%m")
data.all5_7<-data.all[year%in%c("2016","2017")&month%in%c("05","06","07")]#取5 6 7月的数据
data.all5_7$date<-format(data.all5_7$time,"%Y-%m-%d")

unique(data.all5_7$on_off)
data.on<-data.all5_7[on_off=="开机"]
data.off<-data.all5_7[on_off=="关机"]
rm(data.all)
rm(data.all5_7)
gc()
data.on$on_off<-"1"
data.off$on_off<-"0"
newdata<-rbind(data.on,data.off)
rm(data.on)
rm(data.off)
gc()
newdata[1:10]

#导入天气数据
hz_2016<-read.csv("hz_2016.csv",header = TRUE,sep=",")
hz_2017<-read.csv("hz_2017.csv",header = TRUE,sep=",")
hz_weather<-rbind(hz_2016,hz_2017)
rm(hz_2016)
rm(hz_2017)
gc()

hz_weather$time<-as.POSIXct(hz_weather$时间)
hz_weather<-data.table(hz_weather)
hz_weather$year<-format(hz_weather$time,"%Y")
hz_weather$month<-format(hz_weather$time,"%m")
hz_weather<-hz_weather[year%in%c("2016","2017")&month%in%c("05","06","07")]
hz_weather$year<-NULL
hz_weather$month<-NULL#这样就能删除列了？！？！？！
hz_weather$温度<-as.numeric(hz_weather$温度)
hz_weather$风速<-as.numeric(hz_weather$风速)

#
hz_weather1<-hz_weather[,.(
  w_temp=mean(温度),
  w_hum=mean(相对湿度),
  w_winds=mean(风速)
),by=time]
newdata1<-merge(newdata,hz_weather1)#这种merge要比设标签方便多了
rm(newdata)
gc()

###############################计算房间 能耗/建筑内空调开启率
newdata1[1:5]
newdata1$hour<-format(newdata1$time,"%H")
newdata1$ac_code<-as.character(newdata1$ac_code)
newdata1$room_code<-substr(newdata1$ac_code,1,nchar(newdata1$ac_code)-3)
newdata1$lable0<-paste(newdata1$build_code,newdata1$date,newdata1$hour)#标签：建筑号-年月日-小时

newdata2<-newdata1[,.(
  build_code=unique(build_code),
  date=unique(date),
  time=unique(paste(date,hour)),
  ac_num=length(unique(ac_code)),
  room_num=length(unique(room_code)),
  temp_diff=mean(real_temp[on_off==1]-set_temp[on_off==1]),
  on_ratio=sum(on_off==1)/length(on_off),
  set_temp=mean(set_temp[on_off==1]),
  real_temp=mean(real_temp[on_off==1]),
  total_elec=sum(total_elec),
  w_temp=mean(w_temp),
  w_winds=mean(w_winds),
  w_hum=mean(w_hum)
),by=lable0]
###这里还可以搞点事情

newdata2$time<-paste(newdata2$time,"00","00",sep = ":")
#newdata2$time<-as.POSIXct(newdata2$time,"%Y-%m-%d %H:%M:%S")
newdata2$time<-as.POSIXct(strptime(newdata2$time, "%Y-%m-%d %H:%M:%S",tz = "GMT"))

plot(x=newdata2$on_ratio,y=newdata2$total_elec)
ggplot(newdata2[on_ratio!=0],aes(x=on_ratio,y=total_elec/ac_num,color=build_code))+geom_point(position = "jitter")+facet_wrap(~month(time),nrow = 3)
newdata2$on_ratio<-round(newdata2$on_ratio,2)#四舍五入用
newdata2$total_elec<-round(newdata2$total_elec,2)
boxplot(round(newdata2$total_elec,2)~round(newdata2$on_ratio,2))
#str(newdata2)

#######
list.newdata2<-split(newdata2,newdata2$build_code)

coef.f<-function(data){
  fit<-lm(total_elec~on_ratio,data = data)
  coef<-setNames(data.table(fit$coefficients[2]),"coe")
  coef$cor<-cor(data$on_ratio,data$total_elec)
  coef$build_code<-unique(data$build_code)
  return(coef)
}
coef<-do.call("rbind",lapply(lapply(list.newdata2,coef.f),"data.frame"))
coef$coe[is.na(coef$coe)]<-0
coef$cor[is.na(coef$cor)]<-0

newdata3<-merge(newdata2,coef)

##########################
nn1<-newdata3[coe>0&coe<=60]
plot(nn1$on_ratio,nn1$total_elec,main = "1")
nn2<-newdata3[coe>60&coe<=160]
plot(nn2$on_ratio,nn2$total_elec,main = "2")
nn3<-newdata3[coe>160]
plot(nn3$on_ratio,nn3$total_elec,main = "3")

unique(nn1$ac_num)
unique(nn2$ac_num)
unique(nn3$ac_num)

library(ggplot2)
nn1$type<-"1"
nn2$type<-"2"
nn3$type<-"3"
nn<-rbind(nn1,nn2,nn3)
ggplot(nn,aes(x=on_ratio,y=nn$total_elec,colour=type))+geom_point()
fit1<-lm(total_elec~on_ratio,data=nn1)
fit2<-lm(total_elec~on_ratio,data=nn2)
fit3<-lm(total_elec~on_ratio,data=nn3)

summary(fit1)
summary(fit2)
summary(fit3)

##############季节性时间序列预测
#t<-list.newdata2[[10]]
#a<-t[time==as.POSIXlt(t$time[100]-24*60*60)]$total_elec

library(ggplot2)
library(forecast)
nn<-data.table(list.newdata2[[15]])
pred.num<-24
myseries<-ts(nn$total_elec,start = c(2017,5,1,0),frequency = 24)
#时间序列还可以是数据框格式
#myseries<-ts(data.table(total_elec=nn$total_elec,on_ratio=nn$on_ratio),start = c(2017,5,1,0),frequency = 24)

##
fit<-ets(myseries,model = "AAA")#ets-model=AAA 三指数平滑
#####？？？？？？？
#是否可以理解成根据训练数据拟合曲线fit--
#下一步预测基于拟合的fit
#####？？？？
fcst<-forecast(fit,24*7)
plot(fcst)


nn$time_sep<-fit$fitted
plot(nn$total_elec[2099:2200],type="l",main=unique(nn$build_code))
lines(nn$time_sep[2100:2201],col="red")

plot(nn$total_elec[2100:2201],type="l",main=unique(nn$build_code))
lines(nn$time_sep[2100:2201],col="red")

nn$time_sep<-abs(as.numeric(nn$time_sep))
for(i in 1:nrow(nn)){
  if(nn$total_elec[i]==0){
    nn$time_sep[i]<-0
  }
}
nn1<-nn[2100:2201,]
#
mape<-data.table(abs((nn1$time_sep-nn1$total_elec)/nn1$total_elec))
for(i in 1:nrow(mape)){
  if(mape$V1[i]=="NaN"){
    mape$V1[i]<-0
  }
}
mean(mape$V1)
#
mae<-data.table(abs(nn1$time_sep-nn1$total_elec))
mean(mae$V1)

