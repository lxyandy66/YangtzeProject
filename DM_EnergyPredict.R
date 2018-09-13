library(data.table)
library(forecast)
library(ggplot2)
library(car)
library(gvlma)
library(pls)
library(kernlab)

####加载天气数据####
load("杭州2016_2017气象数据.rdata")
load("2018-6-6.RData")#HZNU用电数据

####利用已生成的能耗序列对能耗模型进行修正####
#实际能耗=f(时间序列能耗，室外温度，etc)
#data.regress.raw<-nn

data.regress.raw<-data.regress.total[month(time)%in% c(5,6)&year(time)==2017]

#统计该季节时间段情况下有完整时间序列建筑的个数
data.regress.summary<-data.regress.raw[,.(count=length(time)),by=buildingCode]
data.regress.summary<-data.regress.summary[count==1464]
# > unique(data.regress.summary[count==1464]$buildingCode)
# [1] "330100D250" "330100D253" "330100D265" "330100D262" "330100D257" "330100D261" "330100D267" "330100D268" "330100D269" "330100D278"
# [11] "330100D275" "330100D271"
# 合格样本：330100D261 330100D267 330100D268 330100D269 330100D278 330100D275 330100D271
# 好样本：330100D268(90+/29/25)

buildingSelect<-"330100D268"
data.regress.raw<-data.regress.total[month(time)%in% c(5,6)&year(time)==2017]
data.regress.raw<-data.regress.raw[buildingCode==buildingSelect]


####时间序列构建####
timeSeries<-ts(data.regress.raw$total_elec,start = c(2017,5,1,0),frequency = 24)
fit<-ets(timeSeries,model = "AAA")

####预测冷负荷值生成####
data.regress.raw$hour<-hour(data.regress.raw$time)
data.regress.raw$estCoolingLoad<-getEstCoolingLoad(data.regress.raw$w_temp,hour(data.regress.raw$time))
data.regress.raw$deltaInOutTemp<-data.regress.raw$w_temp-data.regress.raw$real_temp
####用于预测值绘图####
# fcst<-forecast(fit,24)
# plot(fcst)

####时间序列能耗值的绘图####
data.regress.raw$time_sep<-fit$fitted
data.regress.raw[total_elec==0]$time_sep<-0
data.regress.raw[time_sep<=0]$time_sep<-0
ggplot(data = data.regress.raw[date=="2017-06-22"],aes(x=time))+geom_line(aes(y=total_elec))+geom_line(aes(y=time_sep,color="red"))+
  geom_line(aes(y=estCoolingLoad/5,color="blue"))+geom_line(aes(y=on_ratio*100,color="green"))+geom_line(aes(y=(-cos((pi/12)*hour))*10))
# plot(data.regress.raw$total_elec,type="p")
# lines(data.regress.raw$total_elec,type="l",col="red")

####加入中间变量####
data.regress.raw$deltaEC<-data.regress.raw$total_elec - data.regress.raw$time_sep
data.regress.raw$ecHourBefore<-data.regress.raw[c(1,1:nrow(data.regress.raw)-1)]$total_elec
data.regress.raw$ecDayBefore<-data.regress.raw[c(1:24,1:nrow(data.regress.raw))]$total_elec
data.regress.raw$ecWeekBefore<-data.regress.raw[c(1:(24*7),1:nrow(data.regress.raw))]$total_elec
data.regress.raw$deltaECHourBefore<-data.regress.raw[c(1,1:nrow(data.regress.raw))]$deltaEC
data.regress.raw$deltaEC2HourBefore<-data.regress.raw[c(1:2,1:nrow(data.regress.raw))]$deltaEC
data.regress.raw$deltaECMean<-(data.regress.raw$deltaECHourBefore+data.regress.raw$deltaEC2HourBefore)/2
data.regress.raw$c1Ratio<-data.regress.raw[c(1,1:nrow(data.regress.raw))]$c1Ratio
data.regress.raw$c2Ratio<-data.regress.raw[c(1,1:nrow(data.regress.raw))]$c2Ratio
data.regress.raw$c3Ratio<-data.regress.raw[c(1,1:nrow(data.regress.raw))]$c3Ratio
data.regress.raw$c4Ratio<-data.regress.raw[c(1,1:nrow(data.regress.raw))]$c4Ratio
setorder(data.regress.raw,time)

#夏季温和
processStart<-1000#sample(1:(nrow(data.regress.raw)-24*7*2),1)#随机取起始点
data.regress.process <- data.regress.raw[processStart:(processStart+24*7*2),
                       c("buildingCode","time","total_elec","temp_diff","temp_diffRatio","on_ratio","set_temp","real_temp","w_temp","w_hum","deltaInOutTemp",
                         "time_sep","deltaEC","deltaECHourBefore","deltaEC2HourBefore","deltaECMean",
                         "ecHourBefore","ecDayBefore","ecWeekBefore","estCoolingLoad",
                         "c1Ratio","c2Ratio","c3Ratio","c4Ratio","hour")]
data.regress.process<-na.omit(data.regress.process)
ggplot(data = data.regress.process,aes(x=time))+geom_line(aes(y=total_elec,color="total_elec"))+
  geom_line(aes(y=time_sep,color="time_sep"))+geom_line(aes(y=(sin((pi/12)*hour*10)),color="timeSin"))+
  geom_line(aes(y=on_ratio*100,color="on_ratio"))+geom_line(aes(y=w_temp,color="wTemp"))+geom_line(aes(y=deltaInOutTemp,color="deltaTemp"),show.legend = TRUE)


####将数据分为训练集和预测集####
#set.seed(32767)
# sub<-sample(1:nrow(data.regress.process),round(nrow(data.regress.process)*2/3))#三七开
# data.regress.training<-data.regress.process[sub,]
# data.regress.test<-data.regress.process[-sub,]

data.regress.training<-data.regress.process[1:round(nrow(data.regress.process)*2/3),]
data.regress.test<-data.regress.process[round(nrow(data.regress.process)*2/3):nrow(data.regress.process),]

####变量相关性及分布####
scatterplotMatrix(data.regress.process[,3:ncol(data.regress.process)],smoother=list(lty=2),
                  plot.points = FALSE,main="ScatterPlot without 0 EC Data")
cor(data.regress.process[, 3:(ncol(data.regress.process)-3)], use = "complete.obs")
#相关矩阵如下(含零能耗记录)|未清洗能耗
#             temp_diff    on_ratio    set_temp  real_temp      w_temp       w_hum   time_sep  total_elec
# total_elec  0.44015383 -0.04585804 -0.03313324  0.4272608  0.53695398 -0.14108668  0.8383870  1.00000000
#相关性矩阵(不含零能耗记录)|未清洗能耗
#             temp_diff   on_ratio   set_temp  real_temp     w_temp       w_hum    time_sep   total_elec
# total_elec  0.002200785  0.7604641  0.1256533  0.1528087  0.6136918 -0.32046952  0.74128579  1.000000000

###################
####原始未调整参数####
regressFit<-lm(total_elec~time_sep+ecHourBefore+ecDayBefore+ecWeekBefore+
                 on_ratio+hour+
                 temp_diff+
                 w_temp+
                 w_hum+
                 real_temp+
                 set_temp,data=data.regress.training)#未调整_多元线性回归
regressFit<-plsr(total_elec~time_sep+ecWeekBefore+hour+
                   on_ratio+temp_diff+temp_diffRatio+
                   deltaECMean+
                   w_temp+deltaInOutTemp+estCoolingLoad+
                   w_hum+
                   real_temp+set_temp+
                   c1Ratio+c2Ratio+c3Ratio+c4Ratio,data=data.regress.training,validation="LOO",jackknife=TRUE)#未调整_偏最小二乘
####调参数####
regressFit<-lm(total_elec~time_sep+ecHourBefore+#ecDayBefore+ecWeekBefore+
                 I((on_ratio)^0.5)+
                 I(temp_diff^2)+temp_diff+
                 w_temp+I(w_temp^2)+I(w_temp^3)+
                 I(exp(w_hum)^-1)+
                 I((real_temp)^3)+I(real_temp^2)+
                 I(set_temp^2)+set_temp,data=data.regress.training)#多元线性回归

regressFit<-plsr(total_elec~time_sep+ecHourBefore+
                   on_ratio+
                   w_temp+I(temp_diff^2)+
                   I(real_temp^2)+
                   set_temp,data=data.regress.training,validation="LOO",jackknife=TRUE)
####回归诊断####
#模型参数
summary(regressFit,what = "all")
jack.test(regressFit)
R2(regressFit)
coef(regressFit)

####回归实际误差判断####
regress.predict<-(predict(regressFit,data.regress.training))
regress.predict<-data.table(regress.predict[,1,dim(regress.predict)[3]])
# data.nn<-data.table(data.regress.process$time_sep,data.regress.process$total_elec)
# getMAPE(nn$total_elec,nn$time_sep)

paste("训练集MAPE：",getMAPE(regress.predict$V1,data.regress.training$total_elec))
# paste("训练集MAE：",getMAE(regress.predict$V1,data.regress.training$total_elec))
paste("训练集原始时间序列MAPE：",getMAPE(as.numeric(data.regress.training$time_sep),data.regress.training$total_elec))
# paste("训练集原始时间序列MAE：",getMAE(as.numeric(data.regress.training$time_sep),data.regress.training$total_elec))
par(mfrow=c(2,1))
plot(data.regress.training$total_elec,type="o",main=paste(buildingSelect,"PLS Regress"))
lines(regress.predict$V1,type="o",col="red")
lines(as.numeric(data.regress.training$time_sep),type="o",col="blue",lty=2)

regress.predict<-(predict(regressFit,data.regress.test))
regress.predict<-data.table(regress.predict[,1,dim(regress.predict)[3]])
paste("预测集MAPE：",getMAPE(regress.predict$V1,data.regress.test$total_elec))
# paste("预测集MAE：",getMAE(regress.predict$V1,data.regress.test$total_elec))
paste("训练集原始时间序列MAPE：",getMAPE(as.numeric(data.regress.test$time_sep),data.regress.test$total_elec))
# paste("训练集原始时间序列MAE：",getMAE(as.numeric(data.regress.test$time_sep),data.regress.test$total_elec))
plot(data.regress.test$total_elec,type="o")
lines(regress.predict$V1,type="o",col="red")
lines(as.numeric(data.regress.test$time_sep),type="o",col="blue",lty=2)

####原始时间序列误差####


#预测模型前处理
#考虑异常值
vif(regressFit)
sqrt(vif(regressFit))
outlierTest(regressFit)
hat.plot(regressFit)
cutoff <- 4/(nrow(data.regress.raw) - length(regressFit$coefficients) - 2)
plot(regressFit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")
influencePlot(regressFit)
capture.output(summary(regressFit),file = "summary_lm.txt")
confint(regressFit)
par(mfrow=c(1,1))
plot(regressFit)
qqPlot(regressFit,simulate=TRUE,id.method="identify")
residplot(regressFit)
durbinWatsonTest(regressFit)
crPlots(regressFit)
ncvTest(regressFit)
spreadLevelPlot(regressFit,id = TRUE)##有异常
summary(gvlma(regressFit))


####SVM回归####
x.training<-as.matrix(data.regress.training[,c("on_ratio","real_temp","w_hum","w_temp","hour",
                                               "deltaInOutTemp","temp_diff","estCoolingLoad",
                                               "ecDayBefore","ecWeekBefore","ecHourBefore","time_sep","deltaECMean",
                                               "c1Ratio","c2Ratio","c3Ratio","c4Ratio")])
y.training<-as.matrix(data.regress.training[,"total_elec"])
x.test<-as.matrix(data.regress.test[,c("on_ratio","real_temp","w_hum","w_temp","hour",
                                       "deltaInOutTemp","temp_diff","estCoolingLoad",
                                       "ecDayBefore","ecWeekBefore","ecHourBefore","time_sep","deltaECMean",
                                       "c1Ratio","c2Ratio","c3Ratio","c4Ratio")])
y.test<-as.matrix(data.regress.test[,"total_elec"])
# regm<-ksvm(x.training,y.training,epsilon=0.1,kernel="polydot",C=0.3,cross=10)
# regm<-ksvm(x.training,y.training,kernel="polydot",epsilon=0.0001,C=1024,cross=5)#type="eps-bsvr"
regm<-ksvm(x.training,y.training,kernel="polydot",type="eps-bsvr",epsilon=0.001,C=15,cross=10)
training.predict<-data.table(predict(regm,x.training))
test.predict<-data.table(predict(regm,x.test))
#指标检验
getRSquare(training.predict$V1,y.training)
getRSquare(test.predict$V1,y.test)
getMAPE(training.predict$V1,y.training)
getMAPE(test.predict$V1,y.test)
regm

par(mfrow=c(2,1))
plot(y.training,type="o",main=paste(buildingSelect,"SVM-Training Set"))
lines(training.predict$V1,type = "o",col="red")
lines(as.matrix(data.regress.training$time_sep),type="o",lty=2,col="blue")
plot(y.test,type="o",main=paste(buildingSelect,"SVM-Test Set"))
lines(test.predict$V1,type = "o",col="red")
lines(as.matrix(data.regress.test$time_sep),type="o",lty=2,col="blue")

ggplot(data=data.regress.test[as.Date.character(data.regress.test$time)=="2017-06-22"],aes(x=time))+
  geom_line(aes(y=time_sep),color="red")+geom_line(aes(y=estCoolingLoad/30),color="blue")+geom_line(aes(y=real_temp),color="grey")+
  geom_line(aes(y=on_ratio*100),color="green")+geom_line(aes(y=w_temp))+geom_line(aes(y=deltaInOutTemp),color="orange")

####负荷估算等方法####
getEstCoolingLoad<-function(outTemp,hour){
  #这个拟合公式肯定有问题
  if(hour>19){
    param<-c(6.0349,2.3776,289.6508)
  }else if(hour>14){
    param<-c(30.826,5.3092,508.4841)
  }else if(hour>9){
    param<-c(0.6486,21.9506,604.5024)
  }else if(hour>6){
    param<-c(105.826,-2.2768,521.9676)
  }else{
    param<-c(12.9257,-2.2768,331.8660)#0~6
  }
  return(param[1]*(outTemp-26)*(sin((pi/12)*hour+param[2]))+param[3])
  # return(param[1]*(outTemp-26)*sin((pi/12)*hour+param[2]))
}


####用于回归诊断等的函数####
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,xlab="Studentized Residual",main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),lty=1:2, col=c("blue","red"), cex=.7)
}
getRSquare<-function(pred,ref){
  data.caculate<-data.table(pred=pred,ref=ref)
  data.caculate<-na.omit(data.caculate)
  data.caculate$SSE<-(data.caculate$pred- data.caculate$ref)^2
  data.caculate$SST<-(data.caculate$pred- mean(data.caculate$ref,na.rm = TRUE))^2
  return( 1- ( sum(data.caculate$SSE) / sum(data.caculate$SST) ) )
}

getMAPE<-function(yPred,yLook){
  error.table<-data.table(pred=yPred,look=yLook)
  error.table<-na.omit(error.table)
  mean(as.matrix(abs((error.table$pred-error.table$look)/error.table$look)),na.rm = TRUE)
}
getMAE<-function(yPred,yLook){
  mae<-data.table(abs(yPred-yLook))
  mean(mae$V1,na.rm = TRUE)
}
getMaxPredictError<-function(yPred,yLook){
  return(max(abs((yLook-yPred)/yLook)))
}
