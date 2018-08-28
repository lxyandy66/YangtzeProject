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
data.regress.raw<-nn
data.regress.raw$deltaEC<-data.regress.raw$total_elec - data.regress.raw$time_sep
data.regress.raw$ecHourBefore<-data.regress.raw[c(1,1:nrow(data.regress.raw)-1)]$total_elec
data.regress.raw$ecDayBefore<-data.regress.raw[c(1:24,1:nrow(data.regress.raw))]$total_elec
data.regress.raw$ecWeekBefore<-data.regress.raw[c(1:(24*7),1:nrow(data.regress.raw))]$total_elec

#夏季温和
data.regress.process <- data.regress.raw[on_ratio>0&total_elec>0& (month(time) == 5 | month(time) == 6),
                       c("build_code","time","temp_diff","on_ratio","set_temp","real_temp","w_temp","w_hum",
                         "time_sep","ecHourBefore","ecDayBefore","ecWeekBefore","total_elec")]
data.regress.process<-na.omit(data.regress.process)

####变量相关性及分布####
scatterplotMatrix(data.regress.process[,3:ncol(data.regress.process)],smoother=list(lty=2),
                  plot.points = FALSE,main="ScatterPlot without 0 EC Data")
cor(data.regress.process[, 3:ncol(data.regress.process)], use = "complete.obs")
#相关矩阵如下(含零能耗记录)
#             temp_diff    on_ratio    set_temp  real_temp      w_temp       w_hum   time_sep  total_elec
# total_elec  0.44015383 -0.04585804 -0.03313324  0.4272608  0.53695398 -0.14108668  0.8383870  1.00000000
#相关性矩阵(不含零能耗记录)
#             temp_diff   on_ratio   set_temp  real_temp     w_temp       w_hum    time_sep   total_elec
# total_elec  0.002200785  0.7604641  0.1256533  0.1528087  0.6136918 -0.32046952  0.74128579  1.000000000

####将数据分为训练集和预测集####
set.seed(32767)
sub<-sample(1:nrow(data.regress.process),round(nrow(data.regress.process)*0.7))#三七开
data.regress.traning<-data.regress.process[sub,]
data.regress.test<-data.regress.process[-sub,]

####原始未调整参数####
regressFit<-lm(total_elec~time_sep+ecHourBefore+ecDayBefore+ecWeekBefore+
                 on_ratio+
                 temp_diff+
                 w_temp+
                 w_hum+
                 real_temp+
                 set_temp,data=data.regress.traning)#未调整_多元线性回归
regressFit<-plsr(total_elec~time_sep+ecHourBefore+#ecDayBefore+ecWeekBefore+
                   on_ratio+temp_diff+
                   w_temp+
                   w_hum+
                   real_temp+
                   set_temp,data=data.regress.traning,validation="LOO",jackknife=TRUE)#未调整_偏最小二乘
####调参数####
regressFit<-lm(total_elec~time_sep+ecHourBefore+#ecDayBefore+ecWeekBefore+
                 I((on_ratio)^0.5)+
                 I(temp_diff^2)+temp_diff+
                 w_temp+I(w_temp^2)+I(w_temp^3)+
                 I(exp(w_hum)^-1)+
                 I((real_temp)^3)+I(real_temp^2)+
                 I(set_temp^2)+set_temp,data=data.regress.traning)#多元线性回归

regressFit<-plsr(total_elec~time_sep+ecHourBefore+
                   on_ratio+
                   w_temp+I(temp_diff^2)+
                   I(real_temp^2)+
                   set_temp,data=data.regress.traning,validation="LOO",jackknife=TRUE)
####回归诊断####
#模型参数
summary(regressFit,what = "all")
jack.test(regressFit)
R2(regressFit)
coef(regressFit)

####回归实际误差判断####
regress.predict<-data.table(predict(regressFit,data.regress.traning))
data.nn<-data.table(data.regress.process$time_sep,data.regress.process$total_elec)
getMAPE(nn$total_elec,nn$time_sep)

paste("训练集MAPE：",getMAPE(regress.predict$V1,data.regress.traning$total_elec))
paste("训练集MAE：",getMAE(regress.predict$V1,data.regress.traning$total_elec))
paste("训练集原始时间序列MAPE：",getMAPE(data.regress.traning$time_sep,data.regress.traning$total_elec))
paste("训练集原始时间序列MAE：",getMAE(data.regress.traning$time_sep,data.regress.traning$total_elec))

data.nn$mape<-data.table(abs((data.nn$V1-data.nn$V2)/data.nn$V2))
for(i in 1:nrow(mape)){
  if(mape$V1[i]=="NaN"){
    mape$V1[i]<-0
  }
}
mean(mape$V1)

regress.predict<-data.table(predict(regressFit,data.regress.test))
paste("预测集MAPE：",getMAPE(regress.predict$V1,data.regress.test$total_elec))
paste("预测集MAE：",getMAE(regress.predict$V1,data.regress.test$total_elec))
paste("训练集原始时间序列MAPE：",getMAPE(data.regress.test$time_sep,data.regress.test$total_elec))
paste("训练集原始时间序列MAE：",getMAE(data.regress.test$time_sep,data.regress.test$total_elec))


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
x.training<-as.matrix(data.regress.traning[,c("temp_diff","on_ratio","real_temp","w_temp","time_sep")])
y.training<-as.matrix(data.regress.traning[,"total_elec"])
x.test<-as.matrix(data.regress.test[,c("temp_diff","on_ratio","real_temp","w_temp","time_sep")])
y.test<-as.matrix(data.regress.test[,"total_elec"])
regm<-ksvm(x.training,y.training,epsilon=0.1,kernel="polydot",C=0.3,cross=10)
training.predict<-data.table(predict(regm,x.training))
test.predict<-data.table(predict(regm,x.test))
getRSquare(training.predict$V1,y.training)
getRSquare(test.predict$V1,y.test)
regm


#confusionMatrix(test.predict,y.test)

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
  data.caculate$SSE<-(data.caculate$pred- data.caculate$ref)^2
  data.caculate$SST<-(data.caculate$pred- mean(data.caculate$ref,na.rm = TRUE))^2
  return( 1- ( sum(data.caculate$SSE) / sum(data.caculate$SST) ) )
}

getMAPE<-function(yPred,yLook){
  mape<-data.table(abs((yPred-yLook)/yLook))
  for(i in 1:nrow(mape)){
    if(mape$V1[i]=="NaN"){
      mape$V1[i]<-0
    }
  }
  mean(mape$V1)
}
getMAE<-function(yPred,yLook){
  mae<-data.table(abs(yPred-yLook))
  mean(mae$V1)
}

