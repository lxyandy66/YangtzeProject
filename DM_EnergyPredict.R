library(data.table)
library(forecast)
library(ggplot2)
library(car)
library(gvlma)

#加载天气数据
load("杭州2016_2017气象数据.rdata")
load("2018-6-6.RData")#HZNU用电数据

######
#利用已生成的能耗序列对能耗模型进行修正
#实际能耗=f(时间序列能耗，室外温度，etc)

data.regress.raw <- nn[on_ratio>0&total_elec>0& (month(time) == 5 | month(time) == 6),
                       c("build_code","time","temp_diff","on_ratio","set_temp","real_temp","w_temp","w_hum","time_sep","total_elec")]#夏季温和
scatterplotMatrix(data.regress.raw[,3:10],smoother=list(lty=2),plot.points = FALSE,main="ScatterPlot without 0 EC Data")
cor(data.regress.raw[, 3:10], use = "complete.obs")
#相关矩阵如下(含零能耗记录)
#             temp_diff    on_ratio    set_temp  real_temp      w_temp       w_hum   time_sep  total_elec
# total_elec  0.44015383 -0.04585804 -0.03313324  0.4272608  0.53695398 -0.14108668  0.8383870  1.00000000
#相关性矩阵(不含零能耗记录)
#             temp_diff   on_ratio   set_temp  real_temp     w_temp       w_hum    time_sep   total_elec
# total_elec  0.002200785  0.7604641  0.1256533  0.1528087  0.6136918 -0.32046952  0.74128579  1.000000000

#####
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

regressFit<-lm(total_elec~time_sep+I((on_ratio)^0.5)+w_temp+I(w_temp^2)+I(exp(w_hum)^-1)+I((real_temp)^3)+I(real_temp^2)+I(set_temp),data=data.regress.raw)
#检验过程
summary(regressFit)
cat("he\nllo\n")
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