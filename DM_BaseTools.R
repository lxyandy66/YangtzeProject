####该脚本用于课题分析中通用型参数的处理及部分函数加载####
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

####预处理部分####
getMode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


getSeason<-function(month){
  #由月份获取季节
  #异常部分
  if(month<1|month>12){
    if(!is.numeric(month)){
      warning(paste(month," not a numeric, NA is returned",sep = ""),immediate. = TRUE)
      return(NA)
    }
    warning(paste(month," is out of month range, NA is returned",sep = ""),immediate. = TRUE)
    return(NA)
  }
  
  if(month %in% c(6,9))
    return("Summer_warm")
  if(month %in% c(3,12))
    return("Winter_warm")
  if(month %in% c(4,5,10,11))
    return("Transition")
  if(month %in% c(1,2))
    return("Winter")
  if(month %in% c(7,8))
    return("Summer")
  warning(paste(month," general exception, NA is returned",sep = ""),immediate. = TRUE)
  return(NA)
  
}



getMonthPeriod<-function(month){
  #获取月旬
  if(month%in% c(1:10))
    return("1st")
  else
    if(month %in% c(11:20))
      return("2nd")
  else
    if(month %in% c(21:31))
      return("3rd")
  warning(paste(month," general exception, NA is returned",sep = ""),immediate. = TRUE)
  return(NA)
}

####聚类相关####
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
pamkClusterEvaluate <-
  function(data,
           startK = 2,
           endK = 10,
           criter = "ch") {
    pamk.best <-
      pamk(
        data,
        usepam = FALSE,
        critout = TRUE,
        criterion = criter,
        krange = min(startK, endK):max(startK, endK)
      )
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

