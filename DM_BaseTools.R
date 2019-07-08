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
library(reshape2)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(caret)
library(rattle)
library(pROC)
library(rgl)
library(RODBC)
library(RMySQL)
library(factoextra)
library(NbClust)
library(C50)
library(randomForest)
library(adabag)
library(kernlab)
library(magrittr)
library(lubridate)
library(RWeka)

####预处理部分####
getMode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


getSeason<-function(month){
  #由月份获取季节
  #异常部分
  if(is.na(month)){
    warning("NA input, NA is returned",immediate. = TRUE)
    return(NA)
  }
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
  if(month %in% c(4,5))
    return("Spring")
  if(month %in% c(10,11))
    return("Autumn")
  if(month %in% c(1,2))
    return("Winter")
  if(month %in% c(7,8))
    return("Summer")
  warning(paste(month," general exception, NA is returned",sep = ""),immediate. = TRUE)
  return(NA)
  
}



getMonthPeriod<-function(day){
  #获取月旬
  if(day%in% c(1:10))
    return("1st")
  else
    if(day %in% c(11:20))
      return("2nd")
  else
    if(day %in% c(21:31))
      return("3rd")
  warning(paste(day," general exception, NA is returned",sep = ""),immediate. = TRUE)
  return(NA)
}

####聚类相关####
##  拐点法求最佳聚类数
wssClusterEvaluate <- function(data,
                               maxIter = 1000,
                               maxK = 20) {
  wss <-
    (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 1:maxK){
    wss[i] <-
      sum(kmeans(data, centers = i, iter.max = maxIter)$withinss)
    cat(i,"  clusters  ",wss[i],"\n")
  }
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
multiplyClusterEvaluate <- function(data,methodSelected="wss") {
  fviz_nbclust(
    data,
    kmeans,
    method = methodSelected,
    k.max = 10,
    diss = dist(data)
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

####获取分割字符串后各成员####
getSplitMember<-function(x,splitSimbol,index=1,isLastOne=FALSE){
  nn<-unlist(strsplit(x,split = splitSimbol))
  if(!isLastOne){
    return(nn[index])
  }else{
    return(nn[length(nn)])
  }
}##!!!!apply中出现了问题

####异常值处理方法####
#基于聚类方法，对时序温度数据进行聚类，取出聚类样本最多的类型
#1、根据时序温度进行聚类
#2、统计各聚类样本数，得到最大聚类
#3、将含最大聚类占比多的ac_code对应记录保留，其他忽略
#tip:仅用于没有缺失值的宽数据
outlierWidSeqModify<-function (tempSeq,ac_code){
  #应传入宽数据
  #简单检查
  if(nrow(as.data.table(ac_code))!=nrow(as.data.table(tempSeq))){
    warning("length not same",immediate. = TRUE)
    return(NA)
  }
  #合并数据集
  temp.outlier<-data.table(tempSeq,acCode=ac_code,
                           outlierCluster=pamk(data=tempSeq,krange=2,criterion = "ch")$pamobject$clustering)
  return(temp.outlier[outlierCluster==getMode(temp.outlier$outlierCluster)]$acCode)
}

#适用于长数据，效果相对宽数据不敏感，但允许缺失值
outlierModify<-function (data,ac_code){
  #应传入长数据
  data<-na.omit(data)
  temp.outlier<-data.table(dt=data,acCode=ac_code,
                           outlierCluster=pamk(data,krange=2,criterion = "ch")$pamobject$clustering)
  maxCluster<-ifelse(nrow(temp.outlier[outlierCluster==1])>
                       nrow(temp.outlier[outlierCluster==2]),1,2)
  temp.stat.outlier<-temp.outlier[,.(cluster1=length(dt[outlierCluster==1]),
                                     cluster2=length(dt[outlierCluster==2])),by=acCode]
  if(maxCluster==1){
    return(temp.stat.outlier[cluster1>=cluster2]$acCode)
  }else{
    return(temp.stat.outlier[cluster1<=cluster2]$acCode)
  }
}

####用于将室内温度分为高温及低温两类并计算比例####
tempRatioSplit<-function(tempSeq){
  tempSeq<-na.omit(tempSeq)
  data<-data.table(dt=tempSeq)
  tryCatch({
    data$cluster<-pamk(data$dt,krange=2,criterion = "ch")$pamobject$clustering
    stat<-data[,.(meanTemp=mean(dt,na.rm = TRUE),
                  count=length(dt)
    ),by=cluster]
    setorder(stat,meanTemp)
    stat$ratio<-stat$count/nrow(data)
    return(as.matrix(stat[,2:4],byrow = TRUE,ncol = 2))
  },error=function(e){cat("ERROR :",conditionMessage(e),"at First seq: ",tempSeq[1],"\n")})
  
}

####计算水蒸气分压力####
getWaterVp<-function(x){
  return(
    exp(
      (-5800.2206/(x+273.15))+1.3914993+(-0.048640239*(x+273.15))+(0.41764768*10^-4)*(x+273.15)^2+6.5459673*log(x+273.15)
    )
  )
}

####将时间间隔四舍五入至标准间隔####
fixTimeInterval<-function(x,invl,originTime="1970-01-01 00:00.00 UTC"){
  return(as.POSIXct(round(as.numeric(x)/invl)*iclassssssnvl,origin=originTime))
}


####计算极差在最大值中的占比####
rangeRatio<-function(x){
  return(1-(min(x)/max(x)))
}

####对数据框的逐列插补####
approxData<-function(data,colRange){
  for(i in colRange){
    data[,i]<-na.approx(data[,..i],na.rm = FALSE)
  }
  return(data)
}

####归一化处理####
normalize<-function(data,upper=1,lower=0,intercept=0){
  range<-range(data,na.rm = TRUE)
  return(intercept+abs(upper-lower)*(data-range[1])/(range[2]-range[1]))
}


####分类器验证函数####
predictTest<-function(testSet,resultValue,predictableModel,isOutput=FALSE,fileName="testSetEvaluation.txt"){
  predict<-predict(predictableModel,testSet)
  result<-confusionMatrix(table(predict,resultValue))
  if(isOutput){
    capture.output(result,file = fileName)
  }
  return(result)
}

