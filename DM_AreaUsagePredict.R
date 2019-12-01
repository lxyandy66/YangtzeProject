####本脚本用于区域级别上的空调使用率预测####
# 接data.hznu.area.predict.raw
#预测对象 fullOnRatio

data.hznu.area.predict.use<-merge(x=data.hznu.area.predict.raw[,c("date","datetime","buildingCount","buildingCountEnergy",
                                                                     "fullOnRatio","modiElec",
                                                                     "outTemp","rhOut","windSpeed","isWeekday","isBizday")],
                                     y=data.hznu.area.signCheck[,c("datetime","weekday","stdModiElec","modiSeason",
                                                                   "d0h1_FullOnRatio","d1h0_FullOnRatio","d7h0_FullOnRatio",
                                                                   "d1_onDemandRatio","d1_forenoonRatio","d1_afternoonRatio","d1_daytimeRatio","d1_lateDaytimeRatio",
                                                                   "d7_onDemandRatio","d7_daytimeRatio","d7_afternoonRatio","d7_lateDaytimeRatio")],
                                     all.x = TRUE,by.x="datetime",by.y="datetime") %>% .[substr(date,1,4)=="2017"|substr(date,1,7)=="2018-01"]


for(i in unique(data.hznu.area.predict.use$modiSeason)){
  cat("\nbox stat in ",i,": ",boxplot.stats(data.hznu.area.predict.use[modiSeason==i]$fullOnRatio)$stats)#0.000000000 0.006578947 median=0.026402640 0.072816246 0.172000000
  cat("\nmean in ",i,": ",mean(data.hznu.area.predict.use[modiSeason==i]$fullOnRatio))#0.05149866
}

####按季节归一化对象###
data.hznu.area.predict.use$hour<-hour(data.hznu.area.predict.use$datetime) %>% normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use<-mutate(data.hznu.area.predict.use,stdOutTemp= -999,stdRhOut= -999,stdWindSpeed= -999,stdWeekday= -999)
data.hznu.area.predict.use$stdWeekday<-normalize(data.hznu.area.predict.use$weekday,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use<-as.data.table(data.hznu.area.predict.use)#很奇怪会莫名降级
for(i in unique(data.hznu.area.predict.use$modiSeason)){
  #这么写真难看...用within也可以
  data.hznu.area.predict.use[modiSeason==i]$stdOutTemp<-normalize(data.hznu.area.predict.use[modiSeason==i]$outTemp,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$stdRhOut<-normalize(data.hznu.area.predict.use[modiSeason==i]$rhOut,upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$stdWindSpeed<-normalize(data.hznu.area.predict.use[modiSeason==i]$windSpeed,upper = 0.9,lower = 0.1,intercept = 0.1)
}

####时间序列构建####
# ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,01,01),frequency = 15)
fit.ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,1,1,8),frequency = 15) %>% ets(.,model="AAA")
data.hznu.area.predict.use$tsFullOnRatio<-as.numeric(fit.ts.hznu.usage$fitted)
data.hznu.area.predict.use[tsFullOnRatio<0]$tsFullOnRatio<-0
#计算纯时间序列的相对误差
data.hznu.area.predict.use$rlatTsErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$tsFullOnRatio)/data.hznu.area.predict.use$fullOnRatio)
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$tsFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$tsFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#RMSE大概0.022
boxplot.stats(data.hznu.area.predict.use$rlatTsErr)#4.578777e-05 1.217521e-01 3.031872e-01 8.384903e-01 1.903394e+00
mean(data.hznu.area.predict.use[!is.infinite(rlatTsErr)]$rlatTsErr,na.rm = TRUE)#0.5800756
getRSquare(pred = data.hznu.area.predict.use$tsFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.8631175



####试一试KNN####
#增加一个ID行号便于十折

data.hznu.area.predict.use<-as.data.table(cbind(id=1:nrow(data.hznu.area.predict.use),data.hznu.area.predict.use)) %>% 
                            mutate(.,knnFullOnRatio=-999,simpleKnnFullOnRatio= -999)

# "stdOutTemp","stdWeekday","isBizday","hour","d0h1_FullOnRatio"

c("datetime","weekday","stdModiElec","modiSeason",
  "d0h1_FullOnRatio","d1h0_FullOnRatio","d7h0_FullOnRatio",
  "d1_onDemandRatio","d1_forenoonRatio","d1_afternoonRatio","d1_daytimeRatio","d1_lateDaytimeRatio",
  "d7_onDemandRatio","d7_daytimeRatio","d7_afternoonRatio","d7_lateDaytimeRatio")

predictUsageAttr<-list(simpleKnn=c("stdOutTemp","stdWeekday","isBizday","hour"),
                       constant=c("stdOutTemp","stdWeekday","isBizday","hour","d0h1_FullOnRatio"),
                       Winter=c("d1h0_FullOnRatio","d1_onDemandRatio","d1_forenoonRatio","d1_daytimeRatio","d7_onDemandRatio","d7_daytimeRatio"),
                       Winter_warm=c("stdRhOut","d7h0_FullOnRatio","d1_onDemandRatio","d1_afternoonRatio","d1_daytimeRatio","d7_onDemandRatio","d7_afternoonRatio"),
                       Transition=c("stdRhOut","d1h0_FullOnRatio","d1_onDemandRatio","d1_afternoonRatio","d1_daytimeRatio","d1_lateDaytimeRatio","d7_onDemandRatio","d7_afternoonRatio"),
                       Summer_warm=c("d7h0_FullOnRatio","d1_lateDaytimeRatio","d7_daytimeRatio","d1_lateDaytimeRatio"),
                       Summer=c("stdWindSpeed","d1h0_FullOnRatio","d7_onDemandRatio","d7_daytimeRatio"))
for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    #根据id mod 10 来确定交叉验证的划分
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]])#predictUsageAttr[["simpleKnn"]]
    fit.kknn<-kknn(formula = as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  kernel = "optimal",k=10,na.action = na.exclude,
                  train = data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  test = data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]
                  )
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$simpleKnnFullOnRatio<-fit.kknn$fitted.values
  }
}
data.hznu.area.predict.use[knnFullOnRatio== -999]$knnFullOnRatio<-NA
data.hznu.area.predict.use[simpleKnnFullOnRatio== -999]$simpleKnnFullOnRatio<-NA
#考虑一下knn的基本误差
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$knnFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.354
data.hznu.area.predict.use$rlatKnnErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$knnFullOnRatio)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$knnFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.01812446
boxplot.stats(data.hznu.area.predict.use$rlatKnnErr)#0.00000000 0.07528927 0.17103858 0.38354152 0.84391688
mean(data.hznu.area.predict.use[!is.infinite(rlatKnnErr)]$rlatKnnErr,na.rm = TRUE)#0.3537018
getRSquare(pred = data.hznu.area.predict.use$knnFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.9071937

#考虑一下简化knn的基本误差
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$simpleKnnFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.942
data.hznu.area.predict.use$rlatSimpleKnnErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$simpleKnnFullOnRatio)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$simpleKnnFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.0359
boxplot.stats(data.hznu.area.predict.use$rlatSimpleKnnErr)#6.381677e-05 2.090334e-01 4.396562e-01 7.437810e-01 1.543582e+00
mean(data.hznu.area.predict.use[!is.infinite(rlatSimpleKnnErr)]$rlatSimpleKnnErr,na.rm = TRUE)#0.552
getRSquare(pred = data.hznu.area.predict.use$simpleKnnFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.485979


ggplot(data = data.hznu.area.predict.use,aes(y=rlatTsErr))+geom_boxplot()+ylim(0,2)
ggplot(data=data.hznu.area.predict.use[substr(datetime,1,7)=="2017-12",c("datetime","fullOnRatio","tsFullOnRatio","knnFullOnRatio","simpleKnnFullOnRatio")] %>% 
         mutate(.,year=substr(datetime,1,4))%>% melt(.,id.var=c("datetime","year")),
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable))+geom_line(size=0.7)+geom_point(size=2)+facet_wrap(~year,nrow = 2)


####SVM进行预测####

#增加辅助变量
#与KNN预测误差
data.hznu.area.predict.use$stdErrKnn<-(data.hznu.area.predict.use$knnFullOnRatio-data.hznu.area.predict.use$fullOnRatio) %>%
                                      normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use$h1_stdErrKnn<-c(NA,data.hznu.area.predict.use[2:nrow(data.hznu.area.predict.use)-1]$stdErrKnn)

data.hznu.area.predict.use$svmInitPred<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 1:10){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"knnFullOnRatio","h1_stdErrKnn")
    fit.svm<-ksvm(x=as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmInitPred<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
    }
}
data.hznu.area.predict.use[svmInitPred== -999]$svmInitPred<-NA

getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$svmInitPred, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.3011952
data.hznu.area.predict.use$rlatSvmInitErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$svmInitPred)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$svmInitPred,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.01751585
boxplot.stats(data.hznu.area.predict.use$rlatSvmInitErr)#1.783526e-05 6.732056e-02 1.567721e-01 3.565884e-01 7.860812e-01
mean(data.hznu.area.predict.use[!is.infinite(rlatSvmInitErr)]$rlatSvmInitErr,na.rm = TRUE)#0.3011952
getRSquare(pred = data.hznu.area.predict.use$svmInitPred,ref = data.hznu.area.predict.use$fullOnRatio)#0.9202754

# 无knn参考值：
# MAPE 0.3057088
# RMSE 0.01857487
# BoxRlat 3.839995e-05 6.790941e-02 1.569510e-01 3.647188e-01 8.025477e-01
# meanRlat 0.3057
# R^2 0.9097206

#增加SVM初始预测误差作为输入
data.hznu.area.predict.use$stdErrSvm<-(data.hznu.area.predict.use$svmInitPred-data.hznu.area.predict.use$fullOnRatio) %>%
                                        normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use$h1_stdErrSvm<-c(NA,data.hznu.area.predict.use[2:nrow(data.hznu.area.predict.use)-1]$stdErrSvm)

data.hznu.area.predict.use$svmIterPred<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 1:10){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"knnFullOnRatio","h1_stdErrKnn","h1_stdErrSvm")
    fit.svm<-ksvm(x=as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmIterPred<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
  }
}
data.hznu.area.predict.use[svmIterPred== -999]$svmIterPred<-NA

getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$svmIterPred, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.295139
data.hznu.area.predict.use$rlatSvmIterErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$svmIterPred)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$svmIterPred,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.01651134
boxplot.stats(data.hznu.area.predict.use$rlatSvmIterErr)#9.918532e-05 6.631196e-02 1.568308e-01 3.516293e-01 7.795555e-01
mean(data.hznu.area.predict.use[!is.infinite(rlatSvmIterErr)]$rlatSvmIterErr,na.rm = TRUE)# 0.295139
getRSquare(pred = data.hznu.area.predict.use$svmIterPred,ref = data.hznu.area.predict.use$fullOnRatio)#0.9267991


