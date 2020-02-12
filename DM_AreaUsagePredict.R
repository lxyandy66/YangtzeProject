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

data.hznu.area.predict.use$weekCount<-isoweek(data.hznu.area.predict.use$datetime)

####大论文用时间筛选####
paperTime<-list(Summer_warm=c(sprintf("2017-09-%02d",4:17)),
                Summer=c(sprintf("2017-07-%02d",3:16)),
                Winter_warm=c(sprintf("2017-12-%02d",11:24)),
                Winter=c(sprintf("2018-01-%02d",15:28))
                )

####时间序列构建####
# ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,01,01),frequency = 15)
data.hznu.area.predict.use$tsRealFullOnRatio<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,1,1,8),frequency = 15)
# fit.ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,1,1,8),frequency = 15) %>% ets(.,model="AAA")
fit.ts.hznu.usage<-data.hznu.area.predict.use$tsRealFullOnRatio %>% ets(.)#ETS(A,Ad,A) 
data.hznu.area.predict.use$tsFullOnRatio<-as.numeric(fit.ts.hznu.usage$fitted)
data.hznu.area.predict.use[tsFullOnRatio<0]$tsFullOnRatio<-0

#季节性因素分解
stl(ts(data.hznu.area.predict.use[date %in% paperTime$Winter]$fullOnRatio,start = c(2017,1,1,8),frequency = 15), s.window="period") %>% plot(.)


####大论文用时间序列效果评估####
data.hznu.area.predict.use[date %in% paperTime$Winter] %>% {
  cat("MAPE\t",getMAPE(yPred = .[fullOnRatio!=0]$simpleKnnFullOnRatio, yLook = .[fullOnRatio!=0]$fullOnRatio))#0.5800756
  cat("\nRMSE\t",RMSE(pred = .$simpleKnnFullOnRatio,obs = .$fullOnRatio,na.rm = TRUE))#0.022
  cat("\nRSquare\t",getRSquare(pred = .$simpleKnnFullOnRatio,ref = .$fullOnRatio))#0.8631175
}
# tsFullOnRatio,simpleKnnFullOnRatio,knnFullOnRatio

#计算纯时间序列的相对误差
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$tsFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.5800756
RMSE(pred = data.hznu.area.predict.use$tsFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.022
boxplot.stats(data.hznu.area.predict.use$rlatTsErr)#4.578777e-05 1.217521e-01 3.031872e-01 8.384903e-01 1.903394e+00
mean(data.hznu.area.predict.use[!is.infinite(rlatTsErr)]$rlatTsErr,na.rm = TRUE)#0.5800756
getRSquare(pred = data.hznu.area.predict.use$tsFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.8631175

####增加ARIMA的预测####
fit.hznu.usage.arima<-auto.arima(data.hznu.area.predict.use$tsRealFullOnRatio)
data.hznu.area.predict.use$arimaFullOnRatio<-as.numeric(fit.hznu.usage.arima$fitted)
data.hznu.area.predict.use[arimaFullOnRatio<0]$arimaFullOnRatio<-0
#ARIMA效果评估
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$arimaFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.4144662
RMSE(pred = data.hznu.area.predict.use$arimaFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.01969339
boxplot.stats(with(data.hznu.area.predict.use,{abs((fullOnRatio-arimaFullOnRatio)/fullOnRatio)}))#5.580084e-08 8.770985e-02 2.182933e-01 5.741867e-01 1.300477e+00
getRSquare(pred = data.hznu.area.predict.use$arimaFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.8954467


#时刻记得是否一不留神重复了或者多出了行，尤其是需要针对必须唯一的key
data.hznu.area.predict.use[duplicated(data.hznu.area.predict.use$datetime)]

####对于十折的结果进行单独储存####
backup.hznu.area.predict.log<-data.hznu.area.predict.log
data.hznu.area.predict.log<-data.table(id=-999,datetime=as.POSIXct("2019-01-09"),modiSeason="modiSeason",
                                       target="targetResult,fullOnRatio,etc.",method="knn/svm/etc",setType="train/test",
                                       round=-999,predValue=-999,realValue=-999)[-1]
archieveItem<-names(data.hznu.area.predict.log)

####试一试KNN####
#增加一个ID行号便于十折

data.hznu.area.predict.use<-as.data.table(cbind(id=1:nrow(data.hznu.area.predict.use),data.hznu.area.predict.use)) %>% 
                            mutate(.,knnFullOnRatio=-999,simpleKnnFullOnRatio= -999)


data.hznu.area.predict.use$knnFullOnRatio<- -999
# "stdOutTemp","stdWeekday","isBizday","hour","d0h1_FullOnRatio"


predictUsageAttr<-list(constant=c("stdOutTemp","stdWeekday","isBizday","hour","d0h1_FullOnRatio"),
                       Winter=c("d1h0_FullOnRatio","d1_onDemandRatio","d1_forenoonRatio","d1_daytimeRatio","d7_onDemandRatio","d7_daytimeRatio"),
                       Winter_warm=c("stdRhOut","d7h0_FullOnRatio","d1_onDemandRatio","d1_afternoonRatio","d1_daytimeRatio","d7_onDemandRatio","d7_afternoonRatio"),
                       Transition=c("stdRhOut","d1h0_FullOnRatio","d1_onDemandRatio","d1_afternoonRatio","d1_daytimeRatio","d1_lateDaytimeRatio","d7_onDemandRatio","d7_afternoonRatio"),
                       Summer_warm=c("d7h0_FullOnRatio","d1_lateDaytimeRatio","d7_daytimeRatio","d1_lateDaytimeRatio"),
                       Summer=c("stdWindSpeed","d1h0_FullOnRatio","d7_onDemandRatio","d7_daytimeRatio"))

data.hznu.area.predict.use<-as.data.table(data.hznu.area.predict.use)
# cat( system.time(
for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    #根据id mod 10 来确定交叉验证的划分
    seasonalAttr<-c(predictUsageAttr[["constant"]])#,predictUsageAttr[[i]],predictUsageAttr[[i]]
    fit.kknn<-kknn(formula = as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  kernel = "optimal",k=10,na.action = na.exclude,
                  train = data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  test = data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]
                  )
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$simpleKnnFullOnRatio<-fit.kknn$fitted.values
    #单独对十折的结果进行储存
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="fullOnRatio",method="simpleKnn",setType="test",
                 round=j,predValue=.$knnFullOnRatio,realValue=.$fullOnRatio)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
  }
}
# ) )
data.hznu.area.predict.use[knnFullOnRatio== -999]$knnFullOnRatio<-NA
data.hznu.area.predict.use[simpleKnnFullOnRatio== -999]$simpleKnnFullOnRatio<-NA
#考虑一下knn的基本误差
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$knnFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)# 0.3738909
RMSE(pred = data.hznu.area.predict.use$knnFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.0195318
getRSquare(pred = data.hznu.area.predict.use$knnFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.8899948

#考虑一下简化knn的基本误差
getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$simpleKnnFullOnRatio, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.942
data.hznu.area.predict.use$rlatSimpleKnnErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$simpleKnnFullOnRatio)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$simpleKnnFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.0359
boxplot.stats(data.hznu.area.predict.use$rlatSimpleKnnErr)#6.381677e-05 2.090334e-01 4.396562e-01 7.437810e-01 1.543582e+00
mean(data.hznu.area.predict.use[!is.infinite(rlatSimpleKnnErr)]$rlatSimpleKnnErr,na.rm = TRUE)#0.942
getRSquare(pred = data.hznu.area.predict.use$simpleKnnFullOnRatio,ref = data.hznu.area.predict.use$fullOnRatio)#0.485979

# constKnn
# MAPE 0.4097041
# RMSE 0.02042152
# box.stat NA
# R-square 0.8793941


####SVM进行预测####

#增加辅助变量
#与KNN预测误差
#保存一下knn的预测数据
backup.hznu.area.predict.log<-data.hznu.area.predict.log

# data.hznu.area.predict.log<-backup.hznu.area.predict.log #复位
data.hznu.area.predict.use$errBase<-(data.hznu.area.predict.use$simpleKnnFullOnRatio-data.hznu.area.predict.use$fullOnRatio)

#Error in .$simpleKnnFullOnRatio : 类别为'closure'的对象不可以取子集 #?????

#其实不严谨
data.hznu.area.predict.use$h1_errBase<-c(NA,data.hznu.area.predict.use[2:nrow(data.hznu.area.predict.use)-1]$errBase)

data.hznu.area.predict.use$svmInitPred<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"simpleKnnFullOnRatio","h1_errBase")
    fit.svm<-ksvm(x=as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmInitPred<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
    
    #单独对十折的结果进行储存，此处为预测集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="fullOnRatio",method="svmInitPred",setType="test",
                 round=j,predValue=.$svmInitPred,realValue=.$fullOnRatio)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
    #单独对十折的结果进行储存，此处为训练集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])] %>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="fullOnRatio",method="svmInitPred",setType="train",
                 round=j,predValue=as.numeric(predict(fit.svm,.)),realValue=.$fullOnRatio) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      
    
    }
}
data.hznu.area.predict.use[svmInitPred== -999]$svmInitPred<-NA

getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$svmInitPred, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.3694527
RMSE(pred = data.hznu.area.predict.use$svmInitPred,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.0210118
getRSquare(pred = data.hznu.area.predict.use$svmInitPred,ref = data.hznu.area.predict.use$fullOnRatio)#0.8817152

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
  for(j in 0:9){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"knnFullOnRatio","h1_stdErrKnn","svmInitPred","h1_stdErrSvm")
    fit.svm<-ksvm(x=as.formula( paste("fullOnRatio ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmIterPred<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
    
    #单独对十折的结果进行储存，此处为预测集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="fullOnRatio",method="svmIterPred",setType="test",
                 round=j,predValue=.$svmIterPred,realValue=.$fullOnRatio)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
    #单独对十折的结果进行储存，此处为训练集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])] %>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="fullOnRatio",method="svmIterPred",setType="train",
                 round=j,predValue=as.numeric(predict(fit.svm,.)),realValue=.$fullOnRatio) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
  }
}
data.hznu.area.predict.use[svmIterPred== -999]$svmIterPred<-NA

getMAPE(yPred = data.hznu.area.predict.use[fullOnRatio!=0]$svmIterPred, yLook = data.hznu.area.predict.use[fullOnRatio!=0]$fullOnRatio)#0.2677874
data.hznu.area.predict.use$rlatSvmIterErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$svmIterPred)/data.hznu.area.predict.use$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$svmIterPred,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#0.01462666
boxplot.stats(data.hznu.area.predict.use$rlatSvmIterErr)#6.729713e-05 5.901312e-02 1.363222e-01 3.212818e-01 7.135150e-01
getRSquare(pred = data.hznu.area.predict.use$svmIterPred,ref = data.hznu.area.predict.use$fullOnRatio)#0.9417149

无svmInitPred参考
# MAPE 0.295139
# RMSE 0.01651134
# BoxRlat 9.918532e-05 6.631196e-02 1.568308e-01 3.516293e-01 7.795555e-01
# meanRlat 0.295139
# R^2 0.9267991

backup.hznu.area.predict.log<-data.hznu.area.predict.log[target!="modiElec"]

####绘图输出####
ggplot(data = data.hznu.area.predict.use,aes(y=rlatTsErr))+geom_boxplot()+ylim(0,2)
ggplot(data=data.hznu.area.predict.use[date %in% paperTime$Winter_warm,c("datetime","weekCount","weekday","modiSeason","fullOnRatio","simpleKnnFullOnRatio")] %>% #,"svmIterPred",,"svmInitPred","svmIterPred","knnFullOnRatio","tsFullOnRatio"
         mutate(.,year=substr(datetime,1,4),date=date(datetime))%>% melt(.,id.var=c("datetime","modiSeason","year","date","weekday","weekCount")),
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable,group=paste(date,variable)))+geom_line(size=0.7)+geom_point(size=2)+facet_wrap(~modiSeason,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.8))
#一定注意这个group参数，很有用，此处直接将分组变为了日期与方法






