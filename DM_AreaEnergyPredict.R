####本脚本用于区域级别上的空调能耗率预测####
# 接data.hznu.area.predict.raw
#预测对象 fullOnRatio

#讲道理行为预测里面数据集预处理已经整了那么多了应该好多能直接拿来用了
# 直接在data.hznu.area.predict.use上进行预测

backup.hznu.area.predict.use<-data.hznu.area.predict.use

#检查一下signCheck.pickUp和data.hznu.area.predict.use数据集能不能直接cbind
nn<-data.table(datetimeUse=data.hznu.area.predict.use$datetime,datetimeSign=data.hznu.area.signCheck.pickup$datetime)
nn[datetimeUse!=datetimeSign]
# 数据是对齐的



data.hznu.area.predict.use<-cbind(data.hznu.area.predict.use,data.hznu.area.signCheck.pickup[,c("d0h1_modiElec","d1h0_modiElec",
                                                                                         "d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio",
                                                                                         "d7_lowEnergyRatio","d7_ltMeRatio","d7_forenoonRatio")])
#之前没归一化吗？？
View(data.hznu.area.predict.use[is.na(stdModiElec)&!is.na(modiElec)])

###重新归一化，切记最好应该统一处理！！
data.hznu.area.predict.use$stdModiElec<- -999
data.hznu.area.predict.use$d0h1_modiElecStd<- -999
data.hznu.area.predict.use$d1h0_modiElecStd<- -999
for(i in unique(data.hznu.area.predict.use$modiSeason)){
  data.hznu.area.predict.use[modiSeason==i]$stdModiElec<-normalize(data.hznu.area.predict.use[modiSeason==i,"modiElec"],upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$d0h1_modiElecStd<-normalize(data.hznu.area.predict.use[modiSeason==i,"d0h1_modiElec"],upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$d1h0_modiElecStd<-normalize(data.hznu.area.predict.use[modiSeason==i,"d1h0_modiElec"],upper = 0.9,lower = 0.1,intercept = 0.1)
}

data.hznu.area.predict.use[,c("stdModiElec","d0h1_modiElecStd","d1h0_modiElecStd")]<-
                                  data.hznu.area.predict.use[,c("stdModiElec","d0h1_modiElecStd","d1h0_modiElecStd")]%>%mutate_all(funs(ifelse(.==-999,NA,.)))

# nn[,c("a","b")]<-nn[,c("a","b")]%>%.[.[,apply(.SD, MARGIN = 1,
#           FUN = function(x){sum(is.na(x))})!=2],]%>% mutate_all(funs(ifelse(is.na(.), 0, .)))

####能耗时间序列构建####
fit.ts.hznu.energy<-ts(data.hznu.area.predict.use$modiElec,start = c(2017,1,1,8),frequency = 15) %>% ets(.,model="AAA")
data.hznu.area.predict.use$tsPredElec<-as.numeric(fit.ts.hznu.energy$fitted)
data.hznu.area.predict.use[tsPredElec<0]$tsPredElec<-0

#计算纯时间序列的相对误差
getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$tsPredElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#2.376676
RMSE(pred = data.hznu.area.predict.use$tsPredElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#45.48565
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-data.hznu.area.predict.use$tsPredElec)/data.hznu.area.predict.use$modiElec))
# 0.0002797346 0.1863871573 0.5508677115 1.0000000000 2.2199397329
getRSquare(pred = data.hznu.area.predict.use$tsPredElec,ref = data.hznu.area.predict.use$modiElec)#0.8243753
#可怕


####换一个ARIMA试试####
#检测是否是平稳序列
adf.test(data.hznu.area.predict.use$modiElec)
# > adf.test(data.hznu.area.predict.use$modiElec)
# 
# Augmented Dickey-Fuller Test
# 
# data:  data.hznu.area.predict.use$modiElec
# Dickey-Fuller = -7.5116, Lag order = 18, p-value = 0.01
# alternative hypothesis: stationary


#建立模型及预测
fit.hznu.energy.arima<-auto.arima(ts(data.hznu.area.predict.use$modiElec,start = c(2017,1,1,8),frequency = 15))
data.hznu.area.predict.use$arimaPredElec<-as.numeric(fit.hznu.energy.arima$fitted)
#效果评估
getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$arimaPredElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.566239
RMSE(pred = data.hznu.area.predict.use$arimaPredElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#42.90156
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-
                     data.hznu.area.predict.use$arimaPredElec)/data.hznu.area.predict.use$modiElec))# 0.0000000 0.1144587 0.2873544 0.7293574 1.6508176
getRSquare(pred = data.hznu.area.predict.use$arimaPredElec,ref = data.hznu.area.predict.use$modiElec)#0.852643
#也就这样...

backup.hznu.area.predict.log<-data.hznu.area.predict.log


####构建分季节能耗预测属性####
predictElecAttr<-list(constant=c("stdOutTemp","stdWeekday","isBizday","hour","svmIterPred","d0h1_modiElec"),
                      Winter=c("d1_lowEnergyRatio","d1_midEnergyRatio","d7_ltMeRatio"),
                      Winter_warm=c("stdRhOut",
                                     "d1_lowEnergyRatio","d1_ltMeRatio","d7_lowEnergyRatio","d7_ltMeRatio"),
                      Transition=c("d1h0_modiElec",
                                    "d1_midEnergyRatio"),
                      Summer_warm=c("stdRhOut",
                                     "d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio","d7_lowEnergyRatio"),
                      Summer=c("stdWindSpeed",
                                "d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio"))

data.hznu.area.predict.log<-data.hznu.area.predict.log[target!="modiElec"]
####采用随机森林方法建立基础预测并对变量进行筛选####
list.hznu.area.energyForest<-list()
data.hznu.area.predict.use$rfIdelElec<- -999
data.hznu.area.predict.use$rfRealElec<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for (j in 0:9) {
    for(k in c("real","ideal")){
      fullAttr<-c("outTemp","weekday","isBizday","hour","d0h1_modiElec",ifelse(k=="real","svmIterPred","fullOnRatio"),
                  "rhOut","windSpeed","d1h0_modiElec","d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio",
                  "d7_lowEnergyRatio","d7_ltMeRatio","d7_forenoonRatio")
      fit<-randomForest(as.formula( paste("modiElec ~ ",paste(fullAttr,collapse = "+") ) ),
                        data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..fullAttr])],
                        #ntree=1000,cp=localInitCP,
                        na.action = na.omit,importance=TRUE)
      list.hznu.area.energyForest[[i]][[k]][[as.character(j)]]<-fit
      if(k=="real"){
        data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..fullAttr])]$rfRealElec<-
          predict(fit,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..fullAttr])])
      }else{
        data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..fullAttr])]$rfIdelElec<-
          predict(fit,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..fullAttr])])
      }
      
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..fullAttr])]%>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                   target="modiElec",method=paste("rfElec","defTree",k,sep = "_"),setType="test",
                   round=j,predValue=pull(.,ifelse(k=="real","rfRealElec","rfIdelElec")),realValue=.$modiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      #单独对十折的结果进行储存，此处为训练集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..fullAttr])]%>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                   target="modiElec",method=paste("rfElec","defTree",k,sep = "_"),setType="train",
                   round=j,predValue=as.numeric(predict(fit,.)),realValue=.$modiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      
    }
  }
}
data.hznu.area.predict.use[rfIdelElec== -999]$rfIdelElec<-NA
data.hznu.area.predict.use[rfRealElec== -999]$rfRealElec<-NA
getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$rfIdelElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.709286 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$rfIdelElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#33.24661
getRSquare(pred = data.hznu.area.predict.use$rfIdelElec,ref = data.hznu.area.predict.use$modiElec)#0.9135975

####汇总随机森林得出的重要性####
stat.hznu.area.predict.energyFactor<-data.table(type="ideal/real",modiSeason="",round=-999,varName="",IncMSE=-999,IncNodePurity=-999)[-1]
for(season in names(list.hznu.area.energyForest)){
  for(type in names(list.hznu.area.energyForest[[season]])){
    for(round in names(list.hznu.area.energyForest[[season]][[type]])){
      stat.hznu.area.predict.energyFactor<-rbind(stat.hznu.area.predict.energyFactor,
                                                 importance(list.hznu.area.energyForest[[season]][[type]][[round]])%>%
                                                   data.table(type=type,modiSeason=season,round=round,varName=rownames(.),IncMSE=.[,1],IncPur=.[,2])%>%
                                                   .[,on=c(colnames(stat.hznu.area.predict.energyFactor))])
                                            
    }
  }
}


# fit<-train(form=as.formula( paste("modiElec ~ ",paste(fullAttr,collapse = "+") ) ),
#            na.action = "na.fail",
#            data=data.hznu.area.predict.use[modiSeason==i]%>%.[complete.cases(.[,..fullAttr])],method = "rf",importance=TRUE,#mtry=3,ntree=200,
#            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))#ntree能传进不知道能不能调



#马后炮定义一下
# data.hznu.area.predict.log[target=="modiElec"]$method<-paste(data.hznu.area.predict.log[target=="modiElec"]$method,"Ideal",sep = "")
# names(data.hznu.area.predict.use)[53:60]<-c("knnPredElecIdeal","simpleKnnPredElecIdeal","stdElecErrKnnIdeal",
#                                        "h1_stdElecErrKnnIdeal","svmInitElecPredIdeal","stdErrSvmElecIdeal","h1_stdErrSvmElecIdeal","svmIterPredElecIdeal")

data.hznu.area.predict.use$knnPredElec<- -999
data.hznu.area.predict.use$simpleKnnPredElec<- -999


for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    #根据id mod 10 来确定交叉验证的划分
    seasonalAttr<-c(predictElecAttr[["constant"]],predictElecAttr[[i]])#,
    fit.kknn<-kknn(formula = as.formula( paste("modiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                   kernel = "optimal",k=3,na.action = na.exclude,
                   train = data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                   test = data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]
    )
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$knnPredElec<-fit.kknn$fitted.values
    
    #不太放心，单独存一个
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
                                data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                                                 target="modiElec",method="knnPredElec",setType="test",
                                                 round=j,predValue=.$knnPredElec,realValue=.$modiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
  }
}



data.hznu.area.predict.use[knnPredElec== -999]$knnPredElec<-NA
data.hznu.area.predict.use[simpleKnnPredElec== -999]$simpleKnnPredElec<- -NA

##测一下fullKNN的k值
#确实变量一多了就不能取多
ggplot(data = data.table(id=1:nrow(fit.kknn$D),fit.kknn$D) %>% melt(.,id.var="id"),aes(x=variable,y=value,color=id,group=id))+geom_point()+geom_line()

#考虑一下knn的误差
#以下为理想值
getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$knnPredElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.709286 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$knnPredElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#33.24661
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-
                     data.hznu.area.predict.use$knnPredElec)/data.hznu.area.predict.use$modiElec))#0.00000000 0.08039036 0.18758149 0.41816919 0.92396363
getRSquare(pred = data.hznu.area.predict.use$knnPredElec,ref = data.hznu.area.predict.use$modiElec)#0.9135975



#以下为使用增强SVM行为的完整knn预测值
# MAPE 1.987696
# RMSE 45.95319
# R-Square 0.8112507

#考虑一下简化knn的基本误差
getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$simpleKnnPredElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.41261
RMSE(pred = data.hznu.area.predict.use$simpleKnnPredElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#26.15484
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-
                     data.hznu.area.predict.use$simpleKnnPredElec)/data.hznu.area.predict.use$modiElec))#9.185365e-05 9.497209e-02 2.405226e-01 6.035953e-01 1.366439e+00
getRSquare(pred = data.hznu.area.predict.use$simpleKnnPredElec,ref = data.hznu.area.predict.use$modiElec)#0.9405694
#以下为使用增强SVM行为的简化knn预测值
# MAPE 1.829155
# RMSE 40.20034
# R-Square 0.8538488


####SVM####

data.hznu.area.predict.use$stdElecErrKnn<-(data.hznu.area.predict.use$simpleKnnPredElec-data.hznu.area.predict.use$modiElec) %>%
  normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use$h1_stdElecErrKnn<-c(NA,data.hznu.area.predict.use[2:nrow(data.hznu.area.predict.use)-1]$stdElecErrKnn)

data.hznu.area.predict.use$svmInitElecPred<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    seasonalAttr<-c(predictElecAttr[["constant"]],predictElecAttr[[i]],"simpleKnnPredElec","h1_stdElecErrKnn")
    fit.svm<-ksvm(x=as.formula( paste("modiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmInitElecPred<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
    
    #单独对十折的结果进行储存，此处为预测集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="modiElec",method="svmInitPred",setType="test",
                 round=j,predValue=.$svmInitElecPred,realValue=.$modiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
    #单独对十折的结果进行储存，此处为训练集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])] %>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="modiElec",method="svmInitPred",setType="train",
                 round=j,predValue=as.numeric(predict(fit.svm,.)),realValue=.$modiElec) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
    
  }
}
data.hznu.area.predict.use[svmInitElecPred== -999]$svmInitElecPred<-NA

getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$svmInitElecPred, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#0.8568157
RMSE(pred = data.hznu.area.predict.use$svmInitElecPred,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#25.15353
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-data.hznu.area.predict.use$svmInitElecPred)/data.hznu.area.predict.use$modiElec))
#3.503135e-05 8.621326e-02 2.039386e-01 4.602290e-01 1.016743e+00
getRSquare(pred = data.hznu.area.predict.use$svmInitElecPred,ref = data.hznu.area.predict.use$modiElec)#0.944896

#以下为使用增强SVM行为的SVM预测值
# MAPE 1.916767
# RMSE 38.65189
# R-Square 0.8452484


####能耗预测SVM增强####
#增加SVM初始预测误差作为输入

data.hznu.area.predict.use$stdErrSvmElec<-(data.hznu.area.predict.use$svmInitElecPred-data.hznu.area.predict.use$modiElec) %>%
  normalize(.,upper = 0.9,lower = 0.1,intercept = 0.1)
data.hznu.area.predict.use$h1_stdErrSvmElec<-c(NA,data.hznu.area.predict.use[2:nrow(data.hznu.area.predict.use)-1]$stdErrSvmElec)

data.hznu.area.predict.use$svmIterPredElec<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in 0:9){
    seasonalAttr<-c(predictUsageAttr[["constant"]],predictUsageAttr[[i]],"simpleKnnPredElec","h1_stdElecErrKnn","svmInitElecPred","h1_stdErrSvmElec")
    fit.svm<-ksvm(x=as.formula( paste("modiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                  data=data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])],
                  kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
    data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]$svmIterPredElec<-
      predict(fit.svm,data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])])
    
    #单独对十折的结果进行储存，此处为预测集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10==j&modiSeason==i,..seasonalAttr])]%>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="modiElec",method="svmIterPred",setType="test",
                 round=j,predValue=.$svmIterPredElec,realValue=.$modiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    
    #单独对十折的结果进行储存，此处为训练集
    data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..seasonalAttr])] %>%
      data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                 target="modiElec",method="svmIterPred",setType="train",
                 round=j,predValue=as.numeric(predict(fit.svm,.)),realValue=.$modiElec) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
  }
}
data.hznu.area.predict.use[svmIterPredElec== -999]$svmIterPredElec<-NA

getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$svmIterPredElec, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#0.6399979
RMSE(pred = data.hznu.area.predict.use$svmIterPredElec,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#25.45938
boxplot.stats(abs((data.hznu.area.predict.use$modiElec-
                     data.hznu.area.predict.use$svmIterPredElec)/data.hznu.area.predict.use$modiElec))#6.729713e-05 5.901312e-02 1.363222e-01 3.212818e-01 7.135150e-01
getRSquare(pred = data.hznu.area.predict.use$svmIterPredElec,ref = data.hznu.area.predict.use$modiElec)#0.9401302

#以下为使用增强SVM行为的增强SVM能耗预测值
# MAPE 1.785482
# RMSE 38.09734
# R-Square 0.8368258





ggplot(data=data.hznu.area.predict.use[substr(datetime,1,9)=="2017-06-1",c("datetime","modiElec","knnPredElec","simpleKnnPredElec","svmInitElecPred","svmIterPredElec")] %>% 
         melt(.,id.var=c("datetime")),
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable))+geom_line(size=0.7)+geom_point(size=2)+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.12,0.88))#88，12

####统计一下不同目标和不同工况下的准确率####
stat.hznu.area.predict.eva<-data.table(target="",modiSeason="",method="",setType="",
                                       count=-999,mape=-999,rmse=-999,rSquare=-999)[-1]

data.hznu.area.predict.log[target=="modiElec"] %>% 
  {
  for(i in unique(.$modiSeason)){
    for(j in unique(.[modiSeason==i]$method)){
      for(k in unique(.[modiSeason==i&method==j]$setType)){
        stat.hznu.area.predict.eva<<-
          rbind(stat.hznu.area.predict.eva,data.table(target="modiElec",modiSeason=i,method=j,
                                                      setType=k,
                                                      count=nrow(.[modiSeason==i&method==j&setType==k]),
                                                      mape=getMAPE(yPred = .[modiSeason==i&method==j&setType==k&realValue!=0]$predValue, 
                                                                 yLook = .[modiSeason==i&method==j&setType==k&realValue!=0]$realValue),
                                                      rmse=RMSE(pred = .[modiSeason==i&method==j&setType==k]$predValue,
                                                              obs = .[modiSeason==i&method==j&setType==k]$realValue,na.rm = TRUE),
                                                      rSquare=getRSquare(pred = .[modiSeason==i&method==j&setType==k]$predValue,
                                                                       ref = .[modiSeason==i&method==j&setType==k]$realValue)))
      }
    }
  }
}
ggplot(data=stat.hznu.area.predict.eva,aes(x=modiSeason,y=rSquare,group=method,color=method))+geom_line()+geom_point()+facet_wrap(~setType,ncol = 2)
  
  
####绘图输出####
ggplot(data=data.hznu.area.predict.use[date %in% paperTime$Summer_warm,c("datetime","weekCount","weekday","modiSeason","modiElec","rfIdelElec","rfRealElec")] %>% #,"simpleKnnFullOnRatio","svmInitPred","svmIterPred","knnFullOnRatio","tsFullOnRatio"
         mutate(.,year=substr(datetime,1,4),date=date(datetime))%>% melt(.,id.var=c("datetime","modiSeason","year","date","weekday","weekCount")),
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable,group=paste(date,variable)))+geom_line(size=0.7)+geom_point(size=2)+facet_wrap(~modiSeason,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))


