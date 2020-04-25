####本脚本用于区域级别上的空调能耗率预测####
# 接data.hznu.area.predict.raw
#预测对象 fullOnRatio

#讲道理行为预测里面数据集预处理已经整了那么多了应该好多能直接拿来用了
# 直接在data.hznu.area.predict.use上进行预测

backup.hznu.area.predict.use<-data.hznu.area.predict.use
backup.hznu.area.predict.log<-data.hznu.area.predict.log#20200418 考虑全房间的温度

#检查一下signCheck.pickUp和data.hznu.area.predict.use数据集能不能直接cbind
nn<-data.table(datetimeUse=data.hznu.area.predict.use$datetime,datetimeSign=data.hznu.area.signCheck.pickup$datetime)
nn[datetimeUse!=datetimeSign]
# 数据是对齐的



data.hznu.area.predict.use<-cbind(data.hznu.area.predict.use,data.hznu.area.signCheck.pickup[,c("d0h1_modiElec","d0h2_modiElec",
                                                                                                "d1h0_modiElec","d1h1_modiElec","d1h2_modiElec",
                                                                                                "d2h0_modiElec","d2h1_modiElec","d2h2_modiElec",
                                                                                                "d7h0_modiElec","d7h1_modiElec","d7h2_modiElec",
                                                                                         "d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio","d1_ltHeRatio",
                                                                                         "d7_lowEnergyRatio","d7_midEnergyRatio","d7_ltMeRatio","d7_ltHeRatio",
                                                                                         "d0h1_modiTemp","d0h1_setTemp","d0h1_setTempStd","d0h1_modiTempStd")])



data.hznu.area.predict.use[,c("rfIdelElecDeNorm","rfRealElecDeNorm","rfIdelElec","rfRealElec",
                              "h1_errRfIdelBase","h1_errRfRealBase","h1_errSvmInitIdel","h1_errSvmInitReal",
                              "svmInitIdeaElec","svmInitRealElec","svmInitIdeaElecDeNorm","svmInitRealElecDeNorm",
                              "svmIterIdeaElec","svmIterRealElec","svmIterIdeaElecDeNorm","svmIterRealElecDeNorm")]<-NULL

#之前没归一化吗？？
data.hznu.area.predict.use[is.na(stdModiElec)&!is.na(modiElec)]

###重新归一化，切记最好应该统一处理！！
data.hznu.area.predict.use[,c("d0h1_modiTempStd","d0h1_setTempStd",
                              paste(c("d0h1","d0h2",
                                      "d1h0","d1h1","d1h2",
                                      "d2h0","d2h1","d2h2",
                                      "d7h0","d7h1","d7h2"),"_modiElecStd",sep = ""))]<--999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  data.hznu.area.predict.use[modiSeason==i]$stdModiElec<-normalize(data.hznu.area.predict.use[modiSeason==i,"modiElec"],
                                                                   upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$d0h1_modiTempStd<-normalize(data.hznu.area.predict.use[modiSeason==i,"d0h1_modiTemp"],
                                                                      upper = 0.9,lower = 0.1,intercept = 0.1)
  data.hznu.area.predict.use[modiSeason==i]$d0h1_setTempStd<-normalize(data.hznu.area.predict.use[modiSeason==i,"d0h1_setTemp"],
                                                                        upper = 0.9,lower = 0.1,intercept = 0.1)
  for(j in c(paste(c("d0h1","d0h2",
                     "d1h0","d1h1","d1h2",
                     "d2h0","d2h1","d2h2",
                     "d7h0","d7h1","d7h2"),"_modiElec",sep = ""))){
    data.hznu.area.predict.use[modiSeason==i,c(paste(j,"Std",sep=""))]<-normalize(data.hznu.area.predict.use[modiSeason==i,..j],upper = 0.9,lower = 0.1,intercept = 0.1)
  }
}

data.hznu.area.predict.use[,c("d0h1_modiTempStd","d0h1_setTempStd",
                              paste(c("d0h1","d0h2",
                                      "d1h0","d1h1","d1h2",
                                      "d2h0","d2h1","d2h2",
                                      "d7h0","d7h1","d7h2"),"_modiElecStd",sep = ""))]<-
  data.hznu.area.predict.use[,c("d0h1_modiTempStd","d0h1_setTempStd",
                                paste(c("d0h1","d0h2",
                                        "d1h0","d1h1","d1h2",
                                        "d2h0","d2h1","d2h2",
                                        "d7h0","d7h1","d7h2"),"_modiElecStd",sep = ""))]%>%mutate_all(funs(ifelse(.==-999,NA,.)))

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


set.seed(711)

####取增强SVM使用率预测的误差####
data.hznu.area.predict.use<-data.hznu.area.predict.use%>% mutate(.,errSvmIter=fullOnRatio-svmIterPred)%>%as.data.table(.)

data.hznu.area.predict.use$h1_errSvmIter<-apply(data.hznu.area.predict.use[,"refHour1"],MARGIN = 1,
                                                   FUN = function(x){data.hznu.area.predict.use[datetime==as.POSIXct(x)]$errSvmIter[1]})
data.hznu.area.predict.use[hour(datetime)==8]$h1_errSvmIter<-apply(data.hznu.area.predict.use[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                               FUN = function(x){
                                                                 if(anyNA(x))
                                                                   return(NA)
                                                                 if(as.logical(gsub(" ","",x[2]))){
                                                                   #如果是工作日则取前一天均值
                                                                   return(mean(data.hznu.area.predict.use[date==substr(x[1],1,10)]$errSvmIter,na.rm = TRUE))
                                                                 }else{
                                                                   #如果是非工作日则取前一天8h
                                                                   return(data.hznu.area.predict.use[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$errSvmIter[1])
                                                                 }
                                                               })


data.hznu.area.predict.log<-data.hznu.area.predict.log[target!="stdModiElec"]
# backup.hznu.area.predict.log<-data.hznu.area.predict.log
####采用随机森林方法建立基础预测并对变量进行筛选####
list.hznu.area.energyForest<-list()
data.hznu.area.predict.use$rfIdelElec<- -999
data.hznu.area.predict.use$rfRealElec<- -999

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for (j in 0:9) {
    for(k in c("real","ideal")){
      fullAttr<-c("stdOutTemp","stdWeekday","isBizday","hour",ifelse(k=="real","svmIterPred","fullOnRatio"),
                  "stdRhOut","stdWindSpeed",
                  paste(c("d0h1","d0h2",
                          "d1h0","d1h1","d1h2",
                          "d2h0","d2h1","d2h2",
                          "d7h0","d7h1","d7h2"),"_modiElecStd",sep = ""),
                  "d0h1_modiTempStd",
                  "d1_lowEnergyRatio","d1_midEnergyRatio","d1_ltMeRatio","d1_ltHeRatio",
                  "d7_lowEnergyRatio","d7_midEnergyRatio","d7_ltMeRatio","d7_ltHeRatio")
      # if(k=="real"){fullAttr<-append(fullAttr,"h1_errSvmIter")}
      fit<-randomForest(as.formula( paste("stdModiElec ~ ",paste(fullAttr,collapse = "+") ) ),
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
                   target="stdModiElec",method=paste("rfElec","defTree",k,sep = "_"),setType="test",
                   round=j,predValue=pull(.,ifelse(k=="real","rfRealElec","rfIdelElec")),realValue=.$stdModiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      #单独对十折的结果进行储存，此处为训练集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=j&modiSeason==i][complete.cases(data.hznu.area.predict.use[id%%10!=j&modiSeason==i,..fullAttr])]%>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=i,
                   target="stdModiElec",method=paste("rfElec","defTree",k,sep = "_"),setType="train",
                   round=j,predValue=as.numeric(predict(fit,.)),realValue=.$stdModiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      
    }
  }
}
data.hznu.area.predict.use[rfIdelElec== -999]$rfIdelElec<-NA
data.hznu.area.predict.use[rfRealElec== -999]$rfRealElec<-NA
getMAPE(yPred = data.hznu.area.predict.use[stdModiElec!=0]$rfIdelElec, yLook = data.hznu.area.predict.use[stdModiElec!=0]$stdModiElec)#1.071317 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$rfIdelElec,obs = data.hznu.area.predict.use$stdModiElec,na.rm = TRUE)#24.81667
getRSquare(pred = data.hznu.area.predict.use$rfIdelElec,ref = data.hznu.area.predict.use$stdModiElec)#0.951946

#反归一化
data.hznu.area.predict.use$rfIdelElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$rfIdelElec,refReal = .$modiElec,refNorm = .$stdModiElec)
data.hznu.area.predict.use$rfRealElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$rfRealElec,refReal = .$modiElec,refNorm = .$stdModiElec)

getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$rfIdelElecDeNorm, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.071317 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$rfIdelElecDeNorm,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#24.81667
getRSquare(pred = data.hznu.area.predict.use$rfIdelElecDeNorm,ref = data.hznu.area.predict.use$modiElec)#0.951946

# 部分属性下RF-ideal
# MAPE 1.071317 
# RMSE 24.81667
# R-square 0.951946

#全属性下RF-ideal
# MAPE 1.028884
# RMSE 26.07916
# R-square 0.9470695

#全属性下RF-ideal-完全归一化/反归一化
# MAPE 0.06484222 / 0.06422601
# RMSE 0.03012926 / 18.83857
# R-square 0.9525762 / 0.9723243

#全属性下RF-ideal-完全归一化/反归一化 + SVM使用预测误差
# MAPE 0.0651106 / 0.06451802
# RMSE 0.03021496 / 18.88092
# R-square 0.9523314 / 0.9722563

#加入温度相关后全属性下RF-ideal-完全归一化/反归一化
# MAPE 0.06905616 / 0.06794992
# RMSE 0.03099019 / 19.42023
# R-square 0.9522667 / 0.9721413

#全房间温度相关后全属性下RF-ideal-完全归一化/反归一化
# MAPE 0.06918023 / 0.06806173
# RMSE 0.0310698 / 19.51142
# R-square 0.9520212 / 0.9718791

#新行为无设定温度相关后全属性下RF-ideal-完全归一化/反归一化
# MAPE 0.06388993 / 0.06341669
# RMSE 0.02979856 / 18.66425
# R-square 0.9536115 / 0.9728341

####汇总随机森林得出的重要性####
stat.hznu.area.predict.energyFactor<-data.table(type="ideal/real",modiSeason="",round=-999,varName="",IncMSE=-999,IncPur=-999)[-1]
for(season in names(list.hznu.area.energyForest)){
  for(type in names(list.hznu.area.energyForest[[season]])){
    for(round in names(list.hznu.area.energyForest[[season]][[type]])){
      stat.hznu.area.predict.energyFactor<-importance(list.hznu.area.energyForest[[season]][[type]][[round]]) %>%  
                                           data.table(type=type,modiSeason=season,round=round,varName=rownames(.),IncMSE=.[,1],IncPur=.[,2])%>%
                                           .[,3:8] %>% rbind(stat.hznu.area.predict.energyFactor,.)
    }
  }
}
stat.hznu.area.predict.energyFactor.eva<-stat.hznu.area.predict.energyFactor[,.(labelSeasonType=paste(modiSeason[1],type[1],sep = "_"),
                                                                                type=type[1],
                                                                                modiSeason=modiSeason[1],
                                                                                varName=varName[1],
                                                                                IncMSE=mean(IncMSE,na.rm = TRUE),
                                                                                IncPur=mean(IncPur,na.rm = TRUE)
                                                                                ),by=(labelSeasonTypeVar=paste(modiSeason,type,varName,sep = "_"))][,-1]
stat.hznu.area.predict.energyFactor.eva<-stat.hznu.area.predict.energyFactor.eva%>%
                                            .[,.(rlatIncMSE=sum(IncMSE),rlatIncPur=sum(IncPur)),by=labelSeasonType]%>% #汇总统计各季节指标总和
                                            merge(y=.,x=stat.hznu.area.predict.energyFactor.eva,by="labelSeasonType",all.x=TRUE)%>% #合并总和指标
                                            mutate(.,rlatIncMSE=IncMSE/rlatIncMSE,rlatIncPur=IncPur/rlatIncPur)%>%as.data.table(.) #将总和指标转变为占比
write.xlsx(stat.hznu.area.predict.energyFactor.eva,file="HZNU_区域能耗预测_RF全变量_新行为无设定温度_归一化后.xlsx")


# fit<-train(form=as.formula( paste("modiElec ~ ",paste(fullAttr,collapse = "+") ) ),
#            na.action = "na.fail",
#            data=data.hznu.area.predict.use[modiSeason==i]%>%.[complete.cases(.[,..fullAttr])],method = "rf",importance=TRUE,#mtry=3,ntree=200,
#            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))#ntree能传进不知道能不能调


#### 基于RF所得的重要性进行下一步的预测####
predictElecAttr<-list(constant=c("stdOutTemp","stdWeekday","isBizday","hour",
                                 paste(c("d0h1","d0h2","d1h0"),"_modiElecStd",sep="")),
                      Winter=c("d1_ltMeRatio","d1_midEnergyRatio"),
                      Winter_warm=c("d2h0_modiElecStd","d7h0_modiElecStd","stdRhOut"),
                      Transition=c("d1_ltMeRatio","d7_lowEnergyRatio","stdRhOut"),
                      Summer_warm=c("d7h0_modiElecStd","d1_ltMeRatio","d1_ltMeRatio"),
                      Summer=c("stdWindSpeed","d1_ltMeRatio"))
#严格版
predictElecAttr<-list(constant=c("hour",paste(c("d0h1","d0h2","d1h0"),"_modiElecStd",sep="")),#"stdWeekday","isBizday",
                      Winter=c("stdOutTemp","d0h1_modiTempStd"),
                      # Winter_warm=c("stdOutTemp","d2h0_modiElecStd","d7h0_modiElecStd","d1h0_modiElecStd"),#
                      Winter_warm=c("stdOutTemp","d2h0_modiElecStd","d7h0_modiElecStd","d0h1_modiTempStd"),
                      Transition=c("stdOutTemp"),
                      Summer_warm=c("stdOutTemp","d7h0_modiElecStd"),
                      Summer=c("d1_ltMeRatio"))#"d0h1_modiTempStd","d2h0_modiElecStd",

####SVM初始预测####
#取RF预测的基本误差
data.hznu.area.predict.use<-data.hznu.area.predict.use%>% mutate(.,errRfIdel=stdModiElec-rfIdelElec,errRfReal=stdModiElec-rfRealElec)%>%as.data.table(.)

data.hznu.area.predict.use$h1_errRfIdelBase<-apply(data.hznu.area.predict.use[,"refHour1"],MARGIN = 1,
                                             FUN = function(x){data.hznu.area.predict.use[datetime==as.POSIXct(x)]$errRfIdel[1]})
data.hznu.area.predict.use$h1_errRfRealBase<-apply(data.hznu.area.predict.use[,"refHour1"],MARGIN = 1,
                                                   FUN = function(x){data.hznu.area.predict.use[datetime==as.POSIXct(x)]$errRfReal[1]})

data.hznu.area.predict.use[hour(datetime)==8]$h1_errRfIdelBase<-apply(data.hznu.area.predict.use[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                FUN = function(x){
                                                                  if(anyNA(x))
                                                                    return(NA)
                                                                  if(as.logical(gsub(" ","",x[2]))){
                                                                    #如果是工作日则取前一天均值
                                                                    return(mean(data.hznu.area.predict.use[date==substr(x[1],1,10)]$errRfIdel,na.rm = TRUE))
                                                                  }else{
                                                                    #如果是非工作日则取前一天8h
                                                                    return(data.hznu.area.predict.use[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$errRfIdel[1])
                                                                  }
                                                                })
data.hznu.area.predict.use[hour(datetime)==8]$h1_errRfRealBase<-apply(data.hznu.area.predict.use[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                FUN = function(x){
                                                                  if(anyNA(x))
                                                                    return(NA)
                                                                  if(as.logical(gsub(" ","",x[2]))){
                                                                    #如果是工作日则取前一天均值
                                                                    return(mean(data.hznu.area.predict.use[date==substr(x[1],1,10)]$errBase,na.rm = TRUE))
                                                                  }else{
                                                                    #如果是非工作日则取前一天8h
                                                                    return(data.hznu.area.predict.use[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$errRfReal[1])
                                                                  }
                                                                })


data.hznu.area.predict.use[,c("svmInitIdeaElec","svmInitRealElec","svmInitIdeaElecDeNorm", "svmInitRealElecDeNorm",
                              "svmIterIdeaElec","svmIterRealElec","svmIterIdeaElecDeNorm","svmIterRealElecDeNorm",
                              "errSvmInitIdel","errSvmInitReal","h1_errSvmInitIdel","h1_errSvmInitReal")]<-NULL

data.hznu.area.predict.use<-data.hznu.area.predict.use%>% mutate(.,svmInitIdeaElec=-999,svmInitRealElec=-999)%>%as.data.table(.)

for(season in unique(data.hznu.area.predict.use$modiSeason)){
  for(type in c("real","ideal")){
    for(round in 0:9){
      seasonalAttr<-c(predictElecAttr[["constant"]],predictElecAttr[[season]],
                      ifelse(type=="real","rfRealElec","rfIdelElec"),
                      ifelse(type=="real","h1_errRfRealBase","h1_errRfIdelBase"),
                      ifelse(type=="real","svmIterPred","fullOnRatio"))#MMP我之前没有设行为？？？
      # if(type=="real"){seasonalAttr<-append(seasonalAttr,"h1_errSvmIter")}
      fit.svm<-ksvm(x=as.formula( paste("stdModiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                    data=data.hznu.area.predict.use[id%%10!=round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10!=round&modiSeason==season,..seasonalAttr])],
                    kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
      
      if(type=="real"){
        data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]$svmInitRealElec<-
          predict(fit.svm,data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])])
      }else{
        data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]$svmInitIdeaElec<-
          predict(fit.svm,data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])])
      }
      
      
      #单独对十折的结果进行储存，此处为预测集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]%>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=season,
                   target="stdModiElec",method=paste("svmInitPred",type,sep="_"),setType="test",
                   round=round,predValue=pull(.,ifelse(type=="real","svmInitRealElec","svmInitIdeaElec")),realValue=.$stdModiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      
      #单独对十折的结果进行储存，此处为训练集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10!=round&modiSeason==season,..seasonalAttr])] %>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=season,
                   target="stdModiElec",method=paste("svmInitPred",type,sep="_"),setType="train",
                   round=round,predValue=as.numeric(predict(fit.svm,.)),realValue=.$stdModiElec) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    }
  }
}
data.hznu.area.predict.use[svmInitRealElec== -999]$svmInitRealElec<-NA
data.hznu.area.predict.use[svmInitIdeaElec== -999]$svmInitIdeaElec<-NA

getMAPE(yPred = data.hznu.area.predict.use[stdModiElec!=0]$svmInitIdeaElec, yLook = data.hznu.area.predict.use[stdModiElec!=0]$stdModiElec)#0.8568157
RMSE(pred = data.hznu.area.predict.use$svmInitIdeaElec,obs = data.hznu.area.predict.use$stdModiElec,na.rm = TRUE)#25.15353
getRSquare(pred = data.hznu.area.predict.use$svmInitIdeaElec,ref = data.hznu.area.predict.use$stdModiElec)#0.944896

#反归一化
data.hznu.area.predict.use$svmInitIdeaElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$svmInitIdeaElec,refReal = .$modiElec,refNorm = .$stdModiElec)
data.hznu.area.predict.use$svmInitRealElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$svmInitRealElec,refReal = .$modiElec,refNorm = .$stdModiElec)


getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$svmInitIdeaElecDeNorm, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.071317 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$svmInitIdeaElecDeNorm,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#24.81667
getRSquare(pred = data.hznu.area.predict.use$svmInitIdeaElecDeNorm,ref = data.hznu.area.predict.use$modiElec)#0.951946


#全属性下SVMinit-ideal-完全归一化/反归一化
# MAPE 0.05956362 / 0.0597498
# RMSE 0.02809526 / 17.48883
# R-square 0.9588439 / 0.9762337


# 增强严格属性下SVMinit-ideal-完全归一化/反归一化
# MAPE 0.05898961 / 0.05909579
# RMSE 0.02806523 / 17.58568
# R-square 0.9588973 / 0.9758866

#加入温度相关后严格属性下SVMinit-ideal-完全归一化/反归一化
# MAPE 0.06351437 / 0.06307533
# RMSE 0.02894294 / 18.12342
# R-square 0.9587268 / 0.9760055

#全房间温度相关后严格属性下SVMinit-ideal-完全归一化/反归一化
# MAPE 0.06374723 / 0.06328369
# RMSE 0.02916696 / 18.38063
# R-square 0.9580854 / 0.9753196

#fianl-新行为无设定温度相关后严格属性下SVMinit-ideal-完全归一化/反归一化
# MAPE 0.05853448 / 0.0589867
# RMSE 0.02796577 / 17.61689
# R-square 0.9591881 / 0.975801


####能耗预测SVM增强####
#增加SVM初始预测误差作为输入

data.hznu.area.predict.use[,c("errSvmInitIdel","errSvmInitReal","h1_errSvmInitIdel","h1_errSvmInitReal")]<-NULL
data.hznu.area.predict.use<-data.hznu.area.predict.use%>% mutate(.,errSvmInitIdel=stdModiElec-svmInitIdeaElec,
                                                                 errSvmInitReal=stdModiElec-svmInitRealElec)%>%as.data.table(.)

data.hznu.area.predict.use$h1_errSvmInitIdel<-apply(data.hznu.area.predict.use[,"refHour1"],MARGIN = 1,
                                                   FUN = function(x){data.hznu.area.predict.use[datetime==as.POSIXct(x)]$errSvmInitIdel[1]})
data.hznu.area.predict.use$h1_errSvmInitReal<-apply(data.hznu.area.predict.use[,"refHour1"],MARGIN = 1,
                                                   FUN = function(x){data.hznu.area.predict.use[datetime==as.POSIXct(x)]$errSvmInitReal[1]})

data.hznu.area.predict.use[hour(datetime)==8]$h1_errSvmInitIdel<-apply(data.hznu.area.predict.use[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                      FUN = function(x){
                                                                        if(anyNA(x))
                                                                          return(NA)
                                                                        if(as.logical(gsub(" ","",x[2]))){
                                                                          #如果是工作日则取前一天均值
                                                                          return(mean(data.hznu.area.predict.use[date==substr(x[1],1,10)]$errSvmInitIdel,na.rm = TRUE))
                                                                        }else{
                                                                          #如果是非工作日则取前一天8h
                                                                          return(data.hznu.area.predict.use[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$errSvmInitIdel[1])
                                                                        }
                                                                      })
data.hznu.area.predict.use[hour(datetime)==8]$h1_errSvmInitReal<-apply(data.hznu.area.predict.use[hour(datetime)==8,c("refHour1","isBizday")],MARGIN = 1,
                                                                      FUN = function(x){
                                                                        if(anyNA(x))
                                                                          return(NA)
                                                                        if(as.logical(gsub(" ","",x[2]))){
                                                                          #如果是工作日则取前一天均值
                                                                          return(mean(data.hznu.area.predict.use[date==substr(x[1],1,10)]$errSvmInitReal,na.rm = TRUE))
                                                                        }else{
                                                                          #如果是非工作日则取前一天8h
                                                                          return(data.hznu.area.predict.use[datetime==as.POSIXct(paste(substr(x[1],1,10),"08:00:00"))]$errSvmInitReal[1])
                                                                        }
                                                                      })

###仔细看看winter_warm的情况####
# data.hznu.area.predict.log<-data.hznu.area.predict.log[!(modiSeason=="Winter_warm"&method%in% c("svmIterPred_real"))]
# 
# ggplot(data = data.hznu.area.predict.use[date%in% paperTime$Winter_warm],
#        aes(x=datetime,group=date))+geom_line(aes(y=modiElec))+geom_point(aes(y=modiElec))+
#   geom_line(aes(y=svmIterRealElecDeNorm,color="red"))+geom_point(aes(y=svmIterRealElecDeNorm,color="red"))+
#   geom_line(aes(y=rfRealElecDeNorm,color="green"))
# 
# 
# data.hznu.area.predict.log<-data.hznu.area.predict.log[!method%in% c("svmIterPred_real","svmIterPred_ideal")]
# data.hznu.area.predict.use[,c("svmIterRealElec","svmIterIdeaElec")]<-NULL
# #

data.hznu.area.predict.use<-data.hznu.area.predict.use%>% mutate(.,svmIterIdeaElec=-999,svmIterRealElec=-999)%>%as.data.table(.)

for(season in c(unique(data.hznu.area.predict.use$modiSeason))){#unique(data.hznu.area.predict.use$modiSeason),"Winter_warm"
  for(type in c("real","ideal")){# 
    for(round in 0:9){
      seasonalAttr<-c(predictElecAttr[["constant"]],predictElecAttr[[season]],#"isBizday","stdWeekday",
                      ifelse(type=="real","rfRealElec","rfIdelElec"),ifelse(type=="real","h1_errRfRealBase","h1_errRfIdelBase"),
                      ifelse(type=="real","svmInitRealElec","svmInitIdeaElec"),ifelse(type=="real","h1_errSvmInitReal","h1_errSvmInitIdel"),
                      ifelse(type=="real","svmIterPred","fullOnRatio"))
      
      # seasonalAttr<-c(ifelse(type=="real","rfRealElec","rfIdelElec"),ifelse(type=="real","h1_errRfRealBase","h1_errRfIdelBase"),
      #                ifelse(type=="real","svmInitRealElec","svmInitIdeaElec"),ifelse(type=="real","h1_errSvmInitReal","h1_errSvmInitIdel"),
      #                ifelse(type=="real","svmIterPred","fullOnRatio"))
      # if(type=="real"){seasonalAttr<-append(seasonalAttr,"h1_errSvmIter")}
      
      fit.svm<-ksvm(x=as.formula( paste("stdModiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                    data=data.hznu.area.predict.use[id%%10!=round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10!=round&modiSeason==season,..seasonalAttr])],
                    kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
      
      if(type=="real"){
        data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]$svmIterRealElec<-
          predict(fit.svm,data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])])
      }else{
        data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]$svmIterIdeaElec<-
          predict(fit.svm,data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])])
      }
      
      
      #单独对十折的结果进行储存，此处为预测集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10==round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10==round&modiSeason==season,..seasonalAttr])]%>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=season,
                   target="stdModiElec",method=paste("svmIterPred",type,sep="_"),setType="test",
                   round=round,predValue=pull(.,ifelse(type=="real","svmIterRealElec","svmIterIdeaElec")),realValue=.$stdModiElec)%>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
      
      #单独对十折的结果进行储存，此处为训练集
      data.hznu.area.predict.log<-data.hznu.area.predict.use[id%%10!=round&modiSeason==season][complete.cases(data.hznu.area.predict.use[id%%10!=round&modiSeason==season,..seasonalAttr])] %>%
        data.table(id=.$id,datetime=.$datetime,modiSeason=season,
                   target="stdModiElec",method=paste("svmIterPred",type,sep="_"),setType="train",
                   round=round,predValue=as.numeric(predict(fit.svm,.)),realValue=.$stdModiElec) %>% .[,..archieveItem] %>% rbind(data.hznu.area.predict.log,.)
    }
  }
}


data.hznu.area.predict.use[svmIterRealElec== -999]$svmIterRealElec<-NA
data.hznu.area.predict.use[svmIterIdeaElec== -999]$svmIterIdeaElec<-NA

getMAPE(yPred = data.hznu.area.predict.use[stdModiElec!=0]$svmIterIdeaElec, yLook = data.hznu.area.predict.use[stdModiElec!=0]$stdModiElec)#0.8568157
RMSE(pred = data.hznu.area.predict.use$svmIterIdeaElec,obs = data.hznu.area.predict.use$stdModiElec,na.rm = TRUE)#25.15353
getRSquare(pred = data.hznu.area.predict.use$svmIterIdeaElec,ref = data.hznu.area.predict.use$stdModiElec)#0.944896

#反归一化
data.hznu.area.predict.use$svmIterIdeaElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$svmIterIdeaElec,refReal = .$modiElec,refNorm = .$stdModiElec)
data.hznu.area.predict.use$svmIterRealElecDeNorm<-data.hznu.area.predict.use%>%denormalize(targetNorm = .$svmIterRealElec,refReal = .$modiElec,refNorm = .$stdModiElec)


getMAPE(yPred = data.hznu.area.predict.use[modiElec!=0]$svmIterIdeaElecDeNorm, yLook = data.hznu.area.predict.use[modiElec!=0]$modiElec)#1.071317 #k=5 1.740213; k=3 1.709286 #k=10 1.930663
RMSE(pred = data.hznu.area.predict.use$svmIterIdeaElecDeNorm,obs = data.hznu.area.predict.use$modiElec,na.rm = TRUE)#24.81667
getRSquare(pred = data.hznu.area.predict.use$svmIterIdeaElecDeNorm,ref = data.hznu.area.predict.use$modiElec)#0.951946

#SVMiter-ideal-完全归一化/反归一化
# MAPE 0.05478437 / 0.05511059
# RMSE 0.02544581 / 15.75854
# R-square 0.9662921 / 0.9807378


# SVMiter-ideal-完全归一化/反归一化
# MAPE 0.05429961 / 0.05450339
# RMSE 0.02528185 / 15.60317
# R-square 0.9666936 / 0.9810345


# SVMiter-ideal-加入温度相关后严格属性下-完全归一化/反归一化
# MAPE 0.05995678 / 0.05961041
# RMSE 0.02679923 / 16.58518
# R-square 0.964887 / 0.9801159

# SVMiter-ideal-全房间温度相关后严格属性下-完全归一化/反归一化
# MAPE 0.06013354 / 0.05976345
# RMSE 0.02704232 / 16.92143
# R-square 0.9642471 / 0.9793014

# final-SVMiter-ideal-新行为无设定温度后严格属性下-完全归一化/反归一化
# MAPE 0.05427626 / 0.05481015
# RMSE 0.02530741 / 15.61717
# R-square 0.9666262 / 0.9810004



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
write.xlsx(stat.hznu.area.predict.eva,file = "HZNU_区域能耗预测_随机森林全部属性.xlsx")
ggplot(data=stat.hznu.area.predict.eva,aes(x=modiSeason,y=rSquare,group=method,color=method))+geom_line()+geom_point()+facet_wrap(~setType,ncol = 2)
  
####统计大论文中各####
for(i in names(paperTime)){
  for(j in c("real","ideal")){#
    data.hznu.area.predict.use[date %in% paperTime[[i]]] %>% {
      # cat("\n",i,": \nMAPE\t",getMAPE(yPred = .[modiElec!=0]$rfRealElecDeNorm, yLook = .[modiElec!=0]$modiElec))#0.5800756
      # cat("\nRMSE\t",RMSE(pred = .$rfRealElecDeNorm,obs = .$modiElec,na.rm = TRUE))#0.022
      # cat("\nRSquare\t",getRSquare(pred = .$rfRealElecDeNorm,ref = .$modiElec))#0.8631175
      # cat("\nMean\t",mean(.$modiElec,na.rm = TRUE))#0.8631175
      # 
      cat("\n",i,"\t",j,"\t",
          RMSE(pred = pull(.,ifelse(j=="real","svmIterRealElecDeNorm","svmIterIdeaElecDeNorm")),obs = .$modiElec,na.rm = TRUE),"\t",
          getRSquare(pred = pull(.,ifelse(j=="real","svmIterRealElecDeNorm","svmIterIdeaElecDeNorm")),ref = .$modiElec),"\t",
          getMAPE(yPred = pull(.[modiElec!=0],ifelse(j=="real","svmIterRealElecDeNorm","svmIterIdeaElecDeNorm")), yLook = .[modiElec!=0]$modiElec))#0.8631175
    }
  }
}

# svmInitRealElecDeNorm,svmInitIdeaElecDeNorm,rfIdelElecDeNorm,rfRealElecDeNorm,svmIterRealElecDeNorm,svmIterIdeaElecDeNorm

####绘图输出####
ggplot(data=data.hznu.area.predict.use[date %in% paperTime$Winter_warm,c("datetime","weekCount","weekday","modiSeason","modiElec","rfIdelElecDeNorm","rfRealElecDeNorm")] %>% #,"simpleKnnFullOnRatio","svmInitPred","svmIterPred","knnFullOnRatio","tsFullOnRatio"
         mutate(.,year=substr(datetime,1,4),date=date(datetime))%>% melt(.,id.var=c("datetime","modiSeason","year","date","weekday","weekCount")),
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable,group=paste(date,variable)))+geom_line()+geom_point(size=2)+facet_wrap(~modiSeason,nrow = 2)+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))



for(j in names(paperTime)){
  ggsave(file=paste("modiElec_RF_",j,".png",sep = ""),
         plot=ggplot(data=data.hznu.area.predict.use[date %in% paperTime[[j]],c("datetime","weekCount","weekday","modiSeason","modiElec","rfIdelElecDeNorm","rfRealElecDeNorm")] %>% #,"simpleKnnFullOnRatio","svmInitPred","svmIterPred","knnFullOnRatio","tsFullOnRatio"
                       mutate(.,year=substr(datetime,1,4),date=date(datetime))%>% melt(.,id.var=c("datetime","modiSeason","year","date","weekday","weekCount")),
                     aes(x=datetime,y=value,color=variable,shape=variable,lty=variable,group=paste(date,variable)))+geom_line()+geom_point(size=2)+facet_wrap(~modiSeason,nrow = 2)+
           theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(ifelse(j=="Winter",0.15,0.9),0.85)),
         width=16,height = 4.8,dpi = 100  
  )}



