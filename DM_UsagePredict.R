#本脚本用于对行为进行预测（建筑级）
#主要预测总使用率和各模式占比
#接 data.hznu.teaching.use 数据集

####数据概况
# > table(data.hznu.teaching.use$basePattern)
# fullUse   noneUse periodUse 
# 1118    222183     20445 
# 
# > table(data.hznu.teaching.use$season)
# Autumn      Spring      Summer Summer_warm      Winter Winter_warm 
# 36434       37522       40300       39000       50354       40136 

####数据集初处理####
data.hznu.use.predict.raw<-data.hznu.teaching.use[,c("labelRoomDay","date","roomCode","buildingCode","acCount",
                                                 "finalState","modiSeason","runtime","basePattern")]
data.hznu.use.predict.raw[is.na(finalState)&modiSeason %in% c("Winter","Winter_warm")]$finalState<-"heating"
#合并使用强度
data.hznu.use.predict.raw<-merge(x=data.hznu.use.predict.raw,y = data.hznu.all.use[,c("labelRoomDay","acIntensity")],
                             all.x = TRUE,by="labelRoomDay")
data.hznu.use.predict.raw[is.nan(acIntensity)]$acIntensity<-0
#合并使用模式标签
data.hznu.use.predict.raw<-merge(x=data.hznu.use.predict.raw,y=data.hznu.use.final[,c("labelRoomDay","clusterName")],
                             all.x = TRUE,by="labelRoomDay")
data.hznu.use.predict.raw<-data.hznu.use.predict.raw[basePattern!="fullUse"]
data.hznu.use.predict.raw[basePattern=="noneUse"]$clusterName<-"noneUse"

####房间级别合并至建筑级别####
data.hznu.use.predict.raw$labelBuildingDay<-paste(data.hznu.use.predict.raw$buildingCode,
                                                  data.hznu.use.predict.raw$date,sep = "_")
data.hznu.use.predict.building.raw<-data.hznu.use.predict.raw[,.(date=date[1],
                                                             buildingCode=buildingCode[1],
                                                             modiSeason=modiSeason[1],
                                                             finalState=as.character(getMode(finalState[finalState!="off"])),
                                                             count=length(labelRoomDay),
                                                             offCount=length(labelRoomDay[finalState=="off"]),
                                                             onCount=length(labelRoomDay[finalState!="off"]),
                                                             onDemandCount=length(labelRoomDay[clusterName=="OnDemand"]),
                                                             forenoonCount=length(labelRoomDay[clusterName=="Forenoon"]),
                                                             afternoonCount=length(labelRoomDay[clusterName=="Afternoon"]),
                                                             daytimeCount=length(labelRoomDay[clusterName=="Daytime"]),
                                                             lateDaytimeCount=length(labelRoomDay[clusterName=="LateDayTime"]),
                                                             allDayCount=length(labelRoomDay[clusterName=="All-Day"])
                                                             ),by=labelBuildingDay]
data.hznu.use.predict.building.raw[is.na(finalState)]$finalState<-"off"

####房间级合并至区域级####


#单栋楼最多日内数据就38间？？
# ggplot(data=data.hznu.use.predict.building.raw,aes(x=count))+geom_density()
#检测统计数据是否正常
# nn<-data.hznu.use.predict.building.raw[count!=(offCount+onCount)]#0
# nn<-data.hznu.use.predict.building.raw[onCount!=(onDemandCount+forenoonCount+afternoonCount+daytimeCount+lateDaytimeCount+allDayCount)]#0

####将原始数据转换为比例####
data.hznu.use.predict.building.processed<-data.table(data.hznu.use.predict.building.raw[,c("labelBuildingDay","date","buildingCode","modiSeason","count","finalState")],
                                                     weekday=wday(as.POSIXct(data.hznu.use.predict.building.raw$date),week_start = 1),
                                                     onRatio=data.hznu.use.predict.building.raw$onCount/data.hznu.use.predict.building.raw$count,
                                                     onDemandRatio=data.hznu.use.predict.building.raw$onDemandCount/data.hznu.use.predict.building.raw$onCount,
                                                     forenoonRatio=data.hznu.use.predict.building.raw$forenoonCount/data.hznu.use.predict.building.raw$onCount,
                                                     afternoonRatio=data.hznu.use.predict.building.raw$afternoonCount/data.hznu.use.predict.building.raw$onCount,
                                                     daytimeRatio=data.hznu.use.predict.building.raw$daytimeCount/data.hznu.use.predict.building.raw$onCount,
                                                     lateDaytimeRatio=data.hznu.use.predict.building.raw$lateDaytimeCount/data.hznu.use.predict.building.raw$onCount,
                                                     allDayRatio=data.hznu.use.predict.building.raw$allDayCount/data.hznu.use.predict.building.raw$onCount)
data.hznu.use.predict.building.processed[onRatio==0,c("onDemandRatio","forenoonRatio","afternoonRatio","daytimeRatio","lateDaytimeRatio","allDayRatio")]<-0
####合并气象数据####
data.hznu.use.predict.building.processed<-merge(x=data.hznu.use.predict.building.processed,y=data.weather.airport.daily,all.x = TRUE,by="date")

####合并前期参考行为模式####
#我*这代码自己看得都难受
data.hznu.use.predict.building.processed$d1_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-1,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d2_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-2,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d3_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-3,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d4_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-4,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d5_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-5,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d6_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-6,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$d7_OnRatio<-apply(X = data.hznu.use.predict.building.processed[,c("buildingCode","date")],MARGIN = 1,
                                                           FUN = function(x){
                                                             return(data.hznu.use.predict.building.processed[labelBuildingDay==paste(x[1],as.Date(x[2])-7,sep = "_")]$onRatio[1])
                                                           })
data.hznu.use.predict.building.processed$isWorkday<-isWeekday(data.hznu.use.predict.building.processed$date)

####相关性检验####
usagePredictAttr<-c("onRatio","meanOutTemp","meanRhOut","meanWindSpeed","maxOutTemp","minOutTemp","weekday",
                    "d1_OnRatio","d2_OnRatio","d3_OnRatio","d4_OnRatio","d5_OnRatio","d6_OnRatio","d7_OnRatio")

stat.hznu.usage.predict.cor<-data.table(finalState="cooling",cor(use = "complete.obs",method = "spearman",
    x=data.hznu.use.predict.building.processed[onRatio!=0&finalState=="cooling","onRatio"],
    y=data.hznu.use.predict.building.processed[onRatio!=0&finalState=="cooling",..usagePredictAttr]))
stat.hznu.usage.predict.cor<-rbind(stat.hznu.usage.predict.cor,
                                   data.table(finalState="heating",
                                              cor(use = "complete.obs",method = "spearman",
                                                  x=data.hznu.use.predict.building.processed[onRatio!=0&finalState=="heating","onRatio"],
                                                  y=data.hznu.use.predict.building.processed[onRatio!=0&finalState=="heating",..usagePredictAttr])))

write.xlsx(stat.hznu.usage.predict.cor,file = "HZNU_Usage_Predict_Pearson.xlsx")

####logistic回归变量显著性####

usagePredictFormula<-as.formula(paste("onRatio ~ ",paste(usagePredictAttr,collapse = "+")))
signAttr<-list(weatherAttr=c("meanOutTemp","meanRhOut","meanWindSpeed","maxOutTemp","minOutTemp","weekday"),
                    hstAttr=c("d1_OnRatio","d2_OnRatio","d3_OnRatio","d4_OnRatio","d5_OnRatio","d6_OnRatio","d7_OnRatio"))
# scatterplotMatrix(data.hznu.use.predict.building.processed[onRatio!=0&finalState=="heating",..usagePredictAttr],spread=FALSE,smoother.args=list(lty=2))

fit<-glm(usagePredictFormula,data=data.hznu.use.predict.building.processed[finalState=="heating"],family = binomial(),na.action = na.omit)
stat.fit<-summary(fit)
stat.hznu.use.predict.sign<-data.table(finalState="heating",modiSeason="all",attr="all",var=row.names(stat.fit$coefficients),stat.fit$coefficients)

fit<-glm(usagePredictFormula,data=data.hznu.use.predict.building.processed[finalState=="cooling"],family = binomial(),na.action = na.omit)
stat.fit<-summary(fit)
stat.hznu.use.predict.sign<-rbind(stat.hznu.use.predict.sign,data.table(finalState="cooling",
                                                                        modiSeason="all",
                                                                        attr="all",
                                                                        var=row.names(stat.fit$coefficients),
                                                                        stat.fit$coefficients))

for(i in unique(data.hznu.use.predict.building.processed[finalState!="off"]$finalState)){
  for(j in unique(data.hznu.use.predict.building.processed[finalState==i]$modiSeason)){
    for(k in names(signAttr)){
      fit<-glm(as.formula(paste("onRatio ~ ",paste(signAttr[[k]],collapse = "+"))),
               data=data.hznu.use.predict.building.processed[finalState==i & modiSeason==j],family = binomial(),na.action = na.omit)
      stat.fit<-summary(fit)
      stat.hznu.use.predict.sign<-rbind(stat.hznu.use.predict.sign,data.table(finalState=i,modiSeason=j,attr=k,
                                                                              var=row.names(stat.fit$coefficients),stat.fit$coefficients))}
  }
}
write.xlsx(stat.hznu.use.predict.sign,file = "HZNU_Usage_Predict_Sign_split.xlsx")

####预测部分####
#几个变量归一化
data.hznu.use.predict.building.processed$stdMeanTemp<- -999
data.hznu.use.predict.building.processed$stdMinTemp<- -999
data.hznu.use.predict.building.processed$stdMaxTemp<- -999
data.hznu.use.predict.building.processed$stdMeanRh<- -999
data.hznu.use.predict.building.processed$stdMeanWind<- -999

# temp.std.usageRatio<-data.table(matrix(nrow=0,ncol=6))
# names(temp.std.usageRatio)<-c("onDemandRatio","forenoonRatio","afternoonRatio","daytimeRatio","lateDaytimeRatio","allDayRatio")
# data.hznu.use.predict.building.processed<-cbind(data.hznu.use.predict.building.processed,temp.std.usageRatio)
#放弃了，应该要更好的办法

data.hznu.use.predict.building.processed$stdOnRatio<-0
data.hznu.use.predict.building.processed$stdOnDemandRatio<-0
data.hznu.use.predict.building.processed$stdForenoonRatio<-0
data.hznu.use.predict.building.processed$stdAfternoonRatio<-0
data.hznu.use.predict.building.processed$stdDaytimeRatio<-0
data.hznu.use.predict.building.processed$stdLateDaytimeRatio<-0
data.hznu.use.predict.building.processed$stdAllDayRatio<-0


data.hznu.use.predict.building.processed[meanRhOut>1]$meanRhOut<-NA
#根据季节和工况进行标准化
for(i in unique(data.hznu.use.predict.building.processed$finalState)){
  for(j in unique(data.hznu.use.predict.building.processed[finalState==i]$modiSeason)){

    data.hznu.use.predict.building.processed[finalState==i & modiSeason==j]<-
      within(data.hznu.use.predict.building.processed[finalState==i & modiSeason==j],{
        stdMeanTemp<-normalize(data=meanOutTemp,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdMinTemp<-normalize(data=minOutTemp,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdMaxTemp<-normalize(data=maxOutTemp,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdMeanRh<-normalize(data=meanRhOut,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdMeanWind<-normalize(data=meanWindSpeed,upper = 0.9,lower = 0.1,intercept = 0.1)
      })

    #需要重新考虑，是否需要标准化
    data.hznu.use.predict.building.processed[finalState==i & finalState!="off" & modiSeason==j]<-
      within(data.hznu.use.predict.building.processed[finalState==i & finalState!="off" & modiSeason==j],{
        stdOnRatio<-normalize(data=onRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdOnDemandRatio<-normalize(data=onDemandRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdForenoonRatio<-normalize(data=forenoonRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdAfternoonRatio<-normalize(data=afternoonRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdDaytimeRatio<-normalize(data=daytimeRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdLateDaytimeRatio<-normalize(data=lateDaytimeRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
        stdAllDayRatio<-normalize(data=allDayRatio,upper = 0.9,lower = 0.1,intercept = 0.1)
      })
    # Value
    # For with, the value of the evaluated expr. For within, the modified object.
  }
}


list.hznu.use.predict<-split(x=data.hznu.use.predict.building.processed[finalState!="off"],by = "modiSeason")
stat.hznu.use.predict.knn.kSelect<-data.table(modiSeason=character(),finalState=character(),
                                              kSize=numeric(),
                                              meanEucDist=numeric(),sdEucDist=numeric(),
                                              meanWtEucDist=numeric(),sdWtEucDist=numeric(),meanErr=numeric())[0]#,meanErrRatio=numeric()
#确定最佳K值
for(modeSelect in names(list.hznu.use.predict)){
  data.hznu.use.predict.knn.select<-list.hznu.use.predict[[modeSelect]]#[buildingCode=="330100D276"]
  #数据集统计
  # length(unique(data.hznu.use.predict.knn.select$labelBuildingDay)) #2223
  # length(unique(data.hznu.use.predict.knn.select$date)) #149
  
  knnAttr<-c("stdMeanTemp","stdMinTemp","stdMaxTemp","stdMeanRh","stdMeanWind","weekday","isWorkday","d1_OnRatio","d7_OnRatio")
  knnFormula<-as.formula(paste("onRatio ~ ",paste(knnAttr,collapse = "+")))
  
  data.hznu.use.predict.knn.select<-data.hznu.use.predict.knn.select[complete.cases(data.hznu.use.predict.knn.select[,..knnAttr])] %>% 
                                    .[!duplicated(.$labelBuildingDay)]
  
  # set.seed(711)
  # sub<-sample(1:nrow(data.hznu.use.predict.knn.select),round(nrow(data.hznu.use.predict.knn.select))*4/5)
  # data.hznu.use.predict.knn.select.training<-data.hznu.use.predict.knn.select[sub]
  # data.hznu.use.predict.knn.select.test<-data.hznu.use.predict.knn.select[-sub]
  data.hznu.use.predict.knn.select.training<-data.hznu.use.predict.knn.select[!substr(date,1,7)%in% c("2017-06")]
  data.hznu.use.predict.knn.select.test<-data.hznu.use.predict.knn.select[substr(date,1,7) %in% c("2017-06")]
  
  
  
  # ggplot(data=list.hznu.use.predict[[modeSelect]][buildingCode=="330100D280"],aes(x=date,y=onRatio,group=buildingCode,color=buildingCode))+geom_line()
  # for(i in 10){
    fit.kknn<-
      kknn(formula = knnFormula,kernel = "optimal",k=300,
           train = data.hznu.use.predict.knn.select.training,
           test = data.hznu.use.predict.knn.select.test)
    #fit.kknn中包括有所有测试集中对应的近邻样本
    data.hznu.use.predict.knn.select.test$predictOnRatio<-fit.kknn$fitted.values
    data.hznu.use.predict.knn.select.test$meanWtEucDist<-apply(fit.kknn$W*fit.kknn$D,MARGIN = 1,FUN = mean,na.rm=TRUE)
    data.hznu.use.predict.knn.select.test$meanEucDist<-apply(fit.kknn$D,MARGIN = 1,FUN = mean,na.rm=TRUE)
    data.hznu.use.predict.knn.select.test$sdWtEucDist<-apply(fit.kknn$W*fit.kknn$D,MARGIN = 1,FUN = sd,na.rm=TRUE)
    data.hznu.use.predict.knn.select.test$sdEucDist<-apply(fit.kknn$D,MARGIN = 1,FUN = sd,na.rm=TRUE)
    #试一试
    # stat.hznu.use.predict.knn.kSelect<-rbind(stat.hznu.use.predict.knn.kSelect,
    #                                          data.table(modiSeason=modeSelect,
    #                                                     finalState=getMode(data.hznu.use.predict.knn.select.test[finalState!="off"]$finalState),
    #                                                     kSize=i,
    #                                                     meanEucDist=mean(data.hznu.use.predict.knn.select.test$meanEucDist,na.rm = TRUE),
    #                                                     sdEucDist=mean(data.hznu.use.predict.knn.select.test$sdEucDist,na.rm = TRUE),
    #                                                     meanWtEucDist=mean(data.hznu.use.predict.knn.select.test$meanWtEucDist,na.rm = TRUE),
    #                                                     sdWtEucDist=mean(data.hznu.use.predict.knn.select.test$sdWtEucDist,na.rm = TRUE),
    #                                                     meanErr=mean(abs(data.hznu.use.predict.knn.select.test$onRatio-data.hznu.use.predict.knn.select.test$predictOnRatio),na.rm=TRUE)
    #                                                     # meanErrRatio=mean(abs(data.hznu.use.predict.knn.select.test$onRatio-data.hznu.use.predict.knn.select.test$predictOnRatio)/
    #                                                     #                     data.hznu.use.predict.knn.select.test$onRatio,na.rm=TRUE)
    #                                                     ))
  # }
    
    
}
stat.hznu.use.predict.knn.kSelect$errLev<- with(stat.hznu.use.predict.knn.kSelect,{
  meanErr/apply(X=as.matrix(modiSeason),MARGIN = 1,FUN = function(x){ max(stat.hznu.use.predict.knn.kSelect[modiSeason==x]$meanErr,na.rm = TRUE)})})

###knn作为预测k值选取已经大致有数，大致10个左右，但作为选取SVM样本过少                                          
ggplot(data=stat.hznu.use.predict.knn.kSelect,#[kSize<100],
       aes(x=kSize,y=meanEucDist,group=modiSeason,shape=modiSeason,color=modiSeason))+geom_line(size=1)#+facet_grid(~modiSeason)#+geom_line(size=1)#+geom_point()

####试一试KNN作预测####
stat.hznu.use.predict.knn<-data.hznu.use.predict.knn.select.test[,.(meanErr=mean(abs(onRatio-predictOnRatio),na.rm=TRUE)),by=buildingCode]
ggplot(data=(data.hznu.use.predict.knn.select.test[buildingCode %in% sprintf("330100D%d",c(255,260,273,280,281)),
                                                   c("date","buildingCode","onRatio","predictOnRatio")] %>% 
             melt(data=.,id.var=c("date","buildingCode")) ),
       aes(x=date,y=value,group=variable,color=variable,shape=variable))+geom_line()+geom_point()+facet_wrap(~buildingCode,nrow = 5)
#summer_warm 2017 262 273 277 /257 262
#summer 255 260 273 280 281
#winter 258 259 265 278 279 281
#winter_warm 258 259 272 277 
buildingCode %in% sprintf("330100D%d",c(255,260,273,280,281))

####
temp.knn.dist<-data.table(sampleID=1:nrow(fit.kknn$D),fit.kknn$D*fit.kknn$W) %>% melt(data=.,id.var="sampleID") %>% data.table(.,simNo=as.numeric(substring(.$variable,2)))
ggplot(temp.knn.dist[sampleID<=20],aes(x=simNo,y=value,group=sampleID,color=sampleID))+geom_line()

write.xlsx(stat.hznu.use.predict.knn.kSelect,file = "HZNU_Use_Predict_kNN_kSelection.xlsx")
nn<-list.hznu.use.predict[[modeSelect]][labelBuildingDay!="330100D280_2017-03-02"][fit.kknn$C]

####To TX####
usagePredictFormula<-
onRatio ~ meanOutTemp + meanRhOut + meanWindSpeed +
  maxOutTemp + minOutTemp + weekday + d1_OnRatio + d2_OnRatio +
  d3_OnRatio + d4_OnRatio + d5_OnRatio + d6_OnRatio + d7_OnRatio

fit<-randomForest(data=data.hznu.use.predict.knn.select.training,usagePredictFormula,ntree=300)#随机森林回归建模
importance(fit,type=1)#重要性分析，type1或2，不同的指标

#训练集上的预测
temp.predict.training<-data.table(predict(fit,data.hznu.use.predict.knn.select.training))
getRSquare(temp.predict.training$V1,data.hznu.use.predict.knn.select.training$onRatio)
getMAPE(temp.predict.training$V1,data.hznu.use.predict.knn.select.training$onRatio)
#选一栋建筑作图
ggplot(data=cbind(temp.predict.training$V1,
                  data.hznu.use.predict.knn.select.training[,c("date","onRatio","buildingCode")])[buildingCode=="330100D278"])+
  geom_line(aes(x=date,y=V1,color="red",group=buildingCode))+geom_line(aes(x=date,y=onRatio,color="blue",group=buildingCode))

#测试集上的预测，惨不忍睹
temp.predict.test<-data.table(predict(fit,data.hznu.use.predict.knn.select.test))
getRSquare(temp.predict.test$V1,data.hznu.use.predict.knn.select.test$onRatio)
getMAPE(temp.predict.test$V1,data.hznu.use.predict.knn.select.test$onRatio)
#选一栋建筑作图
ggplot(data=cbind(temp.predict.test$V1,
                  data.hznu.use.predict.knn.select.test[,c("date","onRatio","buildingCode")])[buildingCode=="330100D281"])+
  geom_line(aes(x=date,y=V1,color="red",group=buildingCode))+geom_line(aes(x=date,y=onRatio,color="blue",group=buildingCode))



