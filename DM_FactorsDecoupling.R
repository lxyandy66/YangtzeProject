####能耗影响三因素解耦部分####

# data.hznu.use.final 行为已聚类数据
# data.hznu.teaching.energy.std 能耗已聚类数据
# data.hznu.teaching.thermo.final 热环境已聚类数据

####以能耗模式为基准合并####
data.hznu.teaching.decoupling<-merge(
  x=data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date","acCount","energyClusterName","acIntensity")],
  y=data.hznu.teaching.thermo.final[,c("labelRoomDay","runtime","modiSeason","finalState","clusterName","thermoPattern")],
  all.x = TRUE,by.x = "labelRoomDay",by.y = "labelRoomDay")


####加入其他热环境参数####
#加入设定温度
data.hznu.teaching.decoupling<-merge(x=data.hznu.teaching.decoupling,y=data.hznu.teaching.thermo.setTemp[,c("labelRoomDay","setTemp")],
                                     all.x = TRUE,by.x = "labelRoomDay",by.y = "labelRoomDay")
#加入室外气象参数
data.hznu.teaching.decoupling<-merge(x=data.hznu.teaching.decoupling,y=data.weather.airport.daily,
                                     all.x = TRUE,by.x = "date",by.y = "date")
data.hznu.teaching.decoupling[is.na(setTemp)|setTemp==0|is.nan(setTemp)]$setTemp<-NA
data.hznu.teaching.decoupling<-
  data.hznu.teaching.decoupling[complete.cases(data.hznu.teaching.decoupling[,c("clusterName","thermoPattern")])]

ggplot(data=data.hznu.teaching.decoupling,aes(x=acCount))+geom_density()
ggplot(data=data.hznu.teaching.decoupling,aes(x=setTemp))+geom_density()
data.hznu.teaching.decoupling$areaScale<-apply(data.hznu.teaching.decoupling[,"acCount"],MARGIN = 1, FUN = getAreaLevel)

####统一能耗模式参数命名####
此处注意无需重复执行
unique(data.hznu.teaching.decoupling$energyClusterName)
data.hznu.teaching.decoupling$energyClusterName<-apply(X = data.hznu.teaching.decoupling[,"energyClusterName"],MARGIN = 1,
                                                       FUN = function(x){  
                                                         if (x=="ShortTime_LowEnergy") {
                                                           return("LowEnergy")
                                                         }else if(x=="MidEnergy"){
                                                           return("MidEnergy_MidTime")
                                                         }else if(x=="LongTime_MidEnergy"){
                                                           return("MidEnergy_LongTime")
                                                         }else if ("LongTime_HighEnergy") {
                                                           return("HighEnergy")
                                                         }else
                                                           return(x)
                                                      })



#用于统一树类的剪枝情况
localInitCP<-0.01
list.hznu.decoupling.cart<-list()

j<-unique(data.hznu.teaching.decoupling[finalState==i]$clusterName)[6]


####定义能耗解耦关系式####
decouplingAttr<-c("thermoPattern","areaScale","modiSeason","setTemp","meanOutTemp","meanRhOut","runtime","acIntensity")
decouplingFormula<-as.formula(paste("energyClusterName ~ ",paste(decouplingAttr,collapse = "+")))


####训练集/测试集划分####
#分块处理
data.hznu.teaching.decoupling.log$type<-"train"
data.hznu.teaching.decoupling.log$real<-as.character(data.hznu.teaching.decoupling.log$real)
data.hznu.teaching.decoupling.log$pred<-as.character(data.hznu.teaching.decoupling.log$pred)
data.hznu.teaching.decoupling.log<-data.table(labelRoomDay="",finalState="",type="train",
                                              clusterName="",method="method",real="energyClusterName",pred="pred")[-1]
for(i in unique(data.hznu.teaching.decoupling$finalState) ){
  #作为整体准确度计算和算法评估的汇总
  stat.hznu.decoupling.algoAcc<-data.table(algoName="",setType="",finalState="",usagePattern="",count=as.numeric(NA),acc=as.numeric(NA))[-1]
  for(j in unique(data.hznu.teaching.decoupling[finalState==i]$clusterName)){
  
    #变量的预处理
    data.hznu.teaching.decoupling.selected<-data.hznu.teaching.decoupling[finalState==i&clusterName==j] %>% .[complete.cases(.[,..decouplingAttr])] %>%{
      .[,c("energyClusterName","thermoPattern","clusterName","areaScale","modiSeason")]<-
        lapply(.[,c("energyClusterName","thermoPattern","clusterName","areaScale","modiSeason")], as.factor)
      .
    }
    # data.hznu.teaching.decoupling.selected$runtimeClass<-as.factor(apply(X=data.hznu.teaching.decoupling.selected[,c("runtime")],
    #                                                            MARGIN = 1,FUN = getRuntimeClass))
    # data.hznu.teaching.decoupling.selected$setTempClass<-as.factor(
    #   apply(data.hznu.teaching.decoupling.selected[,c("setTemp")],MARGIN = 1,FUN = getSetTempClass,state=i))
    # 
    
    ####训练集/测试集划分####
    set.seed(711)
    sub<-sample(1:nrow(data.hznu.teaching.decoupling.selected),round(nrow(data.hznu.teaching.decoupling.selected))*8/10)
    data.hznu.teaching.decoupling.training<-data.hznu.teaching.decoupling.selected[sub,]
    data.hznu.teaching.decoupling.test<-data.hznu.teaching.decoupling.selected[-sub]
    

    
    if(length(unique(data.hznu.teaching.decoupling.selected$modiSeason))>1){
      tenFoldFormula<-decouplingFormula
    }else{
      tenFoldFormula<-as.formula(paste("energyClusterName ~ ",paste(decouplingAttr[decouplingAttr!="modiSeason"],collapse = "+")))
    }
    
    #CART决策树算法
    if(TRUE){
      algo<-"CART_Tree"
      
      #10折交叉验证法
      #train方法无法对仅有一个级别的变量进行训练
      # fit<-train(form=tenFoldFormula,na.action = "na.omit",
      #            data=data.hznu.teaching.decoupling.selected,method = "rpart",#tuneGrid=expand.grid(cp=seq(from= localInitCP,to=0.2,by=0.005)),
      #            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))
      # plot(as.party(fit$finalModel))
      # list.hznu.decoupling.cart[[i]][[j]][["10Fold"]]<-fit$finalModel
      # outputImg(as.party(fit$finalModel),hit=900,wid = 1600,fileName =paste(i,j,algo,"10Fold_TreeMap.png",sep = "_"))
      # cmResult<-confusionMatrix(data=fit$pred$pred,reference = fit$pred$obs)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="10-fold"))
      # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"10Fold_Result.txt",sep = "_"),
      #                 algoName = algo,tree = fit$finalModel , fmla = tenFoldFormula, logTitle =  paste(i,j,algo,"10Fold_Result",sep = "_"),
      #                 other = list(paste("Total node: ",length(as.party(fit$finalModel))),fit$finalModel$variable.importance))
      # rm(fit,cmResult)
      
      #hold-out验证法
      # tree.both<-rpart(decouplingFormula,cp=0.02,minbucket=20,#localInitCP,
      #                # maxsurrogate=100,maxcompete=10,
      #                data=data.hznu.teaching.decoupling.training)#rpart,即经典决策树，必须都为factor或定性,连char都不行...
      # # tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#tree.both$cptable步长随机，很难保证一致输出
      # rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party"
      # plot(rpartTrue2)
      # # par(mfrow=c(1,1))
      # prp(tree.both,type=5,extra = 8,varlen=0,faclen=0,digits = 3,gap =0,tweak =1.05)
     
      
      # list.hznu.decoupling.cart[[i]][[j]][["holdOut"]]<-tree.both
      
      
      #保存样本的预测值以及各分类的概率
      data.hznu.teaching.decoupling.log<-rbind(data.hznu.teaching.decoupling.log,
                                               data.table(labelRoomDay=data.hznu.teaching.decoupling.test$labelRoomDay,
                                                          finalState=i,
                                                          clusterName=j,
                                                          method="CART",
                                                          type="test",
                                                          real=data.hznu.teaching.decoupling.test$energyClusterName,
                                                          pred=predict(list.hznu.decoupling.cart[[i]][[j]][["holdOut"]],
                                                                       data.hznu.teaching.decoupling.test,type="class"),
                                                          predict(list.hznu.decoupling.cart[[i]][[j]][["holdOut"]],
                                                                  data.hznu.teaching.decoupling.test,type="prob")),fill=TRUE)
      
      
      #测试集验证
      # cmResult<-data.hznu.teaching.decoupling.test%>%
      #           predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = rpartTrue2)
      #   # predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
      #   #             predictableModel = rpartTrue2)
      # #结果输出
      # outputImg(rpartTrue2,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
      # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
      #                 algoName = algo,tree = tree.both , fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
      #                 other = list(paste("Total node: ",length(rpartTrue2)),tree.both$variable.importance) )
      # #内存结果保留
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      # #训练集结果写入内存
      # cmResultTraining<-data.hznu.teaching.decoupling.training%>%
      #                   predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = rpartTrue2)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
      #                                                acc=cmResultTraining$overall["Accuracy"],setType="training"))
      # rm(tree.both,rpartTrue2,cmResult,cmResultTraining)#临时变量清除
    }
    
    #ID3决策树算法
    if(FALSE){
      algo<-"ID3_Tree"
      tree.both<-rpart(decouplingFormula,cp=localInitCP,
                       # maxsurrogate=100,maxcompete=10,
                       data=data.hznu.teaching.decoupling.training,parms=list(split="information"))#rpart,即经典决策树，必须都为factor或定性,连char都不行...
      tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])
      rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
      plot(rpartTrue2)
      #测试集验证
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = rpartTrue2)
      #结果输出
      outputImg(rpartTrue2,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
      outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),
                      algoName = algo,tree = tree.both,fmla = decouplingFormula,logTitle =  paste(i,j,algo,"Result",sep = "_"),
                      other = list(paste("Total node: ",length(rpartTrue2)),tree.both$variable.importance) )
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-data.hznu.teaching.decoupling.training%>%
                        predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = rpartTrue2)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(tree.both,rpartTrue2,cmResult,cmResultTraining)
    }
    
  
   
    #C50
    # tree.both<-C5.0(decouplingFormula,data=data.hznu.teaching.decoupling.training,rules = TRUE)
    # C50有问题
    
    # C4.5
    if(FALSE){
      algo<-"C4.5_Tree_notBinary"
      tree.both<-J48(decouplingFormula,
                       data=data.hznu.teaching.decoupling.training,control = Weka_control(u=FALSE,M=5,R=TRUE,N=10))#输出有问题, B=TRUE会报错
      # Error in weka_tree_split(i) : 
      #   all(sapply(split, head, 1) == "=") is not TRUE
      # 应该是包的问题
      
      #测试集验证
      cmResult<-data.hznu.teaching.decoupling.test%>%
                predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = tree.both)
      
      #结果输出
      rpartTrue2<-as.party(tree.both)
      plot(rpartTrue2)
      outputImg(tree.both,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
      outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),algoName = algo,tree = tree.both,fmla = decouplingFormula,
                      logTitle =  paste(i,j,algo,"Result",sep = "_"),other = paste("Total node: ",length(rpartTrue2)))
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-data.hznu.teaching.decoupling.training%>%
                        predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = tree.both)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      
      rm(tree.both,rpartTrue2,cmResult,cmResultTraining)
    }
    
    
    #CTree
    if(FALSE){
      algo<-"cTree"
      tree.both<-ctree(decouplingFormula,
                       data=data.hznu.teaching.decoupling.training)
      #测试集验证
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = tree.both)
      #结果输出
      plot(tree.both)
      outputImg(tree.both,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
      outputValidRslt(cm = cmResult, fileName = paste(i, j, algo, "Result.txt", sep = "_"), algoName = algo, tree = tree.both, fmla = decouplingFormula,
                      logTitle =  paste(i,j,algo,"Result",sep = "_"),other = paste("Total node: ",length(tree.both)))
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-data.hznu.teaching.decoupling.training%>%
                        predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = tree.both)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(tree.both,cmResult,cmResultTraining)
    }
    
    
    # #随机森林
    
    if(TRUE){
     
      algo<-"RandomForest"
      #10-fold验证
      # fit<-train(form=tenFoldFormula,na.action = "na.omit",
      #            data=data.hznu.teaching.decoupling.selected,method = "rf",tuneGrid=data.frame(mtry=2:5),importance=TRUE,ntree=1000,
      #            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))#ntree能传进不知道能不能调
      # cmResult<-confusionMatrix(data=fit$pred$pred,reference = fit$pred$obs)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="10-fold"))
      # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"10Fold_Result.txt",sep = "_"),
      #                 algoName = algo, fmla = tenFoldFormula, logTitle =  paste(i,j,algo,"10Fold_Result",sep = "_"),
      #                 other = list("nTree = 1000",importance(fit$finalModel,type = 1),importance(fit$finalModel,type = 2)))
      # rm(fit,cmResult)
      
      
      #hold-out验证
      fit.forest<-randomForest(decouplingFormula,data=data.hznu.teaching.decoupling.training,
                               ntree=1000,cp=localInitCP,mty=2,
                               na.action = na.omit,importance=TRUE)
      
      data.hznu.teaching.decoupling.log<-rbind(data.hznu.teaching.decoupling.log,
                                               data.table(labelRoomDay=data.hznu.teaching.decoupling.test$labelRoomDay,
                                                          finalState=i,
                                                          clusterName=j,
                                                          method="randomForest",
                                                          type="test",
                                                          real=data.hznu.teaching.decoupling.test$energyClusterName,
                                                          pred=predict(fit.forest,
                                                                       data.hznu.teaching.decoupling.test,type="class"),
                                                          predict(fit.forest,
                                                                  data.hznu.teaching.decoupling.test,type="prob")),fill=TRUE)
      # #测试集验证
      # cmResult<-data.hznu.teaching.decoupling.test%>%predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = fit.forest)
      # #结果输出
      # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
      #                 algoName = algo, fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
      #                 other = list("nTree = 1000",importance(fit.forest,type = 1),importance(fit.forest,type = 2)))
      # outputImg(plottable = fit.forest,hit=480,wid=640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
      # #内存结果保留
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      # #训练集结果写入内存
      # cmResultTraining<-data.hznu.teaching.decoupling.training%>%
      #                   predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = fit.forest)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
      #                                                acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(fit.forest,cmResult,cmResultTraining)
    } 
    
    ####AdaBoost####
    if(FALSE){
      algo<-"AdaBoost"
      
      #10Fold #迭代次数大于10时就会报错
      # fit<-train(form=tenFoldFormula,
      #            data=data.hznu.teaching.decoupling.selected,method = "AdaBoost.M1",
      #            na.action = "na.omit",
      #            trControl=trainControl(method = "cv",number = 10,savePredictions ="final",search = "random"))
      # cmResult<-confusionMatrix(data=fit$pred$pred,reference = fit$pred$obs)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="10-fold"))
      # outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"10Fold_Result.txt",sep = "_"),
      #                 algoName = algo, fmla = tenFoldFormula, logTitle =  paste(i,j,algo,"10Fold_Result",sep = "_"),
      #                 other = list("nIter = 200",fit$finalModel$importance))
      # rm(fit,cmResult)
      
      #hold-out 验证法
      fit.boost<-boosting(decouplingFormula,
                          data=data.hznu.teaching.decoupling.training,
                          mfinal = 200,cp=localInitCP)
      nn<-predict(fit.boost,data.hznu.teaching.decoupling.test)
      colnames(nn$prob)<-colnames(nn$confusion)#是个神仙
      data.hznu.teaching.decoupling.log<-rbind(data.hznu.teaching.decoupling.log,
                                               data.table(labelRoomDay=data.hznu.teaching.decoupling.test$labelRoomDay,
                                                          finalState=i,
                                                          clusterName=j,
                                                          method="adaboost",
                                                          type="test",
                                                          real=data.hznu.teaching.decoupling.test$energyClusterName,
                                                          pred=nn$class,
                                                          nn$prob),fill=TRUE)
     
      # #检查误差演变
      # outputImg(FUN = function(x){
      #   plot(x$error,type="o",pch=17,ann=FALSE)
      #   title(xlab = "Iteration times",ylab = "Error")
      # },plottable = errorevol(fit.boost,data.hznu.teaching.decoupling.training),
      # hit=480,wid = 640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
      # 
      # #这特么输出都不统一，能不能一致点？！
      # cmResult<-predict(fit.boost,data.hznu.teaching.decoupling.test)
      # outputValidRslt(cm=NA,logTitle =paste(i,j,algo,"Result",sep = "_"),algoName = algo,fileName=paste(i,j,algo,"Result.txt",sep = "_"),
      #                 FUN = function(fileName=paste(i,j,algo,"Result.txt",sep = "_")){
      #                   capture.output(decouplingFormula,file=fileName,append = TRUE)
      #                   capture.output(cmResult$error,file=fileName,append = TRUE)
      #                   capture.output(cmResult$confusion,file=fileName,append = TRUE)
      #                   capture.output(c("nIter = 200"),file=fileName,append = TRUE)
      #                   capture.output(fit.boost$importance,file=fileName,append = TRUE)
      #                 })
      # #内存结果保留
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$confusion),acc=1-cmResult$error,setType="test"))
      # #训练集结果写入内存
      # cmResultTraining<-predict(fit.boost,data.hznu.teaching.decoupling.training)
      # stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
      #                                     data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$confusion),
      #                                                acc=1-cmResultTraining$error,setType="training"))
      # 
      # rm(fit.boost,cmResult,cmResultTraining)
    }
    
    
    ####SVM####
    if(FALSE){
      algo<-"SVM"
      if(length(unique(data.hznu.teaching.decoupling.selected$modiSeason))>1){
        svmFormula<-decouplingFormula
      }else{
        svmFormula<-energyClusterName ~ thermoPattern + areaScale +setTempClass + meanOutTemp + meanRhOut + runtime
      }#不能直接ifelse返回，报错
      # Error in rep(no, length.out = length(ans)) : 
      # attempt to replicate an object of type 'language'
      
      regm<-ksvm(svmFormula,data=data.hznu.teaching.decoupling.training,
                 kernel="rbfdot",type="C-svc",C=10,cross=10)
      cmResult<-data.hznu.teaching.decoupling.test%>%.[complete.cases(.[,..decouplingAttr])]%>%
                predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = regm )
      #结果输出
      outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
                      algoName = algo, fmla = svmFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"))
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-data.hznu.teaching.decoupling.training%>%.[complete.cases(.[,..decouplingAttr])]%>%
                        predictTest(testSet = .,resultValue = .$energyClusterName,predictableModel = regm)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      
      
      rm(regm,svmFormula,cmResultTraining,cmResult)
    }
 
  }
  ####精确度汇总结果输出####
  # stat.hznu.decoupling.algoAcc$correctCount<-stat.hznu.decoupling.algoAcc$acc*stat.hznu.decoupling.algoAcc$count
  # stat.hznu.decoupling.algoAcc$wrongCount<-stat.hznu.decoupling.algoAcc$count*(1-stat.hznu.decoupling.algoAcc$acc)
  # write.xlsx(stat.hznu.decoupling.algoAcc,file = paste("HZNU",i,"round_final.xlsx",sep = "_"))
}

####批量输出决策树规则####
sink(file = "HZNU_decoupling_rules.txt") 
for(i in names(list.hznu.decoupling.cart)){
  for(j in names(list.hznu.decoupling.cart[[i]])){
    list.hznu.decoupling.cart[[i]][[j]][["holdOut"]]
  }
}
sink() 


for(i in names(list.hznu.decoupling.cart)){
  for(j in names(list.hznu.decoupling.cart[[i]]))
    capture.output(cat("\n",i,j,"\n"),
                   list.hznu.decoupling.cart[[i]][[j]][["holdOut"]],
                   file="HZNU_decoupling_rules.txt",append = TRUE)
}
  


# prp(list.hznu.decoupling.cart$cooling$Forenoon$holdOut,type=5,extra = 8,varlen=0,faclen=0,digits = 3,gap =0,tweak =1.05,cex=1.5)



data.hznu.teaching.decoupling[,.(finalState=finalState[1],
                                 modiSeason=modiSeason[1],
                                 thermoPattern=thermoPattern[1],
                                 runtime=mean(runtime,na.rm = TRUE)),
                              by=(labelModeSeasonThermal=paste(finalState,modiSeason,thermoPattern,sep = "_"))]


####尝试画ROC####

tmp.decopling.roc<- data.hznu.teaching.decoupling.log%>%mutate_all(funs(ifelse(is.na(.), 0, .)))%>%as.data.table(.)%>%.[,-c("pred","type")]%>%
  melt(.,id.vars = c("labelRoomDay","finalState","clusterName","method","real"))%>%.[real==variable]
list.hznu.decoupling.roc.obj<-list()
list.hznu.decoupling.roc.mapping<-list()
#对各工况和各方法构建ROC曲线
#好像不对
for(i in unique(tmp.decopling.roc$finalState)){
  for(j in unique(tmp.decopling.roc$method)){
    #一个工况下-一个方法-所有两两组合的ROC
    cat(i,j)
    list.hznu.decoupling.roc.obj[[i]][[j]]<-
      multiclass.roc(predictor=tmp.decopling.roc[finalState==i&method==j]$value,
                     response=tmp.decopling.roc[finalState==i&method==j]$real,plot=TRUE,smooth=FALSE)
    for(k in 1:length(list.hznu.decoupling.roc.obj[[i]][[j]]$rocs)){
      if(!exists("nn1")){
        nn1<-data.table(level1=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$levels[1],
                        level2=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$levels[2],
                        sensitivities=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$sensitivities,
                        specificities=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$specificities)
      }else{
        nn1<-rbind(nn1,data.table(level1=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$levels[1],
                                  level2=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$levels[2],
                                  sensitivities=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$sensitivities,
                                  specificities=list.hznu.decoupling.roc.obj[[i]][[j]]$rocs[[k]]$specificities))
      }
    }
    list.hznu.decoupling.roc.mapping[[i]][[j]]<-nn1
    rm(nn1)
  }
}

ggplot(data=list.hznu.decoupling.roc.mapping$heating$randomForest,
       aes(x=1-specificities,y=sensitivities,color=(com=paste(level1,level2))))+geom_point(size=0.5)+geom_line()


##multiROC包
sub<-sample(1:nrow(data.hznu.teaching.decoupling.log[type=="train"]),round(nrow(data.hznu.teaching.decoupling.log[type=="train"]))*7/10)
# rbind(data.hznu.teaching.decoupling.log[type=="train"][sub],

tmp.multi.roc<-data.hznu.teaching.decoupling.log[type=="test"]%>%
               .[,-"type"]%>%mutate_all(funs(ifelse(is.na(.), 0, .)))%>%
                mutate(.,MidEnergy_LongTime_true=ifelse(real=="MidEnergy_LongTime",1,0),
                                                          HighEnergy_true=ifelse(real=="HighEnergy",1,0),
                                                          LowEnergy_true=ifelse(real=="LowEnergy",1,0),
                                                          MidEnergy_MidTime_true=ifelse(real=="MidEnergy_MidTime",1,0)
                                                          )%>%as.data.table(.)

for(i in 7:10){
  colnames(tmp.multi.roc)[i] <- paste(colnames(tmp.multi.roc)[i],"_pred_PRD",sep = "")
}
list.hznu.decoupling.multiROC<-list()
stat.hznu.decoupling.index<-data.table(finalState="",method="",indexName="",value=-999)[-1]
for(i in unique(tmp.multi.roc$finalState)){
  for(j in unique(tmp.multi.roc$method)){
    # cat(i,j)
    list.hznu.decoupling.multiROC[[i]][[j]]<-multi_roc(tmp.multi.roc[finalState==i&method==j,7:14])
    nn<-confusionMatrix(data=data.hznu.teaching.decoupling.log[finalState==i&method==j&type=="train"]$pred%>%as.factor(.),
                        reference = data.hznu.teaching.decoupling.log[finalState==i&method==j&type=="train"]$real%>%as.factor(.))
    #再次统计kappa等统计量
    stat.hznu.decoupling.index<-rbind(stat.hznu.decoupling.index,
                                      data.table(finalState=i,method=j,indexName="MarcoAUC",
                                                 value=list.hznu.decoupling.multiROC[[i]][[j]]$AUC$PRD$macro),
                                      data.table(finalState=i,method=j,indexName="MicroAUC",
                                                 value=list.hznu.decoupling.multiROC[[i]][[j]]$AUC$PRD$micro),
                                      data.table(finalState=i,method=j,indexName="Kappa",value=nn$overall["Kappa"]),
                                      data.table(finalState=i,method=j,indexName="Accuracy",value=nn$overall["Accuracy"]))
    rm(nn)
  }
}

# ROC绘图
for(i in unique(tmp.multi.roc$finalState)){
  for(j in unique(tmp.multi.roc$method)){
    nn<-list.hznu.decoupling.multiROC[[i]][[j]]%>%plot_roc_data(.)%>%as.data.table(.)%>%.[!Group %in% c("Micro","Macro")]%>%
      ggplot(data=.,aes(x = 1-Specificity, y=Sensitivity,color=Group,lty=Group))+
       geom_line(size=1)+
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                   colour='grey', linetype = 'dotdash')+
      theme_bw() + scale_color_brewer(palette="Dark2")+
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),legend.text = element_text(size=16),
            legend.justification=c(1, 0), legend.position=c(.95, .05),
            legend.title=element_blank(), 
            legend.background = element_rect(fill=NULL, size=0.5, 
                                             linetype="solid", colour ="black"))
      ggsave(plot=nn,file=paste(i,j,".png",sep = ""),width=8,height = 6,dpi = 100)#管道函数默认第一个有点麻烦
  }
}

# geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               colour='grey', linetype = 'dotdash')
data.hznu.teaching.decoupling.log%>%{
  for(i in unique(.$finalState)){
    for(j in unique(.$method)){
      cat("\n",i,j,"\n")
      cat(nrow(.[finalState==i&method==j&real==pred])/nrow(.[finalState==i&method==j]))
    }
  }
}
#train
# heating CART 
# 0.7109701
# heating randomForest 
# 0.9751587
# heating adaboost 
# 0.8944696
# cooling CART 
# 0.7749034
# cooling randomForest 
# 0.9653285
# cooling adaboost 
# 0.8739802

#test
# heating adaboost 
# 0.7031137
# heating CART 
# 0.6741492
# heating randomForest 
# 0.73063
# cooling adaboost 
# 0.7860206
# cooling CART 
# 0.7675815
# cooling randomForest 
# 0.7950257

#full
# heating CART 
# 0.7035963
# heating randomForest 
# 0.9261891
# heating adaboost 
# 0.8561485
# cooling CART 
# 0.7734375
# cooling randomForest 
# 0.9312328
# cooling adaboost 
# 0.8563702



####获取不同空调数对应的面积等级####
getAreaLevel<-function(acCount){
  if(acCount<=2)
    return("Small")
  if(acCount<=4)
    return("Mid")
  if(acCount<=16)
    return("Large")
}
  
  
  
####获取空调运行时间等级####
getRuntimeClass<-function(time){
  if(time<3)
    return("<3")
  if(time<6)
    return("[3,6)")    
  if(time<10)
    return("[6,10)")
  return(">=10")
}

####获取设定温度划分####  
getSetTempClass<-function(setTemp,state){
  if(is.na(setTemp))
    return(NA)
  if(state=="cooling"){
    if(setTemp<=24)
      return("<=24")
    if(setTemp<=26)
      return("(24,26]")
    if(setTemp>26)
      return(">26")
  }
  if(state=="heating"){
    if(setTemp<=22)
      return("<=22")
    if(setTemp<=24)
      return("(22,24]")
    if(setTemp>24)
      return(">24")
  }
}
  
  
####定义批量输出的函数####
outputValidRslt<-function(cm,fileName,tree=NA,logTitle="",fmla="",algoName="",other=NA,FUN=NA){
  capture.output(logTitle,file=fileName)
  capture.output(paste("Algorithm: ",algoName),file=fileName,append = TRUE)
  if(!is.na(FUN)){
    FUN(fileName)
    return()
  }
  capture.output(paste("nCount:",sum(cm$table)),file=fileName,append = TRUE)
  capture.output(cm$overall,file=fileName,append = TRUE)
  capture.output(cm$table,file=fileName,append = TRUE)
  if(!is.na(tree)){
    capture.output(tree,file=fileName,append = TRUE)
  }
  tryCatch({
    if(!is.na(other)){
      for(i in other){
        capture.output(i,file=fileName,append = TRUE)
      }
    }
  },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
}


