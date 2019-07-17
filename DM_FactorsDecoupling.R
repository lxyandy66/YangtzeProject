####影响因素解耦部分####

# data.hznu.use.final 行为已聚类数据
# data.hznu.teaching.energy.std 能耗已聚类数据
# data.hznu.teaching.thermo.final 热环境已聚类数据

####以能耗模式为基准合并####
data.hznu.teaching.decoupling<-merge(
  x=data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date","acCount","energyClusterName")],
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

#作为整体准确度计算和算法评估的汇总
stat.hznu.decoupling.algoAcc<-data.table(algoName="",setType="",finalState="",usagePattern="",count=as.numeric(NA),acc=as.numeric(NA))

set.seed(711)

####训练集/测试集划分####
#分块处理
for(i in c("cooling")){#unique(data.hznu.teaching.decoupling$finalState)
  for(j in unique(data.hznu.teaching.decoupling[finalState==i]$clusterName)){
  
    data.hznu.teaching.decoupling.selected<-data.hznu.teaching.decoupling[finalState==i&clusterName==j]
    #变量的预处理
    data.hznu.teaching.decoupling.selected$energyClusterName<-as.factor(data.hznu.teaching.decoupling.selected$energyClusterName)
    data.hznu.teaching.decoupling.selected$thermoPattern<-as.factor(data.hznu.teaching.decoupling.selected$thermoPattern)
    data.hznu.teaching.decoupling.selected$clusterName<-as.factor(data.hznu.teaching.decoupling.selected$clusterName)
    data.hznu.teaching.decoupling.selected$areaScale<-as.factor(data.hznu.teaching.decoupling.selected$areaScale)
    data.hznu.teaching.decoupling.selected$modiSeason<-as.factor(data.hznu.teaching.decoupling.selected$modiSeason)
    data.hznu.teaching.decoupling.selected$runtimeClass<-as.factor(apply(X=data.hznu.teaching.decoupling.selected[,c("runtime")],
                                                               MARGIN = 1,FUN = getRuntimeClass))
    data.hznu.teaching.decoupling.selected$setTempClass<-as.factor(
      apply(data.hznu.teaching.decoupling.selected[,c("setTemp")],MARGIN = 1,FUN = getSetTempClass))
    # data.hznu.teaching.decoupling.selected$runtime<-as.factor(data.hznu.teaching.decoupling.selected$runtime)
    
    
    ####训练集/测试集划分####
    data.hznu.teaching.decoupling.selected<-data.hznu.teaching.decoupling.selected[complete.cases(data.hznu.teaching.decoupling.selected[,..decouplingAttr])]
    sub<-sample(1:nrow(data.hznu.teaching.decoupling.selected),round(nrow(data.hznu.teaching.decoupling.selected))*8/10)
    data.hznu.teaching.decoupling.training<-data.hznu.teaching.decoupling.selected[sub]
    data.hznu.teaching.decoupling.test<-data.hznu.teaching.decoupling.selected[-sub]
    
    ####定义能耗解耦关系式####
    decouplingAttr<-c("thermoPattern","areaScale","modiSeason","setTempClass","meanOutTemp","meanRhOut","runtime")
    decouplingFormula<-as.formula(paste("energyClusterName ~ ",paste(decouplingAttr,collapse = "+")))
    
    #CART决策树算法
    {
      algo<-"CART_Tree"
      tree.both<-rpart(decouplingFormula,cp=0,#.001,
                     # maxsurrogate=100,maxcompete=10,
                     data=data.hznu.teaching.decoupling.training)#rpart,即经典决策树，必须都为factor或定性,连char都不行...
      tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])
      rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
      plot(rpartTrue2)
      #测试集验证
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = rpartTrue2)
      #结果输出
      outputImg(rpartTrue2,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
      outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
                      algoName = algo,tree = tree.both , fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
                      other = list(paste("Total node: ",length(rpartTrue2)),tree.both$variable.importance) )
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = rpartTrue2)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(tree.both,rpartTrue2,cmResult,cmResultTraining)
    }
    
    #ID3决策树算法
    {
      algo<-"ID3_Tree"
      tree.both<-rpart(decouplingFormula,cp=0,#.001,
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
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = rpartTrue2)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(tree.both,rpartTrue2,cmResult,cmResultTraining)
    }
    
  
   
    #C50
    # tree.both<-C5.0(decouplingFormula,data=data.hznu.teaching.decoupling.training,rules = TRUE)
    # C50有问题
    
    # C4.5
    {
      algo<-"C4.5_Tree_notBinary"
      tree.both<-J48(decouplingFormula,
                       data=data.hznu.teaching.decoupling.training,control = Weka_control(u=FALSE,M=5,R=TRUE,N=10))#输出有问题, B=TRUE会报错
      # Error in weka_tree_split(i) : 
      #   all(sapply(split, head, 1) == "=") is not TRUE
      # 应该是包的问题
      
      #测试集验证
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = tree.both)
      
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
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = tree.both)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      
      rm(tree.both,rpartTrue2,cmResult,cmResultTraining)
    }
    
    
    #CTree
    {
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
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = tree.both)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(tree.both,cmResult,cmResultTraining)
    }
    
    
    # #随机森林
    
    {
      algo<-"RandomForest"
      fit.forest<-randomForest(decouplingFormula,data=data.hznu.teaching.decoupling.training,ntree=1000)
      #测试集验证
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = fit.forest)
      #结果输出
      outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
                      algoName = algo, fmla = decouplingFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"),
                      other = list("nTree = 1000"))
      outputImg(plottable = fit.forest,hit=480,wid=640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = fit.forest)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      rm(fit.forest,cmResult,cmResultTraining)
    } 
    
    ####AdaBoost####
    #这里有问题
    {
      algo<-"AdaBoost"
      fit.boost<-boosting(decouplingFormula,
                          data=data.hznu.teaching.decoupling.training,mfinal = 200)
      #检查误差演变
      outputImg(FUN = function(x){
        plot(x$error,type="o",pch=17,ann=FALSE)
        title(xlab = "Iteration times",ylab = "Error")
      },plottable = errorevol(fit.boost,data.hznu.teaching.decoupling.training),
      hit=480,wid = 640,fileName = paste(i,j,algo,"Err.png",sep = "_"))
      
      #这特么输出都不统一，能不能一致点？！
      cmResult<-predict(fit.boost,data.hznu.teaching.decoupling.test)
      outputValidRslt(cm=NA,logTitle =paste(i,j,algo,"Result",sep = "_"),algoName = algo,fileName=paste(i,j,algo,"Result.txt",sep = "_"),
                      FUN = function(fileName=paste(i,j,algo,"Result.txt",sep = "_")){
                        capture.output(decouplingFormula,file=fileName,append = TRUE)
                        capture.output(cmResult$error,file=fileName,append = TRUE)
                        capture.output(cmResult$confusion,file=fileName,append = TRUE)
                        capture.output(c("nIter = 200"),file=fileName,append = TRUE)
                      })
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$confusion),acc=1-cmResult$error,setType="test"))
      #训练集结果写入内存
      cmResultTraining<-predict(fit.boost,data.hznu.teaching.decoupling.training)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$confusion),
                                                     acc=1-cmResultTraining$error,setType="training"))
      rm(fit.forest,cmResult,cmResultTraining)
    }
    
    
    ####SVM####
    {
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
      cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                            predictableModel = regm)
      #结果输出
      outputValidRslt(cm=cmResult, fileName = paste(i,j,algo,"Result.txt",sep = "_"),
                      algoName = algo, fmla = svmFormula, logTitle =  paste(i,j,algo,"Result",sep = "_"))
      #内存结果保留
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"],setType="test"))
      #训练集结果写入内存
      cmResultTraining<-predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
                                    predictableModel = regm)
      stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                          data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResultTraining$table),
                                                     acc=cmResultTraining$overall["Accuracy"],setType="training"))
      
      
      rm(regm,svmFormula,cmResultTraining,cmResult)
    }
  }
}

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
getSetTempClass<-function(setTemp){
  if(is.na(setTemp))
    return(NA)
  if(setTemp<=24)
    return("<=24")
  if(setTemp<=26)
    return("(24,26]")
  if(setTemp>26)
    return(">26")
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


