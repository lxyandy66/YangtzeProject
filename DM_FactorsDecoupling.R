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
stat.hznu.decoupling.algoAcc<-data.table(algoName="",finalState="",usagePattern="",count=as.numeric(NA),acc=as.numeric(NA))

####训练集/测试集划分####
#分块处理
for(i in unique(data.hznu.teaching.decoupling$finalState)){
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
  set.seed(711)
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
    outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),algoName = algo,tree = tree.both,fmla = decouplingFormula)
    #内存结果保留
    stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                        c(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"]))
    rm(tree.both,rpartTrue2)
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
    outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),algoName = algo,tree = tree.both,fmla = decouplingFormula)
    #内存结果保留
    stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                        data.table(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"]))
    rm(tree.both,rpartTrue2)
  }
  

 
  #C50
  # tree.both<-C5.0(decouplingFormula,data=data.hznu.teaching.decoupling.training,rules = TRUE)
  # C50有问题
  
  # C4.5
  {
    algo<-"C4.5_Tree"
    tree.both<-J48(decouplingFormula,
                     data=data.hznu.teaching.decoupling.training,control = Weka_control(u=FALSE,M=5,R=TRUE,N=10,B=TRUE))#好像有点问题，重置一下环境试试
    #测试集验证
    cmResult<-predictTest(testSet = data.hznu.teaching.decoupling.test,resultValue = data.hznu.teaching.decoupling.test$energyClusterName,
                          predictableModel = tree.both)
    #结果输出
    plot(as.party(tree.both))
    outputImg(tree.both,hit=900,wid = 1600,fileName =paste(i,j,algo,"TreeMap.png",sep = "_"))
    outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),algoName = algo,tree = tree.both,fmla = decouplingFormula)
    #内存结果保留
    stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                        c(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"]))
    rm(tree.both,rpartTrue2)
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
    outputValidRslt(cm = cmResult,fileName = paste(i,j,algo,"Result.txt",sep = "_"),algoName = algo,tree = tree.both,fmla = decouplingFormula)
    #内存结果保留
    stat.hznu.decoupling.algoAcc<-rbind(stat.hznu.decoupling.algoAcc,
                                        c(algoName=algo,finalState=i,usagePattern=j,count=sum(cmResult$table),acc=cmResult$overall["Accuracy"]))
    rm(tree.both,rpartTrue2)
  }
  
  
  # #随机森林
  fit.forest<-randomForest(decouplingFormula,
                           data=data.hznu.teaching.decoupling.training,ntree=1000)
  rpartTrue2<-fit.forest
  
  ####AdaBoost####
  #这里有问题
  tree.both<-boosting(decouplingFormula,
                      data=data.hznu.teaching.decoupling.training,mfinal = 200)
  rpartTrue2<-tree.both
  #检查误差演变
  b<-errorevol(rpartTrue2,data.hznu.teaching.decoupling.training)
  plot(b$error,type="l",main="AdaBoost error vs number of trees")
  
  ####SVM####
  regm<-ksvm(decouplingFormula,
             data=data.hznu.teaching.decoupling.training,
             kernel="rbfdot",type="C-bsvc",C=10,cross=10)
  rpartTrue2<-regm
  

  ####以测试集验证####
  # rtree.predict<-predict(rpartTrue2,data.hznu.teaching.decoupling.test)
  # confusionMatrix(table(rtree.predict,data.hznu.teaching.decoupling.test$energyClusterName))
  # capture.output(c(confusionMatrix(table(rtree.predict,data.hznu.teaching.decoupling.test$energyClusterName)),
  #                  asRules(tree.both)),
  #                file =paste("HZNU_行为_Kheating=5_制热_预测集_决策树评估.txt"))
  rm(rpartTrue2)
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
outputValidRslt<-function(cm,fileName,tree=NA,title="",fmla="",algoName=""){
  capture.output(file = fileName,
                 cat(title,#fmla,
                     paste("nCount:",sum(cm$table)),
                     cm$overall,
                     cm$table,
                     sep = "\n"
                     )
                 )
  sink(file = fileName,append = TRUE)
  algoName
  if(!is.na(tree)){
    tree
  }
  sink()
}
  
####定义图片输出的函数####
#适用于R自带的plot无法直接输出的情况，ggplot不适用，可直接ggsave
outputImg<-function(plottable,hit,wid,fileName){
  png(filename = fileName,height = hit,width = wid)
  plot(plottable)
  dev.off()
}
