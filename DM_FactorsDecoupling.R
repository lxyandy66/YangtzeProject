####影响因素解耦部分####

# data.hznu.use.final 行为已聚类数据
# data.hznu.teaching.energy.std 能耗已聚类数据
# data.hznu.teaching.thermo.final 热环境已聚类数据

####以能耗模式为基准合并####
data.hznu.teaching.decoupling<-merge(
  x=data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date","acCount","sumUiElec",sprintf("euiH%d",8:22),"energyClusterName")],
  y=data.hznu.teaching.thermo.final[,c("labelRoomDay","runtime","modiSeason","finalState","clusterName","thermoPattern")],
  all.x = TRUE,by.x = "labelRoomDay",by.y = "labelRoomDay")
data.hznu.teaching.decoupling<-
  data.hznu.teaching.decoupling[complete.cases(data.hznu.teaching.decoupling[,c("clusterName","thermoPattern")])]

ggplot(data=data.hznu.teaching.decoupling,aes(x=acCount))+geom_density()
data.hznu.teaching.decoupling$areaScale<-apply(data.hznu.teaching.decoupling[,"acCount"],MARGIN = 1, FUN = getAreaLevel)



####训练集/测试集划分####

# for(i in unique(data.hznu.teaching.decoupling[finalState=="cooling"]$clusterName)){

  #分块处理
  data.hznu.teaching.decoupling.selected<-data.hznu.teaching.decoupling[finalState=="cooling"]
  ggplot(data=data.hznu.teaching.decoupling.selected,aes(x=runtime,color=clusterName))+geom_density()
  data.hznu.teaching.decoupling.selected$energyClusterName<-as.factor(data.hznu.teaching.decoupling.selected$energyClusterName)
  data.hznu.teaching.decoupling.selected$thermoPattern<-as.factor(data.hznu.teaching.decoupling.selected$thermoPattern)
  data.hznu.teaching.decoupling.selected$clusterName<-as.factor(data.hznu.teaching.decoupling.selected$clusterName)
  data.hznu.teaching.decoupling.selected$areaScale<-as.factor(data.hznu.teaching.decoupling.selected$areaScale)
  data.hznu.teaching.decoupling.selected$modiSeason<-as.factor(data.hznu.teaching.decoupling.selected$modiSeason)
  data.hznu.teaching.decoupling.selected$runtimeClass<-(apply(X=data.hznu.teaching.decoupling.selected[,c("runtime")],
                                                             MARGIN = 1,FUN = getRuntimeClass))
  # data.hznu.teaching.decoupling.selected$runtime<-as.factor(data.hznu.teaching.decoupling.selected$runtime)
  
  ####训练集/测试集划分####
  set.seed(711)
  sub<-sample(1:nrow(data.hznu.teaching.decoupling.selected),round(nrow(data.hznu.teaching.decoupling.selected))*8/10)
  data.hznu.teaching.decoupling.training<-data.hznu.teaching.decoupling.selected[sub]
  data.hznu.teaching.decoupling.test<-data.hznu.teaching.decoupling.selected[-sub]
  
  #CART决策树算法
  tree.both<-rpart(energyClusterName~clusterName+areaScale+modiSeason,cp=0.001,
                   data=data.hznu.teaching.decoupling.training)#rpart,即经典决策树，必须都为factor或定性,连char都不行...
  rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
  plot(rpartTrue2)
  # tree.both$cptable
  # tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#最优剪枝
 
  #C50
  tree.both<-C5.0.formula(energyClusterName~clusterName+areaScale+modiSeason+runtime,data=data.hznu.teaching.decoupling.training)
  # 
  #CTree
  tree.both<-ctree(energyClusterName~clusterName+areaScale+modiSeason+runtime,
                   data=data.hznu.teaching.decoupling.training)
  rpartTrue2<-tree.both
  
  # #随机森林
  fit.forest<-randomForest(energyClusterName~clusterName+areaScale+modiSeason+runtimeClass,
                           data=data.hznu.teaching.decoupling.training,ntree=1000)
  rpartTrue2<-fit.forest
  
  ####AdaBoost####
  #这里有问题
  tree.both<-boosting(energyClusterName~clusterName+areaScale+modiSeason+runtimeClass,
                      data=data.hznu.teaching.decoupling.training,mfinal = 200)
  rpartTrue2<-tree.both
  #检查误差演变
  b<-errorevol(rpartTrue2,data.hznu.teaching.decoupling.training)
  plot(b$error,type="l",main="AdaBoost error vs number of trees")
  
  ####SVM####
  regm<-ksvm(energyClusterName~clusterName+areaScale+modiSeason+runtime,
             data=data.hznu.teaching.decoupling.training,
             kernel="rbfdot",type="C-bsvc",C=10,cross=10)
  rpartTrue2<-regm
  

  
  ####以训练集验证####
  rtree.training<-predict(rpartTrue2,data.hznu.teaching.decoupling.training)
  confusionMatrix(table(rtree.training,data.hznu.teaching.decoupling.training$energyClusterName))
  capture.output(c(confusionMatrix(table(rtree.training,data.hznu.teaching.decoupling.training$energyClusterName)),
                   asRules(tree.both)),
                 file =paste("HZNU_行为_制冷_训练集_决策树评估.txt"))
  ####以测试集验证####
  rtree.predict<-predict(rpartTrue2,data.hznu.teaching.decoupling.test)
  confusionMatrix(table(rtree.predict,data.hznu.teaching.decoupling.test$energyClusterName))
  capture.output(c(confusionMatrix(table(rtree.predict,data.hznu.teaching.decoupling.test$energyClusterName)),
                   asRules(tree.both)),
                 file =paste("HZNU_行为_Kheating=5_制热_预测集_决策树评估.txt"))
  rm(rpartTrue2)
  
# }




####获取不同空调数对应的面积等级####
getAreaLevel<-function(acCount){
  if(acCount<=2)
    return("Small")
  if(acCount<=8)
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
