#本脚本用于能耗与行为的解耦
#发篇EI用
#用数据集 data.hznu.teaching.energy.std

data.hznu.energy.decoupling<-data.hznu.teaching.energy.std[,c("labelRoomDay","roomCode","date",
                                                              "acCount","finalState","clusterName","acIntensity","runtime","sumElec","energyClusterName")]
data.hznu.energy.decoupling$modiSeason<-as.factor(apply(data.hznu.energy.decoupling[,"date"],MARGIN = 1,FUN = function(x){ getSeason(as.numeric(substr(x,6,7)))}))
data.hznu.energy.decoupling[modiSeason %in% c("Spring","Autumn")]$modiSeason<-"Transition"
data.hznu.energy.decoupling$areaScale<-as.factor(apply(data.hznu.energy.decoupling[,"acCount"],MARGIN = 1,FUN = getAreaLevel))
data.hznu.energy.decoupling$energyClusterName<-as.factor(data.hznu.energy.decoupling$energyClusterName)
data.hznu.energy.decoupling$clusterName<-as.factor(data.hznu.energy.decoupling$clusterName)



modiSelect<-"heating"
{
  # 训练集测试集划分
  data.hznu.energy.decoupling.select<-data.hznu.energy.decoupling[finalState==modiSelect]
  set.seed(711)
  sub<-sample(1:nrow(data.hznu.energy.decoupling.select),round(nrow(data.hznu.energy.decoupling.select))*3/4)
  data.hznu.energy.decoupling.select.training<-data.hznu.energy.decoupling.select[sub]
  data.hznu.energy.decoupling.select.test<-data.hznu.energy.decoupling.select[-sub]
  
  #CART决策树
  tree.energy.decoupling<-rpart(energyClusterName~clusterName+modiSeason+acIntensity+areaScale+runtime,
                                data = data.hznu.energy.decoupling.select.training,cp=0.005)
  tree.energy.decoupling<-prune(tree.energy.decoupling,
                                cp=tree.energy.decoupling$cptable[which.min(tree.energy.decoupling$cptable[,"xerror"]),"CP"])
  tree.energy.decoupling.party<-as.party(tree.energy.decoupling)
  plot(tree.energy.decoupling.party)
  
  predictTest(testSet = data.hznu.energy.decoupling.select.test,resultValue = data.hznu.energy.decoupling.select.test$energyClusterName,
              predictableModel = tree.energy.decoupling.party,isOutput = FALSE,fileName = paste("HZNU",modiSelect,"CART_Evaluation_seed=95.txt",sep = "_"))
  
  #随机森林
  fit.forest.energy.decoupling<-randomForest(energyClusterName~clusterName+modiSeason+acIntensity+areaScale+runtime,cp=0,
                                             data=data.hznu.energy.decoupling.select.training,ntree=1000)
  predictTest(testSet = data.hznu.energy.decoupling.select.test,resultValue = data.hznu.energy.decoupling.select.test$energyClusterName,
              predictableModel = fit.forest.energy.decoupling,isOutput = T,fileName = paste("HZNU",modiSelect,"RF_Evaluation_seed=19950711.txt",sep = "_"))
  
}
