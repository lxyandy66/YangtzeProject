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
  fit.forest.energy.decoupling<-randomForest(energyClusterName~clusterName+modiSeason+acIntensity+areaScale+runtime,cp=0,importance=TRUE,
                                             data=data.hznu.energy.decoupling.select.training,ntree=1000)
  importance(fit.forest.energy.decoupling)
  predictTest(testSet = data.hznu.energy.decoupling.select.test,resultValue = data.hznu.energy.decoupling.select.test$energyClusterName,
              predictableModel = fit.forest.energy.decoupling,isOutput = TRUE,fileName = paste("HZNU",modiSelect,"RF_Evaluation_seed=19950711.txt",sep = "_"))
  
}

tmp.roc<-data.table(real=data.hznu.energy.decoupling.select.test$energyClusterName,
                    predict(fit.forest.energy.decoupling,data.hznu.energy.decoupling.select.test,type="prob")) %>% melt(.,id.vars = "real")%>%
                    .[real==variable] %>% .[,c("real","value")]
#丢python里面试试
# write.csv(x=tmp.roc[,c("real","value")],file = "rf_cooling.csv")
nn<-multiclass.roc(predictor=tmp.roc$value,response=tmp.roc$real,plot=TRUE,smooth=FALSE)

rm(nn1)
for(i in 1:length(nn$rocs)){
  if(!exists("nn1")){
    nn1<-data.table(level1=nn$rocs[[i]]$levels[1],level2=nn$rocs[[i]]$levels[2],sensitivities=nn$rocs[[i]]$sensitivities,specificities=nn$rocs[[i]]$specificities)
  }else{
    nn1<-rbind(nn1,data.table(level1=nn$rocs[[i]]$levels[1],level2=nn$rocs[[i]]$levels[2],sensitivities=nn$rocs[[i]]$sensitivities,specificities=nn$rocs[[i]]$specificities))
  }
}
ggplot(data=nn1,aes(x=1-specificities,y=sensitivities,color=(com=paste(level1,level2))))+geom_point(size=0.5)+geom_line()
nn2<-nn1[,.(specificities=mean(specificities,na.rm = TRUE)),by=(modiSensi=as.numeric(substr(sensitivities,1,4)))]#modiSpeci
ggplot(data=nn2,aes(x=1-specificities,y=modiSensi))+geom_point(size=0.5)+geom_line()


#EI外审意见的一些修改和数据可视化
nn<-data.hznu.teaching.decoupling[,.(meanSetTemp=mean(setTemp,na.rm=TRUE)),by=(month=substr(date,6,7))] %>%
    cbind(.,meanOutTemp=data.weather.airport.final[,.(meanOutTemp=mean(outTemp,na.rm = TRUE)),
                                                 by=(month=substr(labelHour,6,7))]$meanOutTemp)

#室外温度与设定温度的可视化
ggplot()+geom_boxplot(data=data.hznu.teaching.decoupling,aes(x=(month=as.factor(substr(date,6,7))),y=setTemp),outlier.colour = NA,width=0.45)+
  geom_point(data=nn,aes(x=as.factor(month),y=meanOutTemp,group=1),size=3,shape=2,color="red")+geom_line(data=nn,aes(x=as.factor(month),y=meanOutTemp,group=1,color="red"),size=0.75)+
  geom_point(data=nn,aes(x=factor(month),y=meanSetTemp,group=1),size=3)+geom_line(data=nn,aes(x=month,y=meanSetTemp,group=1))+
  scale_y_continuous(breaks = seq(0,35,2.5))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"),strip.text =element_text(size=16),
        legend.text = element_text(size=16))



ggplot(data=data.hznu.teaching.decoupling,aes(x=(month=as.factor(substr(date,6,7))),y=setTemp))+#ylim(0,2.5)+ color=energyClusterName
  geom_boxplot(outlier.colour = NA,width=0.5)+facet_wrap(~finalState)+  
  geom_line(data=stat.hznu.teaching.energy.bySeat[modiSeat!=0],aes(x=factor(modiSeat),y=meanAreaEUI,group=finalState))+
  geom_point(data=stat.hznu.teaching.energy.bySeat[modiSeat!=0],aes(x=factor(modiSeat),y=meanAreaEUI,group=finalState))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,2.5)+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),strip.text =element_text(size=14),
        legend.text = element_text(size=14))
