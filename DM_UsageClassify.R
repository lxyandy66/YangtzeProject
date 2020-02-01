####影响因素解耦部分####

#接list.hznu.room.use
#已统一全年行为模式名称

#本脚本仅考虑行为部分，即记录-行为关系
#对行为数据处理至房间级，并进行行为模式聚类

data.hznu.use.final<-as.data.table(list.hznu.room.use[[1]])
for(i in c(2:6)){
  data.hznu.use.final<-rbind(data.hznu.use.final,list.hznu.room.use[[i]])
}


####全年基本情况统计####
ggplot(data=data.hznu.use.final,aes(x=modiSeason,y=runtime))+geom_boxplot()
boxplot(data = data.hznu.use.final,runtime~
          paste(data.hznu.use.final$modiSeason,data.hznu.use.final$finalState))
#按空调模式统计数据
stat.use.room.cluster.byMode<-data.hznu.use.final[,.(
  count=length(labelRoomDay),
  finalState=unique(finalState),
  clusterName=unique(clusterName),
  runtime=mean(runtime,na.rm = TRUE),
  h8=mean(h8,na.rm = TRUE),
  h9=mean(h9,na.rm = TRUE),
  h10=mean(h10,na.rm = TRUE),
  h11=mean(h11,na.rm = TRUE),
  h12=mean(h12,na.rm = TRUE),
  h13=mean(h13,na.rm = TRUE),
  h14=mean(h14,na.rm = TRUE),
  h15=mean(h15,na.rm = TRUE),
  h16=mean(h16,na.rm = TRUE),
  h17=mean(h17,na.rm = TRUE),
  h18=mean(h18,na.rm = TRUE),
  h19=mean(h19,na.rm = TRUE),
  h20=mean(h20,na.rm = TRUE),
  h21=mean(h21,na.rm = TRUE),
  h22=mean(h22,na.rm = TRUE)
),by=paste(clusterName,finalState,sep = "_")]
plot.use.room.cluster.byMode<-melt(data = stat.use.room.cluster.byMode[,c("paste","finalState","clusterName",sprintf("h%d",8:22))],
                                   id = c("paste","finalState","clusterName"))
ggplot(data = plot.use.room.cluster.byMode[finalState=="cooling"],
       aes(x=variable,y=value,group=paste,color=paste,shape=paste))+geom_point()+geom_line()
ggplot(data = data.hznu.use.final[finalState=="cooling"],aes(x=clusterName,y=runtime,color=clusterName))+geom_boxplot()

stat.use.clusterDescribe<-data.hznu.use.final[,.(
  count=length(labelRoomDay),
  runtime=mean(runtime,na.rm = TRUE),
  h8=mean(h8,na.rm = TRUE),
  h9=mean(h9,na.rm = TRUE),
  h10=mean(h10,na.rm = TRUE),
  h11=mean(h11,na.rm = TRUE),
  h12=mean(h12,na.rm = TRUE),
  h13=mean(h13,na.rm = TRUE),
  h14=mean(h14,na.rm = TRUE),
  h15=mean(h15,na.rm = TRUE),
  h16=mean(h16,na.rm = TRUE),
  h17=mean(h17,na.rm = TRUE),
  h18=mean(h18,na.rm = TRUE),
  h19=mean(h19,na.rm = TRUE),
  h20=mean(h20,na.rm = TRUE),
  h21=mean(h21,na.rm = TRUE),
  h22=mean(h22,na.rm = TRUE)
),by=clusterName]
stat.use.clusterDescribe.plot<-melt(stat.use.clusterDescribe[,-c("count","runtime")],id.vars = c("clusterName"))
ggplot(data = stat.use.clusterDescribe.plot,aes(x=variable,y=value,group=clusterName,color=clusterName,shape=clusterName))+geom_point()+geom_line()

stat.use.yearDist<-data.hznu.use.final[,.(usagePattern=clusterName[1],
                                          count=length(clusterName),
                                          modiSeason=modiSeason[1],
                                          finalState=finalState[1]
                                          ),
                                       by=labelSeasonUsageState<-paste(modiSeason,finalState,clusterName,sep = "_")]


#转换为因子的预处理
modeSelect<-"heating"
data.hznu.use.final.modePickup<-data.hznu.use.final[finalState==modeSelect]#&modiSeason %in% c("Summer_warm","Transition" )]
# > class(data.hznu.use.final.modePickup[,"h8"])
# [1] "data.table" "data.frame"
# > class(data.hznu.use.final.modePickup$h8)
# [1] "numeric"
#不能按列批量直接取


data.hznu.use.final.modePickup[,c(paste("h",c(8:22),sep = ""))]<-
  data.table(sapply(data.hznu.use.final.modePickup[,c(paste("h",c(8:22),sep = ""))],as.factor),stringsAsFactors = TRUE)
data.hznu.use.final.modePickup$modiSeason<-as.factor(data.hznu.use.final.modePickup$modiSeason)
#注意data.table中stringsAsFactors参数，此处sapply先转成matrix，因此转换成data.table时该参数会默认为false，即为字符串型

####统计各模式运行时间####
stat.use.runtime<-boxplot(data = data.hznu.use.final,runtime~data.hznu.use.final$clusterName)
data.hznu.use.final.modePickup$runtimeClass<-as.factor(sapply(data.hznu.use.final.modePickup$runtime,getRuntimeClass))
# data.hznu.use.final.modePickup[clusterName=="halfDaytime"]$clusterName<-"daytime"


####训练集/测试集划分####
set.seed(711)
sub<-sample(1:nrow(data.hznu.use.final.modePickup),round(nrow(data.hznu.use.final.modePickup))*3/4)
data.hznu.use.tree.training<-data.hznu.use.final.modePickup[sub]
data.hznu.use.tree.test<-data.hznu.use.final.modePickup[-sub]

tree.both<-rpart(clusterName~modiSeason+runtimeClass+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.hznu.use.tree.training)#rpart,即经典决策树，必须都为factor或定性,连char都不行...
tree.both$cptable
tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#最优剪枝
rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
plot(rpartTrue2)

####以测试集验证####
rtree.predict<-predict(rpartTrue2,data.hznu.use.tree.test)
confusionMatrix(table(rtree.predict,data.hznu.use.tree.test$clusterName))

capture.output(c(confusionMatrix(table(rtree.predict,data.hznu.use.tree.test$clusterName)),asRules(tree.both)),
               file =paste("HZNU_行为_制冷_决策树评估_bestCP.txt"))


####大论文用十折验证####
# 思路：
# 每一折均生成一棵树、每次循环记录生成的树
# 将最高准确率的折所生成的树作为最终输出，总十折结果作为最终概率

#构建数据记录表
data.hznu.use.classify.log<-data.table(id=-999,datetime="2020-02-02",modiSeason="modiSeason",
                                       target="targetResult,fullOnRatio,etc.",method="cart",setType="train/test",
                                       round=-999,predValue=-999,realValue=-999)[-1]
list.hznu.use.classify.cart<-list()

data.hznu.use.final.modePickup$id<-c(1:nrow(data.hznu.use.final.modePickup))

for(j in c(0:9)){
    tree.both<-rpart(clusterName~modiSeason+runtime+h8+h9+h10+h11+h12
                     +h13+h14+h15+h16+h17+h18
                     +h19+h20+h21+h22,data=data.hznu.use.final.modePickup[id%%10!=j])
    tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#最优剪枝
    rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
    plot(rpartTrue2)
    list.hznu.use.classify.cart[[modeSelect]][[as.character(j)]]<-tree.both
    #测试集验证
    cmResult<-
      predictTest(testSet = data.hznu.use.final.modePickup[id%%10==j],resultValue = data.hznu.use.final.modePickup[id%%10==j]$clusterName,
                  predictableModel = rpartTrue2)
    # predictTest(testSet = data.hznu.teaching.decoupling.training,resultValue = data.hznu.teaching.decoupling.training$energyClusterName,
    #             predictableModel = rpartTrue2)
    #结果输出
    outputImg(rpartTrue2,hit=900,wid = 1600,fileName = paste("HZNU_UsageClassify_",modeSelect,"_fold_",j,".png",sep = ""))
    outputValidRslt(cm=cmResult, fileName =  paste("HZNU_UsageClassify_",modeSelect,"_fold_",j,".txt",sep = ""),
                    algoName = "CART",tree = tree.both , fmla = "clusterName~modiSeason+runtime+timeSeries", 
                    logTitle =  paste("HZNU_UsageClassify_",modeSelect,"_fold_",j,sep = ""),
                    other = list(paste("Total node: ",length(rpartTrue2)),tree.both$variable.importance) )
    
    #单独对十折的结果进行储存，此处为预测集
    data.hznu.use.classify.log<-data.hznu.use.final.modePickup[id%%10==j]%>%
      data.table(id=.$id,datetime=.$date,modiSeason=.$modiSeason,
                 target=paste("usagePattern",modeSelect,sep = ""),method="CART",setType="test",
                 round=j,predValue=as.character(predict(rpartTrue2,.)),realValue=.$clusterName)%>% .[,..archieveItem] %>% rbind(data.hznu.use.classify.log,.)
    
    #单独对十折的结果进行储存，此处为训练集
    data.hznu.use.classify.log<-data.hznu.use.final.modePickup[id%%10!=j]%>%
      data.table(id=.$id,datetime=.$date,modiSeason=.$modiSeason,
                 target=paste("usagePattern",modeSelect,sep = ""),method="CART",setType="train",
                 round=j,predValue=as.character(predict(rpartTrue2,.)),realValue=.$clusterName) %>% .[,..archieveItem] %>% rbind(data.hznu.use.classify.log,.)
    
    rm(tree.both,rpartTrue2,cmResult)
}

#将十折的结果验证其效果
# usagePatterncooling usagePatternheating 
# 125730               78720 
nn<-data.hznu.use.classify.log[setType=="test"& target=="usagePatterncooling"]%>% .[complete.cases(.[,c("predValue","realValue")])] %>% as.data.table(.) 
confusionMatrix(table(nn$predValue,nn$realValue))


getRuntimeClass<-function(time){
  if(time<=3)
    return("<=3")
  if(time<=6)
    return("(3,6]")
  if(time<=9)
    return("(6,9]")
  return(">9")
}
