####影响因素解耦部分####

#接list.hznu.room.use
#已统一全年行为模式名称

data.hznu.use.final<-as.data.table(list.hznu.room.use[[1]])
for(i in c(2:6)){
  data.hznu.use.final<-rbind(data.hznu.use.final,list.hznu.room.use[[i]])
}


####全年基本情况统计####
ggplot(data=data.hznu.use.final,aes(x=modiSeason,y=runtime))+geom_boxplot()
boxplot(data = data.hznu.use.final,runtime~
          paste(data.hznu.use.final$modiSeason,data.hznu.use.final$finalState))

stat.use.clusterDescribe<-data.hznu.use.final[,.(
  count=length(labelRoomDay),
  runtime=mean(runtime),
  h8=mean(h8),
  h9=mean(h9),
  h10=mean(h10),
  h11=mean(h11),
  h12=mean(h12),
  h13=mean(h13),
  h14=mean(h14),
  h15=mean(h15),
  h16=mean(h16),
  h17=mean(h17),
  h18=mean(h18),
  h19=mean(h19),
  h20=mean(h20),
  h21=mean(h21),
  h22=mean(h22)
),by=clusterName]
stat.use.clusterDescribe.plot<-melt(stat.use.clusterDescribe[,-c("count","runtime")],id.vars = c("clusterName"))
ggplot(data = stat.use.clusterDescribe.plot,aes(x=variable,y=value,group=clusterName,color=clusterName,shape=clusterName))+geom_point()+geom_line()


#转换为因子的预处理
modeSelect<-"cooling"
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


getRuntimeClass<-function(time){
  if(time<=3)
    return("<=3")
  if(time<=6)
    return("(3,6]")
  if(time<=9)
    return("(6,9]")
  return(">9")
}
