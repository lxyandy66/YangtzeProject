####Ӱ�����ؽ����####

#��list.hznu.room.use
#��ͳһȫ����Ϊģʽ����

data.hznu.use.final<-as.data.table(list.hznu.room.use[[1]])
for(i in c(2:6)){
  data.hznu.use.final<-rbind(data.hznu.use.final,list.hznu.room.use[[i]])
}


####ȫ��������ͳ��####
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


#ת��Ϊ���ӵ�Ԥ����
modeSelect<-"cooling"
data.hznu.use.final.modePickup<-data.hznu.use.final[finalState==modeSelect]#&modiSeason %in% c("Summer_warm","Transition" )]
# > class(data.hznu.use.final.modePickup[,"h8"])
# [1] "data.table" "data.frame"
# > class(data.hznu.use.final.modePickup$h8)
# [1] "numeric"
#���ܰ�������ֱ��ȡ


data.hznu.use.final.modePickup[,c(paste("h",c(8:22),sep = ""))]<-
  data.table(sapply(data.hznu.use.final.modePickup[,c(paste("h",c(8:22),sep = ""))],as.factor),stringsAsFactors = TRUE)
data.hznu.use.final.modePickup$modiSeason<-as.factor(data.hznu.use.final.modePickup$modiSeason)
#ע��data.table��stringsAsFactors�������˴�sapply��ת��matrix�����ת����data.tableʱ�ò�����Ĭ��Ϊfalse����Ϊ�ַ�����

####ͳ�Ƹ�ģʽ����ʱ��####
stat.use.runtime<-boxplot(data = data.hznu.use.final,runtime~data.hznu.use.final$clusterName)
data.hznu.use.final.modePickup$runtimeClass<-as.factor(sapply(data.hznu.use.final.modePickup$runtime,getRuntimeClass))
# data.hznu.use.final.modePickup[clusterName=="halfDaytime"]$clusterName<-"daytime"

####ѵ����/���Լ�����####
set.seed(711)
sub<-sample(1:nrow(data.hznu.use.final.modePickup),round(nrow(data.hznu.use.final.modePickup))*3/4)
data.hznu.use.tree.training<-data.hznu.use.final.modePickup[sub]
data.hznu.use.tree.test<-data.hznu.use.final.modePickup[-sub]

tree.both<-rpart(clusterName~modiSeason+runtimeClass+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.hznu.use.tree.training)#rpart,����������������붼Ϊfactor����,��char������...
tree.both$cptable
tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#���ż�֦
rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
plot(rpartTrue2)

####�Բ��Լ���֤####
rtree.predict<-predict(rpartTrue2,data.hznu.use.tree.test)
confusionMatrix(table(rtree.predict,data.hznu.use.tree.test$clusterName))

capture.output(c(confusionMatrix(table(rtree.predict,data.hznu.use.tree.test$clusterName)),asRules(tree.both)),
               file =paste("HZNU_��Ϊ_����_����������_bestCP.txt"))


getRuntimeClass<-function(time){
  if(time<=3)
    return("<=3")
  if(time<=6)
    return("(3,6]")
  if(time<=9)
    return("(6,9]")
  return(">9")
}