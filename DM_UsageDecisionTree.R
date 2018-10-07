library(data.table)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(caret)
library(rattle)
library(pROC)

data.behavior.tree.raw<-data.table(cluster=raw.periodOn$cluster)
data.behavior.tree.raw$cluster<-as.factor(data.behavior.tree.raw$cluster)
data.behavior.tree.raw<-cbind(data.behavior.tree.raw,raw.periodOn[,c(5:19,21)])

data.behavior.tree.raw$h1<-as.factor(data.behavior.tree.raw$h1)
data.behavior.tree.raw$h2<-as.factor(data.behavior.tree.raw$h2)
data.behavior.tree.raw$h3<-as.factor(data.behavior.tree.raw$h3)
data.behavior.tree.raw$h4<-as.factor(data.behavior.tree.raw$h4)
data.behavior.tree.raw$h5<-as.factor(data.behavior.tree.raw$h5)
data.behavior.tree.raw$h6<-as.factor(data.behavior.tree.raw$h6)
data.behavior.tree.raw$h7<-as.factor(data.behavior.tree.raw$h7)
data.behavior.tree.raw$h8<-as.factor(data.behavior.tree.raw$h8)
data.behavior.tree.raw$h9<-as.factor(data.behavior.tree.raw$h9)
data.behavior.tree.raw$h10<-as.factor(data.behavior.tree.raw$h10)
data.behavior.tree.raw$h11<-as.factor(data.behavior.tree.raw$h11)
data.behavior.tree.raw$h12<-as.factor(data.behavior.tree.raw$h12)
data.behavior.tree.raw$h13<-as.factor(data.behavior.tree.raw$h13)
data.behavior.tree.raw$h14<-as.factor(data.behavior.tree.raw$h14)
data.behavior.tree.raw$h15<-as.factor(data.behavior.tree.raw$h15)

for(i in c(2:16)){
  names(data.behavior.tree.raw)[i]<-paste("h",i+6,sep = "")
}#把时间改成实际小时数


###以概率建立决策树
##树过大
tr_ctree<-ctree(cluster~isWorkday+h8+h9+h10+h11+h12
                +h13+h14+h15+h16+h17+h18
                +h19+h20+h21+h22,data=data.behavior.tree.raw)
# tr_ctree<-prune(tr_ctree,cp=0.0125)
plot(tr_ctree)

###以判断条件建立决策树
##原始数据
set.seed(1122)
tree.both<-rpart(cluster~isWorkday+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.behavior.tree.raw)
rpart.plot(tree.both,branch=1,shadow.col="gray",box.col="green",
           border.col="blue",split.col="red",split.cex=1.2,main="决策树")

rpartTrue2<-as.party(tree.both)
plot(rpartTrue2)

data.behavior.tree.process<-data.behavior.tree.raw
data.behavior.tree.process$clusterDescribe<-""
data.behavior.tree.process[cluster==2|cluster==5|cluster==6]$clusterDescribe<-"1"#"Half Daytime"
data.behavior.tree.process[cluster==3|cluster==4]$clusterDescribe<-"2"#"By use"
data.behavior.tree.process[cluster==1]$clusterDescribe<-"3"#"WholeDay"
data.behavior.tree.process[cluster==7]$clusterDescribe<-"4"#"Daytime"
data.behavior.tree.process$clusterDescribe<-as.factor(data.behavior.tree.process$clusterDescribe)
data.behavior.tree.process$isWorkday<-as.factor(data.behavior.tree.process$isWorkday)

#####
#####划分训练和预测集的建树
set.seed(1234)
sub<-sample(1:nrow(data.behavior.tree.process),round(nrow(data.behavior.tree.process))*2/3)
data.behavior.tree.training<-data.behavior.tree.process[sub]
data.behavior.tree.test<-data.behavior.tree.process[-sub]

#######
#####条件决策树
##树过大
# tr_ctree<-ctree(clusterDescribe~isWorkday+h8+h9+h10+h11+h12
#                 +h13+h14+h15+h16+h17+h18
#                 +h19+h20+h21+h22,data=data.behavior.tree.training)
# tr_ctree<-prune(tr_ctree,cp=0.0125)#怎么剪枝呢
# plot(tr_ctree)#可视化看不得，不适用
# ctree.predict<-predict(tr_ctree,data.behavior.tree.test)
# capture.output(confusionMatrix(table(ctree.predict,data.behavior.tree.test$clusterDescribe)),file = "ctree_evaluate.txt")
# Accuracy : 0.9773          
# 95% CI : (0.9745, 0.9798)
# No Information Rate : 0.3702          
# P-Value [Acc > NIR] : < 2.2e-16     


############
######经典决策树
set.seed(625)
tree.both<-rpart(clusterDescribe~isWorkday+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.behavior.tree.training)#class(tree.both)-----[1]"rpart"
tree.both$cptable
tree.both<-prune(tree.both,cp=0.01011615)#似乎cp=0.03效果最好
#0.025(>0.03)有8-9点连续判断的分支
rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
plot(rpartTrue2)

rtree.predict<-predict(rpartTrue2,data.behavior.tree.test)
confusionMatrix(table(rtree.predict,data.behavior.tree.test$clusterDescribe))
capture.output(confusionMatrix(table(rtree.predict,data.behavior.tree.test$clusterDescribe)),file ="rtree_evaluate_bestCP.txt")
#不剪枝 Accuracy : 0.8921 | 0.03剪枝后 Accuracy : 0.8225  | 最佳剪枝        
   