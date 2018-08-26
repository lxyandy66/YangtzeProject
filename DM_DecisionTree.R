library(data.table)
library(rpart)
library(party)

data.tree.raw<-data.table(cluster=raw.periodOn$cluster)
data.tree.raw$cluster<-as.factor(data.tree.raw$cluster)
data.tree.raw<-cbind(data.tree.raw,raw.periodOn[,c(5:19,21)])

data.tree.raw$h1<-as.factor(data.tree.raw$h1)
data.tree.raw$h2<-as.factor(data.tree.raw$h2)
data.tree.raw$h3<-as.factor(data.tree.raw$h3)
data.tree.raw$h4<-as.factor(data.tree.raw$h4)
data.tree.raw$h5<-as.factor(data.tree.raw$h5)
data.tree.raw$h6<-as.factor(data.tree.raw$h6)
data.tree.raw$h7<-as.factor(data.tree.raw$h7)
data.tree.raw$h8<-as.factor(data.tree.raw$h8)
data.tree.raw$h9<-as.factor(data.tree.raw$h9)
data.tree.raw$h10<-as.factor(data.tree.raw$h10)
data.tree.raw$h11<-as.factor(data.tree.raw$h11)
data.tree.raw$h12<-as.factor(data.tree.raw$h12)
data.tree.raw$h13<-as.factor(data.tree.raw$h13)
data.tree.raw$h14<-as.factor(data.tree.raw$h14)
data.tree.raw$h15<-as.factor(data.tree.raw$h15)

for(i in c(2:16)){
  names(data.tree.raw)[i]<-paste("h",i+6,sep = "")
}#把时间改成实际小时数

###以概率建立决策树
##树过大
tr_ctree<-ctree(cluster~isWorkday+h8+h9+h10+h11+h12
                +h13+h14+h15+h16+h17+h18
                +h19+h20+h21+h22,data=data.tree.raw)
tr_ctree<-prune(tr_ctree,cp=0.0125)
plot(tr_ctree)

###以判断条件建立决策树
set.seed(1122)
tree.both<-rpart(cluster~isWorkday+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.tree.raw)
rpart.plot(tree.both,branch=1,shadow.col="gray",box.col="green",
           border.col="blue",split.col="red",split.cex=1.2,main="决策树")

rpartTrue2<-as.party(tree.both)
plot(rpartTrue2)

data.tree.process<-data.tree.raw
data.tree.process$clusterDescribe<-""
data.tree.process[cluster==2|cluster==5|cluster==6]$clusterDescribe<-"1"#"Half Daytime"
data.tree.process[cluster==3|cluster==4]$clusterDescribe<-"2"#"By use"
data.tree.process[cluster==1]$clusterDescribe<-"3"#"WholeDay"
data.tree.process[cluster==7]$clusterDescribe<-"4"#"Daytime"

set.seed(4567)
tree.both<-rpart(clusterDescribe~isWorkday+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22,data=data.tree.process)
rpartTrue2<-as.party(tree.both)
plot(rpartTrue2)


