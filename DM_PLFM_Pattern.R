####用于分析平台办公住宅的处理####
#只提供结果，不保证质量

for(i in c("WJJ_cooling_office.xlsx","WJJ_heating_office.xlsx")){#,"WJJ_heating_office.xlsx"
  for(j in seq(1,4)){#应该设成动态的，但是内存会溢出
    if(exists("data.plfm.office.raw")){
      data.plfm.office.raw<-rbind(data.plfm.office.raw,data.table(read.xlsx(file = i,sheetIndex = j),roomType=j))
      gc()
    }else{
      data.plfm.office.raw<-data.table(read.xlsx(file = i,sheetIndex = j),roomType=j)
      gc()
    }
  }
}

names(data.plfm.office.raw)<-c("finalState","date",sprintf("h%d",0:23),"roomType")
data.plfm.office.raw$labelTypeModeDate<-paste(data.plfm.office.raw$roomType,data.plfm.office.raw$finalState,data.plfm.office.raw$date,sep = "_")
data.plfm.office.raw$runtime<-apply(data.plfm.office.raw[,c(sprintf("h%d",0:23))],MARGIN = 1,FUN = sum,na.rm=TRUE)

set.seed(711)
data.plfm.office.select<-data.plfm.office.raw[finalState=="C"&runtime!=0]
wssClusterEvaluate(data.plfm.office.select[,c(sprintf("h%d",0:23))],maxK = 15)
pamkClusterEvaluate(data=data.plfm.office.select[,c(sprintf("h%d",0:23))],endK = 15,criter = "asw")
pamk(data.plfm.office.select[,c(sprintf("h%d",0:23))],krange = 2:10,criterion = "ch",critout = TRUE,usepam = TRUE)

data.plfm.office.select$cluster<-pamk(data.plfm.office.select[,c(sprintf("h%d",0:23))],krange = 4,criterion = "ch",critout = TRUE,usepam = TRUE)$pamobject$clustering
data.plfm.office.select.eva<-data.plfm.office.select[,.(finalState=finalState[1],
                                                        count=length(labelTypeModeDate),
                                                        runtime=mean(runtime,na.rm = TRUE),
                                                        h0=mean(h0,na.rm = TRUE),
                                                        h1=mean(h1,na.rm = TRUE),
                                                        h2=mean(h2,na.rm = TRUE),
                                                        h3=mean(h3,na.rm = TRUE),
                                                        h4=mean(h4,na.rm = TRUE),
                                                        h5=mean(h5,na.rm = TRUE),
                                                        h6=mean(h6,na.rm = TRUE),
                                                        h7=mean(h7,na.rm = TRUE),
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
                                                        h22=mean(h22,na.rm = TRUE),
                                                        h23=mean(h23,na.rm = TRUE)
                                                        ),by=cluster]#其实用reshape包会更好，但是label要调所以懒得管了
write.xlsx(data.plfm.office.select.eva,file="PLFM_office_use_heating_cluster.xlsx")

data.plfm.office.select$modiSeason<-apply(data.plfm.office.select[,c("date")],MARGIN = 1,
                                          FUN = function(x){
                                            nn<-getSeason(as.numeric(substr(x,6,7)))
                                            if(!nn %in% c("Spring","Autumn"))
                                              return(nn)
                                            else
                                              return("Transition")
                                          })
data.plfm.office.select$season<-apply(data.plfm.office.select[,c("date")],MARGIN = 1,
                                          FUN = function(x){
                                            getSeason(as.numeric(substr(x,6,7)))
                                          })

data.plfm.office.select%>% .[,.(count=length(date),
                                modiSeason=modiSeason[1],
                                usagePattern=cluster[1],
                                runtime=mean(runtime,na.rm = TRUE)
                                ),by=(labelSeasonUsage=paste(modiSeason,cluster,sep = "_"))]%>%
  write.xlsx(.,file = "cooling_PLFM_office_usageDist.xlsx")
####决策树用模式识别####
####训练集/测试集划分####
set.seed(711)
sub<-sample(1:nrow(data.plfm.office.select),round(nrow(data.plfm.office.select))*3/4)
data.plfm.office.select$cluster<-as.factor(data.plfm.office.select$cluster)
data.plfm.office.training<-data.plfm.office.select[sub]
data.plfm.office.test<-data.plfm.office.select[-sub]

tree.both<-rpart(cluster~runtime+h0+h1+h2
                 +h3+h4+h5+h6+h7+h8+h9+h10+h11+h12
                 +h13+h14+h15+h16+h17+h18
                 +h19+h20+h21+h22+h23,data=data.plfm.office.training)#rpart,即经典决策树，必须都为factor或定性,连char都不行...
tree.both$cptable
tree.both<-prune(tree.both, cp= tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])#最优剪枝
rpartTrue2<-as.party(tree.both)#class(rpartTrue2)------[1]"constparty" "party" 
plot(rpartTrue2)

####以测试集验证####
rtree.predict<-predict(rpartTrue2,data.plfm.office.test)
confusionMatrix(table(rtree.predict,data.plfm.office.test$cluster))

capture.output(c(confusionMatrix(table(rtree.predict,data.plfm.office.test$cluster)),asRules(tree.both)),
               file =paste("PLFM_office_行为_heating_决策树评估_bestCP.txt"))



