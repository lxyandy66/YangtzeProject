
####本脚本用于热环境聚类####
#更新重写，原脚本停止使用
#接已处理热环境数据"HZNU_含追加_房间级_热环境模式预处理完成.rdata"

nrow(data.hznu.teaching.thermo.day.final[naCount!=0])
data.hznu.teaching.thermo.day.final.modify<-data.hznu.teaching.thermo.day.final
tmp.na.cov<-t(data.hznu.teaching.thermo.day.final[,4:18])
tmp.na.cov[is.nan(tmp.na.cov)]<-NA
tmp.na.modi<-data.table(t(na.approx(tmp.na.cov)))
nrow(tmp.na.modi[!complete.cases(tmp.na.modi)])
names(tmp.na.modi)<-sprintf("modH%02d",8:22)
data.hznu.teaching.thermo.day.final.modify<-cbind(data.hznu.teaching.thermo.day.final.modify,tmp.na.modi)
data.hznu.teaching.thermo.day.final.modify<-merge(x=data.hznu.teaching.thermo.day.final.modify,
                                                  y=data.hznu.use.final[,c("labelRoomDay","finalState","clusterName")],
                                                  by.x = "labelRoomDay",
                                                  by.y = "labelRoomDay",
                                                  all.x = TRUE)
data.hznu.teaching.thermo.day.final.modify$range<-apply(data.hznu.teaching.thermo.day.final.modify[,4:18],
                                                        MARGIN = 1,FUN = function(x){
                                                          return(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))
                                                        })
#删去连续缺失的
data.hznu.teaching.thermo.day.final.modify<-data.hznu.teaching.thermo.day.final.modify[
  complete.cases(data.hznu.teaching.thermo.day.final.modify[,c(sprintf("modH%02d",8:22))]) ]

data.hznu.teaching.thermo.day.final.modify$modiSeason<-data.hznu.teaching.thermo.day.final.modify$season
data.hznu.teaching.thermo.day.final.modify[modiSeason %in% c("Spring","Autumn")]$modiSeason<-"Transition"
data.hznu.teaching.thermo.day.final.modify$labelSeasonState<-paste(data.hznu.teaching.thermo.day.final.modify$modiSeason,
                                                                   data.hznu.teaching.thermo.day.final.modify$finalState,sep = "_")
data.hznu.teaching.thermo.day.final.modify$lowHighRatio<-apply(data.hznu.teaching.thermo.day.final.modify[,24:38],MARGIN = 1,
                                                        FUN = function(x){
                                                          tmp<-(tempRatioSplit(x))
                                                          return(as.numeric(unlist(tmp[1,3]/tmp[2,3])))
                                                        })
data.hznu.teaching.thermo.day.final.modify$lowHighRatioValue<-
  apply(data.hznu.teaching.thermo.day.final.modify[,"lowHighRatio"],MARGIN = 1,FUN = as.numeric )
data.hznu.teaching.thermo.day.final.modify$lowHighRatio<-NULL#为啥...
ggplot(data = data.hznu.teaching.thermo.day.final.modify,aes(x=lowHighRatioValue))+geom_density()

####按季节进行归一化####
tmp.scale<-data.hznu.teaching.thermo.day.final.modify[,c(sprintf("modH%02d",8:22),"labelSeasonState",
                                                         "sd","meanTemp","range","lowHighRatioValue")]
tmp.scale$stdSd<- -999
tmp.scale$stdMeanTemp<- -999
tmp.scale$stdRange<- -999
tmp.scale$stdLowHighRatio<- -999
for(i in unique(tmp.scale$labelSeasonState)){
  tmp.scale[labelSeasonState==i,c(sprintf("modH%02d",8:22))]<-data.table(scale(tmp.scale[labelSeasonState==i,c(sprintf("modH%02d",8:22))]))
  tmp.scale[labelSeasonState==i]$stdSd<-scale(tmp.scale[labelSeasonState==i]$sd)
  tmp.scale[labelSeasonState==i]$stdMeanTemp<-scale(tmp.scale[labelSeasonState==i]$meanTemp)
  tmp.scale[labelSeasonState==i]$stdRange<-scale(tmp.scale[labelSeasonState==i]$range)
  tmp.scale[labelSeasonState==i]$stdLowHighRatio<-scale(tmp.scale[labelSeasonState==i]$lowHighRatioValue)
}
names(tmp.scale)<-c(sprintf("stdModH%02d",8:22),names(tmp.scale)[16:24])
data.hznu.teaching.thermo.day.final.modify<-cbind(
  data.hznu.teaching.thermo.day.final.modify,tmp.scale[,c(sprintf("stdModH%02d",8:22),"stdSd","stdMeanTemp","stdRange","stdLowHighRatio")])

合并至列表
list.hznu.teaching.thermo<-split(data.hznu.teaching.thermo.day.final.modify,
                                 f=as.factor(data.hznu.teaching.thermo.day.final.modify$labelSeasonState))
#####开始聚类分析
stat.hznu.thermo.season<-data.hznu.teaching.thermo.day.final.modify[,.(
  count=length(labelRoomDay),
  finalState=finalState[1],
  modiSeason=modiSeason[1]
),by=labelSeasonState]

{
  modeSelect<-"Summer_warm_cooling"
  data.hznu.thermo.tryCluster<-list.hznu.teaching.thermo[[modeSelect]]
  clusterAttr<-c("stdSd","stdMeanTemp","stdLowHighRatio","stdRange")
  
  # Error in `[.data.table`(data.hznu.thermo.tryCluster, , clusterAttr) : 
  #   j (the 2nd argument inside [...]) is a single symbol but column name 'clusterAttr' is not found. 
  # Perhaps you intended DT[, ..clusterAttr]. 
  # This difference to data.frame is deliberate and explained in FAQ 1.1.
  
  wssClusterEvaluate(data = data.hznu.thermo.tryCluster[,..clusterAttr],
                     maxIter = 1000,
                     maxK = 15)
  pamkClusterEvaluate(
    data = data.hznu.thermo.tryCluster[,..clusterAttr],#8-22时+runtime
    criter = "multiasw",
    startK = 1,
    endK = 10
  )
  thermo.pamk<-pamk(data.hznu.thermo.tryCluster[,..clusterAttr],krange = )
}

