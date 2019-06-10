
####本脚本用于热环境聚类####
#更新重写，原脚本停止使用
#接已处理热环境数据"HZNU_含追加_房间级_热环境模式预处理完成.rdata"

nrow(data.hznu.teaching.thermo.day.final[naCount!=0])
ggplot(data=data.hznu.teaching.thermo.day.final[naCount!=0& naCount<10],aes(x=naCount))+geom_density()
data.hznu.teaching.thermo.day.final.modify<-data.hznu.teaching.thermo.day.final[naCount<5]
tmp.na.cov<-t(data.hznu.teaching.thermo.day.final.modify[,4:18])
tmp.na.cov[is.nan(tmp.na.cov)]<-NA
tmp.na.modi<-data.table(t(na.approx(tmp.na.cov,na.rm = FALSE)))#看文档啊！参数不能漏啊！
nrow(tmp.na.modi[!complete.cases(tmp.na.modi)])
names(tmp.na.modi)<-sprintf("modH%02d",8:22)
data.hznu.teaching.thermo.day.final.modify<-cbind(data.hznu.teaching.thermo.day.final.modify,tmp.na.modi)
data.hznu.teaching.thermo.day.final.modify<-merge(x=data.hznu.teaching.thermo.day.final.modify,
                                                  y=data.hznu.use.final[,c("labelRoomDay","runtime","finalState","clusterName")],
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
data.hznu.teaching.thermo.day.final.modify$lowRatio<-apply(data.hznu.teaching.thermo.day.final.modify[,24:38],MARGIN = 1,
                                                        FUN = function(x){
                                                          tmp<-(tempRatioSplit(x))
                                                          return(as.numeric(unlist(tmp[1,3])))
                                                        })
data.hznu.teaching.thermo.day.final.modify$lowRatioValue<-
  apply(data.hznu.teaching.thermo.day.final.modify[,"lowRatio"],MARGIN = 1,FUN = as.numeric )
data.hznu.teaching.thermo.day.final.modify$lowRatio<-NULL#为啥...
ggplot(data = data.hznu.teaching.thermo.day.final.modify,aes(x=lowRatioValue))+geom_density()

####按季节进行归一化####
tmp.scale<-data.hznu.teaching.thermo.day.final.modify[,c(sprintf("modH%02d",8:22),"labelSeasonState",
                                                         "sd","meanTemp","range","lowRatioValue")]
tmp.scale$stdSd<- -999
tmp.scale$stdMeanTemp<- -999
tmp.scale$stdRange<- -999
tmp.scale$stdlowRatio<- -999
for(i in unique(tmp.scale$labelSeasonState)){
  tmp.scale[labelSeasonState==i,c(sprintf("modH%02d",8:22))]<-data.table(scale(tmp.scale[labelSeasonState==i,c(sprintf("modH%02d",8:22))]))
  tmp.scale[labelSeasonState==i]$stdSd<-scale(tmp.scale[labelSeasonState==i]$sd)
  tmp.scale[labelSeasonState==i]$stdMeanTemp<-scale(tmp.scale[labelSeasonState==i]$meanTemp)
  tmp.scale[labelSeasonState==i]$stdRange<-scale(tmp.scale[labelSeasonState==i]$range)
  tmp.scale[labelSeasonState==i]$stdlowRatio<-scale(tmp.scale[labelSeasonState==i]$lowRatioValue)
}
names(tmp.scale)<-c(sprintf("stdModH%02d",8:22),names(tmp.scale)[16:24])
data.hznu.teaching.thermo.day.final.modify<-cbind(
  data.hznu.teaching.thermo.day.final.modify,tmp.scale[,c(sprintf("stdModH%02d",8:22),"stdSd","stdMeanTemp","stdRange","stdlowRatio")])

ggplot(data=data.hznu.teaching.thermo.day.final.modify,aes(x=lowRatioValue))+geom_density()
#对于部分结果单独显示
nn<-data.hznu.teaching.thermo.day.final.modify[labelRoomDay=="330100D256101_2017-08-17"]
nn<-melt(nn[,c(sprintf("modH%02d",8:22),"date")],id.var="date")
ggplot(data=nn,aes(x=variable,y=value))+geom_point()
# 合并至列表
list.hznu.teaching.thermo<-split(data.hznu.teaching.thermo.day.final.modify,
                                 f=as.factor(data.hznu.teaching.thermo.day.final.modify$labelSeasonState))
####开始试聚类分析####
stat.hznu.thermo.season<-data.hznu.teaching.thermo.day.final.modify[,.(
  count=length(labelRoomDay),
  finalState=finalState[1],
  modiSeason=modiSeason[1]
),by=labelSeasonState]

{
  modeSelect<-"Transition_heating"
  # clusterAttr<-c("stdSd","stdMeanTemp","stdlowRatio","stdRange")
  clusterAttr<-c(sprintf("stdModH%02d",8:22))
  
  data.hznu.thermo.tryCluster<-list.hznu.teaching.thermo[[modeSelect]]
  data.hznu.thermo.tryCluster<-data.hznu.thermo.tryCluster[complete.cases(data.hznu.thermo.tryCluster[,..clusterAttr])]
  
  
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
  hasPAM<-TRUE
  attr<-"seq"
  for(i in c(3:7)){
    thermo.pamk<-pamk(data.hznu.thermo.tryCluster[,..clusterAttr],krange = i,
                      criterion = "ch",critout = TRUE,usepam = hasPAM)
    data.hznu.thermo.tryCluster$thermoCluster<-thermo.pamk$pamobject$clustering
    plot.hznu.thermo.tryCluster<-melt(data.hznu.thermo.tryCluster[,c(sprintf("modH%02d",8:22),"labelRoomDay","thermoCluster")],
                                      id.vars = c("labelRoomDay","thermoCluster"))
    plot.hznu.thermo.tryCluster$thermoCluster<-as.factor(plot.hznu.thermo.tryCluster$thermoCluster)
    stat.hznu.thermo.tryCluster.descr<-data.hznu.thermo.tryCluster[,.(
      count=length(labelRoomDay),
      runtime=mean(runtime,na.rm = TRUE),
      range=mean(range,na.rm = TRUE),
      lowRatioValue=mean(lowRatioValue,na.rm = TRUE),
      sd=sd(sd,na.rm = TRUE),
      meanTemp=mean(meanTemp,na.rm = TRUE),
      onDemandUsage=length(labelRoomDay[clusterName=="OnDemand"]),
      forenoonUsage=length(labelRoomDay[clusterName=="Forenoon"]),
      afternoonUsage=length(labelRoomDay[clusterName=="Afternoon"]),
      daytimeUsage=length(labelRoomDay[clusterName=="Daytime"]),
      laterDaytimeUsage=length(labelRoomDay[clusterName=="LateDayTime"]),
      allDayUsage=length(labelRoomDay[clusterName=="All-Day"])
    ),by=thermoCluster]
    write.xlsx(stat.hznu.thermo.tryCluster.descr,
               file=paste(i,modeSelect,attr,ifelse(hasPAM,"withPAM","noPAM"),"descr","ThermoPattern.xlsx",sep = "_"))
    ggsave(file=paste(i,modeSelect,attr,ifelse(hasPAM,"withPAM","noPAM"),"ThermoPattern_dist.png",sep = "_"),
           plot = ggplot(data=stat.hznu.thermo.tryCluster.descr,
                         aes(x=meanTemp,y=sd,size=lowRatioValue))+geom_point(),
           width=8,height = 6,dpi = 100)
    ggsave(file=paste(i,modeSelect,attr,ifelse(hasPAM,"withPAM","noPAM"),"ThermoPattern_temp.png",sep = "_"),
           plot = ggplot(data=plot.hznu.thermo.tryCluster,
                         aes(x=variable,y=value,color=thermoCluster,group=variable))+
             facet_wrap(~ thermoCluster, nrow = i)+geom_boxplot(outlier.colour = NA)
           ,#+geom_point()+geom_line()
           width=5,height = 10,dpi = 100)
  }
}


####正式聚类####
info.thermo.seasonkSize<-data.table(seasonMode=c("Summer_cooling","Summer_warm_cooling","Transition_cooling",
                                                 "Transition_heating" ,"Winter_heating","Winter_warm_heating"),
                                    kSize=c(3,4,4,4,3,4))
info.thermo.clusterName<-as.data.table(read.xlsx(file = "HZNU_ThermoClusterMapping.xlsx",sheetIndex = 1))
for(i in names(list.hznu.teaching.thermo)){
  list.hznu.teaching.thermo[[i]]$cluster<-pamk(
    list.hznu.teaching.thermo[[i]][,..clusterAttr],
    krange = info.thermo.seasonkSize[seasonMode==i]$kSize,
    criterion = "ch",
    usepam = TRUE,
    critout = TRUE
  )$pamobject$clustering
  #根据各季节聚类对应编码转换为统一名称
  list.hznu.teaching.thermo[[i]]$thermoPattern<-""
  for(j in unique(list.hznu.teaching.thermo[[i]]$cluster)){
    list.hznu.teaching.thermo[[i]][cluster==j]$thermoPattern<-
      info.thermo.clusterName[patternCode==j & seasonMode==i]$patternName
  }
  
}#其实用lapply更好

#heatmap测试
tmp.plot.heatMap<-melt(list.hznu.teaching.thermo[[6]][1:300,c("thermoPattern",sprintf("modH%02d",8:22),"labelRoomDay","labelSeasonState")],
                       id.vars = c("thermoPattern","labelRoomDay","labelSeasonState"))
tmp.plot.heatMap$hour<-substr(tmp.plot.heatMap$variable,5,6)
for(i in names(list.hznu.teaching.thermo)){
  tmp.plot.heatMap<-melt(list.hznu.teaching.thermo[[i]][,c("thermoPattern",sprintf("modH%02d",8:22),"labelRoomDay","labelSeasonState")],
                         id.vars = c("thermoPattern","labelRoomDay","labelSeasonState"))
  tmp.plot.heatMap$hour<-substr(tmp.plot.heatMap$variable,5,6)
  range<-boxplot.stats(tmp.plot.heatMap$value)
  for(j in unique(tmp.plot.heatMap$thermoPattern))
  {
    ggsave(file=paste(i,j,"heatMap.png",sep = "_"),
          plot=ggplot(data=tmp.plot.heatMap[thermoPattern==j],
                      aes(x=hour,y=labelRoomDay,fill=value,group=thermoPattern))+
               geom_raster(interpolate = FALSE)+
               scale_fill_gradient(limits = c(range$stats[1],range$stats[5]),low = "green",high = "red")+
               facet_wrap(~ thermoPattern, nrow = 2)+theme_classic()+
               theme(axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()),
          width=4,height = 3,dpi = 80  
    )}
  }

data.hznu.teaching.thermo.final<-list.hznu.teaching.thermo[[1]]
for(i in 2:length(list.hznu.teaching.thermo)){
  data.hznu.teaching.thermo.final<-rbind(data.hznu.teaching.thermo.final,list.hznu.teaching.thermo[[i]])
}
save(data.hznu.teaching.thermo.final,
     list.hznu.teaching.thermo,file = "HZNU_含追加_房间级_教学_热环境聚类完成.rdata")

nn1<-t(data.hznu.teaching.thermo.day.final.modify[labelRoomDay=="330100D255102_2017-09-04",c(sprintf("modH%02d",8:22))])
nn1<-data.table(nn1[,1])
nn1$seq<-c(1:nrow(nn1))
nn1$cluster<-pamk(nn1$V1,krange=2,criterion = "ch")$pamobject$clustering
ggplot(nn1,aes(x=seq,y=V1,color=cluster))+geom_point()+geom_line()

