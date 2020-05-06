####用于处理HZNU空调已处理的事件性数据####
# 接 热舒适报表汇总_含追加汇总.xlsx

data.hznu.teaching.event<-read.xlsx(file = "HZNU_仅教学_含追加_热舒适报表汇总.xlsx",sheetIndex = 1) %>% as.data.table(.)
names(data.hznu.teaching.event)<-c("roomName","onTime","offTime","runtime","setTemp","onTemp","offTemp","lowestTemp","highestTemp",
                                   "aveTemp","stableTime","stableTemp","usedAcCount","acCount")


####读取房间编码对照表####
info.hznu.hashmap.roomCode<-read.xlsx(file = "杭师大完成_房间属性-教学.xlsx",sheetIndex = 1)%>% as.data.table(.) %>%.[,1:2]
names(info.hznu.hashmap.roomCode)<-c("roomName","roomCode")
info.hznu.hashmap.roomCode$roomName<-paste("恕园",info.hznu.hashmap.roomCode$roomName,sep = "")

####合并房间编码####
data.hznu.teaching.event<-merge(x=data.hznu.teaching.event,y=info.hznu.hashmap.roomCode,by.x = "roomName",by.y = "roomName",all.x = TRUE)
data.hznu.teaching.event[is.na(roomCode)]#有几个缺失，手动命名

data.hznu.teaching.event[is.na(roomCode)&roomName=="恕园34#204房间"]$roomCode<-"330100D279204"
data.hznu.teaching.event[is.na(roomCode)&roomName=="恕园34#304房间"]$roomCode<-"330100D279304"

####加入一些辅助变量####
#季节
data.hznu.teaching.event$modiSeason<-apply(X = data.hznu.teaching.event[,"onTime"],MARGIN = 1,
                                           FUN = function(x){
                                             temp<-getSeason(as.numeric(substr(x,6,7)))
                                             return(ifelse(temp %in% c("Spring","Autumn"),"Transition",temp))
                                           })
#开关所在小时
data.hznu.teaching.event$onHour<-hour(data.hznu.teaching.event$onTime)
data.hznu.teaching.event$offHour<-hour(data.hznu.teaching.event$offTime)

#空调运行工况
data.hznu.teaching.event<-data.hznu.teaching.event%>%mutate(.,labelRoomDay=paste(roomCode,date(onTime),sep = "_"))%>%
                          merge(x=.,y = data.hznu.teaching.energy.std[,c("labelRoomDay","finalState")],all.x = TRUE,
                                by= "labelRoomDay") %>%as.data.table(.)
data.hznu.teaching.event[is.na(finalState)& modiSeason%in% c("Summer","Summer_warm")]$finalState<-"cooling" 
data.hznu.teaching.event[is.na(finalState)& modiSeason%in% c("Winter","Winter_warm")]$finalState<-"heating" 

####对结果进行可视化的分析####
#各季节空调开启动作时间分布
ggplot(data=data.hznu.teaching.event[year(onTime)%in%2016:2017&onHour%in% 8:22& !is.na(finalState)],aes(x=onHour,lty=modiSeason,fill=modiSeason,shape=modiSeason))+
  geom_density(size=0.5,alpha=0.25)+
  scale_x_continuous(breaks=seq(0,25,2))+facet_wrap(~finalState,nrow=2)+
  scale_color_brewer(palette="Greys")+#scale_fill_brewer(palette="Greys")+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))

ggplot(data=data.hznu.teaching.event[onHour%in% 8:22& !is.na(finalState)],aes(x=onHour,lty=modiSeason,fill=modiSeason,shape=modiSeason,color=modiSeason))+
  stat_density(geom="point")+
  scale_x_continuous(breaks=seq(0,25,2))+facet_wrap(~finalState,nrow=2)+
  # scale_fill_brewer(palette="Greys")+scale_color_brewer(palette="Greys")+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))

#各季节空调开启时长分布

ggplot(data=data.hznu.teaching.event[onHour%in% 8:22&runtime<18& !is.na(finalState) & onTemp<36&onTemp>5],#[!(finalState=="heating"&onTemp>30)],
       aes(x=factor(modiSeason,levels = c("Summer","Summer_warm","Transition","Winter_warm","Winter")),y = onTemp))+geom_boxplot(width=0.7,outlier.colour = NA)+
  facet_wrap(~finalState)+scale_y_continuous(breaks=seq(0,50,5))+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))



ggplot(data=data.hznu.teaching.event[onHour%in% 8:22&runtime<18& !is.na(finalState)&onTemp<36],aes(x=onTemp))+geom_density()+
  facet_wrap(~finalState+factor(modiSeason))+#scale_x_continuous(breaks=seq(0,40,5))+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))


