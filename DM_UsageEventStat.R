####���ڴ���HZNU�յ��Ѵ������¼�������####
# �� �����ʱ�������_��׷�ӻ���.xlsx

data.hznu.teaching.event<-read.xlsx(file = "HZNU_����ѧ_��׷��_�����ʱ�������.xlsx",sheetIndex = 1) %>% as.data.table(.)
names(data.hznu.teaching.event)<-c("roomName","onTime","offTime","runtime","setTemp","onTemp","offTemp","lowestTemp","highestTemp",
                                   "aveTemp","stableTime","stableTemp","usedAcCount","acCount")


####��ȡ���������ձ�####
info.hznu.hashmap.roomCode<-read.xlsx(file = "��ʦ�����_��������-��ѧ.xlsx",sheetIndex = 1)%>% as.data.table(.) %>%.[,1:2]
names(info.hznu.hashmap.roomCode)<-c("roomName","roomCode")
info.hznu.hashmap.roomCode$roomName<-paste("ˡ԰",info.hznu.hashmap.roomCode$roomName,sep = "")

####�ϲ��������####
data.hznu.teaching.event<-merge(x=data.hznu.teaching.event,y=info.hznu.hashmap.roomCode,by.x = "roomName",by.y = "roomName",all.x = TRUE)
data.hznu.teaching.event[is.na(roomCode)]#�м���ȱʧ���ֶ�����

data.hznu.teaching.event[is.na(roomCode)&roomName=="ˡ԰34#204����"]$roomCode<-"330100D279204"
data.hznu.teaching.event[is.na(roomCode)&roomName=="ˡ԰34#304����"]$roomCode<-"330100D279304"

####����һЩ��������####
#����
data.hznu.teaching.event$modiSeason<-apply(X = data.hznu.teaching.event[,"onTime"],MARGIN = 1,
                                           FUN = function(x){
                                             temp<-getSeason(as.numeric(substr(x,6,7)))
                                             return(ifelse(temp %in% c("Spring","Autumn"),"Transition",temp))
                                           })
#��������Сʱ
data.hznu.teaching.event$onHour<-hour(data.hznu.teaching.event$onTime)
#�յ����й���
data.hznu.teaching.event<-data.hznu.teaching.event%>%mutate(.,labelRoomDay=paste(roomCode,date(onTime),sep = "_"))%>%
                          merge(x=.,y = data.hznu.teaching.energy.std[,c("labelRoomDay","finalState")],all.x = TRUE,
                                by= "labelRoomDay") %>%as.data.table(.)
data.hznu.teaching.event[is.na(finalState)& modiSeason%in% c("Summer","Summer_warm")]$finalState<-"cooling" 
data.hznu.teaching.event[is.na(finalState)& modiSeason%in% c("Winter","Winter_warm")]$finalState<-"heating" 

####�Խ�����п��ӻ��ķ���####
#�����ڿյ���������ʱ��ֲ�
ggplot(data=data.hznu.teaching.event[onHour%in% 8:22& !is.na(finalState)],aes(x=onHour,lty=modiSeason,fill=modiSeason))+geom_density(size=0.5,alpha=0.25)+
  scale_x_continuous(breaks=seq(0,25,2))+facet_wrap(~finalState,nrow=2)+
  scale_fill_brewer(palette="Greys")+scale_color_brewer(palette="Greys")+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))

#�����ڿյ�����ʱ���ֲ�

ggplot(data=data.hznu.teaching.event[onHour%in% 8:22&runtime<18& !is.na(finalState) & onTemp<36&onTemp>5],#[!(finalState=="heating"&onTemp>30)],
       aes(x=factor(modiSeason,levels = c("Summer","Summer_warm","Transition","Winter_warm","Winter")),y = onTemp))+geom_boxplot(width=0.7,outlier.colour = NA)+
  facet_wrap(~finalState)+scale_y_continuous(breaks=seq(0,50,5))+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))


ggplot(data=data.hznu.teaching.event[onHour%in% 8:22&runtime<18& !is.na(finalState)&onTemp<36],aes(x=onTemp))+geom_density()+
  facet_wrap(~finalState+factor(modiSeason))+scale_x_continuous(breaks=seq(0,40,5))+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))

