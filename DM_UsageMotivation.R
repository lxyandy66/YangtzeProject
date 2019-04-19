####���ű����ڷ�����ѧ�����յ�ʹ�õ���������####
#���Ѵ����õ����ݼ�data.zju.combine.final

data.zju.motivation.raw<-data.zju.combine.final.cleaned[,c("baseLabel","datetime","weekday","semester","isWeekend",
                                                   "roomCode","on_off","temp","modiTemp","humidity","detail",
                                                   "classNo","lessonCode","studentType","studentNum")]

data.zju.motivation.raw$isWeekend<-as.numeric(data.zju.motivation.raw$isWeekend)
data.zju.motivation.raw[temp==0]$temp<-NA
data.zju.motivation.raw[humidity==0]$humidity<-NA

data.zju.motivation.raw$hasClass<-as.numeric(ifelse(data.zju.motivation.raw$lessonCode=="",0,1))
data.zju.motivation.raw$actualDetail<-data.zju.motivation.raw$detail
data.zju.motivation.raw[hasClass!=1&detail=="class"]$actualDetail<-"noClass"

####����ͳ��####
length(data.zju.motivation.raw[on_off==1&lessonCode!=""]$baseLabel)/
  length(data.zju.motivation.raw[on_off==1]$baseLabel)
length(data.zju.motivation.raw[on_off==1&week]$baseLabel)/
  length(data.zju.motivation.raw[on_off==1]$baseLabel)


data.zju.motivation.raw$semester<-as.character(data.zju.motivation.raw$semester)
data.zju.motivation.raw$simpleSemester<-sapply(data.zju.motivation.raw$semester,
                                               FUN = getSplitMember,splitSimbol = "_",isLastOne = TRUE)
data.zju.motivation.raw$hour<-substr(data.zju.motivation.raw$baseLabel,12,13)
data.zju.motivation.raw$labelSemeOnOff<-paste(data.zju.motivation.raw$on_off,data.zju.motivation.raw$simpleSemester,sep = "_")

#�������Ƿ�ʹ�ÿյ����Ȼ���
ggplot(data=data.zju.motivation.raw[temp!=modiTemp],aes(x=hour,y=modiTemp))+
  geom_point(position = "jitter",color="darkgreen")+geom_point(aes(x=hour,y=temp),color="orange",position = "jitter")
ggplot(data=data.zju.motivation.raw,aes(x=modiTemp,color=simpleSemester,fill=on_off,linetype=hasClass))+
  geom_density(alpha=0.3)
#��ʱ��ε�ʹ�ø���
data.zju.motivation.raw$labelHourSemester<-paste(data.zju.motivation.raw$hour,data.zju.motivation.raw$simpleSemester,sep = "_")
stat.motivation.timeUseRate<-data.zju.motivation.raw[,.(
  count=length(baseLabel),
  classRate=mean(hasClass,na.rm = TRUE),
  hour=hour[1],
  simpleSemester=simpleSemester[1],
  useRate=mean(on_off,na.rm = TRUE)
  ),by=labelHourSemester]
# ),by=hour]
write.xlsx(x=stat.motivation.timeUseRate,file = "ZJU_Motivation_timeRate.xlsx")
ggplot(data = stat.motivation.timeUseRate,aes(x=hour,y=useRate,group=simpleSemester,color=simpleSemester))+
  geom_line()+geom_line(aes(x=hour,y=classRate))+geom_point()
#��ѧ�����ݲ���Ԥ��

#ת��Ϊ������
data.zju.motivation.raw$hasClass<-as.factor(data.zju.motivation.raw$hasClass)
data.zju.motivation.raw$on_off<-as.factor(data.zju.motivation.raw$on_off)


stat.motivation.class<-aggregate(data.zju.motivation.raw[,c("baseLabel")],
          by=list(on_off=data.zju.motivation.raw$on_off,isWeekend=data.zju.motivation.raw$isWeekend,
                  detail=data.zju.motivation.raw$detail,hasClass=data.zju.motivation.raw$hasClass,
                  actualDetail=data.zju.motivation.raw$actualDetail,semester=data.zju.motivation.raw$simpleSemester
                  ),length)
write.xlsx(x=stat.motivation.class,file="ZJU_Stat_Motivation.xlsx")

####��������Լ��ֲ�####
#����ûɶ�����
scatterplotMatrix(data.zju.motivation.raw[on_off==1,c("isWeekend","on_off","modiTemp","humidity","detail",
                                            "studentType","studentNum")],smoother.agrs=list(lty=2),plot.points = FALSE)
ggplot(data = data.zju.motivation.raw)
cor(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
    use = "complete.obs",method = "spearman")
corr.test(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
          use = "complete.obs",method = "spearman")
symnum(cor(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
           use = "complete.obs",method = "spearman"))

####Logistic�ع�####
fit<-glm(on_off~actualDetail+hasClass+modiTemp+humidity,data=data.zju.motivation.raw,family = binomial())
summary(fit)