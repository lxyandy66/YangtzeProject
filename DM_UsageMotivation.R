####本脚本用于分析教学建筑空调使用的驱动因素####
#接已处理好的数据集data.zju.combine.final

data.zju.motivation.raw<-data.zju.combine.final[,c("baseLabel","datetime","weekday","semester","isWeekend",
                                                   "roomCode","on_off","temp","modiTemp","humidity","detail",
                                                   "classNo","lessonCode","studentType","studentNum")]

data.zju.motivation.raw$isWeekend<-as.numeric(data.zju.motivation.raw$isWeekend)
data.zju.motivation.raw[temp==0]$temp<-NA
data.zju.motivation.raw[humidity==0]$humidity<-NA

data.zju.motivation.raw$hasClass<-ifelse(data.zju.motivation.raw$lessonCode=="",0,1)
data.zju.motivation.raw$detailType<-ifelse(data.zju.motivation.raw$detail=="rest",-1,
                                           ifelse(data.zju.motivation.raw$detail=="break",0,
                                                  ifelse(data.zju.motivation.raw$detail=="class",1,-2)))

####基本统计####
length(data.zju.motivation.raw[on_off==1&lessonCode!=""]$baseLabel)/
  length(data.zju.motivation.raw[on_off==1]$baseLabel)

data.zju.motivation.raw$semester<-as.character(data.zju.motivation.raw$semester)
data.zju.motivation.raw$simpleSemester<-sapply(data.zju.motivation.raw$semester,
                                               FUN = getSplitMember,splitSimbol = "_",isLastOne = TRUE)
data.zju.motivation.raw$hasClass<-as.factor(data.zju.motivation.raw$hasClass)
data.zju.motivation.raw$on_off<-as.factor(data.zju.motivation.raw$on_off)
data.zju.motivation.raw$labelSemeOnOff<-paste(data.zju.motivation.raw$on_off,data.zju.motivation.raw$simpleSemester,sep = "_")

ggplot(data=data.zju.motivation.raw[simpleSemester=="Summer"],aes(x=modiTemp,color=simpleSemester,fill=on_off,linetype=hasClass))+
  geom_density(alpha=0.3)

stat.motivation.class<-aggregate(data.zju.motivation.raw[,c("baseLabel")],
          by=list(on_off=data.zju.motivation.raw$on_off,isWeekend=data.zju.motivation.raw$isWeekend,
                  detail=data.zju.motivation.raw$detail,hasClass=data.zju.motivation.raw$hasClass,
                  semester=data.zju.motivation.raw$simpleSemester),length)
write.xlsx(x=stat.motivation.class,file="ZJU_Stat_Motivation.xlsx")

####变量相关性及分布####
#好像没啥相关性
scatterplotMatrix(data.zju.motivation.raw[on_off==1,c("isWeekend","on_off","modiTemp","humidity","detail",
                                            "studentType","studentNum")],smoother.agrs=list(lty=2),plot.points = FALSE)
ggplot(data = data.zju.motivation.raw)
cor(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
    use = "complete.obs",method = "spearman")
corr.test(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
          use = "complete.obs",method = "spearman")
symnum(cor(data.zju.motivation.raw[,c("on_off","modiTemp","humidity","isWeekend","hasClass","detailType")],
           use = "complete.obs",method = "spearman"))
