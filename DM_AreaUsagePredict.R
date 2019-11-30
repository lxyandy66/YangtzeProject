####本脚本用于区域级别上的空调使用率预测####
# 接data.hznu.area.predict.raw
#预测对象 fullOnRatio

data.hznu.area.predict.use<-merge(x=data.hznu.area.predict.raw[,c("date","datetime","buildingCount","buildingCountEnergy",
                                                                     "fullOnRatio","modiElec",
                                                                     "outTemp","rhOut","windSpeed","isWeekday","isBizday")],
                                     y=data.hznu.area.signCheck[,c("datetime","weekday","stdModiElec","modiSeason",
                                                                   "d0h1_FullOnRatio","d1h0_FullOnRatio","d7h0_FullOnRatio",
                                                                   "d1_onDemandRatio","d1_forenoonRatio","d1_afternoonRatio","d1_daytimeRatio","d1_lateDaytimeRatio",
                                                                   "d7_onDemandRatio","d7_daytimeRatio","d7_afternoonRatio","d7_lateDaytimeRatio")],
                                     all.x = TRUE,by.x="datetime",by.y="datetime") %>% .[substr(date,1,4)=="2017"|substr(date,1,7)=="2018-01"]


for(i in unique(data.hznu.area.predict.use$modiSeason)){
  cat("\nbox stat in ",i,": ",boxplot.stats(data.hznu.area.predict.use[modiSeason==i]$fullOnRatio)$stats)#0.000000000 0.006578947 median=0.026402640 0.072816246 0.172000000
  cat("\nmean in ",i,": ",mean(data.hznu.area.predict.use[modiSeason==i]$fullOnRatio))#0.05149866
}




####时间序列构建####
# ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,01,01),frequency = 15)
fit.ts.hznu.usage<-ts(data.hznu.area.predict.use$fullOnRatio,start = c(2017,1,1,8),frequency = 15) %>% ets(.,model="AAA")
data.hznu.area.predict.use$tsFullOnRatio<-as.numeric(fit.ts.hznu.usage$fitted)
data.hznu.area.predict.use[tsFullOnRatio<0]$tsFullOnRatio<-0
#计算纯时间序列的相对误差
data.hznu.area.predict.use$rlatTsErr<-abs((data.hznu.area.predict.use$fullOnRatio-data.hznu.area.predict.use$tsFullOnRatio)/data.hznu.area.predict.use$fullOnRatio)
getMAPE(yPred = data.hznu.area.predict.use[complete.cases(data.hznu.area.predict.use)]$tsFullOnRatio, yLook = data.hznu.area.predict.use[complete.cases(data.hznu.area.predict.use)]$fullOnRatio)
RMSE(pred = data.hznu.area.predict.use$tsFullOnRatio,obs = data.hznu.area.predict.use$fullOnRatio,na.rm = TRUE)#RMSE大概0.022

boxplot.stats(data.hznu.area.predict.use$rlatTsErr)

ggplot(data = data.hznu.area.predict.use,aes(y=rlatTsErr))+geom_boxplot()+ylim(0,2)
ggplot(data=data.hznu.area.predict.use[substr(datetime,1,7)=="2017-07",c("datetime","fullOnRatio","tsFullOnRatio")] %>% mutate(.,year=substr(datetime,1,4))%>% melt(.,id.var=c("datetime","year")),
       aes(x=datetime,y=value,color=variable))+geom_line()+geom_point()+facet_wrap(~year,nrow = 2)


####试一试KNN####

for(i in unique(data.hznu.area.predict.use$modiSeason)){
  for(j in c(1/))
  data.hznu.area.predict.use[modiSeason==i]
}


