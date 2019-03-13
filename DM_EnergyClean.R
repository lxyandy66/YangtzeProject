####本脚本用于HZNU能耗数据清理
#此处加载原始能耗数据 HZNU_含追加_能耗_已清洗_有过程属性.rdata
#选取各末端的开启能耗记录，对各末端的开启状态分别进行清洗，不同末端能耗不再统一清洗

##获取每台末端的能耗限制值，以箱形图high-whisker作为最高限额
list.hznu.ac.energy.lim<-tapply(simplify = TRUE,data.hznu.predict[on_off!=0]$total_elec,
                                INDEX = data.hznu.predict[on_off!=0]$ac_code,boxplot.stats)
#为啥直接对于ac_code用sapply取出来ecLim的会是list类型
data.hznu.ac.ecLim<-data.table(ac_code=names(list.hznu.ac.energy.lim))
data.hznu.ac.ecLim$ecLim<- -99

for(i in c(1:length(data.hznu.ac.ecLim$ac_code))){
  data.hznu.ac.ecLim[i]$ecLim<-tail(list.hznu.ac.energy.lim[[i]]$stats,1)
      #ifelse(length(list.hznu.ac.energy.lim[[i]]$stats)-1>0,
       #                               (tail(list.hznu.ac.energy.lim[[i]]$stats,1)+
        #                                 list.hznu.ac.energy.lim[[i]]$stats[length(list.hznu.ac.energy.lim[[i]]$stats)-1])/2,
         #                             tail(list.hznu.ac.energy.lim[[i]]$stats,1))
}


data.hznu.predict$ecLim<-NULL
data.hznu.predict<-merge(data.hznu.predict,data.hznu.ac.ecLim[,c("ac_code","ecLim")],all.x = TRUE,
                               by.x = "ac_code",by.y = "ac_code")

data.hznu.predict$modifyElec<-data.hznu.predict$total_elec

data.hznu.predict[total_elec>ecLim]$modifyElec<-
  data.hznu.predict[total_elec>ecLim]$ecLim+(
    data.hznu.predict[total_elec>ecLim]$total_elec-data.hznu.predict[total_elec>ecLim]$ecLim
  )/10

####可视化能耗修正####
plot(data.hznu.predict[on_off==1]$total_elec,type="l")
lines(data.hznu.predict[on_off==1]$modifyElec,col="red")#绘图太慢

ggplot(data=data.hznu.predict[on_off==1])+geom_density(aes(x=total_elec))+geom_density(aes(x=modifyElec,color="red"))

length(data.hznu.predict[on_off==1&total_elec!=modifyElec]$total_elec)/length(data.hznu.predict[on_off==1]$total_elec)
#按1/2hinge-whisker标准 原始能耗大于此占比 0.103
#按whisker标准 原始能耗大于此占比大于  0.04968963
length(data.hznu.predict[on_off==1&modifyElec>ecLimValue]$total_elec)/length(data.hznu.predict[on_off==1]$total_elec)
#修正后能耗大于总限制 1/2hinge-whisker [1] 0.008954625
#修正后能耗大于总限制 whisker[1] 0.01359423

data.hznu.predict[modifyElec>ecLimValue]$modifyElec<-ecLimValue+
  (data.hznu.predict[modifyElec>ecLimValue]$modifyElec-ecLimValue)/5
