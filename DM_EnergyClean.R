####���ű�����HZNU�ܺ���������
#�˴�����ԭʼ�ܺ����� HZNU_��׷��_�ܺ�_����ϴ_�й�������.rdata
#ѡȡ��ĩ�˵Ŀ����ܺļ�¼���Ը�ĩ�˵Ŀ���״̬�ֱ������ϴ����ͬĩ���ܺĲ���ͳһ��ϴ

##��ȡÿ̨ĩ�˵��ܺ�����ֵ��������ͼhigh-whisker��Ϊ����޶�
list.hznu.ac.energy.lim<-tapply(simplify = TRUE,data.hznu.predict[on_off!=0]$total_elec,
                                INDEX = data.hznu.predict[on_off!=0]$ac_code,boxplot.stats)
#Ϊɶֱ�Ӷ���ac_code��sapplyȡ����ecLim�Ļ���list����
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

####���ӻ��ܺ�����####
plot(data.hznu.predict[on_off==1]$total_elec,type="l")
lines(data.hznu.predict[on_off==1]$modifyElec,col="red")#��ͼ̫��

ggplot(data=data.hznu.predict[on_off==1])+geom_density(aes(x=total_elec))+geom_density(aes(x=modifyElec,color="red"))

length(data.hznu.predict[on_off==1&total_elec!=modifyElec]$total_elec)/length(data.hznu.predict[on_off==1]$total_elec)
#��1/2hinge-whisker��׼ ԭʼ�ܺĴ��ڴ�ռ�� 0.103
#��whisker��׼ ԭʼ�ܺĴ��ڴ�ռ�ȴ���  0.04968963
length(data.hznu.predict[on_off==1&modifyElec>ecLimValue]$total_elec)/length(data.hznu.predict[on_off==1]$total_elec)
#�������ܺĴ��������� 1/2hinge-whisker [1] 0.008954625
#�������ܺĴ��������� whisker[1] 0.01359423

data.hznu.predict[modifyElec>ecLimValue]$modifyElec<-ecLimValue+
  (data.hznu.predict[modifyElec>ecLimValue]$modifyElec-ecLimValue)/5