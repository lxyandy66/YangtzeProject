####专利用SVM预测####


seasonalAttr<-c("simpleKnnFullOnRatio","hour","stdOutTemp",paste(c("d0h1","d0h2","d1h0"),"_modiElecStd",sep=""))
data.hznu.area.predict.pt<-data.hznu.area.predict.use[substr(date,1,7)=="2017-09",
                                                      c("id","datetime","date","modiSeason","fullOnRatio","modiElec","stdModiElec",..seasonalAttr)]
data.hznu.area.predict.pt[,c("ptSvmElec","ptSvmElecDeNorm")]<- -999
data.hznu.area.predict.pt<-data.hznu.area.predict.pt%>%.[complete.cases(.[,..seasonalAttr])]%>%{
  for(season in unique(.$modiSeason)){
    for(round in 0:9){
      cat("\n",season,round,"\n")
      .[modiSeason==season]<-.[modiSeason==season]%>%{
        fit.svm<-ksvm(x=as.formula( paste("stdModiElec ~ ",paste(seasonalAttr,collapse = "+") ) ),
                      data=.[id%%10!=round],
                      kernel="polydot",type="eps-svr",epsilon=0.001,C=15,cross=10)#为啥这么慢
        .[id%%10==round]$ptSvmElec<-predict(fit.svm,.[id%%10==round])
        .#切记每一个%>%传到下一个的都是最后一句话的返回值，如果没有则返回空
      }
    }
  }
  .#切记这里需要返回一个原数据
}%>%{
  .[ptSvmElec== -999]$ptSvmElec<-NA
  .$ptSvmElecDeNorm<-denormalize(targetNorm = .$ptSvmElec,refReal = .$modiElec,refNorm = .$stdModiElec)
  .[ptSvmElecDeNorm==-999]$ptSvmElecDeNorm<-NA
  .
}

#专利用结果检测
data.hznu.area.predict.pt %>% {
  cat(RMSE(pred = pull(.,"ptSvmElecDeNorm"),obs = .$modiElec,na.rm = TRUE),"\t",
      getRSquare(pred = pull(.,"ptSvmElecDeNorm"),ref = .$modiElec),"\t",
      getMAPE(yPred = pull(.,"ptSvmElecDeNorm"), yLook = .[modiElec!=0]$modiElec))#0.8631175
}

ggplot(data=data.hznu.area.predict.pt[,c("datetime","modiElec","ptSvmElecDeNorm")] %>% melt(.,id.var=c("datetime")),#,"stdModiElec","ptSvmElecDeNorm"
       aes(x=datetime,y=value,color=variable,shape=variable,lty=variable,group=paste(date(datetime),variable)))+geom_line()+geom_point(size=2)+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14),legend.position = c(0.9,0.85))
#9月 Denorm 32.79336 	 0.9220783 	 0.1227471  449
#9月 std    0.06006215 	 0.8582036 	 0.1227471 449
#9月 直接    44.24175 	 0.8581754 	 0.5035281 449

