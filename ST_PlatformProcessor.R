####用于淑琴姐姐那个平台可视化部分的数据处理####

data.cplx.monthly<-data.raw.final %>% mutate(.,date=paste(year,month,sep = "-")) %>% 
                   .[typeCode=="M",c("BuildingCode","typeCode","area","date","value","eui")] %>% 
                   mutate(.,dataType="1")

data.cplx.annual<-data.all.annualSum %>% mutate(.,date=paste(year,"01",sep="-")) %>%
                  .[typeCode=="M",c("BuildingCode","typeCode","area","date","annualEnergy","annualEUI")]%>% 
                  mutate(.,dataType="2")

write.xlsx(data.cplx.monthly,file = "CPLX_MonthlyData.xlsx")
write.xlsx(data.cplx.annual,file = "CPLX_AnnualData.xlsx")
