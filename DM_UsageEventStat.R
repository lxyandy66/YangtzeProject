####用于处理HZNU空调已处理的事件性数据####
# 接 热舒适报表汇总_含追加汇总.xlsx

data.hznu.teaching.event<-read.xlsx(file = "HZNU_Teaching_usage_event.xlsx",sheetIndex = 1) %>% as.data.table(.)
