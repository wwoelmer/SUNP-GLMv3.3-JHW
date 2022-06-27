oneinflow <- read_csv("data/oneInflow_14Jun19.csv")

i505 <- read_csv("data/individual_inflows/formatted_ind_inflows_loads/i505_load_input.csv")

str(oneinflow)
str(i505)


'data/individual_inflows/formatted_ind_inflows_loads/i760_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i788_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i790_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i800_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i805_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i830_load_input.csv',
'data/individual_inflows/formatted_ind_inflows_loads/i835_load_input.csv'

lake <- read.csv("output/lake.csv")


plot(corr_outflow_impmodel_baseflow_2022_06_24$date, corr_outflow_impmodel_baseflow_2022_06_24$corr_outflow_m3s, type = 'l')


damlate <- read.csv("data/observeddam/Sunapee_NHDESDB_historical_raw_2010-2019.csv")


damearly <- read.csv("data/observeddam/Sunapee_NHDESDB_dailyhistorical_raw_1981-2010.csv")

library(lubridate)



damlate$datetime <- as.Date(damlate$datetime)

newdam <- damlate %>% 
  group_by(datetime) %>% 
  mutate(lakeElevation_ft = mean(lakeElevation_ft)) %>% 
  mutate(airTemperature_degF = mean(airTemperature_degF)) %>% 
  mutate(outflow_cfs = mean(outflow_cfs)) %>% 
  mutate(precipitation_in = sum(precipitation_in))

newdam <- newdam[!duplicated(newdam), ]


damearly <- select(damearly, date, lakeElevation_average_ft, outflow_average_cfs)
newdam <- select(newdam, datetime, lakeElevation_ft, outflow_cfs)
colnames(newdam) <- c("date", "lakeElevation_average_ft", "outflow_average_cfs")

str(newdam)
str(damearly)

observed_outflow <- rbind(damearly, newdam)

observed_outflow$outflow_average_m3s <- observed_outflow$outflow_average_cfs*0.028316847
observed_outflow <- select(observed_outflow, date, lakeElevation_average_ft, outflow_average_m3s)


plot(as.Date(observed_outflow$date), observed_outflow$outflow_average_m3s)

write.csv(observed_outflow, "data/observed_outflow.csv", row.names = F, quote = F)

out <- read.csv('data/individual_inflows/corr_outflow_impmodel_baseflow_2022-06-24.csv')


merged_outflows <- merge(observed_outflow, out)
colnames(merged_outflows)
colnames(merged_outflows) <- c("date", "lakeElevation_average_ft", "observed_flow_m3s", "modeled_flow_m3s")

plot(as.Date(merged_outflows$date), merged_outflows$observed_flow_m3s)
points(as.Date(merged_outflows$date), merged_outflows$modeled_flow_m3s, col = "red")

plot(merged_outflows$observed_flow_m3s, merged_outflows$modeled_flow_m3s)

lm <- lm(outflow_average_m3s~corr_outflow_m3s, merged_outflows)
summary(lm)

plot(as.Date(merged_outflows$date), merged_outflows$modeled_flow_m3s)



plot(as.Date(damearly$date), damearly$outflow_average_cfs)

inflow <- read.csv("data/individual_inflows/balance_totalinflow_tempj_2022-06-24.csv")


plot(as.Date(inflow$date), inflow$bal_inf_m3s)
  
  
  

  
  
  

out <- read.csv('data/individual_inflows/corr_outflow_impmodel_baseflow_2022-06-24.csv')

colnames(out) <- c("time", "FLOW")

write.csv(out, 'data/individual_inflows/corr_outflow_impmodel_baseflow_2022-06-24_test.csv', row.names = F, 
          quote = F)

plot(as.Date(out$time), out$FLOW)
out$time <- as.Date(out$time)

inf <- read.csv('./from_Nicole/GLM_Sunapee_Drive/oneInflow_14Jun19.csv')
inf$time <- as.Date(inf$time)
plot(as.Date(inf$time), inf$FLOW)

plot(as.Date(out$time), out$FLOW)
points(as.Date(inf$time), inf$FLOW, col = 'red')

out <- out[out$time %in% inf$time,]
inf <- inf[inf$time %in% out$time,]
plot(as.Date(t$time), t$FLOW - inf$FLOW)




  
  observed_dam()
  
  
  
select(observed_outflow, date, outflow_average_m3s)

colnames(observed_outflow) <- c("time", "FLOW")


write.csv(observed_outflow, "data/observed_outflow.csv", row.names = F, quote = F)






