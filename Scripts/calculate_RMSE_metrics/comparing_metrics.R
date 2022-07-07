pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools, Metrics)

setwd("~/Dropbox/SUNP-GLMv3.3-JHW/")
sim_folder <- getwd()

file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed4.nml', 'aed/aed.nml', overwrite = TRUE)

nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 


############## temperature data #######
obstemp<-read_csv("data/formatted-data/manual_buoy_temp.csv") %>%  
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(obstemp = Temp) %>%  
  mutate(Depth = round(Depth)) %>% 
  group_by(Depth, DateTime) %>% 
  mutate(temp_avg = mean(obstemp, na.rm = TRUE)) %>% 
  distinct(DateTime, Depth, .keep_all = TRUE)

unique(obstemp$Depth)

#get modeled temperature readings for focal depths
depths<- unique(obstemp$Depth)
modtemp <- get_temp(nc_file, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  rename(modtemp = temp)




#lets do depth by depth comparisons of the obs vs mod temps for each focal depth
watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth"))
watertemp$Depth <- as.numeric(watertemp$Depth)
watertemp$DateTime <- as.Date(watertemp$DateTime)
#for(i in 1:length(unique(watertemp$Depth))){
#  tempdf<-subset(watertemp, watertemp$Depth==depths[i])
#  plot(as.Date(tempdf$DateTime), tempdf$obstemp, type='p', col='red',
#       ylab='temperature', xlab='time',
#       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,30))
#  points(as.Date(tempdf$DateTime), tempdf$modtemp, type="l",col='black')
#}

# #thermocline depth comparison
# field_file<-file.path(sim_folder,'/field_data/CleanedObsTemp.csv')
# plot_var_compare(nc_file,field_file,var_name = "temp", precision="days",col_lim = c(0,30)) #compare obs vs modeled
# therm_depths <- compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days",method='interp',as_value=TRUE, na.rm=T)
# compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days", method='interp',as_value=F, na.rm=TRUE) #prints RMSE
# plot(therm_depths$DateTime,therm_depths$mod, type="l", ylim=c(1,9),main = paste0("ThermoclineDepth: Obs=Red, Mod=Black"),
#      ylab="Thermocline depth, in m")
# points(therm_depths$DateTime, therm_depths$obs, type="l",col="red")

# #Run sim diagnostics and calculate RMSE using glmtools
# field_file<-file.path(sim_folder,"data/formatted-data/manual_buoy_temp.csv")
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'hypo.temperature', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'epi.temperature', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'thermo.depth', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'water.temperature', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')#raw temp, assuming each depth is treated equally
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'whole.lake.temperature', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')#volume-weighted temp
# compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'schmidt.stability', as_value = FALSE,
#                  na.rm = TRUE, precision = 'days',method = 'interp')

#can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

obstemp$Temp <- obstemp$temp_avg

field_file<-file.path(sim_folder,"data/formatted-data/manual_buoy_temp.csv")
temps <- resample_to_field(nc_file, field_file, precision="mins", method='interp')
temps<-temps[complete.cases(temps),]

temps <- filter(temps, DateTime >= "2007-01-01")

m_temp <- temps$Modeled_temp[temps$Depth==c(1)] #1m depth (epi) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(1)] 
RMSE(m_temp,o_temp)
bias(o_temp, m_temp)


m_temp <- temps$Modeled_temp[temps$Depth==c(30)] #30m depth (hypo) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(30)] 
RMSE(m_temp,o_temp)
bias(o_temp, m_temp)

m_temp <- temps$Modeled_temp[temps$Depth==c(8)] #8m depth (meta) RMSE
o_temp <- temps$Observed_temp[temps$Depth==c(8)] 
RMSE(m_temp,o_temp)
bias(o_temp, m_temp)

m_temp <- temps$Modeled_temp[temps$Depth>=0 & temps$Depth<=33] # (all depths)
o_temp <- temps$Observed_temp[temps$Depth>=0 & temps$Depth<=33] 
RMSE(m_temp,o_temp)
bias(o_temp, m_temp)




################################# Oxygen Data ###############################################
#read in cleaned CTD temp file with long-term obs at focal depths
var="OXY_oxy"
obs_oxy<-read.csv('data/formatted-data/manual_buoy_oxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
field_file <- file.path(sim_folder,'data/formatted-data/oxy_fieldfile.csv') 
depths<- unique(obs_oxy$Depth)
plot_var(nc_file,var_name = var, precision="days",col_lim = c(0, 600)) #compare obs vs modeled

#get modeled oxygen concentrations for focal depths
mod_oxy <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 



#plot_var(nc_file,var_name = var, precision="days",col_lim = c(0,600)) #compare obs vs modeled

colnames(obs_oxy)
colnames(mod_oxy)
obs_oxy$Depth <- as.numeric(obs_oxy$Depth)
mod_oxy$Depth <- gsub('_', '', mod_oxy$Depth)
mod_oxy$Depth <- as.numeric(mod_oxy$Depth)

#lets do depth by depth comparisons of modeled vs. observed oxygen
oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) %>% 
  rename(mod_oxy = OXY_oxy, obs_oxy = DOppm)
depths<- unique(oxy_compare$Depth)


for(i in 1:length(unique(oxy_compare$Depth))){
  tempdf<-subset(oxy_compare, oxy_compare$Depth==depths[i])
  plot(as.Date(tempdf$DateTime),tempdf$obs_oxy, type='p', col='red',
       ylab='mmol/m3', xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,600))
  points(as.Date(tempdf$DateTime), tempdf$mod_oxy, type="l",col='black')
}


#can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#calculate RMSE for oxygen
oxygen <- resample_to_field(nc_file, field_file, precision="days", method='interp', 
                            var_name="OXY_oxy")
oxygen <-oxygen[complete.cases(oxygen),] #remove missing data
oxygen <- filter(oxygen, DateTime >= "2007-01-01")

oxygen$Observed_OXY_oxy_mgl <- oxygen$Observed_OXY_oxy*32/1000
oxygen$Modeled_OXY_oxy_mgl <- oxygen$Modeled_OXY_oxy*32/1000


m_oxygen <- oxygen$Modeled_OXY_oxy_mgl[oxygen$Depth>=1.5 & oxygen$Depth<=1.5] #1m depth
o_oxygen <- oxygen$Observed_OXY_oxy_mgl[oxygen$Depth>=1.5 & oxygen$Depth<=1.5] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy_mgl[oxygen$Depth>=30 & oxygen$Depth<=30] #30m depth
o_oxygen <- oxygen$Observed_OXY_oxy_mgl[oxygen$Depth>=30 & oxygen$Depth<=30] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy_mgl[oxygen$Depth>=8 & oxygen$Depth<=8] #8 m depth
o_oxygen <- oxygen$Observed_OXY_oxy_mgl[oxygen$Depth>=8 & oxygen$Depth<=8] 
RMSE(m_oxygen,o_oxygen)

m_oxygen <- oxygen$Modeled_OXY_oxy_mgl[oxygen$Depth>=0 & oxygen$Depth<=33] #all depths
o_oxygen <- oxygen$Observed_OXY_oxy_mgl[oxygen$Depth>=0 & oxygen$Depth<=33] 
RMSE(m_oxygen,o_oxygen)





############################### chlorophyll #################################################################

var="PHY_tchla"
field_file <- file.path(sim_folder,'data/formatted-data/fieldchla.csv') 

obs<-read.csv('data/formatted-data/field_obs_chla.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(PHY_tchla = PHY_TCHLA) %>% 
  dplyr::select(DateTime, Depth, var) 
obs$Depth <- 1
write.csv(obs, "data/formatted-data/fieldchla.csv")
#modchla <- get_var(nc_file, var)
#modchla_filtered <- select(modchla, DateTime, PHY_tchla.elv_31.4788988658047)
#obs_filtered <- filter(obs, DateTime >= "2015-01-01 12:00:00" & DateTime <= "2020-12-31 12:00:00")
#
#plot(x = modchla_filtered$DateTime, y = modchla_filtered$PHY_tchla.elv_31.4788988658047, col = 'black', type = 'l')
#points(x = obs_filtered$DateTime, y = obs_filtered$PHY_tchla, col = 'red') 


#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,30)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
#compare<-merge(mod, obs, by=c("DateTime","Depth"))
#compare<-na.omit(compare)
for(i in 1:length(depths)){
  modeled <-subset(mod, mod$Depth==depths[i])
  observed <-subset(obs, obs$Depth==depths[i] & DateTime >= "2005-06-27" & DateTime <= "2015-01-01")
  if(nrow(modeled)>1){
    plot(observed$DateTime,observed[,3], type='p', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),
         ylim = c(0,10))
    points(modeled$DateTime, modeled$PHY_tchla, type="l",col='black')
  }
}

#calculate RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]

newdata <- filter(newdata, DateTime >= "2007-01-01")

mod <- eval(parse(text=paste0("newdata$Modeled_",var)))[newdata$Depth>=1 & newdata$Depth<=1] 
obs <- eval(parse(text=paste0("newdata$Observed_",var)))[newdata$Depth>=1 & newdata$Depth<=1] 
RMSE(mod,obs)



r2 <-lm(mod ~ obs)
summary(r2)
#plot individual phyto groups
#plot_var(file=nc_file,"PHY_TCHLA",reference="surface", col_lim=c(0,30))
#plot_var(file=nc_file,"PHY_CYANOPCH1",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_CYANONPCH2",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_CHLOROPCH3",reference="surface", col_lim=c(0,50))
#plot_var(file=nc_file,"PHY_DIATOMPCH4",reference="surface", col_lim=c(0,50))



#### phosphate ########################################

var="TOT_tp"
field_file <- file.path(sim_folder,'/data/formatted-data/fieldTP.csv') 

obs<-read.csv('./data/formatted-data/field_obs_TP.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>% 
  mutate(PHS_frp = TOT_tp) %>% 
  dplyr::select(DateTime, Depth, var) %>%
  na.omit()
write.csv(obs, './data/formatted-data/fieldTP.csv')

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
         ylab=var, xlab='time',
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,0.5))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

#calculate RMSE 
newdata <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
                             var_name=var)
newdata <-newdata[complete.cases(newdata),]
newdata <- filter(newdata, DateTime >= "2007-01-01")

newdata$Observed_TOT_tp_ugl <- newdata$Observed_TOT_tp*30.9738
newdata$Modeled_TOT_tp_ugl <- newdata$Modeled_TOT_tp*30.9738

range(newdata$Observed_TOT_tp_ugl)
range(newdata$Modeled_TOT_tp_ugl)

mean(newdata$Observed_TOT_tp_ugl)
mean(newdata$Modeled_TOT_tp_ugl)

mod <- eval(parse(text=paste0("newdata$Modeled_",var, "_ugl")))[newdata$Depth>=2.0 & newdata$Depth<=2.0] 
obs <- eval(parse(text=paste0("newdata$Observed_",var, "_ugl")))[newdata$Depth>=2.0 & newdata$Depth<=2.0] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var, "_ugl")))[newdata$Depth>=22 & newdata$Depth<=22] 
obs <- eval(parse(text=paste0("newdata$Observed_",var, "_ugl")))[newdata$Depth>=22 & newdata$Depth<=22] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var, "_ugl")))[newdata$Depth>=8 & newdata$Depth<=8] 
obs <- eval(parse(text=paste0("newdata$Observed_",var, "_ugl")))[newdata$Depth>=8 & newdata$Depth<=8] 
RMSE(mod,obs)

mod <- eval(parse(text=paste0("newdata$Modeled_",var, "_ugl")))[newdata$Depth>=0.1 & newdata$Depth<=33] 
obs <- eval(parse(text=paste0("newdata$Observed_",var, "_ugl")))[newdata$Depth>=0.1 & newdata$Depth<=33] 
RMSE(mod,obs)



