# run glm-aed using oxygen only aed nml

# download glm-aed v3.3
#remotes::install_github("FLARE-forecast/GLM3r", force = T)
#remotes::install_github("CareyLabVT/glmtools", force = T)

Sys.setenv(TZ = 'America/New_York')

# Load packages, set sim folder, load nml file ####
#if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, ncdf4, glmtools, here)
glm_version()

setwd(here())
sim_folder <- getwd()

#look at glm and aed nml files
nml_file <- paste0(sim_folder,"/glm3_wAED.nml")
nml <- read_nml(nml_file) 
print(nml)

############################################################################################################
##### run the model #######
GLM3r::run_glm(sim_folder, nml_file = 'glm3_wAED.nml', verbose = T)

############################################################################################################
##### look at output #####
nc_file <- file.path(sim_folder, 'output.nc') #defines the output.nc file 
print(sim_vars(nc_file))

#get water level
water_level<-get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE)
plot(water_level$DateTime,water_level$surface_height)

# avg surface temp
avgsurftemp<- get_var(nc_file,"avg_surf_temp")
plot(avgsurftemp$DateTime, avgsurftemp$avg_surf_temp, ylim=c(-1,30))

##########################################################################################################
# compare to obs
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
for(i in 1:length(unique(watertemp$Depth))){
  tempdf<-subset(watertemp, watertemp$Depth==depths[i])
  plot(as.Date(tempdf$DateTime), tempdf$obstemp, type='p', col='red',
       ylab='temperature', xlab='time',
       main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,30))
  points(tempdf$DateTime, tempdf$modtemp, type="l",col='black')
}

field_file<- file.path(sim_folder,'data/formatted-data/manual_buoy_temp.csv')
plot_var_compare(nc_file,field_file,var_name = "temp", precision="days",col_lim = c(0,30)) #compare obs vs modeled

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

field_file<-file.path(sim_folder,'data/formatted-data/field_temp_noon_obs.csv')
temps <- resample_to_field(nc_file, field_file, precision="mins", method='interp')
temps<-temps[complete.cases(temps),]

m_temp <- temps$Modeled_temp[temps$Depth>=0 & temps$Depth<=30] #depths from 0.1-9m (all depths)
o_temp <- temps$Observed_temp[temps$Depth>=0 & temps$Depth<=30] 
RMSE(m_temp,o_temp)


####################################################################################################
############## oxygen data #######

#read in cleaned CTD temp file with long-term obs at focal depths
var="OXY_oxy"
obs_oxy<-read.csv('data/formatted-data/manual_buoy_oxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
depths<- unique(obs_oxy$Depth)
plot_var(nc_file,var_name = var, precision="days") #compare obs vs modeled

#get modeled oxygen concentrations for focal depths
mod_oxy <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

colnames(obs_oxy)
colnames(mod_oxy)
obs_oxy$Depth <- as.numeric(obs_oxy$Depth)
mod_oxy$Depth <- gsub('_', '', mod_oxy$Depth)
mod_oxy$Depth <- as.numeric(mod_oxy$Depth)

#lets do depth by depth comparisons of modeled vs. observed oxygen
oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) %>% 
  filter(Depth %in% c(1.0, 10.0))

depths<- unique(oxy_compare$Depth)
depths

ggplot(data = oxy_compare, aes(x = DateTime, y = OXY_oxy, col = as.factor(Depth))) +
  geom_line() +
  geom_point(aes(x = DateTime, y = DOppm)) +
  facet_wrap(~Depth)

ggplot(data = oxy_compare, aes(x = DateTime, y = OXY_oxy*32/1000, col = as.factor(Depth))) +
  geom_line() +
  geom_point(aes(x = DateTime, y = DOppm*32/1000)) +
  facet_wrap(~Depth)

#calculate RMSE for oxygen
RMSE(oxy_compare$OXY_oxy,oxy_compare$DOppm)
RMSE(oxy_compare$OXY_oxy*32/1000,oxy_compare$DOppm*32/1000)
