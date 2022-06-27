# script to download and format observed nutrient and chl-a data in Lake Sunapee
library(tidyverse)

# download LSPA LMP monitoring data from zenodo: https://zenodo.org/record/4652076#.YrmxBnbMLa9
# you already copied this data over from the sunapee glm but if you want to hardcode the download, this code will do it if you modify it slightly:
# manually collected data
#if(!file.exists(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2020.1.zip'))){
#  download.file(url = 'https://zenodo.org/record/4652076/files/Lake-Sunapee-Protective-Association/LMP-v2020.1.zip?download=1',
#                destfile = file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2020.1.zip'),
#                mode = 'wb')
#  unzip(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2020.1.zip'),
#        files = file.path('Lake-Sunapee-Protective-Association-LMP-271fcb0', 'master files', 'LSPALMP_1986-2020_v2021-03-29.csv'),
#        exdir = file.path(lake_directory, 'data_raw', 'hist-data', 'LSPA_LMP'),
#        junkpaths = TRUE)
#}

# data stored in './data/manual-data

data <- read.csv('./data/manual-data/master files/LSPALMP_1986-2020_v2021-03-29.csv')

# buoy is site 210
data <- data %>% 
  filter(station==210)

unique(data$parameter)

########## format TP 
chem <- data %>% 
  filter(parameter=='TP_mgl')

# obs file should be DateTime, Depth, TOT_tn, TOT_tp
# how do we get TN data? check Nicole's scripts for this?
chem <- chem %>% 
  select(date, depth_m, value) 
colnames(chem) <- c('DateTime', 'Depth', 'TP_mgL')

# convert to GLM units (mmol/m3)
chem <- chem %>% 
  mutate(TP_ugL = TP_mgL*1000) %>% 
  mutate(TOT_tp = TP_ugL*1000*0.001*(1/30.9738)) %>% 
  select(-TP_mgL, -TP_ugL)

write.csv(chem, './data/formatted-data/field_obs_TP.csv', row.names = FALSE)


######### format chl-a
bio <- data %>% 
  filter(parameter=='chla_ugl')

# obs file should be DateTime, Depth, PHY_TCHLA
bio <- bio %>% 
  select(date, depth_m, value) 
colnames(bio) <- c('DateTime', 'Depth', 'PHY_TCHLA')

#depth is NA here--can compare to 'surface' of aed output?

write.csv(bio, './data/formatted-data/field_obs_chla.csv', row.names = FALSE)


######### format secchi
secchi <- data %>% 
  filter(parameter=='secchidepth_m')

secchi <- secchi %>% 
  select(date, depth_m, value)
colnames(secchi) <- c('DateTime', 'Depth', 'Secchi_m')

secchi <- secchi %>% 
  mutate(extc_coef = Secchi_m/1.7) 

# set depth to '1' following FCR
secchi$Depth <- 1
  
ggplot(secchi, aes(DateTime, Secchi_m)) + 
  geom_point()

write.csv(secchi, './data/formatted-data/field_obs_secchi.csv', row.names = FALSE)

######### format pH
ph <- data %>% 
  filter(parameter=='conc_H_molpl')

ph <- ph %>% 
  select(date, depth_m, value)
colnames(ph) <- c('DateTime', 'Depth', 'conc_H_molpl')

# convert concentration of hydrogen ions in water in moles/L to pH
# (H+ = 10^-pH) 
ph <- ph %>% 
  mutate(pH = log10(conc_H_molpl)*-1)

ggplot(ph, aes(DateTime, pH, color = as.factor(Depth))) +
  geom_point()

# depths with NA's? not sure where these were taken historically but looks like they are all older sites
write.csv(ph, './data/formatted-data/field_obs_pH.csv', row.names = FALSE)
