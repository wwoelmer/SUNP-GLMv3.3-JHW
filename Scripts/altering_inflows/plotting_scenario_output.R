pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools, ggplot2)

# take normal output to create visualizations
# focusing on hypolimnetic DO, surface chl, surface TP
# visualizations are for LSPA

setwd("~/Dropbox/SUNP-GLMv3.3-JHW/")
sim_folder <- getwd()


# read unaltered nc file as well as altered nc files 
nc_file_main <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 
nc_file_i500x2 <- file.path(sim_folder, 'scenario_output/i500/output_500x2.nc')
nc_file_i600x2 <- file.path(sim_folder, 'scenario_output/i600/output_600x2.nc')
nc_file_i700x2 <- file.path(sim_folder, 'scenario_output/i700/output_700x2.nc')
nc_file_i800x2 <- file.path(sim_folder, 'scenario_output/i800/output_800x2.nc')

# Calculate anomalies for each variable using the unaltered file and bind them together 

# time series with box plots -- break box plots into four seasons, or summer and winter depending on how interesting 
## for each variable
### 

# Average DO 
depths = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)
var="OXY_oxy"
mod_oxy_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_oxy)) %>% 
  mutate(id = 'unaltered')

mod_oxy_500x2 <- get_var(nc_file_i500x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_oxy)) %>% 
  mutate(id = 'i500x2')

mod_oxy_600x2 <- get_var(nc_file_i600x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_oxy)) %>% 
  mutate(id = 'i600x2')

mod_oxy_700x2 <- get_var(nc_file_i700x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_oxy)) %>% 
  mutate(id = 'i700x2')

mod_oxy_800x2 <- get_var(nc_file_i800x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_oxy)) %>% 
  mutate(id = 'i800x2')

bound_oxy <- rbind(mod_oxy_unaltered, mod_oxy_500x2, mod_oxy_600x2, mod_oxy_700x2, mod_oxy_800x2)
bound_oxy <- select(bound_oxy, Depth, depth_avg, id)
unique(bound_oxy)

bound_oxy_hypo <- bound_oxy %>% 
  group_by(id) %>% 
  mutate(oxy_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, oxy_avg) 

bound_oxy_hypo <- unique(bound_oxy_hypo)

bound_oxy_hypo$var_type <- as.factor("oxygen")

ggplot(data = bound_oxy_hypo, aes(x = var_type, y = oxy_avg, col = as.factor(id))) + geom_point()

# surface chla

# surface TP 






