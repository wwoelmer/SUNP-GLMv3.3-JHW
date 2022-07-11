pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools, ggplot2, reshape2)

# take normal output to create visualizations
# focusing on hypolimnetic DO, surface chl, surface TP
# visualizations are for LSPA

setwd("~/Dropbox/SUNP-GLMv3.3-JHW/")
sim_folder <- getwd()


# read unaltered nc file as well as altered nc files 
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 
nc_file_i500x2 <- file.path(sim_folder, '')
nc_file_i600x2 <- file.path(sim_folder, 'scenario_output/i600/output_600x2.nc')
nc_file_i700x2 <- file.path(sim_folder, 'scenario_output/i700/output_700x2.nc')
nc_file_i800x2 <- file.path(sim_folder, 'scenario_output/i800/output_800x2.nc')


# time series with box plots -- break box plots into four seasons, or summer and winter depending on how interesting 
## for each variable
### 

# Average DO 
depths = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)
var="OXY_sat"
mod_oxy_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_oxy_500x2 <- get_var(nc_file_i500x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i500x2')

mod_oxy_600x2 <- get_var(nc_file_i600x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i600x2')

mod_oxy_700x2 <- get_var(nc_file_i700x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i700x2')

mod_oxy_800x2 <- get_var(nc_file_i800x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i800x2') 


bound_oxy <- rbind(mod_oxy_unaltered, mod_oxy_500x2, mod_oxy_600x2, mod_oxy_700x2, mod_oxy_800x2)
bound_oxy_depth <- select(bound_oxy, Depth, depth_avg, id)
bound_oxy_date <- select(bound_oxy, DateTime, date_avg, id)
unique(bound_oxy_depth)
bound_oxy_date <- unique(bound_oxy_date)

bound_oxy_date <- bound_oxy_date %>% 
  mutate(month = month(DateTime)) %>% 
  mutate(year = year(DateTime)) %>% 
  group_by(month, year, id) %>% 
  mutate(month_avg = mean(date_avg, na.rm = TRUE))

bound_oxy_date <- select(bound_oxy_date, DateTime, date_avg, id)

oxy_date_wide <- dcast(bound_oxy_date, DateTime~id, value.var = "date_avg")
oxy_date_wide$month <- month(oxy_date_wide$DateTime)


test <- subset(oxy_date_wide, month == 6) 

ggplot(subset(oxy_date_wide, month >= 6 & month <= 8), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() + 
  geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
  geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
  geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
  geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
  ylab("% Saturation") + 
  ggtitle("Summer Oxygen Saturation")
  
ggplot(subset(oxy_date_wide, month >= 1 & month <= 2), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
  geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
  geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
  geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
  ylab("% Saturation") + 
  ggtitle("Winter Oxygen Saturation")
  

obs_oxy<-read.csv('data/formatted-data/manual_buoy_oxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))


obs_oxy_filtered <- obs_oxy %>% 
  filter(Depth >= 10 & DateTime >= "2010-01-01") %>% 
  mutate(year = year(DateTime)) %>% 
  mutate(month = month(DateTime)) %>% 
  mutate(day = day(DateTime)) %>%
  select(DateTime, Depth, DOSat, year, month, day) %>% 
  group_by(year, month, day) %>% 
  mutate(depth_avg = mean(DOSat, na.rm = TRUE)) %>% 
  ungroup()

obs_oxy_filtered <- select(obs_oxy_filtered, DateTime, depth_avg)
colnames(obs_oxy_filtered) <- c("DateTime", "observed_do")


mod_obs_ox <- merge(oxy_date_wide, obs_oxy_filtered, by = "DateTime", all = TRUE)

ggplot(subset(mod_obs_ox, DateTime <= "2016-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
  geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
  geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
  geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
  geom_point(aes(x = DateTime, y = observed_do, col = "observed")) + 
  ylab("% Saturation") + 
  ggtitle("Year round oxygen saturation")

bound_oxy_hypo <- bound_oxy %>% 
  group_by(id) %>% 
  mutate(oxy_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, oxy_avg) 

bound_oxy_hypo <- unique(bound_oxy_hypo)

bound_oxy_hypo$var_type <- as.factor("oxygen")

ggplot(data = bound_oxy_hypo, aes(x = var_type, y = oxy_avg, col = as.factor(id))) + geom_point()

# surface chla
depths = c(0.1)
var="PHY_tchla"
mod_chla_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_chla_500x2 <- get_var(nc_file_i500x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i500x2')

mod_chla_600x2 <- get_var(nc_file_i600x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i600x2')

mod_chla_700x2 <- get_var(nc_file_i700x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i700x2')

mod_chla_800x2 <- get_var(nc_file_i800x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i800x2')

bound_chla <- rbind(mod_chla_unaltered, mod_chla_500x2, mod_chla_600x2, mod_chla_700x2, mod_chla_800x2)
bound_chla <- select(bound_chla, Depth, depth_avg, id)
unique(bound_chla)

bound_chla_surf <- bound_chla %>% 
  group_by(id) %>% 
  mutate(chla_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, chla_avg) 

bound_chla_surf <- unique(bound_chla_surf)

bound_chla_surf$var_type <- as.factor("chla")

ggplot(data = bound_chla_surf, aes(x = var_type, y = chla_avg, col = as.factor(id))) + geom_point()


bound_chla <- rbind(mod_chla_unaltered, mod_chla_500x2, mod_chla_600x2, mod_chla_700x2, mod_chla_800x2)
bound_chla <- select(bound_chla, Depth, depth_avg, id)
unique(bound_chla)

bound_chla_surf <- bound_chla %>% 
  group_by(id) %>% 
  mutate(chla_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, chla_avg) 

bound_chla_surf <- unique(bound_chla_surf)

bound_chla_surf$var_type <- as.factor("chla")

ggplot(data = bound_chla_surf, aes(x = var_type, y = chla_avg, col = as.factor(id))) + geom_point()


bound_chla <- rbind(mod_chla_unaltered, mod_chla_500x2, mod_chla_600x2, mod_chla_700x2, mod_chla_800x2)
bound_chla_depth <- select(bound_chla, Depth, depth_avg, id)
bound_chla_date <- select(bound_chla, DateTime, date_avg, id)
unique(bound_chla_depth)
bound_chla_date <- unique(bound_chla_date)

# bound_oxy_date <- bound_oxy_date %>% 
#   mutate(month = month(DateTime)) %>% 
#   mutate(year = year(DateTime)) %>% 
#   group_by(month, year, id) %>% 
#   mutate(month_avg = mean(date_avg, na.rm = TRUE))

bound_chla_date <- select(bound_chla_date, DateTime, date_avg, id)

chla_date_wide <- dcast(bound_chla_date, DateTime~id, value.var = "date_avg")
chla_date_wide$month <- month(chla_date_wide$DateTime)


ggplot(subset(chla_date_wide), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
  geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
  geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
  geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
  ylab("Total chla") + 
  ggtitle("Surface Chlorophyll-a")









# surface TP 


depths = c(0.1)
var="TOT_tp"
mod_tp_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_tp_500x2 <- get_var(nc_file_i500x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i500x2')

mod_tp_600x2 <- get_var(nc_file_i600x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i600x2')

mod_tp_700x2 <- get_var(nc_file_i700x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i700x2')

mod_tp_800x2 <- get_var(nc_file_i800x2, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i800x2')

bound_tp <- rbind(mod_tp_unaltered, mod_tp_500x2, mod_tp_600x2, mod_tp_700x2, mod_tp_800x2)
bound_tp <- select(bound_tp, Depth, depth_avg, id)
unique(bound_tp)

bound_tp_surf <- bound_tp %>% 
  group_by(id) %>% 
  mutate(tp_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, tp_avg) 

bound_tp_surf <- unique(bound_tp_surf)

bound_tp_surf$var_type <- as.factor("tp")

ggplot(data = bound_tp_surf, aes(x = var_type, y = tp_avg, col = as.factor(id))) + geom_point()


bound_tp <- rbind(mod_tp_unaltered, mod_tp_500x2, mod_tp_600x2, mod_tp_700x2, mod_tp_800x2)
bound_tp_depth <- select(bound_tp, Depth, depth_avg, id)
bound_tp_date <- select(bound_tp, DateTime, date_avg, id)
unique(bound_tp_depth)
bound_tp_date <- unique(bound_tp_date)

# bound_oxy_date <- bound_oxy_date %>% 
#   mutate(month = month(DateTime)) %>% 
#   mutate(year = year(DateTime)) %>% 
#   group_by(month, year, id) %>% 
#   mutate(month_avg = mean(date_avg, na.rm = TRUE))

bound_tp_date <- select(bound_tp_date, DateTime, date_avg, id)

tp_date_wide <- dcast(bound_tp_date, DateTime~id, value.var = "date_avg")
tp_date_wide$month <- month(tp_date_wide$DateTime)


ggplot(subset(tp_date_wide), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
  geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
  geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
  geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
  ylab("Total TP") + 
  ggtitle("Surface Total Phosphorus")





