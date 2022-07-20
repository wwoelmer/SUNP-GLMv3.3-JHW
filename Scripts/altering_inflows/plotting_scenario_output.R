pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools, ggplot2, reshape2)

# take normal output to create visualizations
# focusing on hypolimnetic DO, surface chl, surface TP
# visualizations are for LSPA

setwd("~/Dropbox/SUNP-GLMv3.3-JHW/")

sim_folder <- getwd()

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))


# read unaltered nc file as well as altered nc files 
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 
nc_file_i505 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i505.nc')
nc_file_i510 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i510.nc')
nc_file_i540 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i540.nc')
nc_file_i665 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i665.nc')
nc_file_i760 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i760.nc')
nc_file_i788 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i788.nc')
nc_file_i790 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i790.nc')
nc_file_i800 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i800.nc')
nc_file_i805 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i805.nc')
nc_file_i830 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i830.nc')
nc_file_i835 <- file.path('/Volumes/G-DRIVE SSD/scenario_output/output_i835.nc')



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
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_oxy_505 <- get_var(nc_file_i505, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i505')

 mod_oxy_510 <- get_var(nc_file_i510, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i510')

mod_oxy_540 <- get_var(nc_file_i540, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i540')

mod_oxy_665 <- get_var(nc_file_i665, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i665')

mod_oxy_760 <- get_var(nc_file_i760, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i760')

mod_oxy_788 <- get_var(nc_file_i788, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i788')

mod_oxy_790 <- get_var(nc_file_i790, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i790')

mod_oxy_800 <- get_var(nc_file_i800, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i800')

mod_oxy_805 <- get_var(nc_file_i805, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i805')

mod_oxy_830 <- get_var(nc_file_i830, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i830')

mod_oxy_835 <- get_var(nc_file_i835, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(OXY_sat, na.rm = TRUE)) %>% 
  mutate(id = 'i835')



bound_oxy <- rbind(mod_oxy_unaltered, mod_oxy_505, mod_oxy_510, mod_oxy_540, mod_oxy_665, mod_oxy_760, mod_oxy_790, mod_oxy_788, mod_oxy_800, mod_oxy_805, mod_oxy_830, mod_oxy_835)
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

ggplot(subset(oxy_date_wide, DateTime >= "2015-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() + 
  geom_line(aes(x = DateTime, y = i505, col = "i505")) + 
  geom_line(aes(x = DateTime, y = i510, col = "i510")) + 
  geom_line(aes(x = DateTime, y = i540, col = "i540")) + 
  geom_line(aes(x = DateTime, y = i665, col = "i665")) + 
  geom_line(aes(x = DateTime, y = i760, col = "i760")) + 
  geom_line(aes(x = DateTime, y = i788, col = "i788")) + 
  geom_line(aes(x = DateTime, y = i790, col = "i790")) + 
  geom_line(aes(x = DateTime, y = i800, col = "i800")) + 
  geom_line(aes(x = DateTime, y = i805, col = "i805")) + 
  geom_line(aes(x = DateTime, y = i830, col = "i830")) + 
  geom_line(aes(x = DateTime, y = i835, col = "i835")) + 
  geom_line(aes(x = DateTime, y = i510, col = "i510")) + 
  ylab("% Saturation") + 
  ggtitle("Hypolimnetic Summer Oxygen Saturation") + 
  mytheme

ggplot(subset(bound_oxy, DateTime >= "2015-01-01"), aes(x=factor(id), y=date_avg, fill = id))+
  geom_boxplot() + 
  theme( legend.position = "none" ) +
  xlab("Site") + 
  ylab("% Saturation") + 
  ggtitle("Hypolimnetic Summer Oxygen Saturation") + 
  mytheme
  
# ggplot(subset(oxy_date_wide, month >= 1 & month <= 2), aes(x = DateTime, y = unaltered, col = "Reference")) +
#   geom_line() +
#   geom_line(aes(x = DateTime, y = i500x2, col = "i500x2")) + 
#   geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
#   geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
#   geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
#   ylab("% Saturation") + 
#   ggtitle("Winter Oxygen Saturation")
  

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

# ggplot(subset(mod_obs_ox, DateTime <= "2016-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
#   geom_line() +
#   geom_line(aes(x = DateTime, y = i500, col = "i500x2")) + 
#   geom_line(aes(x = DateTime, y = i600x2, col = "i600x2")) + 
#   geom_line(aes(x = DateTime, y = i700x2, col = "i700x2")) + 
#   geom_line(aes(x = DateTime, y = i800x2, col = "i800x2")) + 
#   geom_point(aes(x = DateTime, y = observed_do, col = "observed")) + 
#   ylab("% Saturation") + 
#   ggtitle("Year round oxygen saturation")

bound_oxy_hypo <- bound_oxy %>% 
  filter(DateTime >= "2010-01-01") %>% 
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
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_chla_505 <- get_var(nc_file_i505, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i505')

mod_chla_510 <- get_var(nc_file_i510, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i510')

mod_chla_540 <- get_var(nc_file_i540, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i540')

mod_chla_665 <- get_var(nc_file_i665, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i665')

mod_chla_760 <- get_var(nc_file_i760, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i760')

mod_chla_788 <- get_var(nc_file_i788, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i788')

mod_chla_790 <- get_var(nc_file_i790, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i790')

mod_chla_800 <- get_var(nc_file_i800, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i800')

mod_chla_805 <- get_var(nc_file_i805, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i805')

mod_chla_830 <- get_var(nc_file_i830, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i830')

mod_chla_835 <- get_var(nc_file_i835, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(PHY_tchla, na.rm = TRUE)) %>% 
  mutate(id = 'i835')





bound_chla <- rbind(mod_chla_unaltered, mod_chla_505, mod_chla_510, mod_chla_540, mod_chla_665, mod_chla_760, mod_chla_788, mod_chla_790, 
                    mod_chla_800, mod_chla_805, mod_chla_830, mod_chla_835)
bound_chla <- select(bound_chla, Depth, depth_avg, date_avg, id)
unique(bound_chla)

bound_chla_surf <- bound_chla %>% 
  group_by(id) %>% 
  mutate(chla_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, chla_avg) 

bound_chla_surf <- unique(bound_chla_surf)

bound_chla_surf$var_type <- as.factor("chla")

ggplot(data = bound_chla_surf, aes(x = var_type, y = chla_avg, col = as.factor(id))) + geom_point()


bound_chla <- select(bound_chla, Depth, depth_avg, id)
unique(bound_chla)

bound_chla_surf <- bound_chla %>%
  filter(DateTime >= "2010-01-01") %>% 
  group_by(id) %>% 
  mutate(chla_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, chla_avg) 

bound_chla_surf <- unique(bound_chla_surf)

bound_chla_surf$var_type <- as.factor("chla")

ggplot(data = bound_chla_surf, aes(x = var_type, y = chla_avg, col = as.factor(id))) + geom_point()


bound_chla <- rbind(mod_chla_unaltered, mod_chla_505, mod_chla_510, mod_chla_540, mod_chla_665, mod_chla_760, mod_chla_788, mod_chla_790, 
                    mod_chla_800, mod_chla_805, mod_chla_830, mod_chla_835)
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


ggplot(subset(chla_date_wide, DateTime >= "2015-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i505, col = "i505")) + 
  geom_line(aes(x = DateTime, y = i510, col = "i510")) + 
  geom_line(aes(x = DateTime, y = i540, col = "i540")) + 
  geom_line(aes(x = DateTime, y = i665, col = "i665")) + 
  geom_line(aes(x = DateTime, y = i760, col = "i760")) + 
  geom_line(aes(x = DateTime, y = i788, col = "i788")) + 
  geom_line(aes(x = DateTime, y = i790, col = "i790")) + 
  geom_line(aes(x = DateTime, y = i800, col = "i800")) + 
  geom_line(aes(x = DateTime, y = i805, col = "i805")) + 
  geom_line(aes(x = DateTime, y = i830, col = "i830")) + 
  geom_line(aes(x = DateTime, y = i835, col = "i835")) + 
  ylab("ug/L") + 
  ggtitle("Surface Chlorophyll-a") + 
  mytheme

ggplot(subset(bound_chla, DateTime >= "2015-01-01"), aes(x=factor(id), y=date_avg, fill = id))+
  geom_boxplot() + 
  theme( legend.position = "none" ) + 
  ylab("ug/L") + 
  xlab("Site") + 
  ggtitle("Surface Chlorophyll-a") + 
  mytheme









# surface TP 


depths = c(0.1)
var="TOT_tp"
mod_tp_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_tp_505 <- get_var(nc_file_i505, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i505')

mod_tp_510 <- get_var(nc_file_i510, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i510')

mod_tp_540 <- get_var(nc_file_i540, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i540')

mod_tp_665 <- get_var(nc_file_i665, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i665')

mod_tp_760 <- get_var(nc_file_i760, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i760')

mod_tp_788 <- get_var(nc_file_i788, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i788')

mod_tp_790 <- get_var(nc_file_i790, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i790')

mod_tp_800 <- get_var(nc_file_i800, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i800')

mod_tp_805 <- get_var(nc_file_i805, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i805')

mod_tp_830 <- get_var(nc_file_i830, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i830')

mod_tp_835 <- get_var(nc_file_i835, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tp, na.rm = TRUE)) %>% 
  mutate(id = 'i835')


bound_tp <- rbind(mod_tp_unaltered, mod_tp_505, mod_tp_510, mod_tp_540, mod_tp_665, mod_tp_760, mod_tp_788, mod_tp_790, mod_tp_800, mod_tp_805,
                  mod_tp_830, mod_tp_835)
bound_tp <- select(bound_tp, Depth, depth_avg, id)
unique(bound_tp)

bound_tp_surf <- bound_tp %>% 
  group_by(id) %>% 
  mutate(tp_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, tp_avg) 

bound_tp_surf <- unique(bound_tp_surf)

bound_tp_surf$var_type <- as.factor("tp")

ggplot(data = bound_tp_surf, aes(x = var_type, y = tp_avg, col = as.factor(id))) + geom_point()


bound_tp <- rbind(mod_tp_unaltered, mod_tp_505, mod_tp_510, mod_tp_540, mod_tp_665, mod_tp_760, mod_tp_788, mod_tp_790, mod_tp_800, mod_tp_805,
                  mod_tp_830, mod_tp_835)

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


ggplot(subset(tp_date_wide, DateTime >= "2015-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() +
  geom_line(aes(x = DateTime, y = i505, col = "i505")) + 
  geom_line(aes(x = DateTime, y = i510, col = "i510")) + 
  geom_line(aes(x = DateTime, y = i540, col = "i540")) + 
  geom_line(aes(x = DateTime, y = i665, col = "i665")) + 
  geom_line(aes(x = DateTime, y = i760, col = "i760")) + 
  geom_line(aes(x = DateTime, y = i788, col = "i788")) + 
  geom_line(aes(x = DateTime, y = i790, col = "i790")) + 
  geom_line(aes(x = DateTime, y = i800, col = "i800")) + 
  geom_line(aes(x = DateTime, y = i805, col = "i805")) + 
  geom_line(aes(x = DateTime, y = i830, col = "i830")) + 
  geom_line(aes(x = DateTime, y = i835, col = "i835")) + 
  ylab("mmol/m3") + 
  ggtitle("Surface Total Phosphorus") + 
  mytheme


ggplot(subset(bound_tp, DateTime >= "2015-01-01"), aes(x=factor(id), y=date_avg, fill = id))+
  geom_boxplot() + 
  theme( legend.position = "none" ) + 
  ylab("mmol/m3") + 
  xlab("Site") + 
  ggtitle("Surface Total Phosphorus") + 
  mytheme





# surface TN 


depths = c(0.1)
var="TOT_tn"
mod_tn_unaltered <- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'unaltered')

mod_tn_505 <- get_var(nc_file_i505, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i505')

mod_tn_510 <- get_var(nc_file_i510, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i510')

mod_tn_540 <- get_var(nc_file_i540, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i540')

mod_tn_665 <- get_var(nc_file_i665, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i665')

mod_tn_760 <- get_var(nc_file_i760, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i760')

mod_tn_788 <- get_var(nc_file_i788, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i788')

mod_tn_790 <- get_var(nc_file_i790, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i790')

mod_tn_800 <- get_var(nc_file_i800, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i800')

mod_tn_805 <- get_var(nc_file_i805, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i805')

mod_tn_830 <- get_var(nc_file_i830, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i830')

mod_tn_835 <- get_var(nc_file_i835, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(var), names_to="Depth", names_prefix=var, values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%  
  mutate(Depth = as.numeric(gsub('_', '', Depth))) %>% 
  group_by(Depth) %>% 
  mutate(depth_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  group_by(DateTime) %>% 
  mutate(date_avg = mean(TOT_tn, na.rm = TRUE)) %>% 
  mutate(id = 'i835')


bound_tn <- rbind(mod_tn_unaltered, mod_tn_505, mod_tn_510, mod_tn_540, mod_tn_665, mod_tn_760, mod_tn_788, mod_tn_790, mod_tn_800, mod_tn_805,
                  mod_tn_830, mod_tn_835)
bound_tn <- select(bound_tn, Depth, depth_avg, id)
unique(bound_tn)

bound_tn_surf <- bound_tn %>% 
  group_by(id) %>% 
  mutate(tn_avg = mean(depth_avg, na.rm = T)) %>% 
  select(id, tn_avg) 

bound_tn_surf <- unique(bound_tn_surf)

bound_tn_surf$var_type <- as.factor("tn")

ggplot(data = bound_tn_surf, aes(x = var_type, y = tn_avg, col = as.factor(id))) + geom_point()


bound_tn <- rbind(mod_tn_unaltered, mod_tn_505, mod_tn_510, mod_tn_540, mod_tn_665, mod_tn_760, mod_tn_788, mod_tn_790, mod_tn_800, mod_tn_805,
                  mod_tn_830, mod_tn_835)

bound_tn_depth <- select(bound_tn, Depth, depth_avg, id)
bound_tn_date <- select(bound_tn, DateTime, date_avg, id)
unique(bound_tn_depth)
bound_tn_date <- unique(bound_tn_date)

# bound_oxy_date <- bound_oxy_date %>% 
#   mutate(month = month(DateTime)) %>% 
#   mutate(year = year(DateTime)) %>% 
#   group_by(month, year, id) %>% 
#   mutate(month_avg = mean(date_avg, na.rm = TRUE))

bound_tn_date <- select(bound_tn_date, DateTime, date_avg, id)

tn_date_wide <- dcast(bound_tn_date, DateTime~id, value.var = "date_avg")
tn_date_wide$month <- month(tn_date_wide$DateTime)


ggplot(subset(tn_date_wide, DateTime >= "2015-01-01"), aes(x = DateTime, y = unaltered, col = "Reference")) +
  geom_line() + 
  geom_line(aes(x = DateTime, y = i505, col = "i505")) + 
  geom_line(aes(x = DateTime, y = i510, col = "i510")) + 
  geom_line(aes(x = DateTime, y = i540, col = "i540")) + 
  geom_line(aes(x = DateTime, y = i665, col = "i665")) + 
  geom_line(aes(x = DateTime, y = i760, col = "i760")) + 
  geom_line(aes(x = DateTime, y = i788, col = "i788")) + 
  geom_line(aes(x = DateTime, y = i790, col = "i790")) + 
  geom_line(aes(x = DateTime, y = i800, col = "i800")) + 
  geom_line(aes(x = DateTime, y = i805, col = "i805")) + 
  geom_line(aes(x = DateTime, y = i830, col = "i830")) + 
  geom_line(aes(x = DateTime, y = i835, col = "i835")) + 
  ylab("mmol/m3") + 
  ggtitle("Surface Total Nitrogen") + 
  mytheme


ggplot(subset(bound_tn, DateTime >= "2015-01-01"), aes(x=factor(id), y=date_avg, fill = id))+
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ylab("mmol/m3") + 
  xlab("Site") +
  ggtitle("Surface Total Nitrogen") + 
  mytheme




