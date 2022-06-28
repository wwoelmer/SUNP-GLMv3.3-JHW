# create file with daily oxygen observations at noon 

library(tidyverse)
library(lubridate)
sim_folder <- getwd()


# download data from EDI: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.499.2
data <-  "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6" 
destination <- paste0(getwd(), '/data/buoy-data') # some location on your computer
try(download.file(data,destfile = paste0(destination, '/data/buoy-data/2007-2020_do_L1_v26Feb2021.csv'),method="curl"))

#field_temp_all <- file.path(paste0(sim_folder, '/data/buoy-data/2007-2020_do_L1_v26Feb2021.csv'))
#field_all <- read.csv(field_temp_all)

field_all <- read.csv("data/buoy-data/2007-2020_do_L1_v26Feb2021.csv")

# extract noon measurements only and only observations when buoy is deployed
field_all$datetime <- as.POSIXct(field_all$datetime, format = "%Y-%m-%d %H:%M:%S")
field_noon <- field_all %>% 
  mutate(day = day(datetime)) %>% 
  mutate(hour = hour(datetime)) %>% 
  mutate(minute = minute(datetime))
field_noon <- field_noon[field_noon$hour=='12' & field_noon$minute=='0',]
field_noon <- field_noon[field_noon$location=='loon',]  
field_noon <- field_noon %>% select(-location, -day, -minute, -hour)

# add depth column and remove from column name
# DO upper is at 1.5m
# DO lower is at 10.5m
# metadata on edi: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.499.2

depths <- c('1.5', '10.5')

oxy <- field_noon[,c(1, 2, 3, 5)]
oxy$Depth <- 1.5
colnames(oxy) <- c('DateTime', 'DOSat', "DOppm", 'Flag', 'Depth')

oxy_2 <- field_noon[,c(1, 6, 7, 9)]
oxy_2$Depth <- 10.5
colnames(oxy_2) <- c('DateTime', 'DOSat', "DOppm", 'Flag', 'Depth')

field_format <- full_join(oxy, oxy_2)


# put depth as second column
field_format <- field_format %>% select( 'DateTime', 'Depth', 'DOSat', 'DOppm', 'Flag')
field_format <- field_format %>% 
  arrange(DateTime, Depth)

# remove some data with flags
field_format <- field_format[field_format$Flag != 'm' & field_format$Flag !='mcep', ]
field_format <- na.omit(field_format)

ggplot(data = field_format, aes(x = DateTime, y = DOSat)) +
  geom_point(aes(col = as.factor(year(DateTime)))) +
  facet_wrap(~Depth)

ggplot(data = field_format, aes(x = DateTime, y = DOppm)) +
  geom_point(aes(col = as.factor(year(DateTime)))) +
  facet_wrap(~Depth)


manual <- read.csv("data/formatted-data/manual_oxy.csv")

manual$DateTime <- as.POSIXct(manual$DateTime, format = "%Y-%m-%d %H:%M:%S")

remove <- manual$DateTime

field_format <- field_format[!field_format$DateTime %in% remove,  ]

# combine the two datasets
oxy_data <- full_join(manual, field_format)
data_nodups <- oxy_data[!duplicated(oxy_data[,1:2]),]

ggplot(data = subset(data_nodups, DateTime >= "2015-01-01 00:00:00"), aes(x = DateTime, y = DOSat, col = as.factor(Depth)))+geom_point()



write.csv(data_nodups, row.names = FALSE, './data/formatted-data/manual_buoy_noon_obs.csv')

#write.csv(field_format, row.names = FALSE, './data/formatted-data/field_oxy_noon_obs.csv')


