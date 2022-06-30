# create file with daily temp observations at noon 

library(tidyverse)
library(lubridate)
sim_folder <- getwd()


# download data from EDI: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.499.2
data <-  "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6" 
destination <- paste0(getwd(), '/data/buoy-data') # some location on your computer
try(download.file(data,destfile = paste0(destination, '/data/buoy-data/2007-2020_buoy_templine_v26Feb2021.csv'),method="curl"))

field_temp_all <- file.path(paste0(sim_folder, '/data/buoy-data/2007-2020_buoy_templine_v26Feb2021.csv'))
field_all <- read.csv(field_temp_all)

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
field_format <- data.frame("DateTime" = as.Date(NA),
                           "Depth" = NA,
                           "Temp" = NA
                           )

depths <- c('0.5', '0.75', '0.85', '1.0', '1.5', '1.75', '1.85', '2.0', '2.5', '2.75',
            '2.85', '3.0', '3.5', '3.75', '3.85', '4.5', '4.75', '4.85', '5.5', '5.75', 
            '5.85', '6.5', '6.75', '6.85', '7.5', '7.75', '7.85', '8.5', '8.75', '8.85',
            '9.5', '9.75', '9.85', '10.5', '11.5', '13.5')

for (i in 1:length(depths)) {
  temp <- field_noon[,c(1, i+1)]
  temp$Depth <- depths[i]
  colnames(temp) <- c('DateTime', 'Temp', 'Depth')
  field_format <- full_join(temp, field_format)
}


# put depth as second column
field_format <- field_format %>% select( 'DateTime', 'Depth', 'Temp') %>% 
  arrange(DateTime, Depth)
field_format <- na.omit(field_format)

ggplot(data = field_format, aes(x = DateTime, y = temp)) +
  geom_point() +
  facet_wrap(~Depth)

write.csv(field_format, row.names = FALSE, './data/formatted-data/field_temp_noon_obs.csv')

# is there reason to combine the X.75/X.85 measurements to aggregate into one depth or are they ok separated as is?
