#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2019.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.0.4, RStudio 1.2.052 *
#* DATE:    16Jun2019                                            *
#* MODIFIED: Feb 2021
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2019 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

source('library_func_lists.R')

#bring in  buoy raw data
buoy2019_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2019 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')
buoy2019_L0

#### format data ####
buoy2019_L0 <- buoy2019_L0  %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rownames_to_column(var ='rowid')
head(buoy2019_L0)

#look for DST artifacts at 2am
buoy2019_L0[buoy2019_L0$date == '2019-03-10',]$datetime
buoy2019_L0[buoy2019_L0$date == '2019-11-03',]$datetime
#looks good!

#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2019_L0 %>% 
  group_by(date) %>% 
  summarize(n = length(datetime))

#looks like there are 6 fewer instances of datetime in 3/10 and 6 additional on 11/4
buoy2019_L0[buoy2019_L0$date == '2019-03-10',]$datetime
#data gap on 3/10 is actually at 23:00-00:00
buoy2019_L0[buoy2019_L0$date == '2019-11-04',]$datetime
#data repeat on 11/4 is at 00:00-01:00

# does observe DST, but at the wrong times. pull out by rowid, force time change for middle section.
buoy2019_L0 <- buoy2019_L0 %>% 
  mutate(instrument_datetime = datetime) 
beginning <- buoy2019_L0 %>% 
  filter(datetime < as.POSIXct('2019-03-10 23:00', tz='UTC')) #in EST
middle <- buoy2019_L0 %>% 
  filter(datetime >= as.POSIXct('2019-03-10 23:00', tz='UTC') & rowid <44196) #in EDT
end <- buoy2019_L0 %>% 
  filter(rowid >= 44196) #in EST
middle <- middle %>% 
  mutate(datetime = instrument_datetime - hours(1)) #get rid of DST
#rejoin all data
buoy2019_L1 <- full_join(beginning, middle) %>% 
  full_join(., end) %>% 
  arrange(datetime)

# add in all date time stamps in L1 data set
alltimes_2019 <- as.data.frame(seq.POSIXt(as.POSIXct('2019-01-01 00:00', tz='UTC'), as.POSIXct('2019-12-31 23:50', tz='UTC'), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2019_L1 <- buoy2019_L1 %>%
  right_join(., alltimes_2019) %>%
  arrange(datetime)
#add flag for missing data from buoy
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(buoyoffline = case_when(is.na(rowid) ~ 'T',
                                 TRUE ~ 'F')) %>% 
  select(-rowid, -index) %>% 
  mutate(location = case_when(buoyoffline == 'T' ~ 'offline',
                              TRUE ~ NA_character_))

#clean up workspace
rm(alltimes_2019, beginning, middle, end, datetimetable)


####THERMISTORS####
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot to see
ggplot(buoy2019_therm_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#recode NA strings of -6999
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot again
ggplot(buoy2019_therm_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#plot together to see relative values
ggplot(buoy2019_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode TempC_0 to NA_real_
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(TempC_0m = NA_real_)
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot together to see relative values, without 0m
ggplot(buoy2019_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode data prior to May 1 (predeployment testing)
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime < as.POSIXct('2019-05-01', tz='UTC') ~ NA_real_,
                        TRUE ~ .))
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot together to see relative values, without pre-deployment blip
ggplot(buoy2019_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# plot by 2-week period, save plot.

# set time period of interest:
start_date = '2019-05-15'
end_date = '2019-11-15'

#create a list of weeks during time period of interest
weekly_2019 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') 

#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(alltemp2011)) %>%
    gather(variable, value, -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = 'water temperature (degrees C)',
         title = paste0('LSPA thermistors ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_therm_weekly_', weekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
}

#data look great - need to check deployment May 23 and removal, Nov 5

#buoy deployment
deployment = '2019-05-23'
ggplot(subset(buoy2019_therm_vert,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just add location information
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-05-23 09:30', tz='UTC') ~ 'loon',
                              datetime <as.POSIXct('2019-05-23 09:30', tz='UTC') & buoyoffline == 'F' ~ 'harbor',
                              TRUE ~ location))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

#buoy removal 
removal = '2019-11-05'
ggplot(subset(buoy2019_therm_vert,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode data after 10:30 (inclusive), update location to in-transit. loon will be added by do string
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz='UTC') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz='UTC') ~ 'in transit',
                              TRUE ~ location))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2019_therm_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(buoy2019_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct column names for sensor offset
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(TempC_9p75m = 'TempC_9m',
         TempC_8p75m = 'TempC_8m',
         TempC_7p75m = 'TempC_7m',
         TempC_6p75m = 'TempC_6m',
         TempC_5p75m = 'TempC_5m',
         TempC_4p75m = 'TempC_4m',
         TempC_3p75m = 'TempC_3m',
         TempC_2p75m = 'TempC_2m',
         TempC_1p75m = 'TempC_1m',
         TempC_0p75m = 'TempC_0m') 

#clean up workspace
rm(buoy2019_therm_vert, buoy2019_therm_vert_L1)

#### DO ####
buoy2019_do_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(buoy2019_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA values and data prior to buoy move to loon as well as those data after the move from loon back to harbor in dec
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            funs(case_when(. == -6999 ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) 

buoy2019_do_vert_L1 <- buoy2019_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(buoy2019_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#look at the DO data on a weekly basis
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(upDO), all_of(lowDO)) %>%
    gather(variable, value, -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA DO sensors ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_do_weekly_', weekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
}

# May 23
#buoy deployment 
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded
buoy2019_L1 <- buoy2019_L1 %>% 
mutate_at(vars(all_of(lowDO), all_of(upDO)),
          funs(case_when(datetime == as.POSIXct('2019-05-23 9:30', tz='UTC') ~ NA_real_,
                         TRUE ~ .))) %>% 
  mutate(upper_do_flag= case_when(datetime == as.POSIXct('2019-05-23 9:30', tz='UTC') ~ 'c',
                        TRUE ~ NA_character_),
          lower_do_flag= case_when(datetime == as.POSIXct('2019-05-23 9:30', tz='UTC') ~ 'c',
                                   TRUE ~ NA_character_))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# Jun 17
date_look = '2019-06-17'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime == as.POSIXct('2019-06-17 9:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#Jul13
date_look = '2019-07-13'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime == as.POSIXct('2019-07-13 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#Jul24pm
date_look = '2019-07-24'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(paste(date_look, '12:00', sep = ' '), tz='UTC') & datetime < (as.POSIXct(paste(date_look, '12:00', sep = ' '), tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#This actually looks okay, eve if a little odd. Leaving as-is.

#aug 15
date_look = '2019-08-15'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just two rows need to be recoded
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime >= as.POSIXct('2019-08-15 19:20', tz='UTC') & 
                             datetime < as.POSIXct('2019-08-15 19:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#aug19
date_look = '2019-08-19'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime == as.POSIXct('2019-08-19 8:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#sep24
date_look = '2019-09-24'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime == as.POSIXct('2019-09-24 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#oct11
date_look = '2019-10-11'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            funs(case_when(datetime == as.POSIXct('2019-10-11 08:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#nov 5
date_look = '2019-11-05'
ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO), all_of(upDO)),
            funs(case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz='UTC')  ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2019_do_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme




ggplot(buoy2019_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#low do seems really high
#plot do ppm together
docheckppm <- buoy2019_L1 %>% 
  select(datetime, DOLowPPM, DOppm) %>% 
  gather(variable, value, -datetime)

ggplot(docheckppm, aes(x = datetime, y = value, color =variable)) +
  geom_point()

#plot do sat together
dochecksat <- buoy2019_L1 %>% 
  select(datetime, DOLowSat, DOSat) %>% 
  gather(variable, value, -datetime)

ggplot(dochecksat, aes(x = datetime, y = value, color =variable)) +
  geom_point()

#clean up workspace
rm(buoy2020_do_vert, buoy2020_do_vert_L1, docheckppm, dochecksat)

####CHLA####
buoy2019_chla_vert <- buoy2019_L1 %>%
  select(datetime, all_of(chla)) %>%
  gather(variable, value, -datetime)

ggplot(buoy2019_chla_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#chla sensor not working
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(Chlor_RFU = NA_real_,
         Chlor_UGL = NA_real_,
         SpecCond = NA_real_)

#clean up workspace
rm(buoy2019_chla_vert)

####wind####
buoy_wind_vert <- buoy2019_L1 %>%
  select(datetime, all_of(wind)) %>%
  gather(variable, value, -datetime)

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2019, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# set time period of interest:
start_date = '2019-01-01'
end_date = '2020-01-01'

#create a list of weeks during time period of interest
weekly_2019 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  filter(date< as.Date('2019-12-31')) %>% 
  add_row(date = as.Date('2020-01-01'))

#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(wind)) %>%
    gather(variable, value, -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA wind variables ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_wind_weekly_', weekly_2019$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#sensor frozen jan 9-14
date_look_start <- '2019-01-09'
date_look_end <- '2019-01-14'

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(date_look_start, tz='UTC') & datetime < (as.POSIXct(date_look_start, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= (as.POSIXct(date_look_end, tz='UTC') - days(1)) & datetime < as.POSIXct(date_look_end, tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(datetime >= as.POSIXct('2019-01-09 5:10', tz='UTC') & datetime < as.POSIXct('2019-01-13 12:30', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(date_look_start, tz='UTC') & datetime < (as.POSIXct(date_look_end, tz='UTC')))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()



#Feb 29
date_look <- '2019-02-28'

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(datetime >= as.POSIXct('2019-02-28 4:30', tz='UTC') & datetime < as.POSIXct('2019-02-28 10:20', tz='UTC') & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()


#mar04
date_look <- '2019-03-04'

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(datetime >= as.POSIXct('2019-03-04 5:20', tz='UTC') & datetime < as.POSIXct('2019-03-04 9:00', tz='UTC') & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(date_look, tz='UTC') & datetime < (as.POSIXct(date_look, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

# buoy deployment
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2019-05-23 7:20', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2019-05-23 7:20', tz='UTC') & datetime < as.POSIXct('2019-05-23 9:30', tz='UTC') ~ 'in transit',
                              TRUE ~ location)) 
buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#removal
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz='UTC') & datetime < as.POSIXct('2019-11-05 12:00', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2019-11-05 12:00', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') +days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()


#Dec 30-31
date_look_start <- '2019-12-30'
date_look_end <- '2020-01-01'

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(date_look_start, tz='UTC') & datetime < (as.POSIXct(date_look_start, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= (as.POSIXct(date_look_end, tz='UTC') - days(1)) & datetime < as.POSIXct(date_look_end, tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(datetime >= as.POSIXct('2019-12-30 06:00', tz='UTC') & datetime < as.POSIXct('2019-12-31 08:00', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(date_look_start, tz='UTC') & datetime < (as.POSIXct(date_look_end, tz='UTC')))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2019, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

#Clean up workspace
rm(buoy_wind_vert, buoy_wind_vert_L1)

# ###PAR####
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, PAR) 
  ggplot(chunk_vert, aes(x = datetime, y = PAR)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA PAR ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_par_weekly_', weekly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}


#recode when in transit or offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                         TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 

#add flag for night time issues; par likely obscured jan 9-20, add flag
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(PAR_flag = case_when(is.na(PAR_flag) ~ 'n',
                              !is.na(PAR_flag) ~ paste('n', PAR_flag, sep = ', ')),
         PAR_flag = case_when(is.na(PAR_flag) & datetime >= as.POSIXct('2019-01-09', tz='UTC') &
                                datetime < as.POSIXct('2019-01-20', tz='UTC') ~ 'o',
                              !is.na(PAR_flag) & datetime >= as.POSIXct('2019-01-09', tz='UTC') &
                                datetime < as.POSIXct('2019-01-20', tz='UTC') ~ paste('o', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))
unique(buoy2019_L1$PAR_flag)



ggplot(buoy2019_L1,
       aes(x=datetime, y=PAR, color=location, shape = PAR_flag)) +
  geom_point() +
  labs(title = 'PAR 2019, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()



#### Air temp ####

for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, AirTempC) 
  ggplot(chunk_vert, aes(x = datetime, y = AirTempC)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA Air Temperature ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_airtemp_weekly_', weekly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#look good

#recode when in transit or offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

#### export graphs for all L1 data week by week with location info ####

for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(alltemp2011), location) %>%
    gather(variable, value, -datetime, -location)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable, shape = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = 'water temperature (degrees C)',
         title = paste0('LSPA thermistors ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L1_therm_weekly_', weekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
}

#look at the DO data on a weekly basis
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(upDO), all_of(lowDO), location) %>%
    gather(variable, value, -datetime, -location)
  ggplot(chunk_vert, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA DO sensors ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L1_do_weekly_', weekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
}

#plot wind
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(wind), location) %>%
    gather(variable, value, -datetime, -location)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA wind variables ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L1_wind_weekly_', weekly_2019$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#par
for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, PAR) 
  ggplot(chunk_vert, aes(x = datetime, y = PAR, color = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA PAR ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L1_par_weekly_', weekly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

for(i in 1:(nrow(weekly_2019)-1)) {
  chunk <- buoy2019_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2019$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2019$date[i+1], tz='UTC'))
  chunk_vert <- chunk %>% 
    select(datetime, AirTempC) 
  ggplot(chunk_vert, aes(x = datetime, y = AirTempC, color = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA Air Temperature ', weekly_2019$date[i], ' - ', weekly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L1_airtemp_weekly_', weekly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#mark when buoy offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(buoyoffline == 'T' ~ 'offline',
                              TRUE ~ location))
unique(buoy2019_L1$location)

#### EXPORT L1 DATA STREAMS ####
colnames(buoy2019_L1)

#export L1 tempstring file
buoy2019_L1 %>%
  select(datetime, TempC_0p75m:TempC_9p75m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2019_tempstring_L1_corrdepths.csv')

#export l1 do file
buoy2019_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2019_do_L1.csv')

#export l1 par file
buoy2019_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_PAR_L1.csv')

#export l1 wind
buoy2019_L1 %>%
  select(datetime, all_of(wind), location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_wind_L1.csv')

#export l1 air temp file
buoy2019_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_airtemp_L1.csv')


