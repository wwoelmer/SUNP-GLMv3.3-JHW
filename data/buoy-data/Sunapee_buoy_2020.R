#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2020.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.0.3, RStudio 1.1.383 *
#* DATE:    16Jun2020                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2020 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

source('library_func_lists.R')

#bring in  buoy raw data
buoy2020_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2020 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')
buoy2020_L0

#### format data ####
buoy2020_L0 <- buoy2020_L0  %>%
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
head(buoy2020_L0)

#look for DST artifacts at 2am
buoy2020_L0[buoy2020_L0$date == '2020-03-08',]$datetime
buoy2020_L0[buoy2020_L0$date == '2020-11-01',]$datetime
#looks good!

#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2020_L0 %>% 
  group_by(date) %>% 
  summarize(n = length(datetime))

#looks like there are 6 fewer instances of datetime in 3/08 and 6 additional on 11/02
buoy2020_L0[buoy2020_L0$date == '2020-03-08',]$datetime
#data gap on 3/10 is actually at 23:00-00:00
buoy2020_L0[buoy2020_L0$date == '2020-11-02',]$datetime
#data repeat on 11/02 is at 00:00-01:00

# does observe DST, but at the wrong times. pull out by rowid, force time change for middle section.
buoy2020_L0 <- buoy2020_L0 %>% 
  mutate(instrument_datetime = datetime) 
beginning <- buoy2020_L0 %>% 
  filter(datetime < as.POSIXct('2020-03-08 23:00', tz='UTC')) #in EST
middle <- buoy2020_L0 %>% 
  filter(datetime >= as.POSIXct('2020-03-08 23:00', tz='UTC') & rowid <43532) #in EDT; this is the row id that is the second occurrance of 0:00-1:00
end <- buoy2020_L0 %>% 
  filter(rowid >= 43532) #in EST
middle <- middle %>% 
  mutate(datetime = instrument_datetime - hours(1)) #get rid of DST
#rejoin all data
buoy2020_L1 <- full_join(beginning, middle) %>% 
  full_join(., end) %>% 
  arrange(datetime)

# add in all date time stamps in L1 data set
alltimes_2020 <- as.data.frame(seq.POSIXt(as.POSIXct('2020-01-01 00:00', tz='UTC'), as.POSIXct('2020-12-31 23:50', tz='UTC'), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2020_L1 <- buoy2020_L1 %>%
  right_join(., alltimes_2020) %>%
  arrange(datetime)
#add flag for missing data from buoy
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(buoyoffline = case_when(is.na(rowid) ~ 'T',
                                 TRUE ~ 'F')) %>% 
  select(-rowid, -index) %>% 
  mutate(location = case_when(buoyoffline == 'T' ~ 'offline',
                              TRUE ~ NA_character_))

#clean up workspace
rm(alltimes_2020, beginning, middle, end, datetimetable)

#### look at battery ####
ggplot(buoy2020_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2020_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()


####THERMISTORS####
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot to see
ggplot(buoy2020_therm_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#recode NA strings of -6999
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~case_when(. < -5000 ~ NA_real_, #note there were some oddball NA values this year, had to change to accomodate that. 
                           TRUE ~ .))
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot again
ggplot(buoy2020_therm_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#plot together to see relative values
ggplot(buoy2020_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode TempC_0 to NA_real_; still not recording correctly until Oct 8, when a single temp sensor and do installed for harbor.
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(TempC_0m = case_when(datetime < as.Date('2020-10-08') ~ NA_real_,
                              TRUE ~ TempC_0m))
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot together to see relative values, without 0m
ggplot(buoy2020_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#looks better. 

# sensor testing april 30;; deployed 3 May
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime < as.POSIXct('2020-05-03', tz='UTC') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(TempC_0m = case_when(TempC_0m>200 ~ NA_real_, #one wonky value that messes with visualization.
                              TRUE ~ TempC_0m))
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot together to see relative values, without pre-deployment blip and high Temp 0m value at end
ggplot(buoy2020_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# plot by 2-week period, save plot.

# set time period of interest:
start_date = '2020-01-01'
end_date = '2021-01-01'

#create a list of weeks during time period of interest
weekly_2020 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  filter(date< as.Date('2020-12-30')) %>% 
  add_row(date = as.Date('2021-01-01'))

#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA thermistors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_therm_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#buoy deployment
deployment = '2020-05-03'
ggplot(subset(buoy2020_therm_vert,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# recode data prior to loon; add location information
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2020-05-03 10:20', tz='UTC') ~ 'loon',
                              datetime < as.POSIXct('2020-05-03 10:20', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(location == 'harbor' ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#Jun 2 - Jun 3
look_date_start = '2020-06-02'
look_date_end = '2020-06-03'

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# recode data prior to loon; add location information
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.Date(look_date_start) & datetime < as.POSIXct(paste(look_date_end, '10:30', sep = ' '), tz = 'UTC') ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# May 30 stray temp reading
look_date = '2020-05-30'
ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.Date(look_date) & datetime < (as.POSIXct(paste(look_date, '00:00'), tz = 'UTC') + days(1)) ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#buoy removal 
removal = '2020-10-05'
ggplot(subset(buoy2020_therm_vert,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode data after 10:30 (inclusive), update location to in-transit. loon will be added by do string
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz='UTC') & datetime < as.Date('2020-10-08') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz='UTC') ~ 'in transit',
                              TRUE ~ location))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# install of 1m temp 
winter_temp_add = '2020-10-08'
ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(winter_temp_add, tz='UTC') & datetime < (as.POSIXct(winter_temp_add, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz='UTC') & datetime < as.POSIXct('2020-10-08 10:50', tz= 'UTC') ~ NA_real_,
                        TRUE ~ .)) 
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2020_therm_vert_L1,
              subset=(datetime >= as.POSIXct(winter_temp_add, tz='UTC') & datetime < (as.POSIXct(winter_temp_add, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme


ggplot(buoy2020_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA thermistors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_therm_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#correct column names for sensor offset
buoy2020_L1 <- buoy2020_L1 %>% 
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
rm(buoy2020_therm_vert, buoy2020_therm_vert_L1)

#### DO ####
buoy2020_do_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2020_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#recode NA values 
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) 

buoy2020_do_vert_L1 <- buoy2020_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#look at the DO data on a weekly basis
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA DO sensors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_do_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#april 30, upper do/lower DO in water, lower has issues
look_date = '2020-04-30'
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme
# hard to tell if the do is in water (by data and operation log), need to recode everything prior to deployment at loon.

#buoy deployment 
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#recode prior to deployment - allow for 3 hours of equilibration; recode all of upDO until Aug 6
buoy2020_L1 <- buoy2020_L1 %>% 
mutate_at(vars(all_of(lowDO), all_of(upDO)),
          ~ case_when(datetime < (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz='UTC') + hours(3)) ~ NA_real_,
                         TRUE ~ .)) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime < as.Date('2020-08-06') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(upper_do_flag = case_when(datetime == (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz='UTC') + hours(3)) ~ 'c',
                                   TRUE ~ NA_character_),
         lower_do_flag = case_when(datetime == (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz='UTC') + hours(3)) ~ 'c',
                                   TRUE ~ NA_character_))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#may 24
look_date = '2020-05-24'
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '11:00', sep = ' '), tz= 'UTC') &
                          datetime < as.POSIXct(paste(look_date, '19:50', sep = ' '), tz= 'UTC') ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#june 5
look_date = '2020-06-05'
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '11:00', sep = ' '), tz= 'UTC') &
                          datetime < as.POSIXct(paste(look_date, '12:00', sep = ' '), tz= 'UTC') ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#august 6
look_date = '2020-08-06'
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime == as.POSIXct(paste(look_date, '11:30', sep = ' '), tz= 'UTC') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime < as.POSIXct(paste(look_date, '12:20', sep = ' '), tz='UTC') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(lower_do_flag = case_when(datetime < as.POSIXct(paste(look_date, '12:20', sep = ' '), tz='UTC') ~ 'cp',
                                   TRUE ~ lower_do_flag))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#note that saturation is REALLY high for the low DO here.

#aug 12
look_date = '2020-08-12'
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '4:00', sep = ' '), tz= 'UTC') &
                          datetime < as.POSIXct(paste(look_date, '5:00', sep = ' '), tz= 'UTC') ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#aug 19-aug 20 upper do has a big jump at night
look_date_start = '2020-08-19'
look_date_end = '2020-08-20'

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date_start, '21:30', sep = ' '), tz= 'UTC') &
                          datetime < as.POSIXct(paste(look_date_end, '12:00', sep = ' '), tz= 'UTC') ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# removal
ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~ case_when(datetime >= as.POSIXct(paste(removal, '09:30', sep = ' '), tz= 'UTC') &
                          datetime < as.Date(winter_temp_add) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# winter upper oct 8

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(winter_temp_add, tz='UTC') & datetime < (as.POSIXct(winter_temp_add, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~ case_when(datetime >= as.POSIXct(paste(winter_temp_add, '00:00', sep = ' '), tz= 'UTC') &
                          datetime < as.POSIXct(paste(winter_temp_add, '10:50', sep = ' '), tz='UTC') ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(winter_temp_add, tz='UTC') & datetime < (as.POSIXct(winter_temp_add, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#plot whole year to see if there are overarching issues
ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#upper do saturation/ppm is wrton from early/mid aug through mid/late aug
look_date_start = '2020-08-12'
look_date_end = '2020-08-21'

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_start, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date_start, '5:00', sep = ' '), tz= 'UTC') &
                          datetime < as.Date(look_date_end) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2020_do_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC') + days(1)))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#plot whole year to see if there are overarching issues
ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#print weekly L1
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA DO sensors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_do_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#clean up workspace
rm(buoy2020_do_vert, buoy2020_do_vert_L1)

####CHLA####
buoy2020_chla_vert <- buoy2020_L1 %>%
  select(datetime, all_of(chla)) %>%
  gather(variable, value, -datetime)

ggplot(buoy2020_chla_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#chla sensor not working
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(Chlor_RFU = NA_real_,
         Chlor_UGL = NA_real_,
         SpecCond = NA_real_)

#clean up workspace
rm(buoy2020_chla_vert)

####wind####
buoy_wind_vert <- buoy2020_L1 %>%
  select(datetime, all_of(wind)) %>%
  gather(variable, value, -datetime)

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2020, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()


#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA wind variables ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_wind_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#jan 7
look_date = '2020-01-07'
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '1:30', sep = ' '), tz='UTC') & 
                             datetime < as.POSIXct(paste(look_date, '3:20', sep = ' '), tz='UTC') & 
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC')+days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#jan 16
look_date = '2020-01-16'
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '5:50', sep = ' '), tz='UTC') & 
                           datetime < as.POSIXct(paste(look_date, '11:00', sep = ' '), tz='UTC') & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC')+days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#feb7-9
look_date_start = '2020-02-07'
look_date_end = '2020-02-09'
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_start, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= (as.POSIXct(look_date_end, tz='UTC')) & datetime < (as.POSIXct(look_date_end, tz='UTC')+ days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date_start, '4:40', sep = ' '), tz='UTC') & 
                           datetime < as.POSIXct(paste(look_date_end, '9:40', sep = ' '), tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC')))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

#feb 13
look_date = '2020-02-13'
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '6:00', sep = ' '), tz='UTC') & 
                           datetime < as.POSIXct(paste(look_date, '12:40', sep = ' '), tz='UTC') & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC')+days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()


#mar 19
look_date = '2020-03-19'
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC') + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '7:00', sep = ' '), tz='UTC') & 
                           datetime < as.POSIXct(paste(look_date, '9:00', sep = ' '), tz='UTC') & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz='UTC') & datetime < (as.POSIXct(look_date, tz='UTC')+days(1)))),
       aes(x=datetime, y=value, color =location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

# buoy deployment
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') +days(1)))),
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct(paste(deployment, '8:00', sep = ' '), tz='UTC') & 
                                datetime < as.POSIXct(paste(deployment, '9:20', sep = ' '), tz='UTC') ~ 'in transit',
                              TRUE ~ location)) 
buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(deployment, tz='UTC') & datetime < (as.POSIXct(deployment, tz='UTC') +days(1)))),
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#removal
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') +days(1)))),
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct(paste(removal, '9:00', sep = ' '), tz='UTC') & 
                                datetime < as.POSIXct(paste(removal, '13:00', sep = ' '), tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct(paste(removal, '13:00', sep = ' '), tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct(removal, tz='UTC') & datetime < (as.POSIXct(removal, tz='UTC') +days(1)))),
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA wind variables ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_wind_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#Clean up workspace
rm(buoy_wind_vert, buoy_wind_vert_L1)

# ###PAR####
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA PAR ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_par_weekly_', weekly_2020$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#nothing egregious week-by-week, but night par is still occuring all year, especially after low par days

#recode when in transit, recode negative par to 0; add flag for night time par values
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                         TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) %>% 
  mutate(PAR_flag = case_when(is.na(PAR_flag) ~ 'n',
                              !is.na(PAR_flag) ~ paste('n', PAR_flag, sep = ', ')))

#look at whole year to see if par obscured at any time by snow
ggplot(buoy2020_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme

#look at Jan and Feb
look_date_start = '2020-01-01'
look_date_end = '2020-02-01'

ggplot(subset(buoy2020_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC')))),
       aes(x=datetime, y=PAR)) +
  geom_point() +
  labs(title = 'jan par 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

look_date_start = '2020-02-01'
look_date_end = '2020-03-01'

ggplot(subset(buoy2020_L1,
              subset=(datetime >= as.POSIXct(look_date_start, tz='UTC') & datetime < (as.POSIXct(look_date_end, tz='UTC')))),
       aes(x=datetime, y=PAR)) +
  geom_point() +
  labs(title = 'feb par 2020, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()


#par likely obscured feb 06-11 and 13 add flag
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(PAR_flag = case_when(is.na(PAR_flag) & datetime >= as.POSIXct('2020-02-06', tz='UTC') &
                                datetime < as.POSIXct('2020-02-12', tz='UTC') ~ 'o',
                              !is.na(PAR_flag) & datetime >= as.POSIXct('2020-02-06', tz='UTC') &
                                datetime < as.POSIXct('2020-02-12', tz='UTC') ~ paste('o', PAR_flag, sep = ', '),
                              is.na(PAR_flag) & datetime >= as.POSIXct('2020-02-12', tz='UTC') &
                                datetime < as.POSIXct('2020-02-13', tz='UTC') ~ 'o',
                              !is.na(PAR_flag) & datetime >= as.POSIXct('2020-02-12', tz='UTC') &
                                datetime < as.POSIXct('2020-02-13', tz='UTC') ~ paste('o', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))
unique(buoy2020_L1$PAR_flag)



ggplot(buoy2020_L1,
       aes(x=datetime, y=PAR, color=location, shape = PAR_flag)) +
  geom_point() +
  labs(title = 'PAR 2020, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()


#### Air temp ####

for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= 'UTC') &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz='UTC'))
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
         title = paste0('LSPA Air Temperature ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_airtemp_weekly_', weekly_2020$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#look good

#mark when buoy offline
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(buoyoffline == 'T' ~ 'offline',
                              TRUE ~ location))


#recode when in transit or offline
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

unique(buoy2020_L1$location)

#### EXPORT L1 DATA STREAMS ####
colnames(buoy2020_L1)

#export L1 tempstring file
buoy2020_L1 %>%
  select(datetime, TempC_0p75m:TempC_9p75m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2020_tempstring_L1_corrdepths.csv')

#export l1 do file
buoy2020_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2020_do_L1.csv')

#export l1 par file
buoy2020_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2020_PAR_L1.csv')

#export l1 wind
buoy2020_L1 %>%
  select(datetime, all_of(wind), location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2020_wind_L1.csv')

#export l1 air temp file
buoy2020_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2020_airtemp_L1.csv')


