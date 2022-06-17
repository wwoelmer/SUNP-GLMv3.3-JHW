#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2014.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    09Oct2017                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2014 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************


#bring in 2014 buoy raw data
buoy2014_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2014 Buoy Data.csv', 
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                     col_names = c('ArrayID', 'Year', 'Day', 'Hr.Min', 'DOTempC', 
                                   'DOSat', 'DOppm', 'TempC_0m', 'TempC_1m', 'TempC_2m',
                                   'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m',
                                   'TempC_8m', 'TempC_9m', 'TempC_10m', 'AirTempC', 'RH',
                                   'PAR', 'InstWindSp', 'InstWindDir', 'AveWindSp', 'AveWindDir', 
                                   'MaxWindSp', 'MaxWindDir', 'LoggerBatV', 'RadioBatV', 'IntLgBxTempC', 
                                   'Heading', 'DOLowTempC', 'DOLowSat', 'DOLowPPM'),
                     skip = 1) %>% 
  select(-ArrayID, -LoggerBatV, -RadioBatV, -IntLgBxTempC) %>% 
  rownames_to_column(var = 'rowid')


####format data####
# format date and time in buoy data
buoy2014_L0 <- buoy2014_L0 %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         minutes = replace(minutes, minutes=='0', '00'),
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%   #create datetime stamp must state UTC so that daylight savings doesn't get messy
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -date) #remove unnecessary columns
  
str(buoy2014_L0)

#add all dates/times to record
alltimes_2014 <- as.data.frame(seq.POSIXt(as.POSIXct('2014-01-01 00:00', tz='UTC'), as.POSIXct('2014-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2014_L1 <- buoy2014_L0 %>% 
  right_join(., alltimes_2014) %>% 
  arrange(datetime)


#double check to make sure there are no DST issues
datelength2014 <- buoy2014_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2014$`length(datetime)`)
min(datelength2014$`length(datetime)`)
#should only be 144 or less if partial days included

#DST observed at odd times - 03-09-15 4a; 11-02-15 00:00
buoy2014_L1a <- buoy2014_L1 %>% 
  filter(datetime < as.POSIXct('2014-03-09 23:00', tz='UTC'))

buoy2014_L1b <- buoy2014_L1 %>% 
  filter(datetime >= as.POSIXct('2014-03-10 00:00', tz='UTC') & rowid < 34947) %>% 
  right_join(alltimes_2014) %>% 
  filter(datetime >= as.POSIXct('2014-03-10 00:00', tz='UTC') & datetime < as.POSIXct('2014-11-03 1:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2014b <- as.data.frame(seq.POSIXt(as.POSIXct('2014-03-09 23:00', tz='UTC'), as.POSIXct('2014-11-02 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2014_L1b <- full_join(buoy2014_L1b, alltimes_2014b)

buoy2014_L1c <- buoy2014_L1 %>% 
  filter(rowid >= 34947)  %>% 
  right_join(alltimes_2014) %>% 
  filter(datetime >= as.POSIXct('2014-11-03 00:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2014c <- as.data.frame(seq.POSIXt(as.POSIXct('2014-11-03 00:00', tz='UTC'), as.POSIXct('2014-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2014_L1c <- full_join(buoy2014_L1c, alltimes_2014c)

buoy2014_L1 <- full_join(buoy2014_L1a, buoy2014_L1b) %>% 
  full_join(., buoy2014_L1c) %>% 
  select(-rowid, -rowid2)

#double check to make sure there are no DST issues
datelength2014 <- buoy2014_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2014$`length(datetime)`)
min(datelength2014$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(alltimes_2014, alltimes_2014b, alltimes_2014c, datelength2014, buoy2014_L1a, buoy2014_L1b, buoy2014_L1c)


####plot  and clean thermistor data####

#initial data visualization
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, alltemp) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels = alltemp))

# ggplot(buoy2014_vert_temp, 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2014 thermisters - raw', 
#        x=NULL, 
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#na values are -6999, 555.4 and 0 in thermisters
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_10m), 
            funs(case_when(.==-6999 ~ NA_real_, 
                           .==555.4 ~ NA_real_, 
                           .==0 ~ NA_real_, 
                           TRUE ~ .)))

#### L0.5 plot - no NA values of -6999, 0 or 555.4
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, alltemp) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=c(alltemp)))

ggplot(buoy2014_vert_temp, 
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014 thermisters - NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


# #buoy deployed June 9, 2014 15:00
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-06-01',tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #thermistors intermittent - add flag
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-06-09',tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(temp_flag = case_when(datetime >=as.POSIXct('2014-06-09',tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC') ~ 'i',
                               TRUE ~ NA_character_))

# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-07-24', tz='UTC') & datetime < as.POSIXct('2014-07-25', tz='UTC'))),
#               aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 24 2014 buoy visit - L0', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(alltemp), 
            funs(case_when(datetime>=as.POSIXct('2014-07-24 8:00', tz='UTC') & datetime<=as.POSIXct('2014-07-24 8:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_temp_L1 <- buoy2014_L1 %>%
  select(datetime, alltemp) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=c(alltemp)))

# ggplot(subset(buoy2014_vert_temp_L1,
#               subset=(datetime>= as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2014  L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#flag data from 10m line from here forward - seems that it is programmed wrong or is hung up above the 9m thermistor
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(temp_flag = case_when(datetime>=as.POSIXct('2014-07-24 8:40', tz='UTC') & datetime<=as.POSIXct('2014-12-31 23:50', tz='UTC') ~ '10.5dn',
                               TRUE ~ temp_flag))

# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #august 13 odd temp behavior
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-08-13', tz='UTC') & datetime < as.POSIXct('2014-08-14', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #just looks like early mixing - seen also in low do
# 
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Sept 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #Oct 14 buoy moved to harbor Oct 14 10:20
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-10-14', tz='UTC') & datetime < as.POSIXct('2014-10-15', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove from Oct 14 10:10
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(alltemp),
            funs(case_when(datetime>=as.POSIXct('2014-10-14 09:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_temp_L1 <- buoy2014_L1 %>%
  select(datetime, temp_flag, alltemp) %>%
  gather(variable, value, -datetime, -temp_flag) %>%
  mutate(variable = factor(variable, levels=c(alltemp)))

# ggplot(subset(buoy2014_vert_temp_L1,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2014_vert_temp_L1, 
       aes(x=datetime, y=value, col=variable, shape = temp_flag)) +
  geom_point() +
  labs(title='2014 thermisters - NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(buoy2014_vert_temp, buoy2014_vert_temp_L1)

#correct for thermistor offset
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(TempC_10p5m = 'TempC_10m',
       TempC_9p5m = 'TempC_9m',
       TempC_8p5m = 'TempC_8m',
       TempC_7p5m = 'TempC_7m',
       TempC_6p5m = 'TempC_6m',
       TempC_5p5m = 'TempC_5m',
       TempC_4p5m = 'TempC_4m',
       TempC_3p5m = 'TempC_3m',
       TempC_2p5m = 'TempC_2m',
       TempC_1p5m = 'TempC_1m',
       TempC_0p5m = 'TempC_0m')
  
####DO data####
buoy2014_vert_do <- buoy2014_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2014_vert_do, 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2014 do probes - raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(upDO, lowDO), 
            funs(case_when(.<=0 ~ NA_real_, 
                           .==555.4 ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_do <- buoy2014_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2014_vert_do, 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 do probes - NA values recoded', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2014-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jan 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-02-01', tz='UTC') & datetime < as.POSIXct('2014-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-03-01', tz='UTC') & datetime < as.POSIXct('2014-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-04-01', tz='UTC') & datetime < as.POSIXct('2014-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #data offline begining apr 7
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-04-07', tz='UTC') & datetime < as.POSIXct('2014-04-08', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# 
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-05-01', tz='UTC') & datetime < as.POSIXct('2014-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jan 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-06-01', tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy online jun 9 at loon
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-06-09', tz='UTC') & datetime < as.POSIXct('2014-06-10', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2014-06-09', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2014-06-09 15:00', tz='UTC') ~ 'loon',
                              TRUE ~ NA_character_))

# ggplot(subset(buoy2014_vert_do, 
#               subset = (datetime>= as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #low do errant from 17 9:30-28 11:30
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-17', tz='UTC') & datetime < as.POSIXct('2014-07-18', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(DOLowSat, DOLowPPM),
            funs(case_when(datetime >= as.POSIXct('2014-06-09 15:00', tz='UTC') & datetime < as.POSIXct('2014-07-17 8:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2014-07-17 8:30', tz='UTC') & datetime < as.POSIXct('2014-07-28 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(lower_do_flag = case_when(datetime>=as.POSIXct('2014-07-28 10:30', tz='UTC') & datetime<as.POSIXct('2014-10-14 9:00', tz='UTC') ~ 'd',
                               TRUE ~ NA_character_))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - qaqc',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
#august 13 is early mixing - seen also in thermistors
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #upper do miscalibrated from jul 28 11:00/14:40-sept 18 17:00
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-09-18', tz='UTC') & datetime < as.POSIXct('2014-09-19', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(upper_do_flag = case_when(datetime >= as.POSIXct('2014-07-28 10:00', tz='UTC') & datetime < as.POSIXct('2014-09-18 16:00', tz='UTC') ~ 'm',
                           TRUE ~ NA_character_)) %>% 
  mutate(DOppm = case_when(datetime >= as.POSIXct('2014-07-28 10:00', tz='UTC') & datetime < as.POSIXct('2014-07-28 13:40', tz='UTC') ~ NA_real_,
                           TRUE ~ DOppm))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, upper_do_flag, upDO, lowDO) %>%
  gather(variable, value, -datetime, -upper_do_flag)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = upper_do_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - qaqc',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do_L1, 
#               subset = (datetime>= as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = upper_do_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor 10-14
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-10-14', tz='UTC') & datetime < as.POSIXct('2014-10-15', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2014-10-14 09:00', tz='UTC') & datetime < as.POSIXct('2014-10-14 10:10', tz='UTC')~ 'in transit',
                              datetime >= as.POSIXct('2014-10-14 10:10', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2014-10-14 09:00', tz='UTC') & datetime < as.POSIXct('2014-10-14 10:20', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2014-10-14 09:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, location, upper_do_flag, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location, -upper_do_flag)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-11-01', tz='UTC') & datetime < as.POSIXct('2014-12-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-12-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_vert_updo <- buoy2014_L1 %>%
  select(datetime, location, upper_do_flag, upDO) %>%
  gather(variable, value, -datetime, -location, -upper_do_flag)


ggplot(buoy2014_vert_updo, 
       aes(x=datetime, y=value, color = upper_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 upper do probes - clean', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2014_vert_lowdo <- buoy2014_L1 %>%
  select(datetime, location, lower_do_flag, lowDO) %>%
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(buoy2014_vert_lowdo, 
       aes(x=datetime, y=value, color = lower_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 lower do probes - clean', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')



# add presumed calibration flags
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(lower_do_flag = case_when(is.na(lower_do_flag) & datetime == as.POSIXct('2014-07-28 10:30', tz='UTC') ~ 'cp',
                                   !is.na(lower_do_flag) & datetime == as.POSIXct('2014-07-28 10:30', tz='UTC') ~ paste(lower_do_flag, 'cp', sep = ', '),
                                 TRUE ~ lower_do_flag)) %>% 
  mutate(upper_do_flag = case_when(is.na(upper_do_flag) & datetime == as.POSIXct('2014-06-09 15:00', tz='UTC') ~ 'wp',
                                   !is.na(upper_do_flag) & datetime == as.POSIXct('2014-06-09 15:00', tz='UTC') ~ paste(upper_do_flag, 'wp', sep = ', '),
                                   is.na(upper_do_flag) & datetime == as.POSIXct('2014-07-28 10:30', tz='UTC') ~ 'cep',
                                   !is.na(upper_do_flag)  & datetime == as.POSIXct('2014-07-28 10:30', tz='UTC') ~ paste(upper_do_flag, 'cep', sep = ', '),
                                   is.na(upper_do_flag) & datetime == as.POSIXct('2014-09-18 16:00', tz='UTC') ~ 'cp',
                                   !is.na(upper_do_flag)  & datetime == as.POSIXct('2014-09-18 16:00', tz='UTC') ~ paste(upper_do_flag, 'cp', sep = ', '),
                                   is.na(upper_do_flag) & datetime == as.POSIXct('2014-10-14 10:20', tz='UTC') ~ 'wp',
                                   !is.na(upper_do_flag)  & datetime == as.POSIXct('2014-10-14 10:20', tz='UTC') ~ paste(upper_do_flag, 'wp', sep = ', '),
                                   TRUE ~ upper_do_flag))

rm(buoy2014_vert_do, buoy2014_vert_do_L1, buoy2014_vert_updo, buoy2014_vert_lowdo)

#### plotPAR ####

#replace negative values with 0
range(buoy2014_L1$PAR, na.rm = T)
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                              TRUE ~ NA_character_))%>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))


ggplot(buoy2014_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2014-02-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jan 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-02-01', tz='UTC') & datetime < as.POSIXct('2014-03-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='feb 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#par likely obscured feb 13-19
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(PAR_flag = case_when(is.na(PAR_flag) & datetime >= as.POSIXct('2014-02-13', tz='UTC') & datetime < as.POSIXct('2014-02-20', tz='UTC') ~ 'o',
                              !is.na(PAR_flag) & datetime >= as.POSIXct('2014-02-13', tz='UTC') & datetime < as.POSIXct('2014-02-20', tz='UTC') ~ paste('o', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))

# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-03-01', tz='UTC') & datetime < as.POSIXct('2014-04-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='mar 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-04-01', tz='UTC') & datetime < as.POSIXct('2014-05-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='apr 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#par errant mar 28-apr 8 recode to na, data errant after that - recode from mar 28 forward
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(PAR = case_when(datetime >= as.POSIXct('2014-03-28', tz='UTC') ~ NA_real_,
                              TRUE ~ PAR))

ggplot(buoy2014_L1,
       aes(x=datetime, y=PAR, color = PAR_flag, shape = location)) +
  geom_point() +
  labs(title='2014 PAR clean', x=NULL, y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


####Wind data ####
buoy2014_vert_wind <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

ggplot(buoy2014_vert_wind, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y=' ') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2014-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ##sensor frozen until Jan 5
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-01-05', tz='UTC') & datetime < as.POSIXct('2014-01-06', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime<as.POSIXct('2014-01-05 12:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

# ggplot(subset(buoy2014_vert_wind_L1, 
#               subset=(datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2014-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-02-01', tz='UTC') & datetime < as.POSIXct('2014-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen feb 14
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-02-14', tz='UTC') & datetime < as.POSIXct('2014-02-15', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #and on the 18/19th
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-02-18 18:00', tz='UTC') & datetime < as.POSIXct('2014-02-19 18:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2014-02-14 4:50', tz='UTC') & datetime<as.POSIXct('2014-02-14 12:50', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2014-02-19 1:30', tz='UTC') & datetime<as.POSIXct('2014-02-19 8:00', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

# ggplot(subset(buoy2014_vert_wind_L1, 
#               subset=(datetime >= as.POSIXct('2014-02-01', tz='UTC') & datetime < as.POSIXct('2014-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-03-01', tz='UTC') & datetime < as.POSIXct('2014-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-04-01', tz='UTC') & datetime < as.POSIXct('2014-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-05-01', tz='UTC') & datetime < as.POSIXct('2014-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-06-01', tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

# ggplot(subset(buoy2014_vert_wind_L1, 
#               subset=(datetime >= as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-11-01', tz='UTC') & datetime < as.POSIXct('2014-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen nov26 through nov 30
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-11-26', tz='UTC') & datetime < as.POSIXct('2014-11-27', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-11-30', tz='UTC') & datetime < as.POSIXct('2014-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2014-11-26 12:30', tz='UTC') & datetime<as.POSIXct('2014-11-30 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

# ggplot(subset(buoy2014_vert_wind_L1, 
#               subset=(datetime >= as.POSIXct('2014-11-01', tz='UTC') & datetime < as.POSIXct('2014-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-12-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen dec 11/12
# ggplot(subset(buoy2014_vert_wind, 
#               subset=(datetime >= as.POSIXct('2014-12-11 9:00', tz='UTC') & datetime < as.POSIXct('2014-12-12 18:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2014-12-11 14:30', tz='UTC') & datetime<as.POSIXct('2014-12-12 14:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value , -datetime, -location)

# ggplot(subset(buoy2014_vert_wind_L1, 
#               subset=(datetime >= as.POSIXct('2014-12-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2014_vert_wind_L1, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y=' ') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(buoy2014_vert_wind, buoy2014_vert_wind_L1)

#### Air Temp data ####
range(buoy2014_L1$AirTempC)

ggplot(buoy2014_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2014 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-03-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-04-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-05-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = ' apr 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-06-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-07-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-08-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-09-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-10-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-11-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-12-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime>=as.POSIXct('2014-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2015-01-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2014_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2014 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#### export L1 data ####
colnames(buoy2014_L1)
#add offline time and recode flags when buoy is offline
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2014-04-17 14:00', tz='UTC') & datetime < as.POSIXct('2014-06-09 15:00', tz='UTC') ~ 'offline',
                              TRUE ~ location)) %>% 
  mutate_at(vars(temp_flag, upper_do_flag, lower_do_flag, PAR_flag),
            funs(case_when(location == 'offline' ~ NA_character_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOTempC:DOLowPPM),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .)))


# export L1 tempstring file
buoy2014_L1 %>%
  select(datetime, location, TempC_0p5m:TempC_10p5m, temp_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2014_tempstring_L1.csv')

#export L1 do file
buoy2014_L1 %>%
  select(datetime, location, upDO, lowDO, upper_do_flag, lower_do_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2014_do_L1.csv')

#export wind file
buoy2014_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2014_wind_L1.csv')

#export PAR file
buoy2014_L1 %>%
  select(datetime, location, PAR, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2014_PAR_L1.csv')

#export L1 air temp file
buoy2014_L1 %>%
  select(datetime, location, AirTempC) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2014_airtemp_L1.csv')




