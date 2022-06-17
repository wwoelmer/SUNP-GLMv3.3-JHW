#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2016.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2016 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

#bring in 2016 buoy raw data
buoy2016_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2016 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

#### format data ####
buoy2016_L0 <- buoy2016_L0  %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rowid_to_column('rowid')

# add in all date time options in L1 data set
alltimes_2016 <- as.data.frame(seq.POSIXt(as.POSIXct('2016-01-01 00:00', tz='UTC'), as.POSIXct('2016-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2016_L1 <- buoy2016_L0 %>% 
  right_join(., alltimes_2016) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2016 <- buoy2016_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2016$`length(datetime)`)
min(datelength2016$`length(datetime)`)
#should only be 144 or less if partial days included

#DST observed at odd times - 03-09-15 4a; 11-02-15 00:00
buoy2016_L1a <- buoy2016_L1 %>% 
  filter(datetime < as.POSIXct('2016-03-13 23:00', tz='UTC'))

buoy2016_L1b <- buoy2016_L1 %>% 
  filter(datetime >= as.POSIXct('2016-03-14 00:00', tz='UTC') & rowid < 44775) %>% 
  right_join(alltimes_2016) %>% 
  filter(datetime >= as.POSIXct('2016-03-14 00:00', tz='UTC') & datetime < as.POSIXct('2016-11-07 1:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2016b <- as.data.frame(seq.POSIXt(as.POSIXct('2016-03-13 23:00', tz='UTC'), as.POSIXct('2016-11-06 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2016_L1b <- full_join(buoy2016_L1b, alltimes_2016b)

buoy2016_L1c <- buoy2016_L1 %>% 
  filter(rowid >= 44775)  %>% 
  right_join(alltimes_2016) %>% 
  filter(datetime >= as.POSIXct('2016-11-07 00:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2016c <- as.data.frame(seq.POSIXt(as.POSIXct('2016-11-07 00:00', tz='UTC'), as.POSIXct('2016-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2016_L1c <- full_join(buoy2016_L1c, alltimes_2016c)

buoy2016_L1 <- full_join(buoy2016_L1a, buoy2016_L1b) %>% 
  full_join(., buoy2016_L1c) %>% 
  select(-rowid, -rowid2)

#double check to make sure there are no DST issues
datelength2016 <- buoy2016_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2016$`length(datetime)`)
min(datelength2016$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(alltimes_2016, alltimes_2016b, alltimes_2016c, datelength2016, buoy2016_L1a, buoy2016_L1b, buoy2016_L1c)

####thermistors####
buoy2016_vert_temp <- buoy2016_L1 %>% 
  select(datetime, alltemp2011) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, raw', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(. == -6999 ~ NA_real_,
                           . >= 555.4 ~ NA_real_,
                           TRUE ~ .)))

buoy2016_vert_temp <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#remove all of 0m thermistor - errant most of year
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(TempC_0m = NA_real_)

buoy2016_vert_temp <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, 0m removed', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Apr 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode therm data to NA prior to move to loon - the depths are not correct - suspect temp not in water. 5-3-16 11am
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-03', tz='UTC') & datetime < as.POSIXct('2016-05-04', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May move 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>%
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime <= as.POSIXct('2016-05-03 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant data on may 18 and 19
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-18', tz='UTC') & datetime < as.POSIXct('2016-05-19', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-19', tz='UTC') & datetime < as.POSIXct('2016-05-20', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime == as.POSIXct('2016-05-18 10:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 4:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy visit June 7 8:10
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-07', tz='UTC') & datetime < as.POSIXct('2016-06-08', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 7 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks okay - a little wonky at 6m, but good otherwise
# 
# #buoy visit June 14 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-14', tz='UTC') & datetime < as.POSIXct('2016-06-15', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 14 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# #buoy visit June 15 10:50a
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-15', tz='UTC') & datetime < as.POSIXct('2016-06-16', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove artifacts
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime == as.POSIXct('2016-06-15 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-06-15 09:00', tz='UTC') & datetime < as.POSIXct('2016-06-15 13:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #buoy visit June 17 8:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-17', tz='UTC') & datetime < as.POSIXct('2016-06-18', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 17 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# #buoy visit June 20 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-20', tz='UTC') & datetime < as.POSIXct('2016-06-21', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 20 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Sept 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor Oct 12 9:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 12 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #no artifacts of buoy move

buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


ggplot(subset(buoy2016_vert_temp_L1, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & 
                        datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct names for sensor offset from surface
buoy2016_L1 <- buoy2016_L1 %>% 
  rename(TempC_9p5m = 'TempC_9m',
         TempC_8p5m = 'TempC_8m',
         TempC_7p5m = 'TempC_7m',
         TempC_6p5m = 'TempC_6m',
         TempC_5p5m = 'TempC_5m',
         TempC_4p5m = 'TempC_4m',
         TempC_3p5m = 'TempC_3m',
         TempC_2p5m = 'TempC_2m',
         TempC_1p5m = 'TempC_1m',
         TempC_0p5m = 'TempC_0m') 

rm(buoy2016_vert_temp, buoy2016_vert_temp_L1)

####DO####
buoy2016_vert_do <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_do, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, raw', x='date', y='') +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_do <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy2016_vert_do, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, NA values recoded', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# #remove data prior to sensor deployment 4-19-16 10:00
# ggplot(subset(buoy2016_vert_do,
#               subset=(datetime>=as.POSIXct('2016-04-19', tz='UTC') & datetime < as.POSIXct('2016-04-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 19 2016', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime <= as.POSIXct('2016-04-19 10:30') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2016-04-19 10:40') ~ 'c',
                                   TRUE  ~ NA_character_))
#low do no at proper depth, will recode to NA once at buoy move

buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_do_L1, 
#               subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_do_L1, 
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-03', tz='UTC') & datetime < as.POSIXct('2016-05-04', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(lower_do_flag = NA_character_) %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2016-05-03 9:10', tz='UTC') & datetime < as.POSIXct('2016-05-03 11:00', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOSat, DOppm),
            funs(case_when(datetime >= as.POSIXct('2016-05-03 11:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 11:40', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime < as.POSIXct('2016-05-03 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime < as.POSIXct('2016-05-03 9:10', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2016-05-03 9:10', tz='UTC') & datetime < as.POSIXct('2016-05-03 11:00', tz='UTC') ~ 'in transit',
                              TRUE ~ 'loon')) %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2016-05-03 11:00', tz='UTC') ~ 'w',
                                   TRUE ~ upper_do_flag)) %>% 
  mutate(lower_do_flag = case_when(datetime == as.POSIXct('2016-05-03 11:00', tz='UTC') ~ 'c',
                                   TRUE ~ lower_do_flag))
  
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# # May 18 and 21
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-18', tz='UTC') & datetime < as.POSIXct('2016-05-19', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-19', tz='UTC') & datetime < as.POSIXct('2016-05-20', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-21', tz='UTC') & datetime < as.POSIXct('2016-05-22', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

#may 18 11:00 low do
#may 19 5:10 low do
#may 21 14:10 low do
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2016-05-18 10:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 4:10', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-21 13:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, NAs recoded', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

#do low sat errant when below 25% - recode sat and ppm at those times
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            funs(case_when(datetime >= as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-06-14', tz='UTC') & DOLowSat<25 ~ NA_real_,
                           TRUE ~ .))) 
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

#do data intermittent from may 22 until jun 13 - flag as such
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upper_do_flag, lower_do_flag),
            funs(case_when(datetime >= as.POSIXct('2016-05-22', tz='UTC') & datetime < as.POSIXct('2016-06-14', tz='UTC') ~ 'i',
                           TRUE ~ .))) 

# #do cleaned Jun 7
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-07', tz='UTC') & datetime < as.POSIXct('2016-06-08', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upper_do_flag, lower_do_flag),
            funs(case_when(datetime == as.POSIXct('2016-06-07 7:20', tz='UTC') ~ 'i, w',
                           TRUE ~ .))) 

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

#remove buoy move artifacts of move and add location flag
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2016-10-12 8:50', tz='UTC') & datetime < as.POSIXct('2016-10-12 11:40', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2016-10-12 8:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2016-10-12 8:50', tz='UTC') & datetime < as.POSIXct('2016-10-12 10:00', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2016-10-12 10:00', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2016-10-12 10:00', tz='UTC') ~ 'wp',
                                   TRUE ~ upper_do_flag))
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, upDO, lowDO, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

ggplot(buoy2016_vert_do_L1,
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

buoy2016_vert_updo <- buoy2016_L1 %>% 
  select(datetime, upDO, upper_do_flag, location) %>% 
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(buoy2016_vert_updo,
       aes(x=datetime, y=value, color = upper_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()


buoy2016_vert_lowdo <- buoy2016_L1 %>% 
  select(datetime, lowDO, lower_do_flag, location) %>% 
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(buoy2016_vert_lowdo,
       aes(x=datetime, y=value, color = lower_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()


rm(buoy2016_vert_do, buoy2016_vert_do_L1, buoy2016_vert_updo, buoy2016_vert_lowdo)


#### wind data ####
buoy2016_vert_wind <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2016_vert_wind, 
  aes(x=datetime, y=value, col=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, raw', x='date', y='') +
  scale_color_colorblind() +
  final_theme

# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-01-18', tz='UTC') & datetime < as.POSIXct('2016-01-19', tz='UTC'))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#select by jan 18
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2016-01-18 3:20', tz='UTC') &
                             datetime < as.POSIXct('2016-01-18 9:50', tz='UTC')  ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-02-09', tz='UTC') & datetime < as.POSIXct('2016-02-10', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-02-24', tz='UTC') & datetime < as.POSIXct('2016-02-25', tz='UTC'))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#select by feb 9 and 24 & max wind =0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2016-02-09 00:10', tz='UTC') &
                             datetime < as.POSIXct('2016-02-09 12:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2016-02-24 11:40', tz='UTC') &
                             datetime < as.POSIXct('2016-02-24 17:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-03-25', tz='UTC') & datetime < as.POSIXct('2016-03-26', tz='UTC'))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2016-03-25 8:10', tz='UTC') &
                             datetime < as.POSIXct('2016-03-25 11:10', tz='UTC') &
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-12-18', tz='UTC') & datetime < as.POSIXct('2016-12-19', tz='UTC'))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-12-29 12:00', tz='UTC') & datetime < as.POSIXct('2016-12-31', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2016-12-18 3:40', tz='UTC') &
                             datetime < as.POSIXct('2016-12-18 7:40', tz='UTC') &
                             MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2016-12-29 19:40', tz='UTC') &
                             datetime < as.POSIXct('2016-12-30 10:50', tz='UTC') &
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2016_vert_wind_L1,
       aes(x=datetime, y=value, col=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, qaqc', x='date', y='') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day')


rm(buoy2016_vert_wind, buoy2016_vert_wind_L1)

# ####chlorophyll sensor####
# # buoy2016_vert_chla <- buoy2016_L1 %>%
# #   select(datetime, location, chla) %>%
# #   gather(variable, value, -datetime, -location)
# # 
# # ggplot(buoy2016_vert_chla, 
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             funs(case_when(. == -6999 ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(Chlor_UGL > 10 ~ 's',
#                                TRUE ~ '')) %>% 
#   mutate(Chlor_UGL = case_when(Chlor_UGL<0 ~ 0,
#                                TRUE ~ Chlor_UGL))
# buoy2016_vert_chla <- buoy2016_L1 %>%
#   select(datetime, location, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -location, -chla_flag)
# 
# ggplot(buoy2016_vert_chla, 
#        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# #     
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-04-19', tz='UTC') & datetime < as.POSIXct('2016-04-20', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             funs(case_when(datetime < as.POSIXct('2016-04-19 11:20', tz='UTC') ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime == as.POSIXct('2016-04-19 11:20', tz='UTC') ~ 'c',
#                                TRUE ~ '')) %>% 
#   mutate(cond_flag = case_when(datetime == as.POSIXct('2016-04-19 11:20', tz='UTC') ~ 'c',
#                                TRUE ~ ''))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             funs(case_when(location == 'in transit' ~ NA_real_,
#                            TRUE ~ .))) 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #spec cond taken out early // check before buoy move
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-03', tz='UTC') & datetime < as.POSIXct('2016-05-04', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate(SpecCond = case_when(datetime >= as.POSIXct('2016-05-03 9:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 11:00') ~ NA_real_,
#                                TRUE ~ SpecCond)) %>% 
#   mutate_at(vars(cond_flag, chla_flag),
#             funs(case_when(datetime == as.POSIXct('2016-05-03 11:00') ~ 'w',
#                            TRUE ~ .)))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #do cleaned Jun 7
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime>=as.POSIXct('2016-06-07', tz='UTC') & datetime < as.POSIXct('2016-06-08', tz='UTC'))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='jun 2016, clean', x='date', y='') +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   scale_color_colorblind()
# 
# #data intermittent from may 22 until jun 13 - flag as such; sensors impacted with do cleaning
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla_flag, cond_flag),
#             funs(case_when(datetime >= as.POSIXct('2016-05-22', tz='UTC') & datetime < as.POSIXct('2016-06-14', tz='UTC') ~ 'i',
#                            TRUE ~ .))) %>% 
#   mutate_at(vars(chla),
#             funs(case_when(datetime == as.POSIXct('2016-06-07 8:20', tz='UTC') ~ NA_real_,
#                            TRUE ~ .))) 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             funs(case_when(location == 'in transit' ~ NA_real_,
#                            TRUE ~ .))) 
# 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -location, -chla_flag)
# 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))),
# #        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(buoy2016_vert_chla_L1,
#        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# #chlorRFU should be linear with ugl. recode to na, because that data stream is labeled incorrectly.
# 
# rm(buoy2016_vert_chla, buoy2016_vert_chla_L1)



#### air temp ####

ggplot(buoy2016_L1, aes(x=datetime, y=AirTempC, col=location)) +
  geom_point() +
  labs(title='2016', x=NULL, y='Air Temp deg C') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1,
#               datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC')),
#        aes(x=datetime, y=AirTempC, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='Air Temp deg C') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2016_L1, aes(x=datetime, y=AirTempC, col=location)) +
  geom_point() +
  labs(title='2016', x=NULL, y='Air Temp deg C') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


#### PAR ####
# ggplot(buoy2016_L1, 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

#adjust negative par to 0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(PAR_flag = case_when(PAR<0 ~ 'z',
                         TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(PAR<0 ~ 0,
                         TRUE ~ PAR))


ggplot(subset(buoy2016_L1, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
       aes(x=datetime, y=PAR, col=location)) +
  geom_point() +
  labs(title='2016', x=NULL, y='PAR') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2016_L1 <-  buoy2016_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(subset(buoy2016_L1, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
       aes(x=datetime, y=PAR, col=location)) +
  geom_point() +
  labs(title='2016', x=NULL, y='PAR') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


#### EXPORT L1 DATA STREAMS ####
colnames(buoy2016_L1)

#export L1 tempstring file
buoy2016_L1 %>%
  select(datetime, alltemp2011.5, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2016_tempstring_L1.csv')

#export l1 do file
buoy2016_L1 %>%
  select(datetime, upDO, lowDO, upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2016_do_L1.csv')

#export l1 par file
buoy2016_L1 %>%
  select(datetime, PAR, location, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2016_PAR_L1.csv')

#export l1 wind
buoy2016_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2016_wind_L1.csv')

#export l1 air temp file
buoy2016_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2016_airtemp_L1.csv')

# #export l1 chla cond file
# buoy2016_L1 %>%
#   select(datetime, Chlor_UGL, SpecCond, location, cond_flag, chla_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_chla_cond_L1.csv')

