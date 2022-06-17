#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2015.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2015 using       *
#*          similar methods to CCC and DR                        *
#* PREVIOUS VERSION: 'Sunapee_buoy_2015_17Oct2017.R'             *
#*                   'Sunapee_buoy_2015_11Oct2017.R'             *
#*                   'Sunapee_buoy_2014-2016_07Aug2017.R'        *
#*****************************************************************

#bring in 2015 buoy raw data
buoy2015_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Buoy Data.csv',
                        col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')


#### format data ####
buoy2015_L0 <- buoy2015_L0 %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         InstWindSp = 'WindSpd',
         InstWindDir = 'CorrWind',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -time, TempC_10m) %>%  #remove unnecessary columns -- 10m not in use
  rowid_to_column(var = 'rowid')

#add all dates/times to record
alltimes_2015 <- as.data.frame(seq.POSIXt(as.POSIXct('2015-01-01 00:00', tz='UTC'), as.POSIXct('2015-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2015_L1 <- buoy2015_L0 %>% 
  right_join(., alltimes_2015) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2015 <- buoy2015_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2015$`length(datetime)`)
min(datelength2015$`length(datetime)`)
#should only be 144 or less if partial days included

#DST observed at odd times - 03-09-15 4a; 11-02-15 00:00
buoy2015_L1a <- buoy2015_L1 %>% 
  filter(datetime < as.POSIXct('2015-03-09 04:00', tz='UTC'))

buoy2015_L1b <- buoy2015_L1 %>% 
  filter(datetime >= as.POSIXct('2015-03-09 05:00', tz='UTC') & rowid < 37880) %>% 
  right_join(alltimes_2015) %>% 
  filter(datetime >= as.POSIXct('2015-03-09 05:00', tz='UTC') & datetime < as.POSIXct('2015-11-02 6:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2015b <- as.data.frame(seq.POSIXt(as.POSIXct('2015-03-09 4:00', tz='UTC'), as.POSIXct('2015-11-02 4:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2015_L1b <- full_join(buoy2015_L1b, alltimes_2015b)

buoy2015_L1c <- buoy2015_L1 %>% 
  filter(rowid >= 37880 & rowid != 44009)  %>% 
  right_join(alltimes_2015) %>% 
  filter(datetime >= as.POSIXct('2015-11-02 5:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2015c <- as.data.frame(seq.POSIXt(as.POSIXct('2015-11-02 5:00', tz='UTC'), as.POSIXct('2015-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2015_L1c <- full_join(buoy2015_L1c, alltimes_2015c)

buoy2015_L1 <- full_join(buoy2015_L1a, buoy2015_L1b) %>% 
  full_join(., buoy2015_L1c) %>% 
  select(-rowid, -rowid2)

#double check to make sure there are no DST issues
datelength2015 <- buoy2015_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2015$`length(datetime)`)
min(datelength2015$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(alltimes_2015, alltimes_2015b, alltimes_2015c, datelength2015, buoy2015_L1a, buoy2015_L1b, buoy2015_L1c)


#####2015 thermistors - remove/replace NA values ####
buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_temp, 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, raw', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_10m),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_temp,
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

# remove 0m completely
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(TempC_0m = NA_real_)

buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

#temp 10m is actually null

# ggplot(buoy2015_vert_temp,
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_vert_temp,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#     scale_x_datetime(date_minor_breaks = '1 day')
# 
ggplot(subset(buoy2015_vert_temp,
              subset=(datetime >= as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-12', tz='UTC'))),
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='jun 2015, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme+
  scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_temp_L1, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, clean', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_temp, 
#               subset=(datetime >= as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jul 2015, NAs recoded', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')

#add intermittent flag for jun 25 through jul 10
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime>=as.POSIXct('2015-06-25', tz='UTC') & datetime<=as.POSIXct('2015-07-10', tz='UTC') ~ NA_real_,
                               TRUE ~ .)))
buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


ggplot(buoy2015_vert_temp_L1,
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, clean, with flags',
       x=NULL,
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#correct for offset from surface
buoy2015_L1 <- buoy2015_L1 %>% 
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

rm(buoy2015_vert_temp, buoy2015_vert_temp_L1)

#### DO ####

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_do, 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

# remove NA values
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2015_vert_do, 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, NAs recoded', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2015_L1 <- buoy2015_L1 %>%
  mutate_at(vars(lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-02-01', tz='UTC') & datetime < as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-03-01', tz='UTC') & datetime < as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-04-22', tz='UTC') & datetime < as.POSIXct('2015-04-23', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-04-22 10:00', tz='UTC') & datetime<=as.POSIXct('2015-04-22 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-05-01', tz='UTC') & datetime < as.POSIXct('2015-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-06-02', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-12', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, DOLowTempC),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 09:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOLowSat, DOLowPPM),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 8:10', tz='UTC') ~ 'harbor',
                              datetime>=as.POSIXct('2015-06-11 8:10', tz='UTC') & datetime<as.POSIXct('2015-06-11 9:10', tz='UTC') ~ 'in transit',
                              TRUE ~ 'loon'))%>% 
  mutate(upper_do_flag = case_when(datetime==as.POSIXct('2015-06-11 9:10', tz='UTC') ~ 'wp',
                                   TRUE ~ NA_character_)) %>% 
  mutate(lower_do_flag = case_when(datetime==as.POSIXct('2015-06-11 10:10', tz='UTC') ~ 'wp',
                                   TRUE ~ NA_character_))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-08-13', tz='UTC') & datetime < as.POSIXct('2015-08-14', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#August 13th visit
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime==as.POSIXct('2015-08-13 09:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(upper_do_flag = case_when(datetime==as.POSIXct('2015-08-13 09:00', tz='UTC') ~ 'w',
                                   TRUE ~ upper_do_flag)) %>% 
  mutate(lower_do_flag = case_when(datetime==as.POSIXct('2015-08-13 09:10', tz='UTC') ~ 'w',
                                   TRUE ~ lower_do_flag))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1, 
#               subset=(datetime >= as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-09-19', tz='UTC') & datetime < as.POSIXct('2015-09-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#errant point around the 19th
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime==as.POSIXct('2015-09-19 4:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1, 
#               subset=(datetime >= as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-10-08', tz='UTC') & datetime < as.POSIXct('2015-10-09', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove artifacts of buoy movement and data thereafter, update location data
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-10-08 09:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-10-08 09:40', tz='UTC') & buoy2015_L1$datetime<as.POSIXct('2015-10-08 10:30', tz='UTC') ~ 'in transit',
                              datetime>=as.POSIXct('2015-10-08 10:30', tz='UTC') ~ 'harbor',
                              TRUE ~ location))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-11-01', tz='UTC') & datetime < as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-12-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')


buoy2015_vert_updo <- buoy2015_L1 %>%
  select(datetime, location, upDO, upper_do_flag) %>%
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(buoy2015_vert_updo, 
       aes(x=datetime, y=value, color = upper_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2015_vert_lowdo <- buoy2015_L1 %>%
  select(datetime, location, lowDO, lower_do_flag) %>%
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(buoy2015_vert_lowdo, 
       aes(x=datetime, y=value, color = lower_do_flag, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(buoy2015_vert_do, buoy2015_vert_do_L1, buoy2015_vert_updo, buoy2015_vert_lowdo)

####PAR ####
ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, raw', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#replace negative values with 0
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                              TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                       TRUE ~ PAR))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jun2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR)) %>% 
  mutate(PAR_flag =case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC') ~ NA_character_,
                             TRUE ~ PAR_flag))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

#remove data possibly errant due to buoy move
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jul 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='aug 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='sept 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='oct 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='nov 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='dec 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, clean', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


####Wind data ####
buoy2015_vert_wind <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2015_vert_wind, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', 
       x=NULL, 
       y=NULL) +
  final_theme

# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor jan 4
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-01-04', tz='UTC') & datetime<as.POSIXct('2015-01-05', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-01-04 8:40', tz='UTC') & datetime<=as.POSIXct('2015-01-04 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-02-14 18:00', tz='UTC') & datetime<as.POSIXct('2015-02-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz='UTC') & datetime<as.POSIXct('2015-02-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-02-15 00:50', tz='UTC') & datetime<as.POSIXct('2015-02-15 14:10', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-02-18 22:40', tz='UTC') & datetime<as.POSIXct('2015-02-19 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-14', tz='UTC') & datetime<as.POSIXct('2015-03-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-05-01', tz='UTC') & datetime<as.POSIXct('2015-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-06-04', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 08:10', tz='UTC') ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode data during buoy move
# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-30', tz='UTC') & datetime<as.POSIXct('2015-12-31', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-31', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-12-30 8:50', tz='UTC') & datetime<as.POSIXct('2015-12-30 20:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-12-31 5:40', tz='UTC') & datetime<as.POSIXct('2015-12-31 7:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  gather(variable, value, -datetime, -location)


# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_vert_wind_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean',
       x=NULL,
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(buoy2015_vert_wind, buoy2015_vert_wind_L1)

# ####chla data ####
# buoy2015_vert_chla <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# #recode cond readings to NA for enitre year
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate(SpecCond = NA_real_) %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             funs(case_when(. == -6999 ~ NA_real_,
#                            . == 6999 ~ NA_real_,
#                            TRUE ~ .)))
# 
# buoy2015_vert_chla <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(buoy2015_vert_chla, aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 month')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime< as.POSIXct('2015-07-01', tz='UTC'))),
# #               aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode data before move to loon
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             funs(case_when(datetime< as.POSIXct('2015-06-11 10:10', tz='UTC') ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime == as.POSIXct('2015-06-11 10:10', tz='UTC') ~ 'cp',
#                                TRUE ~ ''))
# 
# #recode - data as 0
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate(Chlor_UGL = case_when(Chlor_UGL < 0 ~ 0,
#                                TRUE ~ Chlor_UGL))
# 
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime< as.POSIXct('2015-07-01', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime< as.POSIXct('2015-08-01', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #recode outlier on jul 16 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-07-16', tz='UTC') & datetime< as.POSIXct('2015-07-17', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             funs(case_when(datetime==as.POSIXct('2015-07-16 21:20', tz='UTC') ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-07-23', tz='UTC') ~ 's',
#                                TRUE ~ chla_flag))
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime< as.POSIXct('2015-08-01', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime< as.POSIXct('2015-09-01', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond aug 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-08-13', tz='UTC') & datetime< as.POSIXct('2015-08-14', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             funs(case_when(datetime>=as.POSIXct('2015-08-13 10:10', tz='UTC') ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-08-13 10:10', tz='UTC') ~ '',
#                                TRUE ~ chla_flag))
# 
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -chla_flag)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime< as.POSIXct('2015-09-01', tz='UTC'))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond aug 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(buoy2015_vert_chla_L1, 
#        aes(x=datetime, y = value, color = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond 2015, clean with flags',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_colorblind()
# 
# rm(buoy2015_vert_chla, buoy2015_vert_chla_L1)

#### air temp ####
ggplot(buoy2015_L1,
       aes(x=datetime, y=AirTempC, color = location)) +
  geom_point() +
  labs(title = '2015 air temp', x=NULL, y='Air temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jan 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'feb 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'mar 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'apr 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #data gap
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-04', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode data prior to data columns being fixed and data during buoy move
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(AirTempC = case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 08:00', tz='UTC') ~ NA_real_,
                              location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jul 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'aug 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'sept 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'oct 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'nov 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'dec 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')


#### EXPORT L1 DATA ####
colnames(buoy2015_L1)
#add offline time and wipe flags during that time
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-04-22 10:20', tz='UTC') & datetime<as.POSIXct('2015-06-01 13:00', tz='UTC') ~ 'offline',
                              TRUE ~ location))%>% 
  mutate_at(vars(upper_do_flag,lower_do_flag, PAR_flag),
            funs(case_when(location == 'offline' ~ NA_character_,
                           TRUE ~ .)))
  
#export L1 tempstring file
buoy2015_L1 %>%
  select(datetime, alltemp2011.5, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_tempstring_L1.csv')

#export l1 do file
buoy2015_L1 %>%
  select(datetime, upDO, lowDO, upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2015_do_L1.csv')

#export l1 par file
buoy2015_L1 %>%
  select(datetime, PAR, location, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2015_PAR_L1.csv')

#export l1 wind
buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2015_wind_L1.csv')

#export l1 air temp file
buoy2015_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2015_airtemp_L1.csv')

# #export l1 chla cond file
# buoy2015_L1 %>%
#   select(datetime, chla, location, chla_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_chla_L1.csv')




