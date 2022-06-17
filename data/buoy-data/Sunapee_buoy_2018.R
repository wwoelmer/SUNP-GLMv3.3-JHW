#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2018.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    30Oct2018                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2018 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

source('library_func_lists.R')

#bring in  buoy raw data
buoy2018_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2018 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

#### format data ####
buoy2018_L0 <- buoy2018_L0  %>%
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
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>%  #remove unnecessary columns
  rownames_to_column(var ='rowid')

# add in all date time options in L1 data set
alltimes_2018 <- as.data.frame(seq.POSIXt(as.POSIXct('2018-01-01 00:00', tz='UTC'), as.POSIXct('2018-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2018_L1 <- buoy2018_L0 %>% 
  right_join(., alltimes_2018) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2018 <- buoy2018_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2018$`length(datetime)`)
min(datelength2018$`length(datetime)`)
#should only be 144 or less if partial days included

#dst observed
buoy2018_L1a <- buoy2018_L1 %>% 
  filter(datetime < as.POSIXct('2018-03-11 23:00:00', tz='UTC'))

buoy2018_L1b <- buoy2018_L1 %>% 
  filter(datetime >= as.POSIXct('2018-03-12 00:00', tz='UTC') & rowid < 44335) %>% 
  right_join(alltimes_2018) %>% 
  filter(datetime >= as.POSIXct('2018-03-12 00:00', tz='UTC') & datetime < as.POSIXct('2018-11-05 1:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2018b <- as.data.frame(seq.POSIXt(as.POSIXct('2018-03-11 23:00:00', tz='UTC'), as.POSIXct('2018-11-04 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2018_L1b <- full_join(buoy2018_L1b, alltimes_2018b)

#2018-11-16
buoy2018_L1c <- buoy2018_L1 %>% 
  filter(rowid >= 44335)  %>% 
  right_join(alltimes_2018) %>% 
  filter(datetime >= as.POSIXct('2018-11-05 00:00', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
alltimes_2018c <- as.data.frame(seq.POSIXt(as.POSIXct('2018-11-05 00:00', tz='UTC'), as.POSIXct('2018-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2018_L1c <- full_join(buoy2018_L1c, alltimes_2018c)

buoy2018_L1 <- full_join(buoy2018_L1a, buoy2018_L1b) %>% 
  full_join(., buoy2018_L1c) %>% 
  select(-rowid, -rowid2)

#double check to make sure there are no DST issues
datelength2018 <- buoy2018_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2018$`length(datetime)`)
min(datelength2018$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(alltimes_2018, datelength2018, alltimes_2018b, alltimes_2018c, buoy2018_L1a, buoy2018_L1b, buoy2018_L1c)


####THERMISTORS####
buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_therm_vert, aes(x=datetime, y=value, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme

# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy deployment
# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2018-05-21 8:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

#look at 2m temp alone - looks like it is only reporting integers
unique(buoy2018_L1$TempC_2m)

#remove all 2m data - only integers reported
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(TempC_2m = NA_real_)
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy moved for winter
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(temp_flag = case_when(datetime >= as.POSIXct('2018-05-21 8:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 9:30', tz='UTC') ~ '0.85a',
                          TRUE ~ NA_character_))

buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


# ggplot(buoy2018_therm_vert_L1,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#correct colnames for offset of sensors from surface
buoy2018_L1 <- buoy2018_L1 %>% 
  rename(TempC_9p85m = 'TempC_9m',
         TempC_8p85m = 'TempC_8m',
         TempC_7p85m = 'TempC_7m',
         TempC_6p85m = 'TempC_6m',
         TempC_5p85m = 'TempC_5m',
         TempC_4p85m = 'TempC_4m',
         TempC_3p85m = 'TempC_3m',
         TempC_2p85m = 'TempC_2m',
         TempC_1p85m = 'TempC_1m',
         TempC_0p85m = 'TempC_0m')

rm(buoy2018_therm_vert, buoy2018_therm_vert_L1)



#### DO ####
buoy2018_do_vert <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOTempC = NA_real_,
         DOSat = NA_real_,
         DOppm = NA_real_)

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#buoy move 5-21
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime < as.POSIXct('2018-05-21 09:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-05-21 8:30', tz='UTC') ~ 'loon',
                              TRUE ~ NA_character_)) %>% 
  mutate(lowDO_flag = case_when(datetime == as.POSIXct('2018-05-21 9:00', tz='UTC') ~ 'cp',
                                TRUE ~ NA_character_))

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)


# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #june 20-26 do errant
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-20', tz='UTC') & datetime < as.POSIXct('2018-06-21', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-26', tz='UTC') & datetime < as.POSIXct('2018-06-27', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOLowTempC = case_when(datetime == as.POSIXct('2018-06-20 7:10', tz='UTC') ~ NA_real_,
                                datetime == as.POSIXct('2018-06-26 7:50', tz='UTC') ~ NA_real_,
                                TRUE ~ DOLowTempC)) %>% 
  mutate(lowDO_flag =case_when(datetime >= as.POSIXct('2018-06-20 7:10', tz='UTC') & datetime < as.POSIXct('2018-06-26 8:30', tz='UTC') ~ paste('re', lowDO_flag, sep = ', '),
                                TRUE ~ lowDO_flag)) %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            funs(case_when(datetime >= as.POSIXct('2018-06-20 7:10', tz='UTC') & datetime < as.POSIXct('2018-06-26 8:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #oddball 9/10
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-10', tz='UTC') & datetime < as.POSIXct('2018-09-11', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2018-09-10 18:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #buoy to harbor 10-19
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(lowDO_flag = case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz='UTC') ~ NA_character_,
                                TRUE ~ lowDO_flag))

buoy2018_do_vert_L1 <- buoy2018_L1 %>%
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_do_vert_L1,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2018, clean') +
  final_theme +
  scale_color_colorblind()

rm(buoy2018_do_vert, buoy2018_do_vert_L1)


####CHLA####
# buoy2018_chla_vert <- buoy2018_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
#
# ggplot(buoy2018_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#chla sensor not working
buoy2018_L1 <- buoy2018_L1 %>%
  mutate(Chlor_RFU = is.na(Chlor_RFU),
         Chlor_UGL = is.na(Chlor_UGL),
         SpecCond = is.na(SpecCond))



####wind####
buoy_wind_vert <- buoy2018_L1 %>%
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime)

# ggplot(buoy_wind_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2018, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 17-18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-17 12:00', tz='UTC') & datetime < as.POSIXct('2018-01-18 16:00', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 23
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-23', tz='UTC') & datetime < as.POSIXct('2018-01-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-01-17 13:40', tz='UTC') & datetime < as.POSIXct('2018-01-18 14:30', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-01-23 6:10', tz='UTC') & datetime < as.POSIXct('2018-01-23 17:50', tz='UTC')& MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 2
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-02', tz='UTC') & datetime < as.POSIXct('2018-02-03', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-18', tz='UTC') & datetime < as.POSIXct('2018-02-19', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-02-02 4:30', tz='UTC') & datetime < as.POSIXct('2018-02-02 12:50', tz='UTC')  & MaxWindSp == 0  ~ NA_real_,
                           datetime >= as.POSIXct('2018-02-18 4:30', tz='UTC') & datetime < as.POSIXct('2018-02-18 10:30', tz='UTC') & MaxWindSp == 0  ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #mar08 - mar10
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-08', tz='UTC') & datetime < as.POSIXct('2018-03-09', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-10', tz='UTC') & datetime < as.POSIXct('2018-03-11', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #mar13 - mar14
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-13 12:00', tz='UTC') & datetime < as.POSIXct('2018-03-14 16:00', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-03-08 5:00', tz='UTC') & datetime < as.POSIXct('2018-03-10 14:30', tz='UTC')  & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-03-13 20:20', tz='UTC') & datetime < as.POSIXct('2018-03-14 13:20', tz='UTC')  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
#
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 21 buoy moved to loon
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2018-05-21 7:40', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2018-05-21 7:40', tz='UTC') & datetime < as.POSIXct('2018-05-21 8:30', tz='UTC') ~ 'in transit',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'june wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'july wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'august wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 19 buoy to harbor
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 10:30', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2018-10-19 10:30', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozen sensors nov 20-22
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-20', tz='UTC') & datetime < as.POSIXct('2018-11-21', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-22', tz='UTC') & datetime < as.POSIXct('2018-11-23', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozen sensors nov 26-28
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-26', tz='UTC') & datetime < as.POSIXct('2018-11-27', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-28', tz='UTC') & datetime < as.POSIXct('2018-11-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-11-20 7:00', tz='UTC') & datetime < as.POSIXct('2018-11-22 9:50', tz='UTC')  & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-11-26 23:10', tz='UTC') & datetime < as.POSIXct('2018-11-28 13:00', tz='UTC')  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen dec 28-29
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-28', tz='UTC') & datetime < as.POSIXct('2018-12-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-29', tz='UTC') & datetime < as.POSIXct('2018-12-30', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-12-28 13:00', tz='UTC') & datetime < as.POSIXct('2018-12-29 2:40', tz='UTC')  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)


buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(buoy_wind_vert_L1,
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2018, clean') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
# ggplot(buoy2018_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                              TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 
ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# # 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#add flag for nighttime PAR errors begininig Jul 23
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(PAR_flag = case_when(datetime >= as.POSIXct('2018-07-23', tz= 'UTC') & is.na(PAR_flag) ~ 'n',
                              datetime >= as.POSIXct('2018-07-23', tz= 'UTC') & !is.na(PAR_flag) ~ paste('n', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))%>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 

ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#### Air temp ####
ggplot(buoy2018_L1,
       aes(x=datetime, y=AirTempC, color=location)) +
  geom_point() +
  labs(title = 'air temp 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

# ggplot(buoy2018_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()


#### EXPORT L1 DATA STREAMS ####
#export L1 tempstring file
buoy2018_L1 %>%
  select(datetime, TempC_0p85m:TempC_9p85m, temp_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2018_tempstring_L1.csv')

#export l1 do file
buoy2018_L1 %>%
  select(datetime, lowDO, lowDO_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2018_do_L1.csv')

#export l1 par file
buoy2018_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2018_PAR_L1.csv')

#export l1 wind
buoy2018_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2018_wind_L1.csv')

#export l1 air temp file
buoy2018_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2018_airtemp_L1.csv')



