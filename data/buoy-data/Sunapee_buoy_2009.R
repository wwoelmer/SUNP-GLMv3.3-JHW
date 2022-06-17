#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2009.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.5.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#* LAST MODIFIED: 05Sept2019 to create vertical dataset for      *
#*          master collation                                     *
#*****************************************************************

source('library_func_lists.R')

#bring in 2009 buoy raw data
buoy2009_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2009_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2', 
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m', 
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m', 
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir', 
                                      'InstWindSp', 'AveWindSp'), 
                        col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnn',
                        skip=1) %>% 
  select(-DOSat2) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))

#create dummy timestamp so there are no blanks
alltimes_2009 <- as.data.frame(seq.POSIXt(as.POSIXct('2009-01-01 00:00', tz='UTC'), as.POSIXct('2009-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2009_L1 <- buoy2009_L0 %>% 
  right_join(., alltimes_2009) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2009 <- buoy2009_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2009$`length(datetime)`)
min(datelength2009$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(alltimes_2009, datelength2009)


#### thermistors ####
buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009 buoy temp raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", 
#                               "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == -99.9 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon',
         temp_flag = NA_character_)

buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2009_temp_vert,
              subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
                        datetime < as.POSIXct('2010-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=(variable))) +
  geom_point() +
  final_theme +
  labs(title='2009 buoy temp NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(temp_flag = case_when(datetime < as.POSIXct('2009-01-15', tz='UTC') & is.na(temp_flag) ~ 'i,9.5d, 10.5d, 11.5d, 13.5d',
                               TRUE ~ temp_flag))
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #data gap until jul
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #look at beginning of record
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz='UTC') &
#                         datetime < as.POSIXct('2009-07-29', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-24', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-08', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-08', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-15', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# #anomalous points Aug 11,13 and 24
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-11', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-12', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-12', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-13', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-24', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-25', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-31', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-01 12:00', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-02 12:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-26', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-27', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-31', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(TempC_0p5m:TempC_4m), #temp shifts
            funs(case_when(is.na(TempC_5m) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_6m:TempC_13m),
            funs(case_when(TempC_13m > TempC_11m & datetime >= as.POSIXct('2009-07-28', tz='UTC') & datetime < as.POSIXct('2009-10-02 12:00', tz='UTC')~ NA_real_, # temp shifts
                           TempC_13m > TempC_6m & datetime >= as.POSIXct('2009-07-28', tz='UTC') & datetime < as.POSIXct('2009-10-02 12:00', tz='UTC')~ NA_real_, # temp shifts
                           TRUE ~ .))) %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2009-02-01', tz='UTC') & datetime < as.POSIXct('2009-07-28', tz='UTC') ~ NA_real_,
                           TempC_8m < 7.5 & datetime > as.POSIXct('2009-08-01', tz='UTC') & datetime < as.POSIXct('2009-09-01', tz='UTC') ~ NA_real_, #temp shifts
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_8m:TempC_11m),
            funs(case_when(is.na(TempC_13m)~ NA_real_, #temp shifts
                           TRUE ~ .))) %>% 
  mutate(TempC_13m = case_when(is.na(TempC_1m) ~ NA_real_, #temp shifts
                               TRUE ~ TempC_13m)) %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2009-08-31 11:40', tz='UTC') ~ NA_real_, 
                           TRUE ~ .)))
  
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Nov 16
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-11-16', tz='UTC') &
#                         datetime < as.POSIXct('2009-11-17', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2009-11-16 6:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# # #Dec 8, 12, 17, 23/24
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-08', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-09', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-12', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-17', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-21', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-22', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-23', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-24', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-24', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-25', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-29', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2009-12-08 23:20', tz='UTC') ~ NA_real_, #anamolous shift
                           datetime >= as.POSIXct('2009-12-12 4:50', tz='UTC') & datetime < as.POSIXct('2009-12-12 9:00', tz='UTC') ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-17 4:00', tz='UTC') & datetime < as.POSIXct('2009-12-17 5:20', tz='UTC') ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-17 12:20', tz='UTC') & datetime < as.POSIXct('2009-12-17 15:00', tz='UTC') ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-21 19:30', tz='UTC') & datetime < as.POSIXct('2009-12-22 6:10', tz='UTC') ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-22 19:00', tz='UTC') & datetime < as.POSIXct('2009-12-22 22:00', tz='UTC') ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-29 9:50', tz='UTC') & datetime < as.POSIXct('2009-12-29 11:20', tz='UTC') ~ NA_real_, #flatline
                           is.na(TempC_1p5m) ~ NA_real_, #anamolous shift
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(temp_flag = case_when(!is.na(temp_flag) & datetime>=as.POSIXct('2009-12-12', tz='UTC') ~ paste(temp_flag, 'i', sep = ', '),
                               is.na(temp_flag) & datetime>=as.POSIXct('2009-12-12', tz='UTC') ~ 'i',
                               TRUE ~ temp_flag))

#add flag for 11.5 and 13.5 as possibly in sediment
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(temp_flag = case_when(is.na(temp_flag) ~ '11.5b, 13.5b',
                               !is.na(temp_flag) ~ paste(temp_flag, '11.5b, 13.5b', sep = ', '),
                               TRUE ~ temp_flag))
unique(buoy2009_L1$temp_flag)

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007, temp_flag) %>%
  gather(variable, value, -location, -datetime, -temp_flag) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2009_temp_vert_b,
              subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
                        datetime < as.POSIXct('2010-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=(variable), shape = temp_flag)) +
  geom_point() +
  final_theme +
  labs(title='2009, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#add in offline location
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2009-01-01 7:50', tz='UTC') ~ 'offline',   
                              datetime >= as.POSIXct('2009-01-14 18:00', tz='UTC') & 
                                datetime < as.POSIXct('2009-02-06 11:10', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2009-02-06 12:00', tz='UTC') &
                                datetime < as.POSIXct('2009-02-18 0:10', tz='UTC') ~ 'offline',   
                              datetime >= as.POSIXct('2009-02-19 12:50', tz='UTC') & 
                                datetime < as.POSIXct('2009-02-23 13:00', tz='UTC') ~ 'offline',   
                              datetime >= as.POSIXct('2009-04-15 3:00', tz='UTC') & 
                                datetime < as.POSIXct('2009-07-28 13:30', tz='UTC') ~ 'offline',   
                              datetime >= as.POSIXct('2009-12-31 0:10',tz='UTC') ~ 'offline',
                              TRUE ~ location)) %>% 
  mutate(temp_flag = case_when(location == 'offline' ~ NA_character_,
                               TRUE~temp_flag))

#clean up workspace
rm(buoy2009_temp_vert, buoy2009_temp_vert_b)

#correct thermistor depth for offset
buoy2009_L1 <- buoy2009_L1 %>% 
  rename(TempC_13p5m = TempC_13m,
         TempC_11p5m = TempC_11m,
         TempC_10p5m = TempC_10m,
         TempC_9p5m = TempC_9m,
         TempC_8p5m = TempC_8m,
         TempC_7p5m = TempC_7m,
         TempC_6p5m = TempC_6m,
         TempC_5p5m = TempC_5m,
         TempC_4p5m = TempC_4m,
         TempC_3p5m = TempC_3m,
         TempC_3m = TempC_2p5m,
         TempC_2p5m = TempC_2m,
         TempC_2m = TempC_1p5m,
         TempC_1p5m = TempC_1m,
         TempC_1m = TempC_0p5m,
         TempC_0p5m = TempC_0m)


#### DO sensors ####
range(buoy2009_L1$DOSat, na.rm=T)
range(buoy2009_L1$DOppm, na.rm=T)
range(buoy2009_L1$DOTempC, na.rm=T)

do_vert <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(do_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data NA values recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-01-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#start seeing intermittent readings dec 29 - adding flag of intermittent do data from then throught the end of the month
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(upper_do_flag = case_when(datetime < as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                             TRUE ~ NA_character_))

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-02-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#data before feb 23 recoded to NA
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2009-02-01', tz='UTC') &
                             datetime < as.POSIXct('2009-02-23', tz='UTC') ~ NA_real_,
                             TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2009-02-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-03-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant readings beginning apr 5
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-04-05', tz='UTC') &
#                           datetime < as.POSIXct('2009-04-06', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2009-04-05 1:40', tz='UTC') &
                             datetime < as.POSIXct('2009-04-16', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2009-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-06-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-07-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-08-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-09-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-11-01', tz='UTC') &
#                           datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-12-01', tz='UTC') &
#                           datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

do_vert_b <- buoy2009_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(do_vert_b,
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

# add presumed cleaning flags and possible non-calibration flags
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2009-07-28 13:30', tz='UTC') ~ 'wp',
                             TRUE ~ upper_do_flag)) %>% 
  mutate(upper_do_flag = case_when(!is.na(upper_do_flag) ~ paste('x', upper_do_flag, sep = ', '),
                                   TRUE ~ 'x')) %>% 
  mutate(upper_do_flag = case_when(location == 'offline' ~ NA_character_,
                                   TRUE ~ upper_do_flag))
  
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>% 
  gather(variable, value, -datetime, -location, - upper_do_flag)

ggplot(do_vert_b,
       aes(x = datetime, y = value, shape = location, color = upper_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(do_vert, do_vert_b)


#### wind sensors ####
range(buoy2009_L1$InstWindDir, na.rm = T)
range(buoy2009_L1$InstWindSp, na.rm = T)
range(buoy2009_L1$AveWindDir, na.rm = T)
range(buoy2009_L1$AveWindSp, na.rm = T)

wind_vert <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(wind_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(InstWindDir = case_when(InstWindDir==-6999 ~ NA_real_,
                                 TRUE ~ InstWindDir))

wind_vert <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir),
            funs(case_when(datetime<as.POSIXct('2009-02-18 0:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-06-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'july 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# # look at nov 27-28
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-11-27 12:00', tz='UTC') &
#                         datetime<as.POSIXct('2009-11-28 18:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# odd to have so many 0 readings, but not a sensor frozen issue, leaving in
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #dec 26 sensor frozen
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-12-26', tz='UTC') &
#                         datetime<as.POSIXct('2009-12-27', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #dec 26 sensor frozen
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-12-26 12:00', tz='UTC') &
#                         datetime<as.POSIXct('2009-12-27 12:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2009_L1 <- buoy2009_L1 %>%
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir),
            funs(case_when(datetime>=as.POSIXct('2009-12-26 19:40', tz='UTC') &
                             datetime<as.POSIXct('2009-12-26 21:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add flag to wind direction prior to jul 28 when buoy online
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(wind_dir_flag = case_when(datetime < as.POSIXct('2009-07-28', tz='UTC') & !is.na(InstWindDir) ~ 'e',
                                   TRUE ~ NA_character_))
wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp, location, wind_dir_flag) %>% 
  gather(variable, value, -datetime, -location, -wind_dir_flag)

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = wind_dir_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(wind_vert, wind_vert_b)

#### PAR ####
range(buoy2009_L1$PAR, na.rm = T)

# ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR_flag = case_when(PAR <0 ~ 'z',
                         TRUE ~ NA_character_),
         PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#jan data incomplete. flagging as intermittent
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR_flag = case_when(datetime<as.POSIXct('2009-01-15', tz='UTC') & is.na(PAR_flag) ~ 'i',
                              datetime<as.POSIXct('2009-01-15', tz='UTC') & !is.na(PAR_flag) ~ paste('i', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#incomplete data through feb 23 midday
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR = case_when(datetime>=as.POSIXct('2009-02-01', tz='UTC') & datetime<as.POSIXct('2009-02-17', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-03-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-04-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-05-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-06-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-07-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-08-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-09-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-10-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-11-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                           datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')



#### Air Temp ####
range(buoy2009_L1$AirTempC, na.rm = T)

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# data intermittent - recode Feb intermittent data

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#flag through jan 15 as intermittent
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(airtemp_flag = case_when(datetime < as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                                  TRUE ~ NA_character_))

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2009-02-06', tz='UTC') &
                                datetime < as.POSIXct('2009-02-17', tz='UTC') ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC, color = airtemp_flag)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')



#### EXPORT L1 FILES ####

#recode flags to '' when the buoy is offline
buoy2009_L1 <-buoy2009_L1 %>% 
  mutate_at(vars(temp_flag, upper_do_flag, wind_dir_flag, PAR_flag, airtemp_flag),
            funs(case_when(location == 'offline' ~ NA_character_,
                           TRUE ~ .)))

#export L1 tempstring file
buoy2009_L1 %>%
  select(datetime, location, TempC_0p5m:TempC_13p5m, temp_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2009_tempstring_L1.csv')

# export L1 do file
buoy2009_L1 %>%
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2009_do_L1.csv')

# export L1 wind data
buoy2009_L1 %>%
  select(datetime, location, InstWindDir, InstWindSp, AveWindDir, AveWindSp, wind_dir_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(WindSp_ms = 'InstWindSp',
         WindDir_deg = 'InstWindDir',
         AveWindSp_ms = 'AveWindSp',
         AveWindDir_deg = 'AveWindDir') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2009_wind_L1.csv')

# export PAR data
buoy2009_L1 %>%
  select(datetime, location, PAR, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(PAR_umolm2s = 'PAR') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2009_PAR_L1.csv')

# export L1 air temp data
buoy2009_L1 %>%
  select(datetime, location, AirTempC, airtemp_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(AirTemp_degC = 'AirTempC') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2009_AirTemp_L1.csv')

