#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2012.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2012 buoy raw data
buoy2012_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2012_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'DOSat', 'DOppm', 'DOSat2',
                                      'PAR', 'RH', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m',
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'InstWindDir', 'AveWindDir',
                                      'AveWindSp', 'InstWindSp'),
                        col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                        skip=1) %>%
  select(-DOSat2, -AveWindDir, -AveWindSp, -TempC_0p5m, -TempC_1p5m, -TempC_2p5m, -TempC_10m, -TempC_11m, -TempC_13m) %>%  #drop redundant columns
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%Y %H:%M', tz='UTC'))



#create dummy timestamp so there are no blanks
alltimes_2012 <- as.data.frame(seq.POSIXt(as.POSIXct('2012-01-01 00:00', tz='UTC'), as.POSIXct('2012-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2012_L1 <- buoy2012_L0 %>% 
  right_join(., alltimes_2012) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2012 <- buoy2012_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2012$`length(datetime)`)
min(datelength2012$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(datelength2012, alltimes_2012)


#### thermistors ####
buoy2012_temp_vert <- buoy2012_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2012 buoy temp, raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = 'loon')

buoy2012_temp_vert <- buoy2012_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

ggplot(subset(buoy2012_temp_vert,
              subset=(datetime >=as.POSIXct('2012-01-01', tz='UTC') &
                        datetime < as.POSIXct('2013-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2012 buoy temp, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #April 1, buoy moved to loon, temp online apr 18
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-04-18', tz='UTC') &
#                         datetime < as.POSIXct('2012-04-19', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(0,20)) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2012-04-18 15:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2012_temp_vert_b <- buoy2012_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2012-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='June 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='July 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# #Oct 10 temp lines off
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-10-10', tz='UTC') &
#                         datetime < as.POSIXct('2012-10-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime >= as.POSIXct('2012-10-10 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2012_temp_vert_b <- buoy2012_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2012-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
ggplot(subset(buoy2012_temp_vert_b,
              subset=(datetime >=as.POSIXct('2012-01-01', tz='UTC') &
                        datetime < as.POSIXct('2013-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2012 buoy temp, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#correct sensors for offset
buoy2012_L1 <- buoy2012_L1 %>% 
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


#clean up workspace
rm(buoy2012_temp_vert, buoy2012_temp_vert_b)


#### DO sensors ####
range(buoy2012_L1$DOSat, na.rm=T)
range(buoy2012_L1$DOppm, na.rm=T)
range(buoy2012_L1$DOTempC, na.rm=T)

do_vert <- buoy2012_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-01-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-02-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-03-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant sat and ppm data apr 12
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-12', tz='UTC') &
#                           datetime < as.POSIXct('2012-04-13', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #apr 18 buoy moved to loon
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-18', tz='UTC') &
#                           datetime < as.POSIXct('2012-04-19', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2012-04-12 12:20', tz='UTC') &
                             datetime < as.POSIXct('2012-04-12 14:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2012-04-18 7:50', tz='UTC') &
                             datetime < as.POSIXct('2012-04-18 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime < as.POSIXct('2012-04-18 7:50', tz='UTC') ~'harbor',
                              datetime >= as.POSIXct('2012-04-18 7:50', tz='UTC') &
                                datetime < as.POSIXct('2012-04-18 10:00', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2012-04-18 10:00', tz='UTC') ~'loon',
                              TRUE ~ location))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-06-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-07-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-08-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-09-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy move oct18
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-18', tz='UTC') &
#                           datetime < as.POSIXct('2012-10-19', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #DO back online on 22nd
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-22', tz='UTC') &
#                           datetime < as.POSIXct('2012-10-23', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2012-10-18 9:10', tz='UTC') &
                             datetime < as.POSIXct('2012-10-22 10:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime >= as.POSIXct('2012-10-18 9:10', tz='UTC') & 
                                datetime < as.POSIXct('2012-10-18 10:00', tz='UTC')~ 'in transit',
                              datetime >= as.POSIXct('2012-10-18 10:00', tz='UTC') ~'harbor',
                              TRUE ~ location))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-11-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-12-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

#add presumed cleaning flags
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2012-04-18 10:00', tz='UTC') ~ 'wp',
                                   datetime == as.POSIXct('2012-10-18 10:00', tz='UTC') ~ 'wp',
                                   TRUE ~ NA_character_)) %>% 
  mutate(upper_do_flag = case_when(is.na(upper_do_flag) ~ 'x',
                                   !is.na(upper_do_flag) ~ paste('x', upper_do_flag, sep = ', '),
                                   TRUE ~ upper_do_flag))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>% 
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(do_vert_b, aes(x = datetime, y = value, color = location, shape = upper_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2012-12-19 00:10', tz='UTC') ~ 'offline',
                              TRUE ~ location))

#clean up workspace
rm(do_vert, do_vert_b)


#### wind sensors ####
range(buoy2012_L1$InstWindDir, na.rm = T)
range(buoy2012_L1$InstWindSp, na.rm = T)

wind_vert <- buoy2012_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sesnor frozen jan 13
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-13', tz='UTC') &
#                         datetime<as.POSIXct('2012-01-14', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #sensor frozen jan 27
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-27', tz='UTC') &
#                         datetime<as.POSIXct('2012-01-28', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2012-01-13 9:00', tz='UTC') &
                             datetime<as.POSIXct('2012-01-13 20:10', tz='UTC') &
                             InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2012-01-27 3:40', tz='UTC') &
                             datetime<as.POSIXct('2012-01-27 18:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen feb 24-25
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-02-24 12:00', tz='UTC') &
#                         datetime<as.POSIXct('2012-02-25 12:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2012-02-24 21:40', tz='UTC') &
                             datetime<as.POSIXct('2012-02-25 9:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen mar 3
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-03', tz='UTC') &
#                         datetime<as.POSIXct('2012-03-04', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# # sensor frozen mar 10
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-10', tz='UTC') &
#                         datetime<as.POSIXct('2012-03-11', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2012-03-03 8:00', tz='UTC') &
                             datetime<as.POSIXct('2012-03-03 12:00', tz='UTC') &
                             InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2012-03-10 7:20', tz='UTC') &
                             datetime<as.POSIXct('2012-03-10 9:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #april 18 buoy moved 8:30
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-04-18', tz='UTC') &
#                         datetime<as.POSIXct('2012-04-19', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-06-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #oct 18 buoy moved to harbor 910a
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-10-18', tz='UTC') &
#                         datetime<as.POSIXct('2012-10-19', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant data at end of dec - 17-18
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-12-17', tz='UTC') &
#                         datetime<as.POSIXct('2012-12-18 12:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2012-12-17', tz='UTC') &
                             datetime<as.POSIXct('2012-12-17 14:00', tz='UTC') &
                             InstWindSp == 0~ NA_real_,
                           datetime>=as.POSIXct('2012-12-17 16:00', tz='UTC') &
                             datetime<as.POSIXct('2012-12-18 3:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#recode times when buoy moved
buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime, -location)


ggplot(wind_vert_b, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

#### PAR sensors ####
range(buoy2012_L1$PAR, na.rm = T)

# ggplot(buoy2012_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2012_L1 <-  buoy2012_L1 %>%
  mutate(PAR_flag = case_when(PAR <0 ~ 'z',
                         TRUE ~ NA_character_)) %>%
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2012_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2012 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-03-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-04-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-04-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-05-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to loon apr 18
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-05-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-06-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-06-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-07-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-07-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-08-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-08-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-09-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-09-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-10-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-10-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-11-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 18
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-11-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-12-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-12-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#recode times when buoy moved
buoy2012_L1 <- buoy2012_L1 %>%
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

ggplot(buoy2012_L1,
       aes(x = datetime, y = PAR, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2012 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')


#### Air Temp ####
range(buoy2012_L1$AirTempC)

ggplot(buoy2012_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2012 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#looks good

#recode times when buoy moved
buoy2012_L1 <- buoy2012_L1 %>%
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2012_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2012 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')




#### EXPORT L1 FILES ####
colnames(buoy2012_L1)
#recode flags to blank when buoy offline
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(upper_do_flag = case_when(location == 'offline' ~ NA_character_,
                                   location == 'in transit' ~ NA_character_,
                                   TRUE ~ upper_do_flag)) %>% 
  mutate(PAR_flag = case_when(location == 'offline' ~ NA_character_,
                              location == 'in transit' ~ NA_character_,
                              TRUE ~ PAR_flag))


#export L1 tempstring file
buoy2012_L1 %>%
  select(datetime, location, TempC_0p5m:TempC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2012_tempstring_L1.csv')

# export L1 do file
buoy2012_L1 %>%
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2012_do_L1.csv')

# export L1 wind file
buoy2012_L1 %>%
  select(datetime, location, InstWindDir, InstWindSp) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(WindSp_ms = 'InstWindSp',
         WindDir_deg = 'InstWindDir') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2012_wind_L1.csv')

# export L1 PAR file
buoy2012_L1 %>%
  select(datetime, location, PAR, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(PAR_umolm2s = 'PAR') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2012_PAR_L1.csv')

# export L1 air temp file
buoy2012_L1 %>%
  select(datetime, location, AirTempC) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(AirTemp_degC = 'AirTempC') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2012_AirTemp_L1.csv')
