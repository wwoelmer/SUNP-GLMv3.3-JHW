#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2011.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2011 buoy raw data
buoy2011_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2011_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2',
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m',
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir',
                                      'InstWindSp', 'AveWindSp'),
                        col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnn',
                        skip=1) %>%
  select(-DOSat2, -AveWindDir, -AveWindSp, -TempC_0p5m, -TempC_1p5m, -TempC_2p5m, -TempC_10m, -TempC_11m, -TempC_13m) %>%  #drop redundant or blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))

#create dummy timestamp so there are no blanks
alltimes_2011 <- as.data.frame(seq.POSIXt(as.POSIXct('2011-01-01 00:00', tz='UTC'), as.POSIXct('2011-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2011_L1 <- buoy2011_L0 %>% 
  right_join(., alltimes_2011) %>% 
  arrange(datetime)

#double check to make sure there are no DST issues
datelength2011 <- buoy2011_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2011$`length(datetime)`)
min(datelength2011$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(datelength2011, alltimes_2011)


#### thermistors ####
buoy2011_temp_vert <- buoy2011_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2011 buoy temp data, raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2011_L1 <- buoy2011_L1 %>%
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) 

buoy2011_temp_vert <- buoy2011_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
# geom_point() +
# final_theme +
#   labs(title='2011 buoy temp data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
# scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                             "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #buoy deployed May 12
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-05-12', tz='UTC') &
#                         datetime < as.POSIXct('2011-05-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2011-05-12 10:40', tz='UTC') ~NA_real_,
                           TRUE ~ .))) 

buoy2011_temp_vert_b <- buoy2011_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2011_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2011-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='June 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='July 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2011_temp_vert_b,
              subset=(datetime >=as.POSIXct('2011-01-01', tz='UTC') &
                        datetime < as.POSIXct('2012-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2011, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
 
#correct sensors for offset
buoy2011_L1 <- buoy2011_L1 %>% 
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
rm(buoy2011_temp_vert, buoy2011_temp_vert_b)


#### DO sensors ####
range(buoy2011_L1$DOSat, na.rm=T)
range(buoy2011_L1$DOppm, na.rm=T)
range(buoy2011_L1$DOTempC, na.rm=T)

do_vert <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 DO data NA values recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2011-01-01', tz='UTC') & 
#                           datetime < as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #do sensor back online jan 3
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2011-01-03', tz='UTC') & 
#                           datetime < as.POSIXct('2011-01-04', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2011-01-03', tz='UTC') &
                             datetime < as.POSIXct('2011-01-03 12:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'harbor')

do_vert_b <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-01-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-02-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-03-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-04-02', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar/apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2011-04-01 9:50', tz='UTC') &
                                datetime < as.POSIXct('2011-04-01 10:40', tz='UTC') ~'in transit',
                              datetime >= as.POSIXct('2011-04-01 10:40', tz='UTC') ~'loon',
                              TRUE ~ location))
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-03-15', tz='UTC') &
#                           datetime < as.POSIXct('2011-04-15', tz='UTC'))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar/apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #thermister line installed may 12
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-05-12', tz='UTC') &
#                           datetime < as.POSIXct('2011-05-13', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2011-05-12 9:10', tz='UTC') &
                             datetime < as.POSIXct('2011-05-12 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-06-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-07-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-08-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-09-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 26
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-10-26', tz='UTC') &
#                           datetime < as.POSIXct('2011-10-27', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2011-10-26 6:40', tz='UTC') &
                             datetime < as.POSIXct('2011-10-26 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2011-10-26 6:40', tz='UTC') &
                                datetime < as.POSIXct('2011-10-26 10:50', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2011-10-26 10:50', tz='UTC') ~'harbor',
                              TRUE ~ location))

do_vert_b <- buoy2011_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-11-01', tz='UTC') &
#                           datetime < as.POSIXct('2011-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-12-01', tz='UTC') &
#                           datetime < as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

#add presumed cleaning flags
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2011-04-01 10:40', tz='UTC') ~ 'wp',
                                   datetime == as.POSIXct('2011-05-12 10:30', tz='UTC') ~ 'wp',
                                   datetime == as.POSIXct('2011-10-26 10:50', tz='UTC') ~ 'wp',
                                   TRUE ~ NA_character_)) %>% 
  mutate(upper_do_flag = case_when(!is.na(upper_do_flag) ~ paste('x', upper_do_flag, sep = ', '),
                                   is.na(upper_do_flag) ~ 'x',
                                   TRUE ~ upper_do_flag))
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>% 
  gather(variable, value, -datetime, -location, - upper_do_flag)


ggplot(do_vert_b, aes(x = datetime, y = value, color = location, shape = upper_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(do_vert, do_vert_b)


#### wind sensors ####
range(buoy2011_L1$InstWindDir, na.rm = T)
range(buoy2011_L1$InstWindSp, na.rm = T)

wind_vert <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # frozen sesors jan 18-22 noon
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-01-18', tz='UTC') &
#                         datetime<as.POSIXct('2011-01-23', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2011-01-18 12:00', tz='UTC') &
                             datetime<as.POSIXct('2011-01-22 12:00', tz='UTC') &
                             InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen on feb 05-06
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-02-05', tz='UTC') &
#                         datetime<as.POSIXct('2011-02-07', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2011-02-05 12:00', tz='UTC') &
                             datetime<as.POSIXct('2011-02-06 12:00', tz='UTC') &
                             InstWindSp == 0~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-06-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'june 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor stuck oct 27-28
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-27 12:00', tz='UTC') &
#                         datetime<as.POSIXct('2011-10-28 18:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# # sensor stuck oct 29-30
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-29 12:00', tz='UTC') &
#                         datetime<as.POSIXct('2011-10-30 18:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2011-10-26 10:00', tz='UTC') &
                             datetime<as.POSIXct('2011-10-26 11:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2011-10-27 18:10', tz='UTC') &
                             datetime<as.POSIXct('2011-10-28 11:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2011-10-29 21:30', tz='UTC') &
                             datetime<as.POSIXct('2011-10-30 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # sensor frozen nov 23
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-11-23', tz='UTC') &
#                         datetime<as.POSIXct('2011-11-24', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#     scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2011-11-23', tz='UTC') &
                             datetime < as.POSIXct('2011-11-24', tZ='UTC') &
                             InstWindSp == 0~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2011-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor dec 8
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-12-08', tz='UTC') &
#                         datetime<as.POSIXct('2011-12-09', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(datetime>=as.POSIXct('2011-12-08 3:00', tz='UTC') &
                             datetime < as.POSIXct('2011-12-08 13:40', tZ='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#recode data when buoy in transit
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~.)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, location, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime, -location)

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

#### PAR sensors ####
range(buoy2011_L1$PAR, na.rm = T)

# ggplot(buoy2011_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2011_L1 <-  buoy2011_L1 %>%
  mutate(PAR_flag = case_when(PAR <0 ~ 'z',
                         TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))


ggplot(buoy2011_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2011 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# #looks like par obscured from early-mid jan until early feb - recoding all of that to no
#
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2011_L1 <-  buoy2011_L1 %>%
  mutate(PAR_flag = case_when(datetime >= as.POSIXct('2011-01-12', tz='UTC') &
                           datetime < as.POSIXct('2011-02-14', tz='UTC') ~ 'o',
                         TRUE ~ ''))

# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2011 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-03-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-04-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-04-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-05-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-05-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-06-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-06-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-07-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-07-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-08-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-08-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-09-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-09-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-10-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-10-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-11-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 26 10 a
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-10-26', tz='UTC') &
#                           datetime<as.POSIXct('2011-10-27', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <-  buoy2011_L1 %>%
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))


# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-11-01', tz='UTC') &
#                           datetime<as.POSIXct('2011-12-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-12-01', tz='UTC') &
#                           datetime<as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2011_L1,
       aes(x = datetime, y = PAR , color = location, shape = PAR_flag)) +
  geom_point() +
  final_theme +
  labs(title = '2011 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')




#### Air Temp ####
range(buoy2011_L1$AirTempC)

ggplot(buoy2011_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2011 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-03', tz='UTC') &
#                         datetime < as.POSIXct('2011-01-04', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(AirTempC = case_when(datetime < as.POSIXct('2011-01-03 12:10') ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp clean',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#recode during buoy move
buoy2011_L1 <-  buoy2011_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2011_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2011 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')


#### add buoy location as offline for prior to jan 3 and on dec 31 ####
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2011-01-03 11:00', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2011-12-31', tz='UTC') ~ 'offline',
                              TRUE ~ location))


#### EXPORT L1 DATA ####
colnames(buoy2011_L1)
buoy2011_L1 <-buoy2011_L1 %>% 
  mutate_at(vars(upper_do_flag, PAR_flag),
            funs(case_when(location == 'offline' ~ '',
                             TRUE ~ .)))

#export L1 tempstring file
buoy2011_L1 %>%
  select(datetime, location, TempC_0p5m:TempC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2011_tempstring_L1.csv')

# export L1 DO file
buoy2011_L1 %>%
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2011_do_L1.csv')

# export L1 wind file
buoy2011_L1 %>%
  select(datetime, location, InstWindDir, InstWindSp) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(WindSp_ms = 'InstWindSp',
         WindDir_deg = 'InstWindDir') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2011_wind_L1.csv')

# export L1 PAR file
buoy2011_L1 %>%
  select(datetime, location, PAR, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(PAR_umolm2s = 'PAR') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2011_PAR_L1.csv')

# export L1 air temp file
buoy2011_L1 %>%
  select(datetime, location, AirTempC) %>%
  mutate(datetime = as.character(datetime)) %>%
  rename(AirTemp_degC = 'AirTempC') %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2011_AirTemp_L1.csv')

