# do collation

source('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/programs/library_func_lists.R')

# make sure the years are jan-dec
do2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2007_do_L1.csv',
                    col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2007-01-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))
str(do2007)

do2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2008_do_L1.csv',
                   col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2008-01-01', tz='UTC') & datetime < as.POSIXct('2009-01-01', tz='UTC'))
str(do2008)

do2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2009_do_L1.csv',
                   col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2009-01-01', tz='UTC') & datetime < as.POSIXct('2010-01-01', tz='UTC'))
str(do2009)

do2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2010_do_L1.csv',
                   col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2010-01-01', tz='UTC') & datetime < as.POSIXct('2011-01-01', tz='UTC'))
str(do2010)

do2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2011_do_L1.csv',
                   col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2011-01-01', tz='UTC') & datetime < as.POSIXct('2012-01-01', tz='UTC'))
str(do2011)

do2012 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2012_do_L1.csv',
                   col_types = 'Tcnnnc') %>% 
  filter(datetime >= as.POSIXct('2012-01-01', tz='UTC') & datetime < as.POSIXct('2013-01-01', tz='UTC'))
str(do2012)

do2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2013_do_L1.csv',
                   col_types = 'Tcnnnnnncc') %>% 
  filter(datetime >= as.POSIXct('2013-01-01', tz='UTC') & datetime < as.POSIXct('2014-01-01', tz='UTC'))
str(do2013)

do2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2014_do_L1.csv',
                   col_types = 'Tcnnnnnncc') %>% 
  filter(datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))
str(do2014)

do2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2015_do_L1.csv',
                   col_types = 'Tnnnnnnccc') %>% 
  filter(datetime >= as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))
str(do2015)

do2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2016_do_L1.csv',
                   col_types = 'Tnnnnnnccc') %>% 
  filter(datetime >= as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))
str(do2016)

do2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2017_do_L1.csv',
                   col_types = 'Tnnnnnnccc') %>% 
  filter(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))
str(do2017)

do2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2018_do_L1.csv',
                   col_types = 'Tnnncc') %>% 
  filter(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC')) %>% 
  rename(lower_do_flag = lowDO_flag)
str(do2018)

do2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2019_do_L1.csv',
                   col_types = 'Tnnnnnnccc') %>% 
  filter(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2020-01-01', tz='UTC'))
str(do2019)

do2020 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2020_do_L1.csv',
                   col_types = 'Tnnnnnnccc') %>% 
  filter(datetime >= as.POSIXct('2020-01-01', tz='UTC') & datetime < as.POSIXct('2021-01-01', tz='UTC'))
str(do2020)

####collate buoy temp data together ####
buoy_record_do <- full_join(do2007, do2008) %>% 
  full_join(., do2009) %>% 
  full_join(., do2010) %>% 
  full_join(., do2011) %>% 
  full_join(., do2012) %>% 
  full_join(., do2013) %>% 
  full_join(., do2014) %>% 
  full_join(., do2015) %>% 
  full_join(., do2016) %>% 
  full_join(., do2017) %>% 
  full_join(., do2018) %>% 
  full_join(., do2019) %>% 
  full_join(., do2020) %>% 
  mutate_at(vars(upper_do_flag, lower_do_flag), #clear flags where buoy is in transit or offline
            ~ case_when(location == 'in transit' ~ NA_character_,
                           location == 'offline' ~ NA_character_,
                           TRUE ~ .)) %>% 
  arrange(datetime) 
unique(buoy_record_do$upper_do_flag)
unique(buoy_record_do$lower_do_flag)

#clean up flags for eml, otherwise it thinks there are multiple columns in the flag column
buoy_record_do <- buoy_record_do %>% 
  mutate(upper_do_flag = gsub(pattern = ', ', replace = '', x = upper_do_flag),
         lower_do_flag = gsub(pattern = ', ', replace = '', x = lower_do_flag), 
         upper_do_flag = case_when(upper_do_flag == 'xi' ~ 'ix',
                                   is.na(upper_do_flag) ~ '',
                                   TRUE ~ upper_do_flag),
         lower_do_flag = case_when(is.na(lower_do_flag) ~ '',
                                   TRUE ~ lower_do_flag))
unique(buoy_record_do$upper_do_flag)
unique(buoy_record_do$lower_do_flag)

#clean up location for eml
unique(buoy_record_do$location)
buoy_record_do <- buoy_record_do %>% 
  mutate(location = case_when(location == 'harbor, water sensors offline' ~ 'harbor', 
                              TRUE ~ location))

buoy_record_updo_v <- buoy_record_do %>%
  select(datetime, location, upper_do_flag, DOSat, DOppm, DOTempC) %>%
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(buoy_record_updo_v, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ .) +
  final_theme

buoy_record_lodo_v <- buoy_record_do %>%
  select(datetime, location, lower_do_flag, lowDO) %>%
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(buoy_record_lodo_v, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ .) +
  final_theme

buoy_record_ppm <- buoy_record_do %>% 
  select(datetime, DOppm, DOLowPPM) %>% 
  gather(variable, value, -datetime)

ggplot(buoy_record_ppm, aes(x = datetime, y = value, color = variable)) +
  geom_point() +
  final_theme

buoy_record_sat <- buoy_record_do %>% 
  select(datetime, DOSat, DOLowSat) %>% 
  gather(variable, value, -datetime)

ggplot(buoy_record_sat, aes(x = datetime, y = value, color = variable)) +
  geom_point() +
  final_theme

buoy_record_do %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/do/2007-2020_do_L1_v26Feb2021.csv')
