#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                      Weathers Lab                             *
#*                                                               *
#* TITLE:   library_func_lists.R                                 *
#* PROJECT: SunapeeBuoy.RProj                                    *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.6.1, RStudio 1.2.5001*
#* DATE:    01Apr2020                                            *
#* PURPOSE: QAQC and collate buoy data                           *
#*****************************************************************

library(tidyverse)
library(ggthemes)
library(readxl)
library(lubridate)

# ##functions
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }

##ggplot grom
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))


##create lists of sensors
alltemp <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
alltemp2011 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
alltemp2007 <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_13m")
upDO <- c("DOTempC", "DOSat", "DOppm")
lowDO <- c("DOLowTempC", "DOLowSat", "DOLowPPM")
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')
hobotemp <- c('TempC_1m_hobo', 'TempC_2m_hobo', 'TempC_3m_hobo', 'TempC_4m_hobo', 'TempC_5m_hobo', 'TempC_6m_hobo', 'TempC_7m_hobo', 'TempC_8m_hobo', 'TempC_9m_hobo')
hobotemp_17 <- c("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m")
air <-  c('AirTempC', 'RelHum')
alltemp2011.5 <- c("TempC_0p5m", "TempC_1p5m", "TempC_2p5m", "TempC_3p5m", "TempC_4p5m", "TempC_5p5m", "TempC_6p5m", "TempC_7p5m", "TempC_8p5m", 
                   "TempC_9p5m")
alltemp2019 <-   c('TempC_0p5m', 'TempC_0p85m', 
                        'TempC_1p5m', 'TempC_1p85m', 
                        'TempC_2p5m', 'TempC_2p85m', 
                        'TempC_3p5m', 'TempC_3p85m', 
                        'TempC_4p5m', 'TempC_4p85m', 
                        'TempC_5p5m', 'TempC_5p85m', 
                        'TempC_6p5m', 'TempC_6p85m', 
                        'TempC_7p5m', 'TempC_7p85m', 
                        'TempC_8p5m', 'TempC_8p85m', 
                        'TempC_9p5m', 'TempC_9p85m', 
                        'TempC_10p5m', 'TempC_11p5m', 
                        'TempC_13p5m') 
wind <- c('AveWindDir', 'AveWindSp', 'MaxWindDir', 'MaxWindSp')
  

