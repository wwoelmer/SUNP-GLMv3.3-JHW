# run watershed model to simulate inflows to sunapee
# modified from script written by Bethel Steele

#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   CNH-GLM_sunapee_inflowoutflow_23Mar2017.r            *
#* AUTHOR:  B. Steele                                            *
#* SYSTEM:  Lenovo W530, Win 7, R 3.2.2                          *
#* DATE:    23Mar2017                                            *
#* PROJECT: CNH-GLM                                              *
#* PURPOSE: using previously-truthed methods model inflows for   *
#*          Sunapee based on NLDAS-2 met data and GIS analysis   *
#*          of impervious surfaces, calculate baseflow from      *
#*          residence time and daily volume of the lake,         *
#*          model in-stream temperature based on transducers and *
#*          NLDAS-2 met data                                     *
#* LAST MODIFIED: 23Mar2017                                      *
#* BY:      B. Steele                                            *
#* NOTES:   this is a compilation of code from the R files       *
#*          LS_streaminflow_21Mar2017 and LS_GLM_21Mar2017       *
#* UPDATES: Met data through 2016 added, streamlined to          *
#*          calculate inflow and outflow in a single file        *
#*****************************************************************

library(gdata)
library(doBy)
library(ggplot2)
library(GGally)
library(reshape)
library(tidyverse)

#add final theme for formatting ggplots
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=16, face='bold'))

#load workspace. CNH-GLM_sunapee_inflowoutflow_begwrkspc has all the original data frames resulting from the 'read.csv' and 'read.xls' functions
#change this to the appropriate working directory
load("./scripts/create_inflow_files_sunapee/CNH-GLM_sunapee_inflowoutflow_begwrkspc.RData")

#### bring in NLDAS-2 climate data ####
MetData <- read.csv('./NLDASData/NLDAS_Data_2019_2020/Sunapee_2018_12_31_2020_12_31_alldata.csv', header=T, na.strings=NA) 
MetData <- MetData %>%   
  select(local_dateTime, ShortWave.W_m2, LongWave.W_m2, AirTemp.C, RelHum, WindSpeed.m_s, Rain.m_day)
colnames(MetData) <- c("time", 'ShortWave', "LongWave", "AirTemp", "RelHum", "WindSpeed", "Rain")

MetData$dateTime <- as.POSIXct(MetData$time, format='%Y-%m-%d %H:%M:%S') #format date 
MetData$AirTemp.F <- MetData$AirTemp*1.8 + 32 #convert to F
MetData$Rain.m_hr <- MetData$Rain/24 #convert to rain in m/hour 

#### summarize GIS land use data to calculate impervious surfaces ####
#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Lake Sunapee Lake Model") #in begwrkspc
#sunLU <- read.xls('GIS/GIS files/subwatershed areas.xlsx', sheet='ws min lake', header=T) #all of sunapee minus lake #in begwrkspc
#subwsLU <- read.xls('GIS/GIS files/subwatershed areas.xlsx', sheet='subwatersheds', header=T, stringsAsFactors=F) #in begwrkspc

#rename columns to match-able headers
subwsLU <- rename.vars(subwsLU, from=c("VALUE_110", "VALUE_140", "VALUE_211", "VALUE_212", "VALUE_412", "VALUE_414", "VALUE_419", 
                                           "VALUE_421", "VALUE_422", "VALUE_423", "VALUE_430", "VALUE_500", "VALUE_610", "VALUE_620", 
                                           "VALUE_710", "VALUE_790"),
                         to=c("110", "140", "211", "212", "412", "414", "419", "421", "422", "423", "430", "500", "610", "620", 
                              "710", "790")) 
sunLU <- rename.vars(sunLU, from=c("VALUE_110", "VALUE_140", "VALUE_211", "VALUE_212", "VALUE_412", "VALUE_414", "VALUE_419", 
                                       "VALUE_421", "VALUE_422", "VALUE_423", "VALUE_430", "VALUE_500", "VALUE_610", "VALUE_620", 
                                       "VALUE_710", "VALUE_790"),
                       to=c("110", "140", "211", "212", "412", "414", "419", "421", "422", "423", "430", "500", "610", "620", 
                            "710", "790")) 

subwsLU_m <- melt(subwsLU, id=('WSHED_NUM')) #melt to vertical orientation
subwsLU_m$area_ha <- as.numeric(subwsLU_m$value / 107639.1041671) #value is in square feet, convert to ha 1ha=107639.1041671ft2

sunLU_m <- melt(sunLU, id='OBJECTID_1') #melt to vertical orientation
sunLU_m$area_ha <- as.numeric(sunLU_m$value / 107639.1041671) #value is in square feet, convert to ha 1ha=107639.1041671ft2
sunLU_m <- rename.vars(sunLU_m, from='OBJECTID_1', to='WSHED_NUM')

#break out wubwatersheds with multiple parts (505, 665, 805)
LU505 <- subset(subwsLU_m, subset=(WSHED_NUM==505 | WSHED_NUM==1410.5 | WSHED_NUM==1410.05 | WSHED_NUM==1415 | WSHED_NUM==1420 | 
                                   WSHED_NUM==1605 | WSHED_NUM==1610 | WSHED_NUM==1612)) 
                                   #505 made up of 505, 1410.5, 1410.05, 1415, 1420, 1605, 1610, 1612
LU505_c <- cast(subset(LU505, select=c('variable', 'area_ha')), variable ~ ., sum, value='area_ha') #re-cast and aggregate by variable for all watersheds in 505
LU505_c <- rename.vars(LU505_c, from=c('(all)'), to=c('505')) #rename to watershed


LU665 <- subset(subwsLU_m, subset=(WSHED_NUM==665 | WSHED_NUM==670 | WSHED_NUM==680 | WSHED_NUM==1005)) #665 made up of 665, 670, 680, 1005
LU665_c <- cast(subset(LU665, select=c('variable', 'area_ha')), variable ~ ., sum, value='area_ha') #re-cast and aggregate by variable for all watersheds in 665
LU665_c <- rename.vars(LU665_c, from=c('(all)'), to=c('665')) #rename to watershed

#calculate total area of 670 for percentage of 665 only
LU670 <- subset(subwsLU_m, subset=(WSHED_NUM==670 | WSHED_NUM==680 | WSHED_NUM==1005))
LU670_c <- cast(subset(LU670, select=c('variable', 'area_ha')), variable ~ ., sum, value='area_ha') #re-cast and aggregate by variable for all watersheds in 670
LU670_c <- rename.vars(LU670_c, from=c('(all)'), to=c('670')) #rename to watershed

LU805 <- subset(subwsLU_m, subset=(WSHED_NUM==805 | WSHED_NUM==805.5)) #805 made up of  805, 805.5
LU805_c <- cast(subset(LU805, select=c('variable', 'area_ha')), variable ~ ., sum, value='area_ha') #re-cast and aggregate by variable for all watersheds in 805
LU805_c <- rename.vars(LU805_c, from=c('(all)'), to=c('805')) #rename to watershed

#break out other subwatersheds of interest (510, 540, 760, 788, 790, 800, 830, 835)
LU_single <- subset(subwsLU_m, subset=(WSHED_NUM==510 | WSHED_NUM==540 | WSHED_NUM==760 | WSHED_NUM==788 | WSHED_NUM==790 | 
                                       WSHED_NUM==800 | WSHED_NUM==830 | WSHED_NUM==835)) 
LU_single_c <- cast(subset(LU_single, select=c('WSHED_NUM', 'variable', 'area_ha')), variable ~ WSHED_NUM, value='area_ha') #re-cast by waterhsed and variable

#cast ws minus lake
sunLU_c <- cast(subset(sunLU_m, select=c('WSHED_NUM', 'variable', 'area_ha')), variable ~ WSHED_NUM, value='area_ha') #re-cast by waterhsed and variable

#merge 505, 670, 805, singles and whole ws minus lake (leave out 670 - that calculation is only for our records)
LU_all <- merge(LU505_c, LU665_c, by='variable', all=T)
LU_all <- merge(LU_all, LU805_c, by='variable', all=T)
LU_all <- merge(LU_all, LU_single_c, by='variable', all=T)
LU_all <- merge(LU_all, sunLU_c, by='variable', all=T)

#calculate ungagued area as entire ws minus the sum of all of the subwatersheds
LU_all$ung <- LU_all$`ws minus lake` - (LU_all$`505` + LU_all$`510` + LU_all$`540` + LU_all$`665` + 
                                        LU_all$`760` + LU_all$`788` + LU_all$`790` + LU_all$`800` + 
                                        LU_all$`805` + LU_all$`830` + LU_all$`835`)
str(LU_all) #check work

#add code definitions
#LUcode <- read.xls('GIS/GIS files/land cover classification key.xls', sheet='key') #in begwrkspc

#assign total area in ha for later use
wsarea = (sum(LU_all$`ws minus lake`)) 
area505 = (sum(LU_all$`505`)) 
area510 = (sum(LU_all$`510`)) 
area540 = (sum(LU_all$`540`))
area665 = (sum(LU_all$`665`)) 
area760 = (sum(LU_all$`760`)) 
area788 = (sum(LU_all$`788`)) 
area790 = (sum(LU_all$`790`)) 
area800 = (sum(LU_all$`800`)) 
area805 = (sum(LU_all$`805`)) 
area830 = (sum(LU_all$`830`)) 
area835 = (sum(LU_all$`835`))
areaung = sum(LU_all$ung)

#assign proportion of watershed each subwatershed is
proarea505 = area505/wsarea
proarea510 = area510/wsarea
proarea540 = area540/wsarea
proarea665 = area665/wsarea
proarea760 = area760/wsarea 
proarea788 = area788/wsarea 
proarea790 = area790/wsarea
proarea800 = area800/wsarea
proarea805 = area805/wsarea
proarea830 = area830/wsarea 
proarea835 = area835/wsarea
proareaung = areaung/wsarea

#calculate total area of 670
area670 = sum(LU670_c$`670`)
#calculate proportion of 670 that 665 is for our metadata and methods
pro670of665=area670/area665
 
#calculate proportion of land use for each land use type by dividing the area of each landuse type by total area
LU_all$proLUws <- LU_all$`ws minus lake`/wsarea
LU_all$proLU505 <- LU_all$`505`/area505
LU_all$proLU510 <- LU_all$`510`/area510
LU_all$proLU540 <- LU_all$`540`/area540
LU_all$proLU665 <- LU_all$`665`/area665
LU_all$proLU760 <- LU_all$`760`/area760
LU_all$proLU788 <- LU_all$`788`/area788
LU_all$proLU790 <- LU_all$`790`/area790
LU_all$proLU800 <- LU_all$`800`/area800
LU_all$proLU805 <- LU_all$`805`/area805
LU_all$proLU830 <- LU_all$`830`/area830
LU_all$proLU835 <- LU_all$`835`/area835
LU_all$proLUung <- LU_all$ung/areaung
str(LU_all) #check work 

#subset for only columns needed (proLU columns and variable column)
LU_proportion <- subset(LU_all, select=c('variable', 'proLUws', 'proLU505', 'proLU510', 'proLU540', 'proLU665', 'proLU760', 'proLU788', 
                                            'proLU790', 'proLU800', 'proLU805', 'proLU830', 'proLU835', 'proLUung'))
LU_proportion <- rename.vars(LU_proportion, from='variable', to='code')
str(LU_proportion) #check work

#calculate proportion of ws as impervious#
impLUpro <- subset(LU_proportion, subset=(code==110 | code==140)) # transportation and residentail/commercial land use
impLUpro_m <- melt(impLUpro, id='code') #melt by code for vertical orientation
impLUpro_c <- cast(impLUpro_m, variable ~ ., fun=sum) # summarize by variable (watershed number)
impLUpro_c <- rename.vars(impLUpro_c, from='(all)', to='proportion_imp')

# merge ws land use data with land use description codes
LU_all <- merge(LU_all, LUcode, by.y='number', by.x='variable', all.x=T)

#re-order columns for export
LU_all <- subset(LU_all, select=c("variable", "name", "505", "proLU505", "510", "proLU510", "540", "proLU540", "665", 
                                            "proLU665", "760", "proLU760", "788", "proLU788", "790", "proLU790", "800", "proLU800", 
                                            "805", "proLU805", "830", "proLU830", "835", "proLU835", "ws minus lake", "proLUws", 
                                            "ung", "proLUung"))

#export to update methods file
write.csv(impLUpro_c, './data/individual_inflows/Sunapee WS impervious land cover summary 15Mar2021.csv', row.names = F)
write.csv(LU_all, './data/individual_inflows/Sunapee WS land cover summary 15Mar2021.csv', row.names=F)


#### ---- RUNOFF AND SNOW MELT CALCS ---- ####

#calculate runoff coefficients (Rv) and store as values in workspace
# Rv = 0.05 + 0.9Ia 
impWS <- impLUpro_c[1,2] #select and assign proportion of ws is impervious as first row, second column
Rvws = 0.05 + 0.9 * impWS #multiply by assumed variable
imp505 <- impLUpro_c[2,2]
Rv505 = 0.05 + 0.9 * imp505
imp510 <- impLUpro_c[3,2]
Rv510 = 0.05 + 0.9*imp510
imp540 <- impLUpro_c[4,2]
Rv540 = 0.05 + 0.9*imp540
imp665 <- impLUpro_c[5,2]
Rv665 = 0.05 + 0.9*imp665
imp760 <- impLUpro_c[6,2]
Rv760 = 0.05 + 0.9*imp760
imp788 <- impLUpro_c[7,2]
Rv788 = 0.05 + 0.9*imp788
imp790 <- impLUpro_c[8,2]
Rv790 = 0.05 + 0.9*imp790
imp800 <- impLUpro_c[9,2]
Rv800 = 0.05 + 0.9*imp800
imp805 <- impLUpro_c[10,2]
Rv805 = 0.05 + 0.9*imp805
imp830 <- impLUpro_c[11,2]
Rv830 = 0.05 + 0.9*imp830
imp835 <- impLUpro_c[12,2]
Rv835 = 0.05 + 0.9*imp835
impung <- impLUpro_c[13,2]
Rvung = 0.05 + 0.9*impung


##### SNOW ACCUMULATION #####

# If precipitation is falling when the air temperature is less than or equal
# to 0 degrees celsius, then the depth of precipition is stored as SWE
# (Snow Water Equivalents) 


#### Snow Melt #####

# If T(degrees celsius) is greater than 0, the precipitation falls as rain
# and any accumulated SWE contributes to runoff according to the relationship 
# from Part 630 Hydrology National Engineering Handbook with equations converted
# from US to metric:
# melt (meters) = (Cm * ((T * 1.8 + 32) - 32))
# SWE contribution to runoff = melt * Cwr
# Cm (melt rate coefficient) = 0.001524 meters of melt per degree day
# Cwr (winter runoff coefficient) = 0.5

# Degree day method of snowmelt modeling:
# http://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/NEHhydrology/ch11.pdf
# also referred to as the HEC-1 here: http://www.dtic.mil/dtic/tr/fulltext/u2/a366395.pdf
# 

# Table 6-1 http://www.publications.usace.army.mil/Portals/76/Publications/EngineerManuals/EM_1110-2-1406.pdf
# Relative Magnitude of Melt-Rate Factors (Refer to Table 5-4)
# Case  -  in./deg F - comment - cm/deg F
# 1 - 0.068 Clear, low albedo - 0.02677165
# 2 - 0.073 Case 1, 40% forest -  0.02874016
# 3 - 0.040 Case 1, cloud cover - 0.01574803
# 4 - 0.046 Case 1, fresh snow - 0.01811024
#- - - - - - - - - - - - - - - - - - - - - - -- - - - - -
# 5 - 0.180 Heavy rain, windy - 0.07086614
# 6 - 0.163 Light rain, windy - 0.06417323
# 7 - 0.062 Light rain, light wind - 0.02440945
  
# heavy rain is 3in/hr = 0.0762 m/h
# windy is > 15mph = 6.706 mps

#case <- (c(3, 5, 6, 7)) #in begwrkspc
#melt_coeff <- c(0.0001574803, 0.0007086614, 0.0006417323, 0.0002440945) #in begwrkspc
#case_melt <- data.frame(case, melt_coeff)  #in begwrkspc

#assign melt cases to met data  
MetData_sub <- subset(MetData, select=c('dateTime', 'AirTemp', 'AirTemp.F', 'WindSpeed', 'Rain.m_hr')) #subset for needed variables #in begwrkspc
MetData_sub <- subset(MetData_sub, subset=dateTime>'1979-01-01 12:00:00') #remove no data #in begwrkspc
MetData_sub$melt_case <- as.numeric(NA) #create column to fill in with melting case #in begwrkspc
str(MetData_sub)

#assign case number by temp, rain and wind data - in begwrkspc
for (i in 1:nrow(MetData_sub)){
  if(MetData_sub$AirTemp[i] <= 0){  #if temperature is at or below zero there is no melt, only accumulation
    MetData_sub$melt_case[i] <- NA} else  #therefore there is no melt case applicable
      if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and heavy rain and windy, case 5
        MetData_sub$melt_case[i] <- 5} else
          if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and heavy rain and light windy, case 6
            MetData_sub$melt_case[i] <- 6} else
              if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and light rain and windy, case 6
                MetData_sub$melt_case[i] <- 6} else
                  if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 &MetData_sub$Rain.m_hr[i]>0 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and light rain and windy, case 6
                    MetData_sub$melt_case[i] <- 7} else
                      MetData_sub$melt_case[i] <- 3 #any other temperature, rain and windspeed combination would be 
}

#merge with case data for melt coefficient
MetData_sub <- merge(MetData_sub, case_melt, by.x='melt_case', by.y='case', all.x=T) #in begwrkspc

MetData_sub <- MetData_sub[with(MetData_sub, order(dateTime)), ] #re-order by date (R sorts it by melt case and it needs to be in date order for next step)
MetData_sub$SWE <- 0 # column to store SWE (snow accumulation as a depth of water) #in begwrkspc
MetData_sub$runoff_SWE <- 0  # column of SWE contribution to runoff #in begwrkspc
str(MetData_sub) #check work #in begwrkspc

####---- RUNOFF DATA BY SUB WATERSHED ---- ####
# R = P * Pj * Rv 
# Where: 
# R =  runoff (meters) 
# P =  rainfall (meters) 
# Pj = Fraction of  rainfall events that produce runoff (usually 0.9) 
# Rv = Runoff coefficient 

# melt partitioning between infiltration and runoff generation should be
# slightly higher than assumption for non-frozen periods (if we are using 40%
# infiltration for non-winter conditions, let's use 50% for winter to account for 
# some ground being frozen). !!!! I am very open to adjusting this percent !!!!
# using the above logic, we add 0.1 to the Rv value to calculate the Cwr value
# calculate winter runoff coefficient based on land cover type - adding 10% as 
# we assume ground is frozen or saturated
Cwr505 <- Rv505 + 0.1
Cwr510 <- Rv510 + 0.1
Cwr540 <- Rv540 + 0.1
Cwr665 <- Rv665 + 0.1
Cwr760 <- Rv760 + 0.1
Cwr788 <- Rv788 + 0.1
Cwr790 <- Rv790 + 0.1
Cwr800 <- Rv800 + 0.1
Cwr805 <- Rv805 + 0.1
Cwr830 <- Rv830 + 0.1
Cwr835 <- Rv835 + 0.1
CwrWS <- Rvws + 0.1
Cwrung <- Rvung + 0.1

#runoff_reg_m and runoff_reg_m_fro are calcuated for all times so that values can be applied in the for loop in the next step

MetData_505 <- MetData_sub #copy into new dataframe
MetData_505$runoff_reg_m <- MetData_505$Rain.m_hr * Rv505 * 0.9 #apply runoff coefficient specific to subwatershed
MetData_505$runoff_reg_m_fro <- MetData_505$Rain.m_hr * (Cwr505) * 0.9 #caluclate winter/frozen runoff specific to subwatershed

MetData_510 <- MetData_sub
MetData_510$runoff_reg_m <- MetData_510$Rain.m_hr * Rv510 * 0.9
MetData_510$runoff_reg_m_fro <- MetData_510$Rain.m_hr * (Cwr510) * 0.9

MetData_540 <- MetData_sub
MetData_540$runoff_reg_m <- MetData_540$Rain.m_hr * Rv540 * 0.9
MetData_540$runoff_reg_m_fro <- MetData_540$Rain.m_hr * (Cwr540) * 0.9

MetData_665 <- MetData_sub
MetData_665$runoff_reg_m <- MetData_665$Rain.m_hr * Rv665 * 0.9
MetData_665$runoff_reg_m_fro <- MetData_665$Rain.m_hr * (Cwr665) * 0.9

MetData_760 <- MetData_sub
MetData_760$runoff_reg_m <- MetData_760$Rain.m_hr * Rv760 * 0.9
MetData_760$runoff_reg_m_fro <- MetData_760$Rain.m_hr * (Cwr760) * 0.9

MetData_788 <- MetData_sub
MetData_788$runoff_reg_m <- MetData_788$Rain.m_hr * Rv788 * 0.9
MetData_788$runoff_reg_m_fro <- MetData_788$Rain.m_hr * (Cwr788) * 0.9

MetData_790 <- MetData_sub
MetData_790$runoff_reg_m <- MetData_790$Rain.m_hr * Rv790 * 0.9
MetData_790$runoff_reg_m_fro <- MetData_790$Rain.m_hr * (Cwr790) * 0.9

MetData_800 <- MetData_sub
MetData_800$runoff_reg_m <- MetData_800$Rain.m_hr * Rv800 * 0.9
MetData_800$runoff_reg_m_fro <- MetData_800$Rain.m_hr * (Cwr800) * 0.9

MetData_805 <- MetData_sub
MetData_805$runoff_reg_m <- MetData_805$Rain.m_hr * Rv805 * 0.9
MetData_805$runoff_reg_m_fro <- MetData_805$Rain.m_hr * (Cwr805) * 0.9

MetData_830 <- MetData_sub
MetData_830$runoff_reg_m <- MetData_830$Rain.m_hr * Rv830 * 0.9
MetData_830$runoff_reg_m_fro <- MetData_830$Rain.m_hr * (Cwr830) * 0.9

MetData_835 <- MetData_sub
MetData_835$runoff_reg_m <- MetData_835$Rain.m_hr * Rv835 * 0.9
MetData_835$runoff_reg_m_fro <- MetData_835$Rain.m_hr * (Cwr835) * 0.9

MetData_ung <- MetData_sub
MetData_ung$runoff_reg_m <- MetData_ung$Rain.m_hr * Rvung * 0.9
MetData_ung$runoff_reg_m_fro <- MetData_ung$Rain.m_hr * (Cwrung) * 0.9

MetData_ws <- MetData_sub
MetData_ws$runoff_reg_m <- MetData_ws$Rain.m_hr * Rvws * 0.9
MetData_ws$runoff_reg_m_fro <- MetData_ws$Rain.m_hr * (CwrWS) * 0.9


#### 505 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_505)){                                        # for every row (except the first)
    if(MetData_505$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_505$Rain.m_hr[i] + MetData_505$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_505$SWE[i] <- swe_index 
  } else { 
    if ((MetData_505$AirTemp[i]>0) & (MetData_505$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_505$melt_coeff[i] * (MetData_505$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_505$SWE[i-1])                # in case melt value is greater 
      MetData_505$SWE[i] <- MetData_505$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr505*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_505$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_505$runoff_sum <- 0

for (i in 1:nrow(MetData_505)) {
  if (MetData_505$AirTemp[i]>0 & MetData_505$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_505$runoff_reg_m_fro[i] + MetData_505$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_505$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_505$AirTemp[i]>0 & MetData_505$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_505$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_505$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 510 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_510)){                                        # for every row (except the first)
  if(MetData_510$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_510$Rain.m_hr[i] + MetData_510$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_510$SWE[i] <- swe_index 
  } else { 
    if ((MetData_510$AirTemp[i]>0) & (MetData_510$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_510$melt_coeff[i] * (MetData_510$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_510$SWE[i-1])                # in case melt value is greater 
      MetData_510$SWE[i] <- MetData_510$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr510*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_510$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_510$runoff_sum <- 0

for (i in 1:nrow(MetData_510)) {
  if (MetData_510$AirTemp[i]>0 & MetData_510$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_510$runoff_reg_m_fro[i] + MetData_510$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_510$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_510$AirTemp[i]>0 & MetData_510$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_510$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_510$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 540 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_540)){                                        # for every row (except the first)
  if(MetData_540$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_540$Rain.m_hr[i] + MetData_540$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_540$SWE[i] <- swe_index 
  } else { 
    if ((MetData_540$AirTemp[i]>0) & (MetData_540$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_540$melt_coeff[i] * (MetData_540$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_540$SWE[i-1])                # in case melt value is greater 
      MetData_540$SWE[i] <- MetData_540$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr540*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_540$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_540$runoff_sum <- 0

for (i in 1:nrow(MetData_540)) {
  if (MetData_540$AirTemp[i]>0 & MetData_540$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_540$runoff_reg_m_fro[i] + MetData_540$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_540$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_540$AirTemp[i]>0 & MetData_540$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_540$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_540$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 665 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_665)){                                        # for every row (except the first)
  if(MetData_665$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_665$Rain.m_hr[i] + MetData_665$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_665$SWE[i] <- swe_index 
  } else { 
    if ((MetData_665$AirTemp[i]>0) & (MetData_665$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_665$melt_coeff[i] * (MetData_665$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_665$SWE[i-1])                # in case melt value is greater 
      MetData_665$SWE[i] <- MetData_665$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr665*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_665$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_665$runoff_sum <- 0

for (i in 1:nrow(MetData_665)) {
  if (MetData_665$AirTemp[i]>0 & MetData_665$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_665$runoff_reg_m_fro[i] + MetData_665$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_665$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_665$AirTemp[i]>0 & MetData_665$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_665$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_665$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 760 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_760)){                                        # for every row (except the first)
  if(MetData_760$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_760$Rain.m_hr[i] + MetData_760$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_760$SWE[i] <- swe_index 
  } else { 
    if ((MetData_760$AirTemp[i]>0) & (MetData_760$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_760$melt_coeff[i] * (MetData_760$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_760$SWE[i-1])                # in case melt value is greater 
      MetData_760$SWE[i] <- MetData_760$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr760*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_760$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_760$runoff_sum <- 0

for (i in 1:nrow(MetData_760)) {
  if (MetData_760$AirTemp[i]>0 & MetData_760$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_760$runoff_reg_m_fro[i] + MetData_760$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_760$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_760$AirTemp[i]>0 & MetData_760$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_760$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_760$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 788 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_788)){                                        # for every row (except the first)
  if(MetData_788$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_788$Rain.m_hr[i] + MetData_788$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_788$SWE[i] <- swe_index 
  } else { 
    if ((MetData_788$AirTemp[i]>0) & (MetData_788$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_788$melt_coeff[i] * (MetData_788$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_788$SWE[i-1])                # in case melt value is greater 
      MetData_788$SWE[i] <- MetData_788$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr788*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_788$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_788$runoff_sum <- 0

for (i in 1:nrow(MetData_788)) {
  if (MetData_788$AirTemp[i]>0 & MetData_788$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_788$runoff_reg_m_fro[i] + MetData_788$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_788$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_788$AirTemp[i]>0 & MetData_788$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_788$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_788$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 790 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_790)){                                        # for every row (except the first)
  if(MetData_790$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_790$Rain.m_hr[i] + MetData_790$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_790$SWE[i] <- swe_index 
  } else { 
    if ((MetData_790$AirTemp[i]>0) & (MetData_790$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_790$melt_coeff[i] * (MetData_790$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_790$SWE[i-1])                # in case melt value is greater 
      MetData_790$SWE[i] <- MetData_790$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr790*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_790$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_790$runoff_sum <- 0

for (i in 1:nrow(MetData_790)) {
  if (MetData_790$AirTemp[i]>0 & MetData_790$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_790$runoff_reg_m_fro[i] + MetData_790$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_790$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_790$AirTemp[i]>0 & MetData_790$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_790$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_790$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 800 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_800)){                                        # for every row (except the first)
  if(MetData_800$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_800$Rain.m_hr[i] + MetData_800$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_800$SWE[i] <- swe_index 
  } else { 
    if ((MetData_800$AirTemp[i]>0) & (MetData_800$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_800$melt_coeff[i] * (MetData_800$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_800$SWE[i-1])                # in case melt value is greater 
      MetData_800$SWE[i] <- MetData_800$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr800*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_800$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_800$runoff_sum <- 0

for (i in 1:nrow(MetData_800)) {
  if (MetData_800$AirTemp[i]>0 & MetData_800$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_800$runoff_reg_m_fro[i] + MetData_800$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_800$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_800$AirTemp[i]>0 & MetData_800$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_800$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_800$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 805 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_805)){                                        # for every row (except the first)
  if(MetData_805$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_805$Rain.m_hr[i] + MetData_805$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_805$SWE[i] <- swe_index 
  } else { 
    if ((MetData_805$AirTemp[i]>0) & (MetData_805$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_805$melt_coeff[i] * (MetData_805$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_805$SWE[i-1])                # in case melt value is greater 
      MetData_805$SWE[i] <- MetData_805$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr805*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_805$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_805$runoff_sum <- 0

for (i in 1:nrow(MetData_805)) {
  if (MetData_805$AirTemp[i]>0 & MetData_805$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_805$runoff_reg_m_fro[i] + MetData_805$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_805$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_805$AirTemp[i]>0 & MetData_805$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_805$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_805$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 830 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_830)){                                        # for every row (except the first)
  if(MetData_830$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_830$Rain.m_hr[i] + MetData_830$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_830$SWE[i] <- swe_index 
  } else { 
    if ((MetData_830$AirTemp[i]>0) & (MetData_830$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_830$melt_coeff[i] * (MetData_830$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_830$SWE[i-1])                # in case melt value is greater 
      MetData_830$SWE[i] <- MetData_830$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr830*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_830$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_830$runoff_sum <- 0

for (i in 1:nrow(MetData_830)) {
  if (MetData_830$AirTemp[i]>0 & MetData_830$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_830$runoff_reg_m_fro[i] + MetData_830$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_830$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_830$AirTemp[i]>0 & MetData_830$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_830$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_830$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### 835 Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_835)){                                        # for every row (except the first)
  if(MetData_835$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_835$Rain.m_hr[i] + MetData_835$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_835$SWE[i] <- swe_index 
  } else { 
    if ((MetData_835$AirTemp[i]>0) & (MetData_835$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_835$melt_coeff[i] * (MetData_835$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_835$SWE[i-1])                # in case melt value is greater 
      MetData_835$SWE[i] <- MetData_835$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwr835*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_835$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_835$runoff_sum <- 0

for (i in 1:nrow(MetData_835)) {
  if (MetData_835$AirTemp[i]>0 & MetData_835$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_835$runoff_reg_m_fro[i] + MetData_835$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_835$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_835$AirTemp[i]>0 & MetData_835$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_835$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_835$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### ung Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_ung)){                                        # for every row (except the first)
  if(MetData_ung$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_ung$Rain.m_hr[i] + MetData_ung$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_ung$SWE[i] <- swe_index 
  } else { 
    if ((MetData_ung$AirTemp[i]>0) & (MetData_ung$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_ung$melt_coeff[i] * (MetData_ung$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_ung$SWE[i-1])                # in case melt value is greater 
      MetData_ung$SWE[i] <- MetData_ung$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- Cwrung*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_ung$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_ung$runoff_sum <- 0

for (i in 1:nrow(MetData_ung)) {
  if (MetData_ung$AirTemp[i]>0 & MetData_ung$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_ung$runoff_reg_m_fro[i] + MetData_ung$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_ung$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_ung$AirTemp[i]>0 & MetData_ung$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_ung$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_ung$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### watershed Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_ws)){                                        # for every row (except the first)
  if(MetData_ws$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_ws$Rain.m_hr[i] + MetData_ws$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_ws$SWE[i] <- swe_index 
  } else { 
    if ((MetData_ws$AirTemp[i]>0) & (MetData_ws$SWE[i-1]>0)){     # If air temp is above 0, and there is storage,
      SWEmelt_index <- (MetData_ws$melt_coeff[i] * (MetData_ws$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_ws$SWE[i-1])                # in case melt value is greater 
      MetData_ws$SWE[i] <- MetData_ws$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrWS*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_ws$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_ws$runoff_sum <- 0

for (i in 1:nrow(MetData_ws)) {
  if (MetData_ws$AirTemp[i]>0 & MetData_ws$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_ws$runoff_reg_m_fro[i] + MetData_ws$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_ws$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_ws$AirTemp[i]>0 & MetData_ws$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_ws$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_ws$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

####************* scale up to WS and sub WS m3/day over all area **************####
MetData_505$total_runoff_m3_hr <- MetData_505$runoff_sum * area505 * 10000 #runoff in m. area is in ha. one ha is 10000m 
MetData_510$total_runoff_m3_hr <- MetData_510$runoff_sum * area510 * 10000
MetData_540$total_runoff_m3_hr <- MetData_540$runoff_sum * area540 * 10000
MetData_665$total_runoff_m3_hr <- MetData_665$runoff_sum * area665 * 10000
MetData_760$total_runoff_m3_hr <- MetData_760$runoff_sum * area760 * 10000
MetData_788$total_runoff_m3_hr <- MetData_788$runoff_sum * area788 * 10000
MetData_790$total_runoff_m3_hr <- MetData_790$runoff_sum * area790 * 10000
MetData_800$total_runoff_m3_hr <- MetData_800$runoff_sum * area800 * 10000
MetData_805$total_runoff_m3_hr <- MetData_805$runoff_sum * area805 * 10000
MetData_830$total_runoff_m3_hr <- MetData_830$runoff_sum * area830 * 10000
MetData_835$total_runoff_m3_hr <- MetData_835$runoff_sum * area835 * 10000
MetData_ung$total_runoff_m3_hr <- MetData_ung$runoff_sum * areaung * 10000
MetData_ws$total_runoff_m3_hr <- MetData_ws$runoff_sum * wsarea * 10000

# add column for date for aggregation
MetData_505$date <- as.Date(format(MetData_505$dateTime, '%Y-%m-%d'))
MetData_510$date <- as.Date(format(MetData_510$dateTime, '%Y-%m-%d'))
MetData_540$date <- as.Date(format(MetData_540$dateTime, '%Y-%m-%d'))
MetData_665$date <- as.Date(format(MetData_665$dateTime, '%Y-%m-%d'))
MetData_760$date <- as.Date(format(MetData_760$dateTime, '%Y-%m-%d'))
MetData_788$date <- as.Date(format(MetData_788$dateTime, '%Y-%m-%d'))
MetData_790$date <- as.Date(format(MetData_790$dateTime, '%Y-%m-%d'))
MetData_800$date <- as.Date(format(MetData_800$dateTime, '%Y-%m-%d'))
MetData_805$date <- as.Date(format(MetData_805$dateTime, '%Y-%m-%d'))
MetData_830$date <- as.Date(format(MetData_830$dateTime, '%Y-%m-%d'))
MetData_835$date <- as.Date(format(MetData_835$dateTime, '%Y-%m-%d'))
MetData_ws$date <- as.Date(format(MetData_ws$dateTime, '%Y-%m-%d'))
MetData_ung$date <- as.Date(format(MetData_ung$dateTime, '%Y-%m-%d'))

# summarize runoff/area by day
MetData_505_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_505, FUN = sum)) ; names(MetData_505_daily) <- c("date", "total_runoff_m3d")
MetData_510_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_510, FUN = sum)) ; names(MetData_510_daily) <- c("date", "total_runoff_m3d")
MetData_540_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_540, FUN = sum)) ; names(MetData_540_daily) <- c("date", "total_runoff_m3d")
MetData_665_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_665, FUN = sum)) ; names(MetData_665_daily) <- c("date", "total_runoff_m3d")
MetData_760_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_760, FUN = sum)) ; names(MetData_760_daily) <- c("date", "total_runoff_m3d")
MetData_788_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_788, FUN = sum)) ; names(MetData_788_daily) <- c("date", "total_runoff_m3d")
MetData_790_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_790, FUN = sum)) ; names(MetData_790_daily) <- c("date", "total_runoff_m3d")
MetData_800_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_800, FUN = sum)) ; names(MetData_800_daily) <- c("date", "total_runoff_m3d")
MetData_805_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_805, FUN = sum)) ; names(MetData_805_daily) <- c("date", "total_runoff_m3d")
MetData_830_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_830, FUN = sum)) ; names(MetData_830_daily) <- c("date", "total_runoff_m3d")
MetData_835_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_835, FUN = sum)) ; names(MetData_835_daily) <- c("date", "total_runoff_m3d")
MetData_ung_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_ung, FUN = sum)) ; names(MetData_ung_daily) <- c("date", "total_runoff_m3d")
MetData_ws_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_ws, FUN = sum)) ; names(MetData_ws_daily) <- c("date", "total_runoff_m3d")

#initialize column for discharge as proportion of runoff from previous days
 MetData_505_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_510_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_540_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_665_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_760_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_788_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_790_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_800_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_805_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_830_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_835_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_ung_daily$total_runoff_model_imp_50_50_m3d <- NA
 MetData_ws_daily$total_runoff_model_imp_50_50_m3d  <- NA

# calculate discharge as proportion of runoff from previous days
for(i in 2: nrow(MetData_505_daily)) {
  model_50_50 =  MetData_505_daily$total_runoff_m3d [i] * .5 + MetData_505_daily$total_runoff_m3d [i-1] * .5
  MetData_505_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_510_daily)) {
  model_50_50 =  MetData_510_daily$total_runoff_m3d [i] * .5 + MetData_510_daily$total_runoff_m3d [i-1] * .5
  MetData_510_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_540_daily)) {
  model_50_50 =  MetData_540_daily$total_runoff_m3d [i] * .5 + MetData_540_daily$total_runoff_m3d [i-1] * .5
  MetData_540_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_665_daily)) {
  model_50_50 =  MetData_665_daily$total_runoff_m3d [i] * .5 + MetData_665_daily$total_runoff_m3d [i-1] * .5
  MetData_665_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_760_daily)) {
  model_50_50 =  MetData_760_daily$total_runoff_m3d [i] * .5 + MetData_760_daily$total_runoff_m3d [i-1] * .5
  MetData_760_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_788_daily)) {
  model_50_50 =  MetData_788_daily$total_runoff_m3d [i] * .5 + MetData_788_daily$total_runoff_m3d [i-1] * .5
  MetData_788_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_790_daily)) {
  model_50_50 =  MetData_790_daily$total_runoff_m3d [i] * .5 + MetData_790_daily$total_runoff_m3d [i-1] * .5
  MetData_790_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_800_daily)) {
  model_50_50 =  MetData_800_daily$total_runoff_m3d [i] * .5 + MetData_800_daily$total_runoff_m3d [i-1] * .5
  MetData_800_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_805_daily)) {
  model_50_50 =  MetData_805_daily$total_runoff_m3d [i] * .5 + MetData_805_daily$total_runoff_m3d [i-1] * .5
  MetData_805_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_830_daily)) {
  model_50_50 =  MetData_830_daily$total_runoff_m3d [i] * .5 + MetData_830_daily$total_runoff_m3d [i-1] * .5
  MetData_830_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_835_daily)) {
  model_50_50 =  MetData_835_daily$total_runoff_m3d [i] * .5 + MetData_835_daily$total_runoff_m3d [i-1] * .5
  MetData_835_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_ung_daily)) {
  model_50_50 =  MetData_ung_daily$total_runoff_m3d [i] * .5 + MetData_ung_daily$total_runoff_m3d [i-1] * .5
  MetData_ung_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_ws_daily)) {
  model_50_50 =  MetData_ws_daily$total_runoff_m3d [i] * .5 + MetData_ws_daily$total_runoff_m3d [i-1] * .5
  MetData_ws_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}


#reality check
sum(MetData_ws_daily$total_runoff_model_imp_50_50_m3d, na.rm = T)
sum(MetData_505_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_510_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) +
  sum(MetData_540_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_665_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) +
  sum(MetData_760_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_788_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + 
  sum(MetData_790_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_800_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + 
  sum(MetData_805_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_830_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + 
  sum(MetData_835_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_ung_daily$total_runoff_model_imp_50_50_m3d, na.rm = T)

## convert to m3ps
MetData_505_daily$total_runoff_model_imp_50_50_m3s <- MetData_505_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_510_daily$total_runoff_model_imp_50_50_m3s <- MetData_510_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_540_daily$total_runoff_model_imp_50_50_m3s <- MetData_540_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_665_daily$total_runoff_model_imp_50_50_m3s <- MetData_665_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_760_daily$total_runoff_model_imp_50_50_m3s <- MetData_760_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_788_daily$total_runoff_model_imp_50_50_m3s <- MetData_788_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_790_daily$total_runoff_model_imp_50_50_m3s <- MetData_790_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_800_daily$total_runoff_model_imp_50_50_m3s <- MetData_800_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_805_daily$total_runoff_model_imp_50_50_m3s <- MetData_805_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_830_daily$total_runoff_model_imp_50_50_m3s <- MetData_830_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_835_daily$total_runoff_model_imp_50_50_m3s <- MetData_835_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_ung_daily$total_runoff_model_imp_50_50_m3s <- MetData_ung_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_ws_daily$total_runoff_model_imp_50_50_m3s <- MetData_ws_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)

#subset for desired variables
MetData_505_daily_sub <- subset(MetData_505_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_510_daily_sub <- subset(MetData_510_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_540_daily_sub <- subset(MetData_540_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_665_daily_sub <- subset(MetData_665_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_760_daily_sub <- subset(MetData_760_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_788_daily_sub <- subset(MetData_788_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_790_daily_sub <- subset(MetData_790_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_800_daily_sub <- subset(MetData_800_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_805_daily_sub <- subset(MetData_805_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_830_daily_sub <- subset(MetData_830_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_835_daily_sub <- subset(MetData_835_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_ung_daily_sub <- subset(MetData_ung_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_ws_daily_sub <- subset(MetData_ws_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))


#### ----- TEMPERATURE DATA VIA TRANSDUCRES ----- ####

#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Sunapee tribs/Discharge Modeling/final data 160413")

#import transducer data
#OP_505 <- read.csv('OtterPond_505_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc
#CJ_665 <- read.csv('ChandlerJohnson_665_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc
#BS_788 <- read.csv('BlodgettsSouth_788_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc
#BN_790 <- read.csv('BlodgettsNorth_790_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc
#KH_805 <- read.csv('KingHill_805_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc
#HS_830 <- read.csv('HerrickSouth_830_16Apr2016.csv', header = T, na.strings = NA) #in begwrkspc

#subset for datetime, stream number, stream temp
#OP_505 <- subset(OP_505, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc
#CJ_665 <- subset(CJ_665, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc
#BS_788 <- subset(BS_788, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc
#BN_790 <- subset(BN_790, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc
#KH_805 <- subset(KH_805, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc
#HS_830 <- subset(HS_830, select=c('datetime', 'date', 'stream_no', 'temp_C')) #in begwrkspc

#format date and create hour column
OP_505$date <- as.Date(OP_505$date, format='%Y-%m-%d')
OP_505$datetime <- as.POSIXlt(OP_505$datetime, format='%m/%d/%y %I:%M:%S %p')

CJ_665$date <- as.Date(CJ_665$date, format='%Y-%m-%d')
CJ_665$datetime <- as.POSIXlt(CJ_665$datetime, format='%m/%d/%y %I:%M:%S %p')

BS_788$date <- as.Date(BS_788$date, format='%Y-%m-%d')
BS_788$datetime <- as.POSIXlt(BS_788$datetime, format='%m/%d/%y %I:%M:%S %p')

BN_790$date <- as.Date(BN_790$date, format='%Y-%m-%d')
BN_790$datetime <- as.POSIXlt(BN_790$datetime, format='%m/%d/%y %I:%M:%S %p')

KH_805$date <- as.Date(KH_805$date, format='%Y-%m-%d')
KH_805$datetime <- as.POSIXlt(KH_805$datetime, format='%m/%d/%y %I:%M:%S %p')

HS_830$date <- as.Date(HS_830$date, format='%Y-%m-%d')
HS_830$datetime <- as.POSIXlt(HS_830$datetime, format='%m/%d/%y %I:%M:%S %p')

all_trans <- merge(OP_505, CJ_665, all=T)
all_trans <- merge(all_trans, BS_788, all=T)
all_trans <- merge(all_trans, BN_790, all=T)
all_trans <- merge(all_trans, KH_805, all=T)
all_trans <- merge(all_trans, HS_830, all=T)

#calculate daily mean
daily_t_505 <- summaryBy(temp_C ~ stream_no + date, 
                         data = OP_505, na.rm=TRUE, 
                         FUN = mean)
daily_t_665 <- summaryBy(temp_C ~ stream_no + date, 
                         data = CJ_665, na.rm=TRUE, 
                         FUN = mean)
daily_t_788 <- summaryBy(temp_C ~ stream_no + date, 
                         data = BS_788, na.rm=TRUE, 
                         FUN = mean)
daily_t_790 <- summaryBy(temp_C ~ stream_no + date, 
                         data = BN_790, na.rm=TRUE, 
                         FUN = mean)
daily_t_805 <- summaryBy(temp_C ~ stream_no + date, 
                         data = KH_805, na.rm=TRUE, 
                         FUN = mean)
daily_t_830 <- summaryBy(temp_C ~ stream_no + date, 
                         data = HS_830, na.rm=TRUE, 
                         FUN = mean)
daily_t_alltrans <- summaryBy(temp_C ~ date, 
                         data=all_trans, na.rm=TRUE,
                         FUN=mean)

daily_t_all <- merge(daily_t_505, daily_t_665, all=T)
daily_t_all <- merge(daily_t_all, daily_t_788, all=T)
daily_t_all <- merge(daily_t_all, daily_t_790, all=T)
daily_t_all <- merge(daily_t_all, daily_t_805, all=T)
daily_t_all <- merge(daily_t_all, daily_t_830, all=T)
daily_t_all <- merge(daily_t_all, daily_t_alltrans, all=T)

daily_t_all$date <- as.Date(daily_t_all$date, format='%Y-%m-%d')

str(daily_t_all) #check work


####********** compare NLDAS2 and transducer stream temp data ***********####
# read in historical met data to match with the pressure transducer data
hist <- read.csv("C:/Users/wwoel/Desktop/Sunapee-GLM/data/SunapeeMet_1979_2018EST.csv")
hist$date <- as.Date(hist$time)
temp_daily <- summaryBy(AirTemp ~ date, 
                        data=hist,
                        FUN=mean)
met_trans <- merge(temp_daily, daily_t_all, by='date', all.y=T)
met_trans <- subset(met_trans, subset=date<'2014-01-01') #subset for original time window ending 2013-12-31
met_trans <- subset(met_trans, subset=!is.na(AirTemp.mean))
met_trans$year <- as.numeric(format(met_trans$date, '%Y'))
met_trans$jday <- as.numeric(format(met_trans$date, '%j'))
met_trans$month <- as.numeric(format(met_trans$date, '%m'))

met_trans_505 <- subset(met_trans, subset=stream_no==505)
met_trans_665 <- subset(met_trans, subset=stream_no==665)
met_trans_788 <- subset(met_trans, subset=stream_no==788)
met_trans_790 <- subset(met_trans, subset=stream_no==790)
met_trans_805 <- subset(met_trans, subset=stream_no==805)
met_trans_830 <- subset(met_trans, subset=stream_no==830)
met_trans_all <- subset(met_trans, subset=is.na(stream_no))

#quick plot comparison
ggplot(met_trans, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_point(color='blue', size=1) +
  facet_grid(stream_no~.) +
  labs(title='Air Temperature and Stream Temperature', 
       x='mean daily NLDAS-2 air temperature (deg C)',
       y='mean daily stream temperature (deg C)') #+
  #final_theme


# we need to model stream temperature based on air temp and transducer in-stream temperature.
# we will make two linear models - one is for cold temps (i.e. below freezing) and the other
# is for those above the cutoff between cold and warm. This is done by a quick and dirty method
# of inserting a value for the cutoff for both cold and warm temps, creating linear models
# and determining where the two linear models meet. if they meet at the same value as your 
# cutoff, you have the right cutoff value. if not, adjust it until the cutof is the same as
# where the two lines intersect.

## all data for ungauged streams
ung_t_cutoff <- -1.224 #choose a value here to work from

met_trans_sub <- subset(met_trans_all, subset=(AirTemp.mean > ung_t_cutoff)) #subset for above the cutoff
lm_all_s <- lm(temp_C.mean ~ AirTemp.mean, met_trans_sub) #create a linear model of the data
lm_all_summ <- summary(lm_all_s) #store the summary
coef_all <- lm_all_summ[["coefficients"]] #assign the coefficients to a table

met_trans_all_cold <- subset(met_trans_all, subset=(AirTemp.mean <=ung_t_cutoff)) # subset for below the cutoff
cold_temp_all <- mean(met_trans_all_cold$temp_C.mean) # find the mean value (this will be the y-intercept of the linear model with a slope of 0)
cold_temp_all_SD <- sd(met_trans_all_cold$temp_C.mean) # calculate the SD for the values

#do we match?
(cold_temp_all-coef_all[1])/coef_all[2] #this is the dummy check. adjust the cutoff above until the result here is the same as the cutoff

all_met_trans <- ggplot(met_trans_all, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_vline(xintercept = ung_t_cutoff, color='grey', size=1.1) +
  geom_point(size=1) +
  labs(title='Air Temperature and Stream Temperature
All Transducers', 
       x='mean daily NLDAS-2 air temperature (deg C)',
       y='mean daily stream temperature (deg C)') +
  geom_hline(yintercept = cold_temp_all, color='#E69F00', size=1.1) +
  geom_abline(slope = coef_all[2], intercept = coef_all[1], color='#56B4E9', size=1.1) #+
  #final_theme

label.all_sum <- function(met_trans_sub){
  m <- lm_all_s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.all_mean <- function(met_trans_all_cold){
  eq <- substitute(italic(y) == meanall*","~~SD~"="~SDall, 
                   list(meanall = format(cold_temp_all, digits=3),
                        SDall = format(cold_temp_all_SD, digits=3)))
  as.character(as.expression(eq));                 
}

all_met_trans + 
  geom_text(x = -20, y = 23.5, label='mean of temperatures <= -1.224', hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 22, label = label.all_mean(met_trans_all_cold), parse = TRUE, hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 20.5, label='linear model of temperatures > -1.224', hjust=0, color='#56B4E9') +
  geom_text(x = -20, y = 19, label = label.all_sum(met_trans_sub), parse = TRUE, hjust=0, color='#56B4E9')

## 505 OP
t_505_cutoff <- -0.700

met_trans_505_sub <- subset(met_trans_505, subset=(AirTemp.mean > t_505_cutoff))
lm_505s <- lm(temp_C.mean ~ AirTemp.mean, met_trans_505_sub)
lm_505_summ <- summary(lm_505s)
coef_505s <- lm_505_summ[["coefficients"]]

met_trans_505_cold <- subset(met_trans_505, subset=(AirTemp.mean <= t_505_cutoff))
cold_temp_505 <- mean(met_trans_505_cold$temp_C.mean)
cold_temp_505_SD <- sd(met_trans_505_cold$temp_C.mean)

#do we match?
(cold_temp_505-coef_505s[1])/coef_505s[2]

mettrans_505 <- ggplot(met_trans_505, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_vline(xintercept = t_505_cutoff, color='grey', size=1.1) +
  geom_point(size=1) +
  labs(title='Air Temperature and Stream Temperature
505 Otter Pond', 
       x='mean daily NLDAS-2 air temperature (deg C)',
       y='mean daily stream temperature (deg C)') +
  geom_hline(yintercept = cold_temp_505, color='#E69F00', size=1.1) +
  geom_abline(slope = coef_505s[2], intercept = coef_505s[1], color='#56B4E9', size=1.1) #+
  #final_theme

label.505_sum <- function(met_trans_505_sub){
  m <- lm_505s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.505_mean <- function(met_trans_505_cold){
  eq <- substitute(italic(y) == mean505*","~~SD~"="~SD505, 
                   list(mean505 = format(cold_temp_505, digits=3),
                        SD505 = format(cold_temp_505_SD, digits=3)))
  as.character(as.expression(eq));                 
}

mettrans_505 + 
  geom_text(x = -20, y = 26.5, label='mean of temperatures <= -0.700', hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 25, label = label.505_mean(met_trans_505_cold), parse = TRUE, hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 23.5, label='linear model of temperatures > -0.700', hjust=0, color='#56B4E9') +
  geom_text(x = -20, y = 22, label = label.505_sum(met_trans_505_sub), parse = TRUE, hjust=0, color='#56B4E9')


## 665 CJ 
t_665_cutoff <- -1.340

met_trans_665_sub <- subset(met_trans_665, subset=(AirTemp.mean >t_665_cutoff))
lm_665s <- lm(met_trans_665_sub$temp_C.mean~met_trans_665_sub$AirTemp.mean)
lm_665_summ <- summary(lm_665s)
coef_665s <- lm_665_summ[["coefficients"]]

met_trans_665_cold <- subset(met_trans_665, subset=(AirTemp.mean <=t_665_cutoff))
cold_temp_665 <- mean(met_trans_665_cold$temp_C.mean)
cold_temp_665_SD <- sd(met_trans_665_cold$temp_C.mean)

#do we match?
(cold_temp_665-coef_665s[1])/coef_665s[2]

mettrans_665 <- ggplot(met_trans_665, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_vline(xintercept = t_665_cutoff, color='grey', size=1.1) +
  geom_point(size=1) +
  labs(title='Air Temperature and Stream Temperature
665 Chandler Johnson', 
       x='mean daily air temperature (deg C)',
       y='mean daily stream temperature (deg C)') +
  geom_hline(yintercept = cold_temp_665, color='#E69F00', size=1.1) +
  geom_abline(slope = coef_665s[2], intercept = coef_665s[1], color='#56B4E9', size=1.1) 

label.665_sum <- function(met_trans_665_sub){
  m <- lm_665s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.665_mean <- function(met_trans_665_cold){
  eq <- substitute(italic(y) == mean665*","~~SD~"="~SD665, 
                   list(mean665 = format(cold_temp_665, digits=3),
                        SD665 = format(cold_temp_665_SD, digits=3)))
  as.character(as.expression(eq));                 
}

mettrans_665 + 
  geom_text(x = -20, y = 25, label='mean of temperatures <= -1.340', hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 23.5, label = label.665_mean(met_trans_665_cold), parse = TRUE, hjust=0, color='#E69F00') +
  geom_text(x = -20, y = 22, label='linear model of temperatures > -1.340', hjust=0, color='#56B4E9') +
  geom_text(x = -20, y = 20.5, label = label.665_sum(met_trans_665_sub), parse = TRUE, hjust=0, color='#56B4E9')


## 788 BS
t_788_cutoff <- -2.137

met_trans_788_sub <- subset(met_trans_788, subset=(AirTemp.mean > t_788_cutoff))
lm_788s <- lm(met_trans_788_sub$temp_C.mean~met_trans_788_sub$AirTemp.mean)
lm_788_summ <- summary(lm_788s)
coef_788s <- lm_788_summ[["coefficients"]]

met_trans_788_cold <- subset(met_trans_788, subset=(AirTemp.mean <= t_788_cutoff))
cold_temp_788 <- mean(met_trans_788_cold$temp_C.mean)
cold_temp_788_SD <- sd(met_trans_788_cold$temp_C.mean)

#do we match?
(cold_temp_788-coef_788s[1])/coef_788s[2]

#mettrans_788 <- ggplot(met_trans_788, aes(x=AirTemp.mean, y=temp_C.mean)) +
#  geom_vline(xintercept = t_788_cutoff, color='grey', size=1.1) +
#  geom_point(size=1) +
#  labs(title='Air Temperature and Stream Temperature
#788 Blodgetts South', 
#       x='mean daily NLDAS-2 air temperature (deg C)',
#       y='mean daily stream temperature (deg C)') +
#  geom_hline(yintercept = cold_temp_788, color='#E69F00', size=1.1) +
#  geom_abline(slope = coef_788s[2], intercept = coef_788s[1], color='#56B4E9', size=1.1)

label.788_sum <- function(met_trans_788_sub){
  m <- lm_788s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.788_mean <- function(met_trans_788_cold){
  eq <- substitute(italic(y) == mean788*","~~SD~"="~SD788, 
                   list(mean788 = format(cold_temp_788, digits=3),
                        SD788 = format(cold_temp_788_SD, digits=3)))
  as.character(as.expression(eq));                 
}

#mettrans_788 + 
#  geom_text(x = -20, y = 17.5, label='mean of temperatures <= -2.137', hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 16.5, label = label.788_mean(met_trans_788_cold), parse = TRUE, hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 15.5, label='linear model of temperatures > -2.137', hjust=0, color='#56B4E9') +
#  geom_text(x = -20, y = 14.5, label = label.788_sum(met_trans_788_sub), parse = TRUE, hjust=0, color='#56B4E9')


## 790BN
t_790_cutoff <- -1.512

met_trans_790_sub <- subset(met_trans_790, subset=(AirTemp.mean > t_790_cutoff))
lm_790s <- lm(met_trans_790_sub$temp_C.mean~met_trans_790_sub$AirTemp.mean)
lm_790_summ <- summary(lm_790s)
coef_790s <- lm_790_summ[["coefficients"]]

met_trans_790_cold <- subset(met_trans_790, subset=(AirTemp.mean <= t_790_cutoff))
cold_temp_790 <- mean(met_trans_790_cold$temp_C.mean)
cold_temp_790_SD <- sd(met_trans_790_cold$temp_C.mean)

#do we match?
(cold_temp_790-coef_790s[1])/coef_790s[2]

#mettrans_790 <- ggplot(met_trans_790, aes(x=AirTemp.mean, y=temp_C.mean)) +
#  geom_vline(xintercept = t_790_cutoff, color='grey', size=1.1) +
#  geom_point(size=1) +
#  labs(title='Air Temperature and Stream Temperature
#790 Blodgetts North', 
#       x='mean daily NLDAS-2 air temperature (deg C)',
#       y='mean daily stream temperature (deg C)') +
#  geom_hline(yintercept = cold_temp_790, color='#E69F00', size=1.1) +
#  geom_abline(slope = coef_790s[2], intercept = coef_790s[1], color='#56B4E9', size=1.1) +
#  final_theme

label.790_sum <- function(met_trans_790_sub){
  m <- lm_790s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.790_mean <- function(met_trans_790_cold){
  eq <- substitute(italic(y) == mean790*","~~SD~"="~SD790, 
                   list(mean790 = format(cold_temp_790, digits=3),
                        SD790 = format(cold_temp_790_SD, digits=3)))
  as.character(as.expression(eq));                 
}

#mettrans_790 + 
#  geom_text(x = -20, y = 22.5, label='mean of temperatures <= -1.512', hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 21.5, label = label.790_mean(met_trans_790_cold), parse = TRUE, hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 20.5, label='linear model of temperatures > -1.512', hjust=0, color='#56B4E9') +
#  geom_text(x = -20, y = 19.5, label = label.790_sum(met_trans_790_sub), parse = TRUE, hjust=0, color='#56B4E9')


## 805 King Hill
t_805_cutoff <- -0.531

met_trans_805_sub <- subset(met_trans_805, subset=(AirTemp.mean > t_805_cutoff))
lm_805s <- lm(met_trans_805_sub$temp_C.mean~met_trans_805_sub$AirTemp.mean)
lm_805_summ <- summary(lm_805s)
coef_805s <- lm_805_summ[["coefficients"]]

met_trans_805_cold <- subset(met_trans_805, subset=(AirTemp.mean <= t_805_cutoff))
cold_temp_805 <- mean(met_trans_805_cold$temp_C.mean)
cold_temp_805_SD <- sd(met_trans_805_cold$temp_C.mean)

#do we match?
(cold_temp_805-coef_805s[1])/coef_805s[2]

mettrans_805 <- ggplot(met_trans_805, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_vline(xintercept = t_805_cutoff, color='grey', size=1.1) +
  geom_point(size=1) +
  labs(title='Air Temperature and Stream Temperature
805 King Hill', 
       x='mean daily NLDAS-2 air temperature (deg C)',
       y='mean daily stream temperature (deg C)') +
  geom_hline(yintercept = cold_temp_805, color='#E69F00', size=1.1) +
  geom_abline(slope = coef_805s[2], intercept = coef_805s[1], color='#56B4E9', size=1.1) +
  final_theme

label.805_sum <- function(met_trans_805_sub){
  m <- lm_805s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.805_mean <- function(met_trans_805_cold){
  eq <- substitute(italic(y) == mean805*","~~SD~"="~SD805, 
                   list(mean805 = format(cold_temp_805, digits=3),
                        SD805 = format(cold_temp_805_SD, digits=3)))
  as.character(as.expression(eq));                 
}

#mettrans_805 + 
#  geom_text(x = -20, y = 21.5, label='mean of temperatures <= -0.531', hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 20.5, label = label.805_mean(met_trans_805_cold), parse = TRUE, hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 19.5, label='linear model of temperatures > -0.531', hjust=0, color='#56B4E9') +
#  geom_text(x = -20, y = 18.5, label = label.805_sum(met_trans_805_sub), parse = TRUE, hjust=0, color='#56B4E9')

## 830 Herrick South
t_830_cutoff <- -1.310

met_trans_830_sub <- subset(met_trans_830, subset=(AirTemp.mean > t_830_cutoff))
lm_830s <- lm(met_trans_830_sub$temp_C.mean~met_trans_830_sub$AirTemp.mean)
lm_830_summ <- summary(lm_830s)
coef_830s <- lm_830_summ[["coefficients"]]

met_trans_830_cold <- subset(met_trans_830, subset=(AirTemp.mean <= t_830_cutoff))
cold_temp_830 <- mean(met_trans_830_cold$temp_C.mean)
cold_temp_830_SD <- sd(met_trans_830_cold$temp_C.mean)

#do we match?
(cold_temp_830-coef_830s[1])/coef_830s[2]

mettrans_830 <- ggplot(met_trans_830, aes(x=AirTemp.mean, y=temp_C.mean)) +
  geom_vline(xintercept = t_830_cutoff, color='grey', size=1.1) +
  geom_point(size=1) +
  labs(title='Air Temperature and Stream Temperature
830 Herrick South', 
       x='mean daily NLDAS-2 air temperature (deg C)',
       y='mean daily stream temperature (deg C)') +
  geom_hline(yintercept = cold_temp_830, color='#E69F00', size=1.1) +
  geom_abline(slope = coef_830s[2], intercept = coef_830s[1], color='#56B4E9', size=1.1) +
  final_theme

label.830_sum <- function(met_trans_830_sub){
  m <- lm_830s;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

label.830_mean <- function(met_trans_830_cold){
  eq <- substitute(italic(y) == mean830*","~~SD~"="~SD830, 
                   list(mean830 = format(cold_temp_830, digits=3),
                        SD830 = format(cold_temp_830_SD, digits=3)))
  as.character(as.expression(eq));                 
}

#mettrans_830 + 
#  geom_text(x = -20, y = 23.5, label='mean of temperatures <= -1.310', hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 22.5, label = label.830_mean(met_trans_830_cold), parse = TRUE, hjust=0, color='#E69F00') +
#  geom_text(x = -20, y = 21.5, label='linear model of temperatures > -1.310', hjust=0, color='#56B4E9') +
#  geom_text(x = -20, y = 20.5, label = label.830_sum(met_trans_830_sub), parse = TRUE, hjust=0, color='#56B4E9')


#### add stream temp to runoff data ####
#merge file with predicted runoff with observed air temp data

MetData$date <- as.Date(MetData$time)
temp_daily_new <- summaryBy(AirTemp ~ date, 
                        data=MetData,
                        FUN=mean)

run505_temp <- merge(MetData_505_daily_sub, temp_daily_new, by='date', all.x=T)
run510_temp <- merge(MetData_510_daily_sub, temp_daily_new, by='date', all.x=T)
run540_temp <- merge(MetData_540_daily_sub, temp_daily_new, by='date', all.x=T)
run665_temp <- merge(MetData_665_daily_sub, temp_daily_new, by='date', all.x=T)
run760_temp <- merge(MetData_760_daily_sub, temp_daily_new, by='date', all.x=T)
run788_temp <- merge(MetData_788_daily_sub, temp_daily_new, by='date', all.x=T)
run790_temp <- merge(MetData_790_daily_sub, temp_daily_new, by='date', all.x=T)
run800_temp <- merge(MetData_800_daily_sub, temp_daily_new, by='date', all.x=T)
run805_temp <- merge(MetData_805_daily_sub, temp_daily_new, by='date', all.x=T)
run830_temp <- merge(MetData_830_daily_sub, temp_daily_new, by='date', all.x=T)
run835_temp <- merge(MetData_835_daily_sub, temp_daily_new, by='date', all.x=T)
runung_temp <- merge(MetData_ung_daily_sub, temp_daily_new, by='date', all.x=T)

# initialize modeled stream temp column
run505_temp$ModStreamTemp_degC <- NA
run510_temp$ModStreamTemp_degC <- NA
run540_temp$ModStreamTemp_degC <- NA
run665_temp$ModStreamTemp_degC <- NA
run760_temp$ModStreamTemp_degC <- NA
run788_temp$ModStreamTemp_degC <- NA
run790_temp$ModStreamTemp_degC <- NA
run800_temp$ModStreamTemp_degC <- NA
run805_temp$ModStreamTemp_degC <- NA
run830_temp$ModStreamTemp_degC <- NA
run835_temp$ModStreamTemp_degC <- NA
runung_temp$ModStreamTemp_degC <- NA

#apply equations to calculate stream temp for each location
for (i in 2:nrow(run505_temp)) {
  if (run505_temp$AirTemp.mean[i] <= t_505_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_505 #apply mean of 505 cold temps
    run505_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run505_temp$AirTemp.mean[i] > t_505_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_505s)[1] + coef(lm_505s)[2]*run505_temp$AirTemp.mean[i]) #run linear model defined earlier
      run505_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run510_temp)) {
  if (run510_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    run510_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run510_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*run510_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      run510_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run540_temp)) {
  if (run540_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    run540_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run540_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*run540_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      run540_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run665_temp)) {
  if (run665_temp$AirTemp.mean[i] <= t_665_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_665 #apply mean of 665 cold temps
    run665_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run665_temp$AirTemp.mean[i] > t_665_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_665s)[1] + coef(lm_665s)[2]*run665_temp$AirTemp.mean[i]) #run linear model defined earlier
      run665_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run760_temp)) {
  if (run760_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    run760_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run760_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*run760_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      run760_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run788_temp)) {
  if (run788_temp$AirTemp.mean[i] <= t_788_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_788 #apply mean of 788 cold temps
    run788_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run788_temp$AirTemp.mean[i] > t_788_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_788s)[1] + coef(lm_788s)[2]*run788_temp$AirTemp.mean[i]) #run linear model defined earlier
      run788_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run790_temp)) {
  if (run790_temp$AirTemp.mean[i] <= t_790_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_790 #apply mean of 790 cold temps
    run790_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run790_temp$AirTemp.mean[i] > t_790_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_790s)[1] + coef(lm_790s)[2]*run790_temp$AirTemp.mean[i]) #run linear model defined earlier
      run790_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run800_temp)) {
  if (run800_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    run800_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run800_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*run800_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      run800_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}


for (i in 2:nrow(run805_temp)) {
  if (run805_temp$AirTemp.mean[i] <= t_805_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_805 #apply mean of 805 cold temps
    run805_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run805_temp$AirTemp.mean[i] > t_805_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_805s)[1] + coef(lm_805s)[2]*run805_temp$AirTemp.mean[i]) #run linear model defined earlier
      run805_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run830_temp)) {
  if (run830_temp$AirTemp.mean[i] <= t_830_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_830 #apply mean of 830 cold temps
    run830_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run830_temp$AirTemp.mean[i] > t_830_cutoff) { #if temp greater than cutoff,
      MODTEMP <- (coef(lm_830s)[1] + coef(lm_830s)[2]*run830_temp$AirTemp.mean[i]) #run linear model defined earlier
      run830_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(run835_temp)) {
  if (run835_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    run835_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (run835_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*run835_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      run835_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

for (i in 2:nrow(runung_temp)) {
if (runung_temp$AirTemp.mean[i] <= ung_t_cutoff) { #run with ungauged temp cutoff - if temp less than or equal to,
    COLD <- cold_temp_all #apply mean of all cold temps
    runung_temp$ModStreamTemp_degC [i] <- COLD #store in data frame
  } else {
    if (runung_temp$AirTemp.mean[i] > ung_t_cutoff) {  #if temp greater than cutoff, 
      MODTEMP <- (coef(lm_all_s)[1] + coef(lm_all_s)[2]*runung_temp$AirTemp.mean[i]) #run linear model for all data defined earlier
      runung_temp$ModStreamTemp_degC [i] <- MODTEMP #store in data frame
    }
  }
}

## subset for runoff and stream temp only
run505_temp_sub <- remove.vars(run505_temp, names='AirTemp.mean')
run510_temp_sub <- remove.vars(run510_temp, names='AirTemp.mean')
run540_temp_sub <- remove.vars(run540_temp, names='AirTemp.mean')
run665_temp_sub <- remove.vars(run665_temp, names='AirTemp.mean')
run760_temp_sub <- remove.vars(run760_temp, names='AirTemp.mean')
run788_temp_sub <- remove.vars(run788_temp, names='AirTemp.mean')
run790_temp_sub <- remove.vars(run790_temp, names='AirTemp.mean')
run800_temp_sub <- remove.vars(run800_temp, names='AirTemp.mean')
run805_temp_sub <- remove.vars(run805_temp, names='AirTemp.mean')
run830_temp_sub <- remove.vars(run830_temp, names='AirTemp.mean')
run835_temp_sub <- remove.vars(run835_temp, names='AirTemp.mean')
runung_temp_sub <- remove.vars(runung_temp, names='AirTemp.mean')

#double check - values should be the same
sum(MetData_ws_daily_sub$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum(run505_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + sum(run510_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) +
  sum(run540_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + sum(run665_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) +
  sum(run760_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + sum(run788_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + 
  sum(run790_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + sum(run800_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + 
  sum(run805_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + sum(run830_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + 
  sum(run835_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T) + 
  sum(runung_temp_sub$total_runoff_model_imp_50_50_m3s, na.rm = T)


#### calculate inflow proportions for baseflow attribution ####
#import daily runoff and stream temp into new data frame
runtemp505 <- run505_temp_sub
runtemp510 <- run510_temp_sub
runtemp540 <- run540_temp_sub
runtemp665 <- run665_temp_sub
runtemp760 <- run760_temp_sub
runtemp788 <- run788_temp_sub
runtemp790 <- run790_temp_sub
runtemp800 <- run800_temp_sub
runtemp805 <- run805_temp_sub
runtemp830 <- run830_temp_sub
runtemp835 <- run835_temp_sub
runtempung <- runung_temp_sub
WS <- MetData_ws_daily_sub

#add stream identifier to files
runtemp505$stream_no <- 505
runtemp510$stream_no <- 510
runtemp540$stream_no <- 540
runtemp665$stream_no <- 665
runtemp760$stream_no <- 760
runtemp788$stream_no <- 788
runtemp790$stream_no <- 790
runtemp800$stream_no <- 800
runtemp805$stream_no <- 805
runtemp830$stream_no <- 830
runtemp835$stream_no <- 835
runtempung$stream_no <- 'ung'

#total inflow to Sunapee is assumed at the total of these 11 inflows
#merge files
runoff_all <- merge(runtemp505, runtemp510, all=T)
runoff_all <- merge(runoff_all, runtemp540, all=T)
runoff_all <- merge(runoff_all, runtemp665, all=T)
runoff_all <- merge(runoff_all, runtemp760, all=T)
runoff_all <- merge(runoff_all, runtemp788, all=T)
runoff_all <- merge(runoff_all, runtemp790, all=T)
runoff_all <- merge(runoff_all, runtemp800, all=T)
runoff_all <- merge(runoff_all, runtemp805, all=T)
runoff_all <- merge(runoff_all, runtemp830, all=T)
runoff_all <- merge(runoff_all, runtemp835, all=T)
runoff_all <- merge(runoff_all, runtempung, all=T)
runoff_all$date <- as.Date(runoff_all$date, format='%Y-%m-%d')
str(runoff_all) #check work

WS$date <- as.Date(WS$date, format='%Y-%m-%d')

## calculate proportion of inflow from each gauged stream for use later when calculating baseflow ##
WS_sum_inf <- sum(WS$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_505 <- sum(runtemp505$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_510 <- sum(runtemp510$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_540 <- sum(runtemp540$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_665 <- sum(runtemp665$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_760 <- sum(runtemp760$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_788 <- sum(runtemp788$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_790 <- sum(runtemp790$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_800 <- sum(runtemp800$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_805 <- sum(runtemp805$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_830 <- sum(runtemp830$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_835 <- sum(runtemp835$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_ung <- sum(runtempung$total_runoff_model_imp_50_50_m3s, na.rm = T)

#reality check - these should be the same
(sum_inf_505 + sum_inf_510 + sum_inf_540 + sum_inf_665 + sum_inf_760 + sum_inf_788 + sum_inf_790 + sum_inf_800 + sum_inf_805 + sum_inf_830 + 
    sum_inf_835 + sum_inf_ung)
WS_sum_inf

pro_505 <- sum_inf_505/WS_sum_inf
pro_510 <- sum_inf_510/WS_sum_inf
pro_540 <- sum_inf_540/WS_sum_inf
pro_665 <- sum_inf_665/WS_sum_inf
pro_760 <- sum_inf_760/WS_sum_inf
pro_788 <- sum_inf_788/WS_sum_inf
pro_790 <- sum_inf_790/WS_sum_inf
pro_800 <- sum_inf_800/WS_sum_inf
pro_805 <- sum_inf_805/WS_sum_inf
pro_830 <- sum_inf_830/WS_sum_inf
pro_835 <- sum_inf_835/WS_sum_inf
pro_ung <- sum_inf_ung/WS_sum_inf

#reality check - this should be 1
pro_505 + pro_510 + pro_540 + pro_665 + pro_760 + pro_788 + pro_790 + pro_800 +  pro_805 + pro_830 + pro_835 + pro_ung

#### bring in lake sunapee area and volume data for other calculations ####
#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Lake Sunapee Lake Model/Final Data/lake level and storage") # in begwrkspc
dam_volarea <- read.csv('./from_Bethel/Raw_Data/HypsographyStorage/historical area and volume according to dam depth.csv') #in begwrkspc

dam_volarea$date <- as.Date(dam_volarea$date, format='%Y-%m-%d')

#aggregate rain to the day
MetData_daily <- aggregate(Rain.m_hr ~ date, data=MetData, FUN = sum)
MetData_daily <- rename.vars(MetData_daily, from='Rain.m_hr', to='Rain.m_day')

dam_met <- left_join(MetData_daily, dam_volarea, by='date', all.y=T)

#calculate change in storage between days
dam_met$chg_stor_m3 <- as.numeric('')

for(i in 2:nrow(dam_met)){
  dam_met$chg_stor_m3[i] = dam_met$volume_m3[i-1] - dam_met$volume_m3[i]
}

dam_met_chgstor <- subset(dam_met, select=c('date', 'chg_stor_m3'))

#### calculate base flow based on residence time and that (residence time = volume of lake / inflow) ####
#subset dam data for storage#
stor_dam_met <- subset(dam_met, select=c('date', 'volume_m3'))

#merge storage with WS inflow data
stor_dam_met <- merge(stor_dam_met, WS, by='date', all.x=T)
stor_dam_met <- remove.vars(stor_dam_met, names=('ModStreamTemp_degC'))

#calculate baseflow as volume of lake divided by residence time (converted to seconds) minus inflow model
stor_dam_met$baseflow_m3s <- (stor_dam_met$volume_m3/(3.1 * 365 * 24 * 60 * 60)) - (stor_dam_met$total_runoff_model_imp_50_50_m3s)

#determine the mean baseflow so that value can be applied to the dataset. (too many negative values to apply as is.)
mean_baseflow <- mean(stor_dam_met$baseflow_m3s, na.rm = T)
range(stor_dam_met$baseflow_m3s)

#subset for date and baseflow only
baseflow <- subset(stor_dam_met, select=c('date', 'baseflow_m3s'))
baseflow_ws <- subset(stor_dam_met, select=c('date', 'baseflow_m3s'))

#apply proportion of runoff as the proportion of baseflow (assuming baseflow is equal in all watersheds, proportional to area)
baseflow$base_505_m3s = baseflow$baseflow_m3s * pro_505
baseflow$base_510_m3s = baseflow$baseflow_m3s * pro_510
baseflow$base_540_m3s = baseflow$baseflow_m3s * pro_540
baseflow$base_665_m3s = baseflow$baseflow_m3s * pro_665
baseflow$base_760_m3s = baseflow$baseflow_m3s * pro_760
baseflow$base_788_m3s = baseflow$baseflow_m3s * pro_788
baseflow$base_790_m3s = baseflow$baseflow_m3s * pro_790
baseflow$base_800_m3s = baseflow$baseflow_m3s * pro_800
baseflow$base_805_m3s = baseflow$baseflow_m3s * pro_805
baseflow$base_830_m3s = baseflow$baseflow_m3s * pro_830
baseflow$base_835_m3s = baseflow$baseflow_m3s * pro_835
baseflow$base_ung_m3s = baseflow$baseflow_m3s * pro_ung

#create a list of baseflow for dates and streams (vertical data frame)
baseflow_m <- remove.vars(baseflow, names='baseflow_m3s')

baseflow_m <- melt(baseflow_m, id='date')

#change columng title to stream number
baseflow_m$stream_no <- as.character('')
ix=which(baseflow_m$variable == 'base_505_m3s')
baseflow_m$stream_no[ix]=505
ix=which(baseflow_m$variable == 'base_510_m3s')
baseflow_m$stream_no[ix]=510
ix=which(baseflow_m$variable == 'base_540_m3s')
baseflow_m$stream_no[ix]=540
ix=which(baseflow_m$variable == 'base_665_m3s')
baseflow_m$stream_no[ix]=665
ix=which(baseflow_m$variable == 'base_760_m3s')
baseflow_m$stream_no[ix]=760
ix=which(baseflow_m$variable == 'base_788_m3s')
baseflow_m$stream_no[ix]=788
ix=which(baseflow_m$variable == 'base_790_m3s')
baseflow_m$stream_no[ix]=790
ix=which(baseflow_m$variable == 'base_800_m3s')
baseflow_m$stream_no[ix]=800
ix=which(baseflow_m$variable == 'base_805_m3s')
baseflow_m$stream_no[ix]=805
ix=which(baseflow_m$variable == 'base_830_m3s')
baseflow_m$stream_no[ix]=830
ix=which(baseflow_m$variable == 'base_835_m3s')
baseflow_m$stream_no[ix]=835
ix=which(baseflow_m$variable == 'base_ung_m3s')
baseflow_m$stream_no[ix]='ung'

unique(baseflow_m$stream_no)

#### join inflow and baseflow ####
inf_base <- merge(runoff_all, baseflow_m, by=c('date', 'stream_no'), all.y=T)
inf_base <- rename.vars(inf_base, from=c('value'), to=c('baseflow_m3ps'))
inf_base <- remove.vars(inf_base, names='variable')
inf_base$model_imp_50_50_plusbase_m3ps = inf_base$total_runoff_model_imp_50_50_m3s + inf_base$baseflow_m3ps

#plot inflow plus baseflow to see if there are negatives
plot(inf_base$date, inf_base$model_imp_50_50_plusbase_m3ps)
range(inf_base$model_imp_50_50_plusbase_m3ps, na.rm = TRUE)

inf_base_505 <- subset(inf_base, subset=(stream_no==505))
inf_base_510 <- subset(inf_base, subset=(stream_no==510))
inf_base_540 <- subset(inf_base, subset=(stream_no==540))
inf_base_665 <- subset(inf_base, subset=(stream_no==665))
inf_base_760 <- subset(inf_base, subset=(stream_no==760))
inf_base_788 <- subset(inf_base, subset=(stream_no==788))
inf_base_790 <- subset(inf_base, subset=(stream_no==790))
inf_base_800 <- subset(inf_base, subset=(stream_no==800))
inf_base_805 <- subset(inf_base, subset=(stream_no==805))
inf_base_830 <- subset(inf_base, subset=(stream_no==830))
inf_base_835 <- subset(inf_base, subset=(stream_no==835))
inf_base_ung <- subset(inf_base, subset=(stream_no=='ung'))

#subset for data and total inflow for Nicole
#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Lake Sunapee Lake Model/Final Data/inflow plus baseflow")

#subset by stream
inf_base_505_s <- subset(inf_base_505, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_505_s <- rename.vars(inf_base_505_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_505_s$stream_id <- '505'
write.csv(inf_base_505_s, file=paste0('./data/individual_inflows/505_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_510_s <- subset(inf_base_510, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_510_s <- rename.vars(inf_base_510_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_510_s$stream_id <- '510'
write.csv(inf_base_510_s, file=paste0('./data/individual_inflows/510_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_540_s <- subset(inf_base_540, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_540_s <- rename.vars(inf_base_540_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_540_s$stream_id <- '540'
write.csv(inf_base_540_s, file=paste0('./data/individual_inflows/540_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_665_s <- subset(inf_base_665, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_665_s <- rename.vars(inf_base_665_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_665_s$stream_id <- '665'
write.csv(inf_base_665_s, file=paste0('./data/individual_inflows/665_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_760_s <- subset(inf_base_760, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_760_s <- rename.vars(inf_base_760_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_760_s$stream_id <- '760'
write.csv(inf_base_760_s, file=paste0('./data/individual_inflows/760_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_788_s <- subset(inf_base_788, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_788_s <- rename.vars(inf_base_788_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_788_s$stream_id <- '788'
write.csv(inf_base_788_s, file=paste0('./data/individual_inflows/788_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_790_s <- subset(inf_base_790, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_790_s <- rename.vars(inf_base_790_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_790_s$stream_id <- '790'
write.csv(inf_base_790_s, file=paste0('./data/individual_inflows/790_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_800_s <- subset(inf_base_800, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_800_s <- rename.vars(inf_base_800_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_800_s$stream_id <- '800'
write.csv(inf_base_800_s, file=paste0('./data/individual_inflows/800_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_805_s <- subset(inf_base_805, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_805_s <- rename.vars(inf_base_805_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_805_s$stream_id <- '805'
write.csv(inf_base_805_s, file=paste0('./data/individual_inflows/805_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_830_s <- subset(inf_base_830, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_830_s <- rename.vars(inf_base_830_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_830_s$stream_id <- '830'
write.csv(inf_base_830_s, file=paste0('./data/individual_inflows/830_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_835_s <- subset(inf_base_835, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_835_s <- rename.vars(inf_base_835_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_835_s$stream_id <- '835'
write.csv(inf_base_835_s, file=paste0('./data/individual_inflows/835_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

inf_base_ung_s <- subset(inf_base_ung, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_ung_s <- rename.vars(inf_base_ung_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
inf_base_ung_s$stream_id <- 'ung'
write.csv(inf_base_ung_s, file=paste0('./data/individual_inflows/ung_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

#### calculate additional storage via rain on lake ####
#additional storage is area * aggregated rain/day
dam_met$add_stor_rain_m3 <- dam_met$Rain.m_day * dam_met$area_m2

#### calculate evaporation ####
# per email on 12/1/16, we're assuming that evaporation is even throughout the year and is similar to lake Winnepesaukee at 571.5mm/y
#http://winnipesaukeegateway.org/the-watershed/introduction/

evap_mm_yr <- 571.5
evap_mm_day <- evap_mm_yr/365
evap_m_day <- evap_mm_day/1000

dam_met$evap_m3 <- dam_met$area_m2 * evap_m_day

#### merge runoff, met data, lake level to calculate outflow ####
outflow <- merge(dam_met_chgstor, dam_met, all=T)
outflow <- merge(outflow, WS, by='date', all=T)
outflow <- merge(outflow, baseflow_ws, by='date', all=T)
outflow <- subset(outflow, subset=!is.na(chg_stor_m3))
outflow$outflow_m3d <- outflow$chg_stor_m3 + 
  (outflow$total_runoff_model_imp_50_50_m3s *60 *60 *24) + 
  outflow$add_stor_rain_m3 -
  outflow$evap_m3 +
  (outflow$baseflow_m3s * 60 * 60 * 24)

outflow <- subset(outflow, select=c('date', 'outflow_m3d'))
outflow$outflow_m3s = outflow$outflow_m3d/ (60 * 60 *24)
outflow <- subset(outflow, select=c('date', 'outflow_m3s'))

range(outflow$outflow_m3s)
plot(outflow$date, outflow$outflow_m3s)

#### correct for negative outflows ####
#create a dataframe with negative outflows for the 'balance inflow'
balance_inflow <- subset(outflow, select=c('date', 'outflow_m3s'))
ix=which(balance_inflow$outflow_m3s<0) #select negative values
balance_inflow$bal_inf_m3s = 0 #create a column of 0 values for next step
balance_inflow$bal_inf_m3s[ix] = 0 - (balance_inflow$outflow_m3s [ix]) #create offsetting value
balance_inflow_temp <- merge(balance_inflow, run510_temp_sub, by='date', all=T) #merge with a temp file made from all transducer data to mock an inflow file
balance_inflow_sub <- subset(balance_inflow_temp, select=c('date', 'modelinflow_m3ps', 'ModStreamTemp_degC'))
balance_inflow_sub <- subset(balance_inflow_sub, subset=!is.na(bal_inf_m3s))
balance_inflow_sub$stream_id <- 'bal'


#write .csv
#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Lake Sunapee Lake Model/Final Data/inflow plus baseflow")
write.csv(balance_inflow_sub, paste0('./data/individual_inflows/balance_totalinflow_temp_', Sys.Date(),  '.csv'), row.names = F)

#### correct the outflow dataset and export ####
#merge balance inflows with outflow for 'corrected outflow' dataset
corr_outflow <- merge(outflow, balance_inflow_sub, by='date', all=T)
corr_outflow$corr_outflow_m3s = corr_outflow$outflow_m3s + corr_outflow$bal_inf_m3s

range(corr_outflow$corr_outflow_m3s)

#check work to see if it seems correct
plot(corr_outflow$date, corr_outflow$corr_outflow_m3s)

#subset for needed fields
corr_outflow_sub <- subset(corr_outflow, select=c('date', 'corr_outflow_m3s'))

#write .csv
#setwd("C:/Users/steeleb/Dropbox/Lake Sunapee/Lake Sunapee Lake Model/Final Data/outflow")
write.csv(corr_outflow_sub, paste0('./data/individual_inflows/corr_outflow_impmodel_baseflow_', Sys.Date(),  '.csv'), row.names = F)


