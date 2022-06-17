# script to XXX
# modified from NKW's original version by WMW

library(tidyverse)
library(ggplot2)

alldata <- read.csv('./data/inflowchem_TNTPDOC_allstream_01Mar2021.csv')

alldata$date<-as.character(alldata$date)
alldata$date<-as.POSIXct(alldata$date, format="%Y-%m-%d")
alldata$year<-as.POSIXct(alldata$date, format="%Y")
ggplot(alldata, aes(x = date, y = TP_mmolm3)) +
  geom_point() +
  facet_wrap(~stream_no)

alldata$NtoP<-alldata$TN_mmolm3/alldata$TP_mmolm3
hist(alldata$NtoP)
mu<-mean(alldata$NtoP, na.rm = TRUE)  # Set the mean.
sigma<-sd(alldata$NtoP, na.rm = TRUE) # Set the standard deviation.

Boot_TP<-read.csv("./data/individual_inflows/Boot_Inflows_2021-03-16.csv", header=TRUE)

inflows<-as.numeric(c("505","790","830","788","510","540","800","835","805","670","760"))
inflowName<-c("i505", "i790", "i830", "i788","i510","i540","i800","i835","i805","i670","i760")

NtoP<-NA*numeric(length=nrow(Boot_TP))
n = 1000       # Number of random numbers to use.

for (i in 1:length(inflows)){
  for(j in 1:nrow(Boot_TP)){
    
    randNorm <- rnorm(n, mu, sigma) # Use rnorm function to generate random numbers from N:P distribution.
    TN_iteration<-Boot_TP[j,2+i]*randNorm[min(which(randNorm > 0))]
    NtoP[j]<-TN_iteration
  }
title<-paste(inflowName[i],"_TN", sep="")
assign(title,NtoP)
}

NtoP_Inflows<-as.data.frame(cbind(Boot_TP,i505_TN,
                                  i790_TN, i830_TN, i788_TN,
                                  i510_TN,i540_TN,i800_TN,i835_TN,i805_TN,i670_TN,i760_TN))
NtoP_Inflows$V1 <- as.Date(NtoP_Inflows$V1)

write.csv(x = NtoP_Inflows,file = paste0("./data/individual_inflows/TN_TP_Inflow_conc_boot_", Sys.Date(), ".csv"),
          row.names = FALSE, quote = FALSE)

### N and P fractions ## based on fractions described in prospectus draft to committee March 13 2017



ads<-NA*numeric(length=length(NtoP_Inflows$i505_TP))
frp<-NA*numeric(length=length(NtoP_Inflows$i505_TP))
pop<-NA*numeric(length=length(NtoP_Inflows$i505_TP))
dop<-NA*numeric(length=length(NtoP_Inflows$i505_TP))

for (i in 1:length(inflows)){
  for(j in 1:nrow(NtoP_Inflows)){
    TPindex<-NtoP_Inflows[j,(1+i)]
    adsindex<-TPindex*0.5
    frpindex<-TPindex*0.0295
    popindex<-TPindex*0.327
    dopindex<-TPindex*0.1435
    
    ads[j]<-adsindex
    frp[j]<-frpindex
    pop[j]<-popindex
    dop[j]<-dopindex
  }
  title1<-paste(inflowName[i],"_PHS_ads", sep="")
  assign(title1,ads)
  title2<-paste(inflowName[i],"_PHS_frp", sep="")
  assign(title2,frp)
  title3<-paste(inflowName[i],"_OGM_pop", sep="")
  assign(title3,pop)
  title4<-paste(inflowName[i],"_OGM_dop", sep="")
  assign(title4,dop)
}

NtoP_Inflows_frac<-as.data.frame(cbind(NtoP_Inflows,
                                       i505_PHS_ads,i505_PHS_frp,i505_OGM_pop,i505_OGM_dop,
                                       i790_PHS_ads,i790_PHS_frp,i790_OGM_pop,i790_OGM_dop,
                                       i830_PHS_ads,i830_PHS_frp,i830_OGM_pop,i830_OGM_dop,
                                       i788_PHS_ads,i788_PHS_frp,i788_OGM_pop,i788_OGM_dop,
                                       i510_PHS_ads,i510_PHS_frp,i510_OGM_pop,i510_OGM_dop,
                                       i540_PHS_ads,i540_PHS_frp,i540_OGM_pop,i540_OGM_dop,
                                       i800_PHS_ads,i800_PHS_frp,i800_OGM_pop,i800_OGM_dop,
                                       i835_PHS_ads,i835_PHS_frp,i835_OGM_pop,i835_OGM_dop,
                                       i805_PHS_ads,i805_PHS_frp,i805_OGM_pop,i805_OGM_dop,
                                       i670_PHS_ads,i670_PHS_frp,i670_OGM_pop,i670_OGM_dop,
                                       i760_PHS_ads,i760_PHS_frp,i760_OGM_pop,i760_OGM_dop))

nit<-NA*numeric(length=length(NtoP_Inflows$i505_TN))
amm<-NA*numeric(length=length(NtoP_Inflows$i505_TN))
pon<-NA*numeric(length=length(NtoP_Inflows$i505_TN))
don<-NA*numeric(length=length(NtoP_Inflows$i505_TN))

for (i in 1:length(inflows)){
  for(j in 1:nrow(NtoP_Inflows)){
    TPindex<-NtoP_Inflows[j,(13+i)]
    nitindex<-TPindex*0.1
    ponindex<-TPindex*0.4
    nit[j]<-nitindex
    amm[j]<-nitindex
    pon[j]<-ponindex
    don[j]<-ponindex
  }
  title1<-paste(inflowName[i],"_NIT_nit", sep="")
  assign(title1,nit)
  title2<-paste(inflowName[i],"_NIT_amm", sep="")
  assign(title2,amm)
  title3<-paste(inflowName[i],"_OGM_pon", sep="")
  assign(title3,pon)
  title4<-paste(inflowName[i],"_OGM_don", sep="")
  assign(title4,don)
}

NtoP_Inflows_fracNP<-as.data.frame(cbind(NtoP_Inflows_frac,
                                         i505_NIT_nit,i505_NIT_amm,i505_OGM_pon,i505_OGM_don,
                                         i790_NIT_nit,i790_NIT_amm,i790_OGM_pon,i790_OGM_don,
                                         i830_NIT_nit,i830_NIT_amm,i830_OGM_pon,i830_OGM_don,
                                         i788_NIT_nit,i788_NIT_amm,i788_OGM_pon,i788_OGM_don,
                                         i510_NIT_nit,i510_NIT_amm,i510_OGM_pon,i510_OGM_don,
                                         i540_NIT_nit,i540_NIT_amm,i540_OGM_pon,i540_OGM_don,
                                         i800_NIT_nit,i800_NIT_amm,i800_OGM_pon,i800_OGM_don,
                                         i835_NIT_nit,i835_NIT_amm,i835_OGM_pon,i835_OGM_don,
                                         i805_NIT_nit,i805_NIT_amm,i805_OGM_pon,i805_OGM_don,
                                         i670_NIT_nit,i670_NIT_amm,i670_OGM_pon,i670_OGM_don,
                                         i760_NIT_nit,i760_NIT_amm,i760_OGM_pon,i760_OGM_don))

write.csv(x = NtoP_Inflows_fracNP,file = paste0("./data/individual_inflows/TN_TP_Inflow_conc_fracNP_boot_", Sys.Date(), ".csv"),
          row.names = FALSE, quote = FALSE)
# STOP HERE WW ###
#######################################################################################################################################################

## N AND P LOADS ##
### TP load ###
#setwd('/Users/nicoleward/Documents/VirginiaTech/GLM/Sunapee_GLM/inflow model')
files<-list.files("./data/individual_inflows/", pattern = "*totalinflow_temp_2021-03-17.csv")

flowdata <- do.call(rbind,lapply(paste0("./data/individual_inflows/", files), read.csv))

#flowdata$date<-as.character(flowdata$date)
flowdata$date<-as.Date(flowdata$date)

require(plyr); require(lubridate)
flowdata<-mutate(flowdata, date1 = ymd(date), year = year(date1))


tpload505_mg<-rep(NA,length=nrow(NtoP_Inflows))
tpload790_mg<-rep(NA,length=nrow(NtoP_Inflows))
tpload805_mg<-rep(NA,length=nrow(NtoP_Inflows))
tpload830_mg<-rep(NA,length=nrow(NtoP_Inflows))
tpload788_mg<-rep(NA,length=nrow(NtoP_Inflows))
tnload505_mg<-rep(NA,length=nrow(NtoP_Inflows))
tnload790_mg<-rep(NA,length=nrow(NtoP_Inflows))
tnload805_mg<-rep(NA,length=nrow(NtoP_Inflows))
tnload830_mg<-rep(NA,length=nrow(NtoP_Inflows))
tnload788_mg<-rep(NA,length=nrow(NtoP_Inflows))
date_load<-rep(NA,length=nrow(NtoP_Inflows))
loads<-cbind(date_load,tpload505_mg,tpload790_mg,tpload805_mg,tpload830_mg,tpload788_mg,
             tnload505_mg,tnload790_mg,tnload805_mg,tnload830_mg,tnload788_mg)
loads<-as.data.frame(loads)

# limit to just days where inflow data are available
NtoP_Inflows <- NtoP_Inflows[NtoP_Inflows$V1 %in% flowdata$date,]

for (i in 1:5){
  for (j in 1:nrow(NtoP_Inflows)){
    Tpcon_in<-NtoP_Inflows[j,(2+i)] #mmol per m3
    Tncon_in<-NtoP_Inflows[j,(8+i)] #mmol per m3
    date_in<-as.character(NtoP_Inflows[j,1]) #previously: as.character(NtoP_Inflows[j,2])
    flow_index<-flowdata[which(flowdata$stream_id==inflows[i]),]
    vol_in<-flow_index[which(flow_index$date==date_in),2] ##ML/day
    vol_in<-vol_in*1000 #m3 per day
    Tpcon_in<-Tpcon_in/30 #mg per m3
    Tncon_in<-Tncon_in/30 #mg per m3
    
    pload_in<- vol_in*Tpcon_in #mg per day
    nload_in<- vol_in*Tncon_in #mg per day
    loads[j,1]<-date_in
    loads[j,(1+i)]<-pload_in
    loads[j,(6+i)]<-nload_in
  }
}

loads1<-mutate(loads, date1 = ymd(date_load), year = year(date1))

### CUMULATIVE ANNUAL LOADS ####
loads1$yearly.pcumsum.505_g=unlist(tapply(loads1$tpload505_mg/1000, loads1$year, FUN=cumsum))
loads1$yearly.pcumsum.790_g=unlist(tapply(loads1$tpload790_mg/1000, loads1$year, FUN=cumsum))
loads1$yearly.pcumsum.805_g=unlist(tapply(loads1$tpload805_mg/1000, loads1$year, FUN=cumsum))
loads1$yearly.pcumsum.830_g=unlist(tapply(loads1$tpload830_mg/1000, loads1$year, FUN=cumsum))
loads1$yearly.pcumsum.788_g=unlist(tapply(loads1$tpload788_mg/1000, loads1$year, FUN=cumsum))
loads1$yearly.ncumsum.505_kg=unlist(tapply(loads1$tnload505_mg/1000000, loads1$year, FUN=cumsum))
loads1$yearly.ncumsum.790_kg=unlist(tapply(loads1$tnload790_mg/1000000, loads1$year, FUN=cumsum))
loads1$yearly.ncumsum.805_kg=unlist(tapply(loads1$tnload805_mg/1000000, loads1$year, FUN=cumsum))
loads1$yearly.ncumsum.830_kg=unlist(tapply(loads1$tnload830_mg/1000000, loads1$year, FUN=cumsum))
loads1$yearly.ncumsum.788_kg=unlist(tapply(loads1$tnload788_mg/1000000, loads1$year, FUN=cumsum))
###### PLOTS ####
index<-loads1[complete.cases(loads1$yearly.pcumsum.505_g),]
index<-as.data.frame(index)
index$date_load<-as.Date(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.pcumsum.505_g, type="l",ylab="Cumulative Annual P Load (g)", 
     main="505 Otter Pond",col="dodgerblue4",lwd=3,
     xlab="Year")#, xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.ncumsum.505_kg),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.ncumsum.505_kg, type="l",ylab="Cumulative Annual N Load (kg)", 
     main="505 Otter Pond",col="darkred",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.pcumsum.790_g),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.pcumsum.790_g, type="l",ylab="Cumulative Annual P Load (g)", 
     main="790 Blodgett N",col="dodgerblue4",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))


index<-loads1[complete.cases(loads1$yearly.ncumsum.790_kg),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.ncumsum.790_kg, type="l",ylab="Cumulative Annual N Load (kg)", 
     main="790 Blodgett N",col="darkred",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.pcumsum.805_g),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.pcumsum.805_g, type="l",ylab="Cumulative Annual P Load (g)", 
     main="805 King Hill",col="dodgerblue4",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.ncumsum.805_kg),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.ncumsum.805_kg, type="l",ylab="Cumulative Annual N Load (kg)", 
     main="805 King Hill",col="darkred",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.pcumsum.830_g),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.pcumsum.830_g, type="l",ylab="Cumulative Annual P Load (g)", 
     main="830 Herrick S",col="dodgerblue4",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.ncumsum.830_kg),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.ncumsum.830_kg, type="l",ylab="Cumulative Annual N Load (kg)", 
     main="830 Herrick S",col="darkred",lwd=3,
     xlab="Year", xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.pcumsum.788_g),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.pcumsum.788_g, type="l",ylab="Cumulative Annual P Load (g)", 
     main="788 Blodgett S",col="dodgerblue4",lwd=3,
     xlab="Year")#, xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

index<-loads1[complete.cases(loads1$yearly.ncumsum.788_kg),]
index<-as.data.frame(index)
index$date_load<-as.character(index$date_load)
index$date_load<-as.POSIXct(index$date_load, format="%Y-%m-%d")
plot(index$date_load,index$yearly.ncumsum.788_kg, type="l",ylab="Cumulative Annual N Load (kg)", 
     main="788 Blodgett S",col="darkred",lwd=3,
     xlab="Year")#, xlim=c(as.POSIXct("1988-01-01"),as.POSIXct("2014-01-01")))

