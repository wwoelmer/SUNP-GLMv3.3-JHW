# modified from NKW's original script by WMW
## Bootstrapping for Sunapee GLM Ch. 1 paper NKW Nov 7 2016 (Day before the fateful election)



rm(list=ls())
# read in file with daily stream data for each of the locations
alldata <- read.csv('./data/inflowchem_TNTPDOC_allstream_01Mar2021.csv')

alldata$date<-as.character(alldata$date)
alldata$date<-as.POSIXct(alldata$date, format="%Y-%m-%d")
alldata$year<-as.POSIXct(alldata$date, format="%Y")

plot(alldata$date, alldata$TP_mmolm3)
plot(alldata$date, alldata$TN_mmolm3)
plot(alldata$date, alldata$DOC_mmolm3)


require(plyr); require(lubridate)
alldata<-mutate(alldata, date1 = ymd(date), year = year(date1))

inflows<-as.numeric(c("505","790","830","788","510","540","800","835","805","670","760"))
inflowName<-c("i505", "i790", "i830", "i788","i510","i540","i800","i835","i805","i670","i760")
#check for zero!!!

## table of mu and sigma ####
blocks<-c("pre1991", "1991-1995", "1996-2000", "2001-2005", "2006-2010", "post 2011")
bl_start_yr<-c(1985,1990,1995,2000,2005,2010)
bl_end_yr<-c(1991,1996,2001,2006,2011,2018)


block_dist<-matrix(data = NA,nrow = (length(blocks)*length(inflows)),ncol = 3)
colnames(block_dist)<-c("name","mu","sigma")

## remove crazy values (~3 in otter pond and ~15 in blodgett S)
index<-alldata[-(which(alldata$stream_no==505 & alldata$TP_mmolm3>3)),] ##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
index<-index[-(which(index$stream_no==788 & index$TP_mmolm3>15)),]
index<-index[-(which(index$stream_no==540 & index$TP_mmolm3>20)),]
index<-index[-(which(index$stream_no==670 & index$TP_mmolm3>6)),]
index$TP_mmolm3[index$TP_mmolm3<0.1520642]<-0.1520642

k=1
for (i in 1:length(inflows)){
  for (j in 1:length(blocks)){
    obs_dist_in<-index[which(index$stream_no==inflows[i] & index$year>bl_start_yr[j] 
                             & index$year<bl_end_yr[j]),] 
    mu<-mean(obs_dist_in$TP_mmolm3, na.rm = TRUE)  # Set the mean.
    sigma<-sd(obs_dist_in$TP_mmolm3, na.rm = TRUE) # Set the standard deviation.
    
    block_dist[k,1]<-paste(inflowName[i],"_",blocks[j], sep="")
    block_dist[k,2]<-round(mu, digits = 3)
    block_dist[k,3]<-round(sigma, digits = 3)
    k<-k+1
  }
}


##### bootstrapping ######
date<-seq(as.Date("1979-01-01"), as.Date("2020-12-30"), by="days")
boot<-NA*numeric(length=length(date))

#index<-alldata[-(which(alldata$stream_no==505 & alldata$TP_mmolm3>3)),] ##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#index<-index[-(which(index$stream_no==788 & index$TP_mmolm3>15)),]
j=1
i=1

for(j in 1:length(inflows)){
  inflow<-index[which(index$stream_no==inflows[j]),]
  for(i in 1:length(date)){
    datemin<-min(inflow$year, na.rm=TRUE)
    boot_min<-datemin+2
    yr_in<-year(date[i])
    
    if(year(date[i])<boot_min){
      obs_dist_in<-inflow[which(inflow$year<(datemin+5)),] 
      n = 1000       # Number of random numbers to use.
      mu<-mean(obs_dist_in$TP_mmolm3, na.rm = TRUE)  # Set the mean.
      sigma<-sd(obs_dist_in$TP_mmolm3, na.rm = TRUE) # Set the standard deviation.
      randNorm <- rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
      TP_iteration<-randNorm[min(which(randNorm > 0))]
      
      boot[i]<-TP_iteration
    } else if(year(date[i])>2012){
      obs_dist_in<-inflow[which(inflow$year>(2010)),] 
      n = 1000       # Number of random numbers to use.
      mu<-mean(obs_dist_in$TP_mmolm3, na.rm = TRUE)  # Set the mean.
      sigma<-sd(obs_dist_in$TP_mmolm3, na.rm = TRUE) # Set the standard deviation.
      randNorm <- rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
      TP_iteration<-randNorm[min(which(randNorm > 0))]
      
      boot[i]<-TP_iteration
    } else{
      obs_dist_in<-inflow[which(inflow$year>(yr_in-2) & inflow$year<(yr_in+2)),] 
      n = 1000       # Number of random numbers to use.
      mu<-mean(obs_dist_in$TP_mmolm3, na.rm = TRUE)  # Set the mean.
      sigma<-sd(obs_dist_in$TP_mmolm3, na.rm = TRUE) # Set the standard deviation.
      randNorm <- rnorm(n, mu, sigma) # Use rnorm function to generate random numbers.
      TP_iteration<-randNorm[min(which(randNorm > 0))]
      
      boot[i]<-TP_iteration
    }
  }
  title<-paste(inflowName[j],"_TP", sep="")
  assign(title,boot)
}

Boot_Inflows<-as.data.frame(cbind(as.character(date),i505_TP,
                                  i790_TP, i830_TP, i788_TP,
                                  i510_TP,i540_TP,i800_TP,i835_TP,i805_TP,i670_TP,i760_TP))

for(i in 1:nrow(Boot_Inflows)){
  TP_index<-sum(as.numeric(as.character(Boot_Inflows[i,2])), 
                as.numeric(as.character(Boot_Inflows[i,3])),
                as.numeric(as.character(Boot_Inflows[i,4])), 
                as.numeric(as.character(Boot_Inflows[i,5])),
                as.numeric(as.character(Boot_Inflows[i,6])),
                as.numeric(as.character(Boot_Inflows[i,7])), 
                as.numeric(as.character(Boot_Inflows[i,8])),
                as.numeric(as.character(Boot_Inflows[i,9])), 
                as.numeric(as.character(Boot_Inflows[i,10])),
                as.numeric(as.character(Boot_Inflows[i,11])),
                as.numeric(as.character(Boot_Inflows[i,12])),na.rm = TRUE)

  Boot_Inflows[i,13]<-TP_index
}

colnames(Boot_Inflows)[1] <- 'Date'
colnames(Boot_Inflows)[13] <- 'Inflow_all_TP'

write.csv(x = Boot_Inflows,file = paste0("./data/individual_inflows/Boot_Inflows_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)



