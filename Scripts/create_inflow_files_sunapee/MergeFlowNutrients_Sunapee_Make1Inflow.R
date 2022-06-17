rm(list=ls())

#options(scipen = 999)

nuts<-read.csv("./data/individual_inflows/TN_TP_Inflow_conc_fracNP_boot_2021-03-16.csv")
files<-list.files("./data/individual_inflows/", pattern = "*totalinflow_temp_2021-03-17.csv")
alldata <- do.call(rbind,lapply(paste0("./data/individual_inflows/", files), read.csv))

alldata$date<-as.character(alldata$date)
alldata$date<-as.POSIXct(alldata$date, format="%Y-%m-%d")
alldata$year<-as.POSIXct(alldata$date, format="%Y")


library(dplyr)

i505<-alldata[which(alldata$stream_id==505),]
i505<- subset(i505, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i505)[names(i505)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW505","TEMP505")

nut505<-subset(nuts,select=c("V1","i505_TP","i505_TN"))
names(nut505)[names(nut505)==c("V1", "i505_TP","i505_TN")] <- c("time","TP505","TN505")
nut505$time<-as.character(nut505$time)
nut505$time<-as.POSIXct(nut505$time, format="%Y-%m-%d")
nut505$time<-as.POSIXct(nut505$time, format="%Y")
i505 <- i505 %>% left_join(nut505)

i790<-alldata[which(alldata$stream_id==790),]
i790<- subset(i790, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i790)[names(i790)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW790","TEMP790")
nut790<-subset(nuts,select=c("V1","i790_TP","i790_TN"))
names(nut790)[names(nut790)==c("V1", "i790_TP","i790_TN")] <- c("time","TP790","TN790")
nut790$time<-as.character(nut790$time)
nut790$time<-as.POSIXct(nut790$time, format="%Y-%m-%d")
nut790$time<-as.POSIXct(nut790$time, format="%Y")
i790 <- i790 %>% left_join(nut790)

i830<-alldata[which(alldata$stream_id==830),]
i830<- subset(i830, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i830)[names(i830)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW830","TEMP830")
nut830<-subset(nuts,select=c("V1","i830_TP","i830_TN"))
names(nut830)[names(nut830)==c("V1", "i830_TP","i830_TN")] <- c("time","TP830","TN830")
nut830$time<-as.character(nut830$time)
nut830$time<-as.POSIXct(nut830$time, format="%Y-%m-%d")
nut830$time<-as.POSIXct(nut830$time, format="%Y")
i830 <- i830 %>% left_join(nut830)

i788<-alldata[which(alldata$stream_id==788),]
i788<- subset(i788, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i788)[names(i788)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW788","TEMP788")
nut788<-subset(nuts,select=c("V1","i788_TP","i788_TN"))
names(nut788)[names(nut788)==c("V1", "i788_TP","i788_TN")] <- c("time","TP788","TN788")
nut788$time<-as.character(nut788$time)
nut788$time<-as.POSIXct(nut788$time, format="%Y-%m-%d")
nut788$time<-as.POSIXct(nut788$time, format="%Y")
i788 <- i788 %>% left_join(nut788)

i510<-alldata[which(alldata$stream_id==510),]
i510<- subset(i510, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i510)[names(i510)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW510","TEMP510")
nut510<-subset(nuts,select=c("V1","i510_TP","i510_TN"))
names(nut510)[names(nut510)==c("V1", "i510_TP","i510_TN")] <- c("time","TP510","TN510")
nut510$time<-as.character(nut510$time)
nut510$time<-as.POSIXct(nut510$time, format="%Y-%m-%d")
nut510$time<-as.POSIXct(nut510$time, format="%Y")
i510 <- i510 %>% left_join(nut510)

i540<-alldata[which(alldata$stream_id==540),]
i540<- subset(i540, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i540)[names(i540)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW540","TEMP540")
nut540<-subset(nuts,select=c("V1","i540_TP","i540_TN"))
names(nut540)[names(nut540)==c("V1", "i540_TP","i540_TN")] <- c("time","TP540","TN540")
nut540$time<-as.character(nut540$time)
nut540$time<-as.POSIXct(nut540$time, format="%Y-%m-%d")
nut540$time<-as.POSIXct(nut540$time, format="%Y")
i540 <- i540 %>% left_join(nut540)

# 
i800<-alldata[which(alldata$stream_id==800),]
i800<- subset(i800, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i800)[names(i800)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW800","TEMP800")
nut800<-subset(nuts,select=c("V1","i800_TP","i800_TN"))
names(nut800)[names(nut800)==c("V1", "i800_TP","i800_TN")] <- c("time","TP800","TN800")
nut800$time<-as.character(nut800$time)
nut800$time<-as.POSIXct(nut800$time, format="%Y-%m-%d")
nut800$time<-as.POSIXct(nut800$time, format="%Y")
i800 <- i800 %>% left_join(nut800)

i835<-alldata[which(alldata$stream_id==835),]
i835<- subset(i835, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i835)[names(i835)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW835","TEMP835")
nut835<-subset(nuts,select=c("V1","i835_TP","i835_TN"))
names(nut835)[names(nut835)==c("V1", "i835_TP","i835_TN")] <- c("time","TP835","TN835")
nut835$time<-as.character(nut835$time)
nut835$time<-as.POSIXct(nut835$time, format="%Y-%m-%d")
nut835$time<-as.POSIXct(nut835$time, format="%Y")
i835 <- i835 %>% left_join(nut835)

i805<-alldata[which(alldata$stream_id==805),]
i805<- subset(i805, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i805)[names(i805)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW805","TEMP805")
nut805<-subset(nuts,select=c("V1","i805_TP","i805_TN"))
names(nut805)[names(nut805)==c("V1", "i805_TP","i805_TN")] <- c("time","TP805","TN805")
nut805$time<-as.character(nut805$time)
nut805$time<-as.POSIXct(nut805$time, format="%Y-%m-%d")
nut805$time<-as.POSIXct(nut805$time, format="%Y")
i805 <- i805 %>% left_join(nut805)

i665<-alldata[which(alldata$stream_id==665),]
i665<- subset(i665, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i665)[names(i665)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW665","TEMP665")
nut665<-subset(nuts,select=c("V1","i670_TP","i670_TN"))
names(nut665)[names(nut665)==c("V1", "i670_TP","i670_TN")] <- c("time","TP665","TN665")
nut665$time<-as.character(nut665$time)
nut665$time<-as.POSIXct(nut665$time, format="%Y-%m-%d")
nut665$time<-as.POSIXct(nut665$time, format="%Y")
i665 <- i665 %>% left_join(nut665)


i760<-alldata[which(alldata$stream_id==760),]
i760<- subset(i760, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(i760)[names(i760)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOW760","TEMP760")
nut760<-subset(nuts,select=c("V1","i760_TP","i760_TN"))
names(nut760)[names(nut760)==c("V1", "i760_TP","i760_TN")] <- c("time","TP760","TN760")
nut760$time<-as.character(nut760$time)
nut760$time<-as.POSIXct(nut760$time, format="%Y-%m-%d")
nut760$time<-as.POSIXct(nut760$time, format="%Y")
i760 <- i760 %>% left_join(nut760)


nuts<-nuts%>%mutate(aveTP=(i505_TP+i790_TP+i830_TP+i788_TP+i510_TP+i540_TP+i800_TP+
                               i835_TP+i805_TP+i670_TP+i760_TP)/11)
nuts<-nuts%>%mutate(aveTN=(i505_TN+i790_TN+i830_TN+i788_TN+i510_TN+i540_TN+i800_TN+
                             i835_TN+i805_TN+i670_TN+i760_TN)/11)
test<-subset(nuts,select=c("V1","aveTP","aveTN"))

iUNG<-alldata[which(alldata$stream_id=="ung"),]
iUNG<- subset(iUNG, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(iUNG)[names(iUNG)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOWung","TEMPung")

#nutUNG<-test #!!!14June19 testing how to reduce nutrient concentration for overall inflow concentration being ~3x epi concentration
nutUNG<-subset(nuts,select=c("V1","i505_TP","i505_TN"))

#names(nutUNG)[names(nutUNG)==c("V1","aveTP","aveTN")] <- c("time","TPung","TNung")
names(nutUNG)[names(nutUNG)==c("V1","i505_TP","i505_TN")] <- c("time","TPung","TNung")
nutUNG$time<-as.character(nutUNG$time)
nutUNG$time<-as.POSIXct(nutUNG$time, format="%Y-%m-%d")
nutUNG$time<-as.POSIXct(nutUNG$time, format="%Y")
iUNG <- iUNG %>% left_join(nutUNG)

iBAL<-alldata[which(alldata$stream_id=="bal"),]
iBAL<- subset(iBAL, select=c("date", "modelinflow_m3ps", "ModStreamTemp_degC"))
names(iBAL)[names(iBAL)==c("date", "modelinflow_m3ps", "ModStreamTemp_degC")] <- c("time","FLOWBAL","TEMPBAL")
nutBAL<-test
names(nutBAL)[names(nutBAL)==c("V1","aveTP","aveTN")] <- c("time","TPBAL","TNBAL")
nutBAL$time<-as.character(nutBAL$time)
nutBAL$TPBAL<-0.0001
nutBAL$TNBAL<-0.0001
nutBAL$time<-as.POSIXct(nutBAL$time, format="%Y-%m-%d")
nutBAL$time<-as.POSIXct(nutBAL$time, format="%Y")
iBAL <- iBAL %>% left_join(nutBAL)

iALL <- iUNG %>% left_join(iBAL)%>%left_join(i505)%>% left_join(i790) %>% 
  left_join(i830)  %>% left_join(i788) %>% left_join(i510) %>% left_join(i540) %>% left_join(i800) %>% 
  left_join(i835) %>% left_join(i805)%>% left_join(i665)%>% left_join(i760)

iALL<-iALL%>%mutate(totFLOW=FLOWung+FLOWBAL+FLOW505+FLOW790+FLOW830+FLOW788+FLOW510+FLOW540+FLOW800+
                      FLOW835+FLOW805+FLOW665+FLOW760)
iALL<-iALL%>%mutate(vwsTP=(TPung*FLOWung/totFLOW)+(TPBAL*FLOWBAL/totFLOW)+(TP505*FLOW505/totFLOW)+(TP790*FLOW790/totFLOW)+
                      (TP830*FLOW830/totFLOW)+(TP788*FLOW788/totFLOW)+(TP510*FLOW510/totFLOW)+
                      (TP540*FLOW540/totFLOW)+(TP800*FLOW800/totFLOW)+
                      (TP835*FLOW835/totFLOW)+
                      (TP805*FLOW805/totFLOW)+(TP665*FLOW665/totFLOW)+(TP760*FLOW760/totFLOW))
iALL<-iALL%>%mutate(vwsTN=(TNung*FLOWung/totFLOW)+(TNBAL*FLOWBAL/totFLOW)+(TN505*FLOW505/totFLOW)+(TN790*FLOW790/totFLOW)+
                      (TN830*FLOW830/totFLOW)+(TN788*FLOW788/totFLOW)+(TN510*FLOW510/totFLOW)+
                      (TN540*FLOW540/totFLOW)+(TN800*FLOW800/totFLOW)+
                      (TN835*FLOW835/totFLOW)+
                      (TN805*FLOW805/totFLOW)+(TN665*FLOW665/totFLOW)+(TN760*FLOW760/totFLOW))
iALL<-iALL%>%mutate(vwsTEMP=(TEMPung*FLOWung/totFLOW)+(TEMPBAL*FLOWBAL/totFLOW)+(TEMP505*FLOW505/totFLOW)+(TEMP790*FLOW790/totFLOW)+
                      (TEMP830*FLOW830/totFLOW)+(TEMP788*FLOW788/totFLOW)+(TEMP510*FLOW510/totFLOW)+
                      (TEMP540*FLOW540/totFLOW)+(TEMP800*FLOW800/totFLOW)+
                      (TEMP835*FLOW835/totFLOW)+
                      (TEMP805*FLOW805/totFLOW)+(TEMP665*FLOW665/totFLOW)+(TEMP760*FLOW760/totFLOW))
iALL<-iALL%>%mutate(SALT=0)
oneINFLOW<-subset(iALL,select=c("time","totFLOW","SALT","vwsTEMP","vwsTP","vwsTN"))
names(oneINFLOW)[names(oneINFLOW)==c("time","totFLOW","SALT","vwsTEMP","vwsTP","vwsTN")] <- 
  c("time","FLOW","SALT","TEMP","TP","TN")

test<-oneINFLOW[which(oneINFLOW$time>"2004-12-26"),]
test<-test[which(test$time<"2011-01-01"),]
mean(test$TP,na.rm=TRUE)*31/1000
mean(test$TN,na.rm=TRUE)*14/1000

oneINFLOW<-oneINFLOW%>%mutate(OGM_doc=125)%>%mutate(OGM_poc=12.5)%>%mutate(OGM_don=0.4*TN)%>%
  mutate(NIT_nit=0.1*TN)%>%mutate(NIT_amm=0.1*TN)%>%mutate(OGM_pon=0.4*TN)%>%
  mutate(PHS_frp=0.0295*TP)%>%mutate(OGM_dop=0.1435*TP)%>%mutate(OGM_pop=0.327*TP)%>%
  mutate(PHS_frp_ads=0.5*TP)

oneINFLOW<-subset(oneINFLOW,select=c("time","FLOW","SALT","TEMP","OGM_doc","OGM_poc",
                                     "OGM_don","NIT_nit","NIT_amm","OGM_pon","PHS_frp","OGM_dop",
                                     "OGM_pop","PHS_frp_ads"))
write.csv(oneINFLOW,paste0("./data/oneInflow", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)
