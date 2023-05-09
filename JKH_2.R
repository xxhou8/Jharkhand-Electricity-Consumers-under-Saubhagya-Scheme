library("dplyr")
library("tidyverse")
library(ggplot2)
library(stargazer)
library(margins)
library(readstata13)
library(survey)
library(Weighted.Desc.Stat)
library(gridExtra)
library(scales)
library(ggpubr)
library(plyr)
library(rlang)

jkhdata_old<-read.csv("JKH.csv")
jkhdata<-read.dta13("jharkhand_access_data.dta")

# jkhdata_raw = jkhdata[is.na(jkhdata.o$survey_weights) %in% F,]
# sdesign = svydesign(id = ~UniqueId,
#                     strata = ~Village_name_cal,
#                     data = jkhdata_raw,
#                     weights = ~survey_weights,
#                     nest = T)
# 
# 
# jkhdata.w <- svydesign(ids = ~1, data = jkhdata.o, weights = jkhdata.o$survey_weights) # ids = ~ 1 to indicate that all respondents originated from the same cluster.
# save(jkhdata.w, file = "jharkhand_access_data_weighted.RData")
# load("jharkhand_access_data_weighted.RData")
# 
# temp<-summary(jkhdata.w)$variables
# jkh<-weighted.mean(q_16,jkhdata.w)
# 
# jkhdata<-read.dta13("jharkhand_access_data_weighted.RData")
# 

### Select basic geographical information and electricity access questions
#data<-svytable( ~"UniqueId"+"District_cal"+"Tehsil_name_cal", jkhdata )
data<-dplyr::select(jkhdata, "UniqueId","District_cal","Tehsil_name_cal","GP_name_cal","Village_name_cal","survey_weights","survey_probability",
                    "q_16", "q_17","q_20","q_21","q_22","q_23","q_24","q_25","q_28","q_31",
                    "q_60","q_60_1","q_60_2","q_60_3","q_60_4", "q_60_5_1","q_60_5_2","q_60_5_3",
                    "q_60_5_4","q_60_6","q_60_6_1","q_60_6_2","q_60_6_2_1","q_60_7","q_60_7_1",
                    "q_60_7_2","q_60_7_2_specify","q_60_7_3", 
                    "q_63","q_64","q_69_1","q_69_2","q_70","q_71","q_74","q_75","q_76","q_77","q_78",
                    "q_79_1","q_79_2","q_79_3","q_79_4","q_79_5","q_79_6","q_79_7","q_79_8","q_79_9",
                    "q_79_10","q_79_11","q_79_12","q_79_13","q_79_14","q_79_15","q_79_16","q_79_17",
                    "q_65","q_66","q_67","q_68",
                    "q_62","q_57","q_58_1","q_62_4","q_62_2","q_62_3_1","q_62_3_2","q_62_3_3","q_62_3_4","q_62_3_5")


### Targeted subsets
grid<-subset(data, q_60=="Yes")

no_grid<-subset(data, q_60=="No")
#a<-subset(grid,grid$q_60_1=="Get connection from other's home"|is.na(grid$q_60_1))

grid$q_60_1<-as.character(grid$q_60_1)
grid$q_60_1<-ifelse(grid$q_60_1=="DK"&!is.na(grid$q_60_1), 0, grid$q_60_1)
#grid[grid$q_60_1=="DK"&!is.na(grid$q_60_1),"q_60_1"]<- 0
grid$q_60_1<-as.numeric(grid$q_60_1)

#grid$q_60_1<-if(grid$q_60_1=="DK"|grid$q_60_1=="Get connection from other's homm"|is.na(grid$q_60_1))
#                    grid$q_60_1

grid$SC<-ifelse(grid$q_60_2=="1"&grid$q_60_1<=2,1,0)

grid$OnlyGrid<-ifelse(grid$q_63=="No"&grid$q_64=="No",1,0)

grid$hhsize<-grid$q_23+grid$q_24+grid$q_25
grid$dayhour<-grid$q_74-grid$q_75
grid$dayhour<-ifelse(grid$dayhour>=0, grid$dayhour, 0)
grid$use<-grid$q_70+grid$q_71
grid$use2<-grid$q_79_1+grid$q_79_2+grid$q_79_3+grid$q_79_4+grid$q_79_5+grid$q_79_6+grid$q_79_7+grid$q_79_8+grid$q_79_9+grid$q_79_10+grid$q_79_11+grid$q_79_12+grid$q_79_13+grid$q_79_14+grid$q_79_15+grid$q_79_16

# Use wattage data to represent load
# data based on assumptions using median data from ISEP-SPI survey reported by households
wattage<-data.frame("IncandescentBulbs"=100,
                    "CF"=14,
                    "LED"=7,
                    "Tubelight"=30,
                    "Fan"=60,
                    "ElectricIron"=750,
                    "Refrigerator"=150,
                    "Television"=40,
                    "ElectricRadio"=15, #q_79_9+q_79_15
                    "Cooler"=180,
                    "WashingMachine"=400,
                    "ElectricStove"=1250,
                    "ElectricWaterPump"=745.7, #1HP
                    "Mixer"=450,
                    "MobileCharger"=5)


grid$use3<-wattage$IncandescentBulbs*grid$q_79_1+
  wattage$CF*grid$q_79_2+wattage$LED*grid$q_79_3+
  wattage$Tubelight*grid$q_79_4+wattage$Fan*grid$q_79_5+
  wattage$ElectricIron*grid$q_79_7+wattage$Refrigerator*grid$q_79_8+
  wattage$Television*grid$q_79_9+wattage$ElectricRadio*(grid$q_79_9+grid$q_79_15)+
  wattage$Cooler*grid$q_79_10+wattage$WashingMachine*grid$q_79_11+
  wattage$ElectricStove*grid$q_79_12+wattage$ElectricWaterPump*grid$q_79_14

no_grid$use3<-wattage$IncandescentBulbs*no_grid$q_79_1+
  wattage$CF*no_grid$q_79_2+wattage$LED*no_grid$q_79_3+
  wattage$Tubelight*no_grid$q_79_4+wattage$Fan*no_grid$q_79_5+
  wattage$ElectricIron*no_grid$q_79_7+wattage$Refrigerator*no_grid$q_79_8+
  wattage$Television*no_grid$q_79_9+wattage$ElectricRadio*(no_grid$q_79_9+no_grid$q_79_15)+
  wattage$Cooler*no_grid$q_79_10+wattage$WashingMachine*no_grid$q_79_11+
  wattage$ElectricStove*no_grid$q_79_12+wattage$ElectricWaterPump*no_grid$q_79_14

grid["Edu_NoFormalSchooling"] <- ifelse(grid$q_20 == "No formal schooling", 1, 0)
grid["Edu_UpTo5thStandard"] <- ifelse(grid$q_20 == "Up to 5th standard", 1, 0)
grid["Edu_MoreThan5thStandard"] <- ifelse(grid$q_20 == "Up to 10th standard" | 
                                            grid$q_20 == "12th standard or diploma" | 
                                            grid$q_20 == "Graduate and above", 1,0)

grid$Male<-ifelse(grid$q_17=="Male",1,0)

grid["Hindu"] <- ifelse(grid$q_21 == "Hindu", 1, 0)
grid["Other"] <- ifelse(grid$Hindu == 1, 1, 0)

grid["General"] <- ifelse(grid$q_22=="General"|grid$q_22=="Other",1,0)
grid["OBC"] <- ifelse(grid$q_22=="Other Backward Class",1,0)
grid["SCST"] <- ifelse(grid$q_22=="SC"|grid$q_22=="ST",1,0)

grid["APL"] <- ifelse(grid$q_28=="APL",1,0)
grid["BPL"] <- ifelse(grid$q_28=="BPL",1,0)
grid["Antyodaya"] <- ifelse(grid$q_28=="Antyodaya",1,0)
grid["None"] <- ifelse(grid$q_28=="None",1,0)

grid$exp<-log(grid$q_31+1)

#grid[c(452,540,648,1266,1267,1268,1269),"SC"]<-0
#grid[grid$UniqueId=="369166_7",]$SC<-0
# |grid$UniqueId=="369233_8"|grid$UniqueId=="369247_10"|grid$Uniq,UniqueId=="355323_2"|
#                   grid$UniqueId=="355323_3"|grid$UniqueID=="355323_4"|grid$UniqueId=="355323_5"]<-0

grid$NSC<-ifelse(grid$SC=="1",0,1)

SC<-subset(grid, SC=="1") 
NSC<-subset(grid, SC=="0") 

SC$nopay<-ifelse(SC$q_60_4=="No payment",0,1)
NSC$nopay<-ifelse(NSC$q_60_4=="No payment",0,1)

NSCusage<-data.frame("IncandescentBulbs"=4.471014,
                    "CF"=0.3454969,
                    "LED"=14.51825,
                    "Tubelight"=0.0799689,
                    "Fan"=7.962629,
                    "ElectricIron"=0.0266822,
                    "Refrigerator"=1.195652,
                    "Television"=0,
                    "ElectricRadio"=0, #q_79_9+q_79_15
                    "Cooler"=0.1521739,
                    "WashingMachine"=0.0091874,
                    "ElectricStove"=0.0031056,
                    "ElectricWaterPump"=0.0243012, #1HP
                    "Mixer"=0.0158126,
                    "MobileCharger"=0)

NSCusage_l<-data.frame("IncandescentBulbs"=3.779885707,
                     "CF"=0.159940786,
                     "LED"=13.46121657,
                     "Tubelight"=0.02454361,
                     "Fan"=7.237860849,
                     "ElectricIron"=0.017526576,
                     "Refrigerator"=0.90230311,
                     "Television"=0,
                     "ElectricRadio"=0, #q_79_9+q_79_15
                     "Cooler"=0.084539222,
                     "WashingMachine"=0.003005571,
                     "ElectricStove"=0,
                     "ElectricWaterPump"=0.012387111, #1HP
                     "Mixer"=0.011918196,
                     "MobileCharger"=0)

NSCusage_u<-data.frame("IncandescentBulbs"=5.162142293,
                       "CF"=0.531053014,
                       "LED"=15.57528343,
                       "Tubelight"=0.13539419,
                       "Fan"=8.687397151,
                       "ElectricIron"=0.035837824,
                       "Refrigerator"=1.48900089,
                       "Television"=0,
                       "ElectricRadio"=0, #q_79_9+q_79_15
                       "Cooler"=0.219808578,
                       "WashingMachine"=0.015369229,
                       "ElectricStove"=0.009192553,
                       "ElectricWaterPump"=0.036215289, #1HP
                       "Mixer"=0.019707004,
                       "MobileCharger"=0)

SCusage<-data.frame("IncandescentBulbs"=5.741071,
                     "CF"=0.1815476,
                     "LED"=9.020833,
                     "Tubelight"=0.0059524,
                     "Fan"=3.865575,
                     "ElectricIron"=0.0048611,
                     "Refrigerator"=0.2589286,
                     "Television"=0,
                     "ElectricRadio"=0, #q_79_9+q_79_15
                     "Cooler"=0,
                     "WashingMachine"=0,
                     "ElectricStove"=0,
                     "ElectricWaterPump"=0.0029762, #1HP
                     "Mixer"=0.0046627 ,
                     "MobileCharger"=0)

SCusage_l<-data.frame("IncandescentBulbs"=4.622242601,
                    "CF"=0,
                    "LED"=7.290816553,
                    "Tubelight"=0,
                    "Fan"=2.932884945,
                    "ElectricIron"=0.000684146,
                    "Refrigerator"=0,
                    "Television"=0,
                    "ElectricRadio"=0, #q_79_9+q_79_15
                    "Cooler"=0,
                    "WashingMachine"=0,
                    "ElectricStove"=0,
                    "ElectricWaterPump"=0, #1HP
                    "Mixer"=0.000250407 ,
                    "MobileCharger"=0)

SCusage_u<-data.frame("IncandescentBulbs"=6.859899399,
                      "CF"=0.397222308,
                      "LED"=10.75084945,
                      "Tubelight"=0.01761907,
                      "Fan"=4.798265055,
                      "ElectricIron"=0.009038054,
                      "Refrigerator"=0.576811675,
                      "Television"=0,
                      "ElectricRadio"=0, #q_79_9+q_79_15
                      "Cooler"=0,
                      "WashingMachine"=0,
                      "ElectricStove"=0,
                      "ElectricWaterPump"=0.008809528, #1HP
                      "Mixer"=0.009074993 ,
                      "MobileCharger"=0)

SCusage1<-gather(SCusage, Appliances, "Daily average usage (Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)
NSCusage1<-gather(NSCusage, Appliances, "Daily average usage (Non-Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)
SCusage2<-gather(SCusage_l, Appliances, "Daily average usage (Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)
NSCusage2<-gather(NSCusage_l, Appliances, "Daily average usage (Non-Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)
SCusage3<-gather(SCusage_u, Appliances, "Daily average usage (Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)
NSCusage3<-gather(NSCusage_u, Appliances, "Daily average usage (Non-Saubhagya) (kWh)", IncandescentBulbs:MobileCharger, factor_key=TRUE)



usage<-SCusage1
usage[c("Lower (Saubhagya) (kWh)")]<-SCusage2[,2]
usage[c("Upper (Saubhagya) (kWh)")]<-SCusage3[,2]
usage[c("Daily average usage (Non-Saubhagya) (kWh)")]<-NSCusage1[,2]
usage[c("Lower (Non-Saubhagya) (kWh)")]<-NSCusage2[,2]
usage[c("Upper (Non-Saubhagya) (kWh)")]<-NSCusage3[,2]
  
write.table(usage, "usage.txt", quote=FALSE, eol="\\\\\n", sep=" & ")


######## CALCULATE FINANCIAL LOSS #######

SC_usage_month<-(wattage$IncandescentBulbs*SCusage$IncandescentBulbs+
  wattage$CF*SCusage$CF+wattage$LED*SCusage$LED+
  wattage$Tubelight*SCusage$Tubelight+wattage$Fan*SCusage$Fan+
  wattage$ElectricIron*SCusage$ElectricIron+wattage$Refrigerator*SCusage$Refrigerator+
  wattage$Television*SCusage$Television+wattage$ElectricRadio*SCusage$ElectricRadio+
  wattage$Cooler*SCusage$Cooler+wattage$WashingMachine*SCusage$WashingMachine+
  wattage$ElectricStove*SCusage$ElectricStove+wattage$ElectricWaterPump*SCusage$ElectricWaterPump)*30/1000

SC_usage_month_l<-(wattage$IncandescentBulbs*SCusage_l$IncandescentBulbs+
                   wattage$CF*SCusage_l$CF+wattage$LED*SCusage_l$LED+
                   wattage$Tubelight*SCusage_l$Tubelight+wattage$Fan*SCusage_l$Fan+
                   wattage$ElectricIron*SCusage_l$ElectricIron+wattage$Refrigerator*SCusage_l$Refrigerator+
                   wattage$Television*SCusage_l$Television+wattage$ElectricRadio*SCusage_l$ElectricRadio+
                   wattage$Cooler*SCusage_l$Cooler+wattage$WashingMachine*SCusage_l$WashingMachine+
                   wattage$ElectricStove*SCusage_l$ElectricStove+wattage$ElectricWaterPump*SCusage_l$ElectricWaterPump)*30/1000

SC_usage_month_u<-(wattage$IncandescentBulbs*SCusage_u$IncandescentBulbs+
                   wattage$CF*SCusage_u$CF+wattage$LED*SCusage_u$LED+
                   wattage$Tubelight*SCusage_u$Tubelight+wattage$Fan*SCusage_u$Fan+
                   wattage$ElectricIron*SCusage_u$ElectricIron+wattage$Refrigerator*SCusage_u$Refrigerator+
                   wattage$Television*SCusage_u$Television+wattage$ElectricRadio*SCusage_u$ElectricRadio+
                   wattage$Cooler*SCusage_u$Cooler+wattage$WashingMachine*SCusage_u$WashingMachine+
                   wattage$ElectricStove*SCusage_u$ElectricStove+wattage$ElectricWaterPump*SCusage_u$ElectricWaterPump)*30/1000

  
NSC_usage_month<-(wattage$IncandescentBulbs*NSCusage$IncandescentBulbs+
                   wattage$CF*NSCusage$CF+wattage$LED*NSCusage$LED+
                   wattage$Tubelight*NSCusage$Tubelight+wattage$Fan*NSCusage$Fan+
                   wattage$ElectricIron*NSCusage$ElectricIron+wattage$Refrigerator*NSCusage$Refrigerator+
                   wattage$Television*NSCusage$Television+wattage$ElectricRadio*NSCusage$ElectricRadio+
                   wattage$Cooler*NSCusage$Cooler+wattage$WashingMachine*NSCusage$WashingMachine+
                   wattage$ElectricStove*NSCusage$ElectricStove+wattage$ElectricWaterPump*NSCusage$ElectricWaterPump)*30/1000

NSC_usage_month_l<-(wattage$IncandescentBulbs*NSCusage_l$IncandescentBulbs+
                    wattage$CF*NSCusage_l$CF+wattage$LED*NSCusage_l$LED+
                    wattage$Tubelight*NSCusage_l$Tubelight+wattage$Fan*NSCusage_l$Fan+
                    wattage$ElectricIron*NSCusage_l$ElectricIron+wattage$Refrigerator*NSCusage_l$Refrigerator+
                    wattage$Television*NSCusage_l$Television+wattage$ElectricRadio*NSCusage_l$ElectricRadio+
                    wattage$Cooler*NSCusage_l$Cooler+wattage$WashingMachine*NSCusage_l$WashingMachine+
                    wattage$ElectricStove*NSCusage_l$ElectricStove+wattage$ElectricWaterPump*NSCusage_l$ElectricWaterPump)*30/1000

NSC_usage_month_u<-(wattage$IncandescentBulbs*NSCusage_u$IncandescentBulbs+
                    wattage$CF*NSCusage_u$CF+wattage$LED*NSCusage_u$LED+
                    wattage$Tubelight*NSCusage_u$Tubelight+wattage$Fan*NSCusage_u$Fan+
                    wattage$ElectricIron*NSCusage_u$ElectricIron+wattage$Refrigerator*NSCusage_u$Refrigerator+
                    wattage$Television*NSCusage_u$Television+wattage$ElectricRadio*NSCusage_u$ElectricRadio+
                    wattage$Cooler*NSCusage_u$Cooler+wattage$WashingMachine*NSCusage_u$WashingMachine+
                    wattage$ElectricStove*NSCusage_u$ElectricStove+wattage$ElectricWaterPump*NSCusage_u$ElectricWaterPump)*30/1000



SC$meter<-ifelse(SC$q_60_6=="Yes",1,0)
NSC$meter<-ifelse(NSC$q_60_6=="Yes",1,0)


tariff<-data.frame("Subsidized"=c(1,0,1,0,1,0, 1,0,1,0,1,0),
                         "meter"=c(1,1,1,1,1,1,0,0,0,0,0,0),
                      "year"=c("FY19","FY19","FY20 (approved)","FY20 (approved)", "FY20 (proposed)","FY20 (proposed)",
                               "FY19","FY19","FY20 (approved)","FY20 (approved)", "FY20 (proposed)","FY20 (proposed)"),
                       "fixed"=c(20,35,20,20,75,75, 250,250,0,0,250,250),
                       "variable"=c(4.4,4.75,5.75,5.75,6,6,0,0,0,0,0,0),
                       "demand"=c(SC_usage_month,NSC_usage_month,SC_usage_month,NSC_usage_month,SC_usage_month,NSC_usage_month),
                       "number"=c(nrow(SC),nrow(NSC),nrow(SC),nrow(NSC),nrow(SC),nrow(NSC)),
                       "pay"=c(sum(SC$nopay==1&SC$meter==1),sum(NSC$nopay==1&NSC$meter==1),sum(SC$nopay==1),sum(NSC$nopay==1),sum(SC$nopay==1&SC$meter==1),sum(NSC$nopay==1&NSC$meter==1),
                               sum(SC$nopay==1&SC$meter==0),sum(NSC$nopay==1&NSC$meter==0),0,0,sum(SC$nopay==1&SC$meter==0),sum(NSC$nopay==1&NSC$meter==0)),
                      "nopay"=c(sum(SC$nopay==0&SC$meter==1),sum(NSC$nopay==0&NSC$meter==1),sum(SC$nopay==0),sum(NSC$nopay==0),sum(SC$nopay==0&SC$meter==1),sum(NSC$nopay==0&NSC$meter==1),
                              sum(SC$nopay==0&SC$meter==0),sum(NSC$nopay==0&NSC$meter==0),0,0,sum(SC$nopay==0&SC$meter==0),sum(NSC$nopay==0&NSC$meter==0)))



tariff<-data.frame("Subsidized"=c(1,0,1,0,1,0, 1,0,1,0,1,0),
                   "meter"=c(1,1,1,1,1,1,0,0,0,0,0,0),
                   "year"=c("FY19","FY19","FY20 (approved)","FY20 (approved)", "FY20 (proposed)","FY20 (proposed)",
                            "FY19","FY19","FY20 (approved)","FY20 (approved)", "FY20 (proposed)","FY20 (proposed)"),
                   "fixed"=c(20,35,20,20,75,75, 250,250,0,0,250,250),
                   "variable"=c(4.4,4.75,5.75,5.75,6,6,0,0,0,0,0,0),
                   "demand"=c(SC_usage_month,NSC_usage_month,SC_usage_month,NSC_usage_month,SC_usage_month,NSC_usage_month),
                   "number"=c(nrow(SC),nrow(NSC),nrow(SC),nrow(NSC),nrow(SC),nrow(NSC)),
                   "pay"=c(sum(SC$nopay==1&SC$meter==1),sum(NSC$nopay==1&NSC$meter==1),sum(SC$nopay==1),sum(NSC$nopay==1),sum(SC$nopay==1),sum(NSC$nopay==1),
                           sum(SC$nopay==1&SC$meter==0),sum(NSC$nopay==1&NSC$meter==0),0,0,0,0),
                   "nopay"=c(sum(SC$nopay==0&SC$meter==1),sum(NSC$nopay==0&NSC$meter==1),sum(SC$nopay==0),sum(NSC$nopay==0),sum(SC$nopay==0),sum(NSC$nopay==0),
                             sum(SC$nopay==0&SC$meter==0),sum(NSC$nopay==0&NSC$meter==0),0,0,0,0))

write.csv(tariff, "tariff.csv")

tariff$revenue<-tariff$pay*tariff$demand*tariff$variable+tariff$fixed*tariff$pay
tariff$loss<-tariff$nopay*tariff$demand*tariff$variable+tariff$fixed*tariff$nopay

# Whole population of JHK

tariff$revenue<-tariff$pay*tariff$demand*tariff$variable+tariff$fixed*tariff$pay
tariff$loss<-tariff$nopay*tariff$demand*tariff$variable+tariff$fixed*tariff$nopay

tariff$Revenue<-3874.889*tariff$revenue  #5579840/1440
tariff$Loss<-3874.889*tariff$loss


tariff<-read.csv("tariff1.csv")


#tariff$Loss<--tariff$Loss

tariff_scenario<-tariff %>% group_by(year) %>% summarise(Revenue=sum(Revenue), Loss=sum(Loss))

tariff_SC<-tariff %>% group_by(year, Subsidized) %>% summarise(Revenue=sum(Revenue), Loss=sum(Loss))

tariff_meter<-tariff %>% group_by(year, meter) %>% summarise(Revenue=sum(Revenue), Loss=sum(Loss))


tariff_meter_sub<-tariff %>% group_by(year, meter, Subsidized) %>% summarise(Revenue=sum(Revenue), Loss=sum(Loss))


tariff_scenario<-gather(tariff_scenario, variable, value, Revenue:Loss)
tariff_scenario$label<-round(tariff_scenario$value/10000000,2)

tariff_scenario$variable<-as.factor(tariff_scenario$variable)
levels(tariff_scenario$variable)<-c("Loss","Revenue")

ggplot(tariff_scenario, aes(y=label, x=year, col=variable,linetype=variable)) + 
  geom_bar(position="stack", stat="identity",fill="transparent", size=1)+
  guides(linetype = guide_legend(override.aes = list(fill = NA,
                                                     col = "black")))+
  scale_linetype_manual(values=c("dashed","solid"))+
  geom_text(aes(label=label),position=position_stack(0.5), size=5)+
  xlab("Scenarios")+
  ylab("Monthly Revenue/Loss in Rs. Crores")+
  ggtitle("Financial status under different pricing strategies")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

tariff_SC19<-gather(tariff_SC[c(1,2),], variable, value, Revenue:Loss)
tariff_SC19$label<-round(tariff_SC19$value/10000000,2)
tariff_SC19$Subsidized<-as.factor(tariff_SC19$Subsidized)
levels(tariff_SC19$Subsidized)<-c("Non-Saubhagya","Saubhagya")

tariff_SC19$variable<-as.factor(tariff_SC19$variable)
levels(tariff_SC19$variable)<-c("Loss","Revenue")

ggplot(tariff_SC19, aes(y=label, x=Subsidized,col=variable,linetype=variable)) + 
  geom_bar(position="stack", stat="identity",fill="transparent", size=1)+
  guides(linetype = guide_legend(override.aes = list(fill = NA,
                                                     col = "black")))+
  scale_linetype_manual(values=c("dashed","solid"))+
  geom_text(aes(label=tariff_SC19$label),position=position_stack(0.5), size=5)+
  xlab("")+
  ylab("Revenue/Loss in Rs. Crores")+
  ggtitle("Monthly cost of supplying different groups in FY19")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())


table<-tariff[,c(3,1,2,4,5,6)]
table$Subsidized<-as.factor(table$Subsidized)
levels(table$Subsidized)<-c("Non-Saubhagya","Saubhagya")
table$meter<-as.factor(table$meter)
levels(table$meter)<-c("unmetered", "metered")
names(table)<-c("Scenarios","Groups","Metering","Fixed Cost", "Variable Cost", "Household Monthly Consumption")        
table$`Household Monthly Consumption`<-round(table$`Household Monthly Consumption`,2)

write.table(table, "loss_scenario.txt", quote=FALSE, eol="\\\\\n", sep=" & ")

print(xtable(table, type = "latex"), file = "loss_scenario.tex")


########### Survey weights ###########
grid_raw = grid[is.na(grid$survey_weights) %in% F,]
sdesign_grid = svydesign(id = ~UniqueId,
                         strata = ~Village_name_cal,
                         data = grid_raw,
                         weights = ~survey_weights,
                         nest = T)

SC_raw = SC[is.na(SC$survey_weights) %in% F,]
sdesign_SC = svydesign(id = ~UniqueId,
                         strata = ~Village_name_cal,
                         data = SC_raw,
                         weights = ~survey_weights,
                         nest = T)

NSC_raw = NSC[is.na(NSC$survey_weights) %in% F,]
sdesign_NSC = svydesign(id = ~UniqueId,
                       strata = ~Village_name_cal,
                       data = NSC_raw,
                       weights = ~survey_weights,
                       nest = T)

### Descriptive stats for Sabhaugya connectees
# 1. Ratio of connectees
# mytable <- table(grid$SC)
# lbls <- paste(names(mytable), "\n", mytable, sep="")
# pie(mytable, labels = lbls,
#     main="Pie Chart of Saubhagya Connectees\n (with sample sizes)")

sw.sum.table<-data.frame(matrix(NA, 2, 1))
sw.sum.table[1,1] = round(w.mean(grid_raw$SC, grid_raw$survey_weights), 2)
sw.sum.table[2,1] = round(w.mean(grid_raw$NSC, grid_raw$survey_weights), 2)
table<-c(sw.sum.table[1,1], sw.sum.table[2,1])
pie(table, labels=paste0(sprintf("%.0f", table*100), "%"), col=c("grey","white"),
    main="Pie Chart of Saubhagya Connectees")


#2. Payment of Saubhagya conectees
grid_raw$nopay<-ifelse(grid_raw$q_60_4=="No payment",0,1)
SC_raw$nopay<-ifelse(SC_raw$q_60_4=="No payment",0,1)
SC_raw$pay<-ifelse(SC_raw$nopay==1,0,1)
NSC_raw$nopay<-ifelse(NSC_raw$q_60_4=="No payment",0,1)
NSC_raw$pay<-ifelse(NSC_raw$nopay==1,0,1)
# summary(SC_raw$nopay)
# 
# mytable1 <- table(grid$nopay)
# lbls1 <- paste(names(mytable1), "\n", mytable1, sep="")
# pie(mytable1, labels = lbls1, main="Payment of Grid User")

colors <- c("yellow2","orangered3")

sw.sum.table1<-data.frame(matrix(NA, 2, 1))
sw.sum.table1[1,1] = round(w.mean(SC_raw$nopay, SC_raw$survey_weights), 2)
sw.sum.table1[2,1] = round(w.mean(SC_raw$pay, SC_raw$survey_weights), 2)
table1<-c(sw.sum.table1[1,1], sw.sum.table1[2,1])
pie1<-data.frame(class=c("Pay","No pay"), prop=table1, lab.ypos=c(0.145, 0.645))
# p1<-pie(table1, labels= paste0(sprintf("%.0f", table1*100), "%"), col=c("grey","white"),
#     main="Payment of Saubhagya Connectees")

p1<-ggplot(pie1, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(sprintf("%.0f", table1*100), "%")), color = "white")+
  scale_fill_manual(values = c("#868686FF", "#0073C2FF")) +
  ggtitle("Payment of Saubhagya Connectees")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank())


sw.sum.table2<-data.frame(matrix(NA, 2, 1))
sw.sum.table2[1,1] = round(w.mean(NSC_raw$nopay, NSC_raw$survey_weights), 2)
sw.sum.table2[2,1] = round(w.mean(NSC_raw$pay, NSC_raw$survey_weights), 2)
table2<-c(sw.sum.table2[1,1], sw.sum.table2[2,1])
pie2<-data.frame(class=c("Pay","No pay"), prop=table2, lab.ypos=c(0.295, 0.795))
# p2<-pie(table2, labels=paste0(sprintf("%.0f", table2*100), "%"), col=colors,
#     main="Payment of Non-Saubhagya Connectees")
# legend(.95, .05, c("Pay","No pay"), cex = 0.7, col=colors)

p2<-ggplot(pie2, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(sprintf("%.0f", table2*100), "%")), color = "white")+
  scale_fill_manual(values = c("#868686FF", "#0073C2FF")) +
  ggtitle("Payment of non-Saubhagya Connectees")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

ggarrange(p1,p2, ncol=2, legend="bottom",common.legend = TRUE)

# mytable2 <- table(SC$nopay)
# lbls2 <- paste(names(mytable2), "\n", mytable2, sep="")
# pie(mytable2, labels = lbls2, 
#     main="Payment of Saubhagya Connectees")

# 3. Payment under different criteria

#a) Meter
table_1<-table(SC$q_60_6, SC$nopay)
# SC$nopaynometer<-ifelse(SC$q_60_6=="No"&SC$nopay==0,1,0)
# SC$nopaymeter<-ifelse(SC$q_60_6=="Yes"&SC$nopay==0,1,0)
# SC$paynometer<-ifelse(SC$q_60_6=="No"&SC$nopay==1,1,0)
# SC$paymeter<-ifelse(SC$q_60_6=="Yes"&SC$nopay==1,1,0)
# sw.sum.table_1<-data.frame(matrix(NA, 2, 1))
# sw.sum.table_1[1,1] = round(w.mean(SC_raw$nopaynometer, SC_raw$survey_weights), 2)
# sw.sum.table_1[2,1] = round(w.mean(SC_raw$nopaymeter, SC_raw$survey_weights), 2)
# sw.sum.table_1[1,2] = round(w.mean(SC_raw$paynometer, SC_raw$survey_weights), 2)
# sw.sum.table_1[2,2] = round(w.mean(SC_raw$paymeter, SC_raw$survey_weights), 2)
# table_1<-c(sw.sum.table_1[1,1], sw.sum.table_1[2,1],sw.sum.table_1[1,2], sw.sum.table_1[2,2])

#names(meter)<-c("Meter","Payment")
meter<-c(rep("Meter",2),rep("No Meter",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table_1[2,2],table_1[2,1],table_1[1,2],table_1[1,1])
percent<-round(c(table_1[2,2]/(table_1[2,2]+table_1[2,1]), table_1[2,1]/(table_1[2,2]+table_1[2,1]),
                 table_1[1,2]/(table_1[1,2]+table_1[1,1]), table_1[1,1]/(table_1[1,2]+table_1[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data1<-data.frame(meter,payment,observations,percent)
g1<-ggplot(data1, aes(fill=payment, y=observations, x=meter)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data1$percent),position=position_stack(0.5), size=5)+
  ggtitle("Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



NSC$nopay<-ifelse(NSC$q_60_4=="No payment",0,1)
table1b<-table(NSC$q_60_6, NSC$nopay)
meter<-c(rep("Meter",2),rep("No Meter",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table1b[2,2],table1b[2,1],table1b[1,2],table1b[1,1])
percent<-round(c(table1b[2,2]/(table1b[2,2]+table1b[2,1]), table1b[2,1]/(table1b[2,2]+table1b[2,1]),
                 table1b[1,2]/(table1b[1,2]+table1b[1,1]), table1b[1,1]/(table1b[1,2]+table1b[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data1b<-data.frame(meter,payment,observations,percent)
g1b<-ggplot(data1b, aes(fill=payment, y=observations, x=meter)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data1b$percent),position=position_stack(0.5), size=5)+
  ggtitle("Non-Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(g1,g1b, ncol=2, legend="bottom",common.legend = TRUE)


#b) Bill and Payment
table2<-table(SC$q_60_7, SC$nopay)
bill<-c(rep("Bill",2),rep("No Bill",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table2[2,2],table2[2,1],table2[1,2],table2[1,1])
percent<-round(c(table2[2,2]/(table2[2,2]+table2[2,1]), table2[2,1]/(table2[2,2]+table2[2,1]),
                 table2[1,2]/(table2[1,2]+table2[1,1]), table2[1,1]/(table2[1,2]+table2[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data2<-data.frame(bill,payment,observations,percent)
g2<-ggplot(data2, aes(fill=payment, y=observations, x=bill)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data2$percent),position=position_stack(0.5), size=5)+
  ggtitle("Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))




table2b<-table(NSC$q_60_7, NSC$nopay)
bill<-c(rep("Bill",2),rep("No Bill",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table2b[2,2],table2b[2,1],table2b[1,2],table2b[1,1])
percent<-round(c(table2b[2,2]/(table2b[2,2]+table2b[2,1]), table2b[2,1]/(table2b[2,2]+table2b[2,1]),
                 table2b[1,2]/(table2b[1,2]+table2b[1,1]), table2b[1,1]/(table2b[1,2]+table2b[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data2b<-data.frame(bill,payment,observations,percent)
g2b<-ggplot(data2b, aes(fill=payment, y=observations, x=bill)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data2b$percent),position=position_stack(0.5), size=5)+
  ggtitle("Non-Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(g2,g2b, ncol=2, legend="bottom",common.legend = TRUE)



# c) Metered: share of meter and payment

MT<-subset(SC, q_60_6=="Yes")
table3<-table(MT$q_60_6_2, MT$nopay)
meter<-c(rep("Share Meter",2),rep("Do Not share",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table3[2,2],table3[2,1],table3[1,2],table3[1,1])
percent<-round(c(table3[2,2]/(table3[2,2]+table3[2,1]), table3[2,1]/(table3[2,2]+table3[2,1]),
                 table3[1,2]/(table3[1,2]+table3[1,1]), table3[1,1]/(table3[1,2]+table3[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data3<-data.frame(bill,payment,observations,percent)
g3<-ggplot(data3, aes(fill=payment, y=observations, x=meter)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data3$percent),position=position_stack(c(0.1,0.95)), size=c(5,5,3,3))+
  theme_bw()


MTb<-subset(NSC, q_60_6=="Yes")
table3b<-table(MTb$q_60_6_2, MTb$nopay)
meter<-c(rep("Share Meter",2),rep("Do Not share",2))
payment<-c(rep(c("Payment","No Payment"),2))
observations<-c(table3b[2,2],table3b[2,1],table3b[1,2],table3b[1,1])
percent<-round(c(table3b[2,2]/(table3b[2,2]+table3b[2,1]), table3b[2,1]/(table3b[2,2]+table3b[2,1]),
                 table3b[1,2]/(table3b[1,2]+table3b[1,1]), table3b[1,1]/(table3b[1,2]+table3b[1,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data3b<-data.frame(bill,payment,observations,percent)
g3b<-ggplot(data3b, aes(fill=payment, y=observations, x=meter)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data3b$percent),position=position_stack(c(0.1,0.95)), size=c(5,5,3,3))+
  theme_bw()


# d)Billed: Frequency

Billed<-subset(SC, q_60_7=="Yes" & q_60_7_1!="No bill")
table4<-table(Billed$q_60_7_1, Billed$nopay)
Frequency<-c(rep(">= once/month",2),rep("Every 2 months",2),
             rep("Every 3 months",2), rep("< every 3 months",2))
payment<-c(rep(c("Payment","No Payment"),4))
observations<-c(table4[1,2]+table4[2,2],table4[1,1]+table4[2,1],
                table4[3,2],table4[3,1],table4[4,2],table4[4,1],table4[5,2],table4[5,1])
percent<-round(c((table4[1,2]+table4[2,2])/(table4[1,2]+table4[2,2]+table4[1,1]+table4[2,1]), 
                 (table4[1,1]+table4[2,1])/(table4[1,2]+table4[2,2]+table4[1,1]+table4[2,1]),
                 table4[3,2]/(table4[3,2]+table4[3,1]), table4[3,1]/(table4[3,2]+table4[3,1]),
                 table4[4,2]/(table4[4,2]+table4[4,1]), table4[4,1]/(table4[4,2]+table4[4,1]),
                 table4[5,2]/(table4[5,2]+table4[5,1]), table4[5,1]/(table4[5,2]+table4[5,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")
data4<-data.frame(Frequency,payment,observations,percent)
Frequency1 <- factor(Frequency, levels=c(">= once/month","Every 2 months","Every 3 months","< every 3 months"))


g4<-ggplot(data4, aes(fill=payment, y=observations, x=Frequency1)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data4$percent),position=position_stack(0.5), size=4)+
  xlab("Frequency of Bill")+
  ggtitle("Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))



Billedb<-subset(NSC, q_60_7=="Yes" & q_60_7_1!="No bill")
table4b<-table(Billedb$q_60_7_1, Billedb$nopay)
Frequency<-c(rep(">= once/month",2),rep("Every 2 months",2),
             rep("Every 3 months",2), rep("< every 3 months",2))
payment<-c(rep(c("Payment","No Payment"),4))
observations<-c(table4b[1,2]+table4b[2,2],table4b[1,1]+table4b[2,1],
                table4b[3,2],table4b[3,1],table4b[4,2],table4b[4,1],table4b[5,2],table4b[5,1])
percent<-round(c((table4b[1,2]+table4b[2,2])/(table4b[1,2]+table4b[2,2]+table4b[1,1]+table4b[2,1]), 
                 (table4b[1,1]+table4b[2,1])/(table4b[1,2]+table4b[2,2]+table4b[1,1]+table4[2,1]),
                 table4b[3,2]/(table4b[3,2]+table4b[3,1]), table4b[3,1]/(table4b[3,2]+table4b[3,1]),
                 table4b[4,2]/(table4b[4,2]+table4b[4,1]), table4b[4,1]/(table4b[4,2]+table4b[4,1]),
                 table4b[5,2]/(table4b[5,2]+table4b[5,1]), table4b[5,1]/(table4b[5,2]+table4b[5,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")

data4b<-data.frame(Frequency,payment,observations,percent)
Frequency1 <- factor(Frequency, levels=c(">= once/month","Every 2 months","Every 3 months","< every 3 months"))


g4b<-ggplot(data4b, aes(fill=payment, y=observations, x=Frequency1)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data4b$percent),position=position_stack(0.5), size=4)+
  xlab("Frequency of Bill")+
  ggtitle("Non-Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))


ggarrange(g4,g4b, ncol=2, legend="bottom",common.legend = TRUE)
 
# e) Billed: Pay method and payment

table5<-table(Billed$q_60_7_2, Billed$nopay)
Method<-c(rep("Agent/Home",2),rep("Agent/Office",2),
             rep("Middleman & Other",2))
payment<-c(rep(c("Payment","No Payment"),3))
observations<-c(table5[1,2],table5[1,1],table5[2,2],table5[2,1],
                table5[3,2]+table5[4,2]+table5[5,2]+table5[6,2],
                table5[3,1]+table5[4,1]+table5[5,1]+table5[6,1])
percent<-round(c(table5[1,2]/(table5[1,2]+table5[1,1]), table5[1,1]/(table5[1,2]+table5[1,1]),
                 table5[2,2]/(table5[2,2]+table5[2,1]), table5[2,1]/(table5[2,2]+table5[2,1]),
                 (table5[3,2]+table5[4,2]+table5[5,2]+table5[6,2])/(table5[3,2]+table5[4,2]+table5[5,2]+table5[6,2]+table5[3,1]+table5[4,1]+table5[5,1]+table5[6,1]),
                 (table5[3,1]+table5[4,1]+table5[5,1]+table5[6,1])/(table5[3,2]+table5[4,2]+table5[5,2]+table5[6,2]+table5[3,1]+table5[4,1]+table5[5,1]+table5[6,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")

data5<-data.frame(Method,payment,observations,percent)
Method1 <- factor(Method, levels=c("Agent/Home","Agent/Office","Middleman & Other"))


g5<-ggplot(data5, aes(fill=payment, y=observations, x=Method1)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data5$percent),position=position_stack(0.5), size=4)+
  xlab("Pay Method")+
  ggtitle("Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


table5b<-table(Billedb$q_60_7_2, Billedb$nopay)
Method<-c(rep("Agent/Home",2),rep("Agent/Office",2),
          rep("Middleman & Other",2))
payment<-c(rep(c("Payment","No Payment"),3))
observations<-c(table5b[1,2],table5b[1,1],table5b[2,2],table5b[2,1],
                table5b[3,2]+table5b[4,2]+table5b[5,2]+table5b[6,2],
                table5b[3,1]+table5b[4,1]+table5b[5,1]+table5b[6,1])
percent<-round(c(table5b[1,2]/(table5b[1,2]+table5b[1,1]), table5b[1,1]/(table5b[1,2]+table5b[1,1]),
                 table5b[2,2]/(table5b[2,2]+table5b[2,1]), table5b[2,1]/(table5b[2,2]+table5b[2,1]),
                 (table5b[3,2]+table5b[4,2]+table5b[5,2]+table5b[6,2])/(table5b[3,2]+table5b[4,2]+table5b[5,2]+table5b[6,2]+table5b[3,1]+table5b[4,1]+table5b[5,1]+table5b[6,1]),
                 (table5b[3,1]+table5b[4,1]+table5b[5,1]+table5b[6,1])/(table5b[3,2]+table5b[4,2]+table5b[5,2]+table5b[6,2]+table5b[3,1]+table5b[4,1]+table5b[5,1]+table5b[6,1])),2)*100
percent = paste0(sprintf("%.0f", percent), "%")

data5b<-data.frame(Method,payment,observations,percent)
Method1 <- factor(Method, levels=c("Agent/Home","Agent/Office","Middleman & Other"))


g5b<-ggplot(data5b, aes(fill=payment, y=observations, x=Method1)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=data5b$percent),position=position_stack(0.5), size=4)+
  xlab("Pay Method")+
  ggtitle("Non-Saubhagya")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(g5,g5b, ncol=2, legend="bottom",common.legend = TRUE)


# Demand Analysis


sum_stats <- cbind(log(grid$use3+1),
                        grid$q_60_1, grid$SC, grid$dayhour, grid$q_75,
                        grid$q_76, grid$q_77, grid$q_78, grid$q_16, 
                        grid$Male, grid$Edu_UpTo5thStandard, grid$Edu_MoreThan5thStandard,
                        grid$Hindu, grid$OBC, grid$SCST, grid$hhsize, 
                        grid$APL, grid$BPL, grid$Antyodaya, grid$exp)

sum_stats_output<- data.frame(matrix(data = NA,
                              nrow = 20,
                              ncol = 6))
for(i in 1:20) {
  #n
  sum_stats_output[,6] = rep(nrow(sum_stats), 20)
  #mean; sd; min and max
  sum_stats_output[i,1] = signif(mean(sum_stats[,i],na.rm=T), 3)
  sum_stats_output[i,2] = signif(sd(sum_stats[,i],na.rm=T), 3)
  sum_stats_output[i,3] = signif(min(sum_stats[,i],na.rm=T), 3)
  sum_stats_output[i,4] = signif(max(sum_stats[,i],na.rm=T), 3)
  sum_stats_output[i,5] = sum(is.na(sum_stats[,i]))
}
sum_stats_output[,6] = format(sum_stats_output[,6], big.mark=",")
colnames(sum_stats_output) = c("Mean", "SD", "Min", "Max","NA", "Observations")
rownames(sum_stats_output) = c("Electricity Demand (log)", "Connection Years", "Saubhagya Connectee",
                               "Hours (day)", "Hours (night)","Outage Days","Volateg Fluation Days","Low Voltage Days", 
                               "Household Head Age", "Male Household Head", "Education (>5th)", "Education (<=5th)", 
                               "Religion (Hindu)", "Caste (OBC)",  "Caste (SC/ST)","Household Size",  
                               "Ration (Antyodaya)","Ration (APL)","Ration (BPL)", "Monthly Expenditure (log)")

sum_stats_output
stargazer(sum_stats_output, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/Jharkhand Electricity Consumers under Saubhagya Scheme/Manuscript/tables/sum_stats.tex")



model1<-lm(log(use3+1)~q_60_1+SC+dayhour+q_75+q_76+q_77+q_78+q_16+Male+
                Edu_UpTo5thStandard+Edu_MoreThan5thStandard+
                Hindu+OBC+SCST+hhsize+APL+BPL+Antyodaya+exp+District_cal, data=grid)

model<-svyglm(log(use3+1)~q_60_1+SC+dayhour+q_75+q_76+q_77+q_78+q_16+Male+
            Edu_UpTo5thStandard+Edu_MoreThan5thStandard+
            Hindu+OBC+SCST+hhsize+APL+BPL+Antyodaya+exp+District_cal, data=grid_raw, design=sdesign_grid)

summary(model1)

a<-subset(grid_raw,SC=="0")
summary(a$use3)
b<-subset(grid_raw,SC=="1")
summary(b$use3)

# build marginal plot
## Marginal Effect 

# margins(model, data = find_data(model, parent.frame()),
#         design=sdesign_grid, variables = NULL)
#x<-summary(margins(model1))[c(1:19),]
x<-summary(margins(model1))[c(1,2,3,4,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42),]
x$factor<-c("Antyodaya","APL","BPL","Hours (day)", "Edu_MoreThan5thStandard", "Edu_UpTo5thStandard", "Monthly Expenditure (log)", "Household Size", "Hindu", 
            "Male Household Head", "OBC", "Household Head Age", "Connection Years", "Hours (night)", "Outage Days","Volateg Fluation Days","Low Voltage Days", "Saubhagya Connectees", "SCST")
x$factor<-factor(x$factor, levels=c("Saubhagya Connectees", "Connection Years", "Hours (day)","Hours (night)", "Outage Days","Volateg Fluation Days","Low Voltage Days","Household Head Age",
                                    "Male Household Head", "Edu_UpTo5thStandard", "Edu_MoreThan5thStandard", "Hindu", "OBC", "SCST", "Household Size","APL","BPL","Antyodaya","Monthly Expenditure (log)")
)

g6<-ggplot(data = x, aes(factor, AME)) +
  geom_point(aes(factor, AME), size=3, col="red") +
  #geom_bar(position=position_dodge(), stat="identity")+
  #geom_col() +
  #scale_fill_manual(values="red")+
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper), width=0.1, size=1) +
  geom_hline(yintercept = 0) +
  ylab("Average Marginal Effect")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,hjust=1))


## Stealing Attitude

g7<-ggplot(grid, aes(q_65))+
  xlab("How acceptable is Kathiya Connection")+
  geom_bar()+
  theme_bw()

g8<-ggplot(grid, aes(q_66))+
  xlab("Is it acceptable for people to have kathiya connections\n if their village faces frequent blackouts?")+
  geom_bar()+
  theme_bw()
  
  
g9<-ggplot(grid, aes(q_67))+
  xlab("Is it acceptable for people to have kathiya connections if they are poor?")+
  geom_bar()+
  theme_bw()

grid$q_68<-factor(grid$q_68, levels=c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree"))
g10<-ggplot(grid, aes(q_68))+
  xlab("If people have a kathiya connection, they should be fined?")+
  geom_bar()+
  theme_bw()

############ SHS analysis #############

# grid$use4<-ifelse(grid$use3>=1250, ">1250",
#                   ifelse(grid$use3>=750, "750-1250",
#                          ifelse(grid$use3>=450,"450-750",
#                                 ifelse(grid$use3>=400,"400-450",
#                                        ifelse(grid$use3>=250,"250-400",
#                                               ifelse(grid$use3>=150,"150-250",
#                                                      ifelse(grid$use3>=100,"100-150",
#                                                             ifelse(grid$use3>=60,"60-100",
#                                                                    ifelse(grid$use3>=40,"40-60","<40")))))))))
#                                                                    
# grid$useSHS<-ifelse(grid$q_62=="Yes", 1,0)
# grid$SHSprim<-ifelse(grid$q_62=="Yes"&grid$q_57=="Solar home system/solar lantern",1,0)
# grid$SHSenough<-ifelse(grid$q_62=="Yes"&grid$q_57=="Solar home system/solar lantern"&grid$q_58_1=="Yes",1,0)
# 
# t<-table(grid$SHSenough, grid$use4)
# t_df<-data.frame(t)
# wat1<-data.frame(wattage=10,SHS=1)
# wat1[1,2]<-t_df[2,3]/(t_df[2,3]+t_df[1,3])
# wat1[2,2]<-t_df[4,3]/(t_df[4,3]+t_df[3,3])
# wat1[3,2]<-t_df[6,3]/(t_df[6,3]+t_df[5,3])
# wat1[4,2]<-t_df[8,3]/(t_df[8,3]+t_df[7,3])
# wat1[5,2]<-t_df[10,3]/(t_df[10,3]+t_df[9,3])
# wat1[6,2]<-t_df[12,3]/(t_df[12,3]+t_df[11,3])
# wat1[7,2]<-t_df[14,3]/(t_df[14,3]+t_df[13,3])
# wat1[8,2]<-t_df[16,3]/(t_df[16,3]+t_df[15,3])
# wat1[9,2]<-t_df[18,3]/(t_df[18,3]+t_df[17,3])
# wat1[10,2]<-t_df[20,3]/(t_df[20,3]+t_df[19,3])
# wat1[,1]<-c("<40",">1250","100-150","150-250","250-400","40-60","400-450","450-750","60-100","750-1250")
# wat1$SHS<-percent(wat1$SHS)
# wat1$wattage<-factor(wat1$wattage, levels=c("<40","40-60","60-100","100-150","150-250","250-400","400-450","450-750","750-1250",">1250"))
# g1 <- ggplot(wat1, aes(x=wattage, y=SHS))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   ggtitle("Proportion of Households who have SHS that can satisfy thier lighting demand")+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits=c(0.00,0.07))+ 
#   ylab("SHS Proportion")+
#   xlab("Energy Use Level: wattage")+
#   theme_bw()
# 
# RT<-subset(grid, q_22=="SC"|q_22=="ST"|q_22=="Other Backward Class")
# 
# t2<-table(RT$SHSenough, RT$use4)
# t_df2<-data.frame(t2)
# wat2<-data.frame(wattage=10,SHS=1)
# wat2[1,2]<-t_df2[2,3]/(t_df2[2,3]+t_df2[1,3])
# wat2[2,2]<-t_df2[4,3]/(t_df2[4,3]+t_df2[3,3])
# wat2[3,2]<-t_df2[6,3]/(t_df2[6,3]+t_df2[5,3])
# wat2[4,2]<-t_df2[8,3]/(t_df2[8,3]+t_df2[7,3])
# wat2[5,2]<-t_df2[10,3]/(t_df2[10,3]+t_df2[9,3])
# wat2[6,2]<-t_df2[12,3]/(t_df2[12,3]+t_df2[11,3])
# wat2[7,2]<-t_df2[14,3]/(t_df2[14,3]+t_df2[13,3])
# wat2[8,2]<-t_df2[16,3]/(t_df2[16,3]+t_df2[15,3])
# wat2[9,2]<-t_df2[18,3]/(t_df2[18,3]+t_df2[17,3])
# wat2[10,2]<-t_df2[20,3]/(t_df2[20,3]+t_df2[19,3])
# wat2[,1]<-c("<40",">1250","100-150","150-250","250-400","40-60","400-450","450-750","60-100","750-1250")
# #wat2$SHS<-percent(wat2$SHS)
# wat2$wattage<-factor(wat2$wattage, levels=c("<40","40-60","60-100","100-150","150-250","250-400","400-450","450-750","750-1250",">1250"))
# g2 <- ggplot(wat2, aes(x=wattage, y=SHS))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   ggtitle("Proportion of Remote Tribal Households who have SHS that\n can satisfy thier lighting demand")+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits=c(0.00,0.07))+
#   ylab("SHS Proportion")+
#   xlab("Energy Use Level: wattage")+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5))


# SC$useSHS<-ifelse(SC$q_62=="Yes", 1,0)
# SC$SHSprim<-ifelse(SC$q_62=="Yes"&SC$q_57=="Solar home system/solar lantern",1,0)
# SC$SHSenough<-ifelse(SC$q_62=="Yes"&SC$q_57=="Solar home system/solar lantern"&SC$q_58_1=="Yes",1,0)

grid$rank<-rank(as.numeric(grid$use3))


grid$SHSwat<-grid$q_62_2*100+grid$q_62_3_1*5+grid$q_62_3_2*60+grid$q_62_3_3*15+grid$q_62_3_4*40+grid$q_62_3_5*150
grid$cum<-grid$rank/nrow(grid)

grid_rank<-arrange(grid, rank)
# line(grid_rank$use3)
# abline(grid$SHSwat)

# p1= ggplot() + 
#   geom_line(data = grid_rank, aes(x = use3, y = cum), color="blue", size=1) +
#   #geom_point(data = grid_rank, aes(x = SHSwat, y = cum), color = "red", size=2)+ 
#   geom_line(data = SC_rank, aes(x = use3, y = cum), color="red", size=1) +
#   geom_vline(xintercept=200, color="dark green", size=0.5)+
#   #geom_line(data = data.frame(x = c(0, 200), y = c(0.4135954, 0.4135954)), aes(x = x , y = y)) +
#   #geom_segment(aes(x=0,xend=200,y=0.4135954,yend=0.4135954),col="dark green",linetype="dashed",size=1)+
#   geom_hline(yintercept=0.4135954, linetype="dashed", size=0.5)+
#   # scale_colour_manual(name="Color",
#   #                     values=c(EnergyUse ="blue", SHS="red"))+
#   #geom_point(data=grid_rank, aes(x=rank, y=q_62_4))+
#   xlab('Grid Users') +
#   ylab('')+
#   scale_x_continuous(breaks = c(0,200,500,1000,1500,2000,2500,3000), 
#                       labels = c("0","200","500","1000","1500","2000","2500","3000"),expand = c(0, 0)) +
#   scale_y_continuous(breaks = c(0,0.25,0.4135954,0.5,0.75,1),
#                      labels = scales::percent_format(accuracy = 1),limits=c(0,1),expand = c(0, 0.05))+
#   #ggtitle('How many household could be satisfied by solar home systems?')+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5),plot.margin = unit(c(0.5,1,0.5,0.1), "cm"))
# p1

SC$rank<-rank(as.numeric(SC$use3))

SC$SHSwat<-SC$q_62_2*100+SC$q_62_3_1*5+SC$q_62_3_2*60+SC$q_62_3_3*15+SC$q_62_3_4*40+SC$q_62_3_5*150
SC$cum<-SC$rank/nrow(SC)

SC_rank<-arrange(SC, rank)


no_grid$rank<-rank(as.numeric(no_grid$use3))

no_grid$SHSwat<-no_grid$q_62_2*100+no_grid$q_62_3_1*5+no_grid$q_62_3_2*60+no_grid$q_62_3_3*15+no_grid$q_62_3_4*40+no_grid$q_62_3_5*150
no_grid$cum<-no_grid$rank/33

no_grid_rank<-arrange(no_grid, rank)

# p2= ggplot() + 
#   geom_line(data = SC_rank, aes(x = use3, y = cum), color="blue", size=1) +
#   #geom_point(data = SC_rank, aes(x = SHSwat, y = cum), color = "red", size=2)+ 
#   geom_vline(xintercept=200, color="dark green",  size=0.5)+
#   #geom_segment(aes(x=0,xend=200,y=0.542735,yend=0.542735),col="dark green",linetype="dashed",size=1)+
#   geom_hline(yintercept=0.542735, linetype="dashed", size=0.5)+
#   # scale_colour_manual(name="Color",
#   #                     values=c(EnergyUse ="blue", SHS="red"))+
#   #geom_point(data=grid_rank, aes(x=rank, y=q_62_4))+
#   xlab('Saubhagya Connectees') +
#   ylab('')+
#   scale_x_continuous(limits=c(0, 1500), breaks = c(0,200,500,750,1000,1250,1500), 
#                      labels = c("0","200","500","750","1000","1250","1500"),expand = c(0, 0)) +
#   scale_y_continuous(breaks = c(0,0.25,0.542735,0.5,0.75,1),
#                      labels = scales::percent_format(accuracy = 1),limits=c(0,1),expand = c(0, 0.05))+
#   #ggtitle('How many household signed up under Saubhagya Scheme \n could be satisfied by solar home systems?')+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.5,1,0.5,0.1), "cm"))
#   
# p2
# 
# figure <- grid.arrange(p1, p2, nrow=1, ncol=2)
# 
# # Annotate the figure by adding a common labels
# annotate_figure(figure,
#                 top = text_grob("Proportion of Households that could be Satisfied  by Solar Home System",  face = "bold", size = 14),
#                 #right = text_grob("Grid Users             Saubhagya Connectees", rot = 270, vjust=-1),
#                 bottom =text_grob("wattage", vjust=-1), 
#                 left = text_grob("Cumulative Percentage", rot = 90, vjust=2)
# )


p= ggplot() + 
  geom_line(data = grid_rank, aes(x = use3, y = cum), color="blue", size=1) +
  #geom_point(data = grid_rank, aes(x = SHSwat, y = cum), color = "red", size=2)+ 
  geom_line(data = SC_rank, aes(x = use3, y = cum), color="red", size=1) +
  geom_line(data = no_grid, aes(x = use3, y = cum), color="purple", size=1) +
  geom_vline(xintercept=200, color="dark green", size=1)+
  #geom_line(data = data.frame(x = c(0, 200), y = c(0.4135954, 0.4135954)), aes(x = x , y = y)) +
  #geom_segment(aes(x=0,xend=200,y=0.4135954,yend=0.4135954),col="dark green",linetype="dashed",size=1)+
  geom_hline(yintercept=0.4135954, linetype="dashed", color="blue", size=0.5)+
  geom_hline(yintercept=0.542735, linetype="dashed", color="red", size=0.5)+
  geom_hline(yintercept=0.67, linetype="dashed", color="purple", size=0.5)+
  # scale_colour_manual(name="Color",
  #                     values=c(EnergyUse ="blue", SHS="red"))+
  #geom_point(data=grid_rank, aes(x=rank, y=q_62_4))+
  xlab('Energy Demand (Watt)') +
  ylab('Cumulative Percentage')+
  scale_x_continuous(limits=c(0,1500), breaks = c(0,200,500,1000,1500), 
                     labels = c("0","200","500","1000","1500"),expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0,0.25,0.4135954,0.542735,0.5,0.67,0.75,1),
                   labels = scales::percent_format(accuracy = 1),limits=c(0,1),expand = c(0, 0.05))+
  ggtitle('How many household could be satisfied by solar home systems?')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"),plot.margin = unit(c(0.5,1,0.5,0.1), "cm"))
p


table<-data.frame('SHS size (Watts)'=c(rep(NA,7)), 'Grid User (%)'=c(rep(NA,7)), "Saubhagya Connectees (%)"=c(rep(NA,7)), 
                  'Cost of Supplying (Rs.)'=c(rep(NA,7)), 'Cost of Every Extra Percent (Rs.)'=c(rep(NA,7)))
colnames(table)<-c('SHS size (Watts)', 'Grid User (%)', "Saubhagya Connectees (%)", 'Cost of Supplying (Rs.Crs)', 'Cost of Every Extra Percent (Rs.Crs)')

table[1,1]<-100
table[2,1]<-150
table[3,1]<-200
table[4,1]<-250
table[5,1]<-300
table[6,1]<-350
table[7,1]<-500

table[1,2]<-round(grid_rank[grid_rank$use3=="100",]$cum[1]*100)
table[2,2]<-round(grid_rank[grid_rank$use3=="148",]$cum[1]*100)
table[3,2]<-round(grid_rank[grid_rank$use3=="200",]$cum[1]*100)
table[4,2]<-round(grid_rank[grid_rank$use3=="250",]$cum[1]*100)
table[5,2]<-round(grid_rank[grid_rank$use3=="300",]$cum[1]*100)
table[6,2]<-round(grid_rank[grid_rank$use3=="350",]$cum[1]*100)
table[7,2]<-round(grid_rank[grid_rank$use3=="500",]$cum[1]*100)

table[1,3]<-round(SC_rank[SC_rank$use3=="100",]$cum[1]*100)
table[2,3]<-round(SC_rank[SC_rank$use3=="155",]$cum[1]*100)
table[3,3]<-round(SC_rank[SC_rank$use3=="200",]$cum[1]*100)
table[4,3]<-round(SC_rank[SC_rank$use3=="250",]$cum[1]*100)
table[5,3]<-round(SC_rank[SC_rank$use3=="300",]$cum[1]*100)
table[6,3]<-round(SC_rank[SC_rank$use3=="350",]$cum[1]*100)
table[7,3]<-round(SC_rank[SC_rank$use3=="510",]$cum[1]*100)

table[1,4]<-40000/200*table[1,1]*53298/10000000
table[2,4]<-40000/200*table[2,1]*53298/10000000
table[3,4]<-40000/200*table[3,1]*53298/10000000
table[4,4]<-40000/200*table[4,1]*53298/10000000
table[5,4]<-40000/200*table[5,1]*53298/10000000
table[6,4]<-40000/200*table[6,1]*53298/10000000
table[7,4]<-40000/200*table[7,1]*53298/10000000


table[1,5]<-NA
table[2,5]<-(table[2,4]-table[1,4])/(table[2,3]-table[1,3])
table[3,5]<-(table[3,4]-table[2,4])/(table[3,3]-table[2,3])
table[4,5]<-(table[4,4]-table[3,4])/(table[4,3]-table[3,3])
table[5,5]<-(table[5,4]-table[4,4])/(table[5,3]-table[4,3])
table[6,5]<-(table[6,4]-table[5,4])/(table[6,3]-table[5,3])
table[7,5]<-(table[7,4]-table[6,4])/(table[7,3]-table[6,3])

print(xtable(table, type = "latex"), file = "SHS.tex")

# cost<-data.frame(table)
# 
# ggplot(cost, aes(x=))

grid_rank$SHScostperwatt=grid_rank$q_62_4/grid_rank$SHSwat
grid_rank$SHScost=grid_rank$SHScostperwatt*grid_rank$use3


### Usage Level

jkhdata$SHSprimary<-ifelse(jkhdata$q_57=="Solar home system/solar lantern" & jkhdata$q_62=="Yes", 1,0)
jkhdata$SHSsecondary<-ifelse(jkhdata$q_57!="Solar home system/solar lantern" & jkhdata$q_62=="Yes", 1,0)
jkhdata$SHSnoSHS<-ifelse(jkhdata$q_57!="Solar home system/solar lantern" & jkhdata$q_62=="No", 1,0)
jkhdata$SHS1<-ifelse(jkhdata$SHSprimary==1,"primary",
                    ifelse(jkhdata$SHSsecondary==1, "secondary","no SHS"))
  
  
  

jkhdata$SHS<-ifelse(jkhdata$q_57=="Solar home system/solar lantern", 1,0)
jkhdata$kero<-ifelse(jkhdata$q_57=="Kerosene lamp/lantern", 1,0)
jkhdata$grid<-ifelse(jkhdata$q_57=="Grid electricity", 1,0)
jkhdata$other<-ifelse(jkhdata$q_57=="Other"|jkhdata$q_57=="Micro Grid", 1,0)



jkhdata$light<-"Other"
jkhdata$light[jkhdata$SHS==1]<-"SHS" 
jkhdata$light[jkhdata$kero==1]<-"Kerosene"
jkhdata$light[jkhdata$grid==1]<-"Grid"



jkhdata$tribal<-ifelse(jkhdata$q_22=="ST","Scheduled Tribe","Non-tribal")
# tribal<-subset(jkhdata, q_22=="ST")
# nontribal<-subset(jkhdata, q_22!="ST")

# t <- table(jkhdata$SHS, jkhdata$tribal)
# mosaicplot(t)
# 
# count <- table(jkhdata$SHS1,jkhdata$tribal)
# count

# mosaic(count, labeling = labeling_cells(text = count.tab))
# # 
# mosaic(count, data=data, pop=F, set_labels=list(tribal = c("Not Tribal", "Tribal"), SHS=c("primary","secondary", "no SHS")),
#        labeling_args = list(set_varnames = list(tribal = "Scheduled Tribe", SHS = "SHS Use")))
# labeling_cells(text = labs, margin = 0)(count)

# 
# mosaicplot(count, main = "SHS usage situation among tribes and non-tribes",
#            xlab = "Tribal or not",
#            ylab = "SHS",
#            las = 1,
#            dir = c("h", "v"))
# points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
# text(6,6, "aa", cex =50)



struct <- structable(~ SHS1 + tribal, data = jkhdata)
struct<-as.table(struct)
struct <-struct/nrow(jkhdata)

labs<-paste0(sprintf("%.0f", struct*100), "%")
dim(labs) <- dim(struct)
dimnames(labs) <- dimnames(struct)


mosaic(struct, data=jkhdata, pop=F,legend=T, main = "SHS usage situation among tribes and non-tribes",
       set_labels=list(tribal = c("Non-tribal", "Scheduled Tribe"), SHS1=c("No SHS","Primary","Secondary")),
       labeling_args = list(set_varnames = list(tribal = "Tribal or not", SHS1 = "SHS"),
                            gp_labels = gpar(fontsize = 8),
                            gp_varnames = gpar(fontsize = 10, fontface = 2),
                            offset_varnames = c(0, 0, 0, 1.7),
                            offset_labels = c(0, 0, 0, 1.1),
                            rot_labels = c(0, 90, 0, 0), rot_varnames = c(0, 90, 0, 90)))
labeling_cells(text = labs, margin = 0)(struct)

dev.off()




# pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Mosaic.pdf")
# 
# mosaic(struct, data=data, pop=F, set_labels=list(tribal = c("Non-tribal", "Scheduled Tribe"), SHS=c("no SHS","primary","secondary")),
#        labeling_args = list(set_varnames = list(tribal = "Scheduled Tribe", SHS = "SHS Use")))
# labeling_cells(text = labs, margin = 0)(count)
# 
# dev.off()

jkhdata$use3<-wattage$IncandescentBulbs*jkhdata$q_79_1+
  wattage$CF*jkhdata$q_79_2+wattage$LED*jkhdata$q_79_3+
  wattage$Tubelight*jkhdata$q_79_4+wattage$Fan*jkhdata$q_79_5+
  wattage$ElectricIron*jkhdata$q_79_7+wattage$Refrigerator*jkhdata$q_79_8+
  wattage$Television*jkhdata$q_79_9+wattage$ElectricRadio*(jkhdata$q_79_9+jkhdata$q_79_15)+
  wattage$Cooler*jkhdata$q_79_10+wattage$WashingMachine*jkhdata$q_79_11+
  wattage$ElectricStove*jkhdata$q_79_12+wattage$ElectricWaterPump*jkhdata$q_79_14


# df.new<-ddply(jkhdata,.(q_59),summarise,
#               prop=prop.table(table()),
#               LetterGrade=names(table(LetterGrade)))
# 
# ddplyhead(df.new)


### Satisfactory Level
#SHSuser<-subset(jkhdata, SHSprimary==1|SHSsecondary==1)
t1<-table(jkhdata$light,jkhdata$q_59)
t1<-data.frame(t1)[c(1:12),]    
t1$prop<-NA
t1[1,4]<-t1[1,3]/(t1[1,3]+t1[5,3]+t1[9,3])
t1[5,4]<-t1[5,3]/(t1[1,3]+t1[5,3]+t1[9,3])
t1[9,4]<-t1[9,3]/(t1[1,3]+t1[5,3]+t1[9,3])
t1[2,4]<-t1[2,3]/(t1[2,3]+t1[6,3]+t1[10,3])
t1[6,4]<-t1[6,3]/(t1[2,3]+t1[6,3]+t1[10,3])
t1[10,4]<-t1[10,3]/(t1[2,3]+t1[6,3]+t1[10,3])
t1[3,4]<-t1[3,3]/(t1[3,3]+t1[7,3]+t1[11,3])
t1[7,4]<-t1[7,3]/(t1[3,3]+t1[7,3]+t1[11,3])
t1[11,4]<-t1[11,3]/(t1[3,3]+t1[7,3]+t1[11,3])
t1[4,4]<-t1[4,3]/(t1[4,3]+t1[8,3]+t1[12,3])
t1[8,4]<-t1[8,3]/(t1[4,3]+t1[8,3]+t1[12,3])
t1[12,4]<-t1[12,3]/(t1[4,3]+t1[8,3]+t1[12,3])
t1$prop<-round(t1$prop,2)
t1$perc<- paste0(sprintf("%.0f", t1$prop*100), "%")

t1$Var1 <- factor(t1$Var1, levels=c("SHS","Grid","Kerosene","Other"))

ggplot(t1,aes(x=Var1,y=prop,fill=Var2))+
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits=c(0,1))+
  geom_text(aes(label=perc),position=position_stack(0.5), size=5)+
  xlab('Primary Lighting Fuel') +
  ylab('Proportion')+
  ggtitle("Satisfactory Levels of Different Lighting Fuels")+
  theme_bw()+
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


# grid_rank_satis<-subset(grid_rank,use3<mean(grid_rank$SHSwat, na.rm=T))
# ggplot(grid_rank_satis, aes(x=use3, y=q_62_4))+
#   geom_bar(position=position_dodge(), stat="identity", fill="red")+
#   xlab("ranked household energy use (watt)")+
#   ylab("SHS cost (Rs)")+
#   ggtitle("How much would SHS cost to support household's energy use")+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5),legend.position="right")

# hist(RT$use3)
# hist(grid$use3)
