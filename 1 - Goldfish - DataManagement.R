###################
## Load Packages ##
###################
rm(list=ls(all=TRUE))

library(devtools)
library(remotes) 
library(pacman)
p_load(dplyr)
p_load(plyr)
p_load(reshape2)
p_load(data.table)
p_load(lubridate)
p_load(reshape)
p_load(glatos)
p_load(maptools)
p_load(rgdal)
p_load(rgeos)
p_load(gdalUtils)
p_load(raster)
p_load(sp)
p_load(lattice)
source(file = "HighstatLibV11.R")

#########################
##set working directory##
#########################

#all relevant data are ported into the same folder for this specific project
setwd("I:/Fish Ecology Science/HH/Goldfish CGDD/Data")
getwd()

#get file list for referrence
fileList <- list.files()
fileList



###########################
## Functions from source ##
###########################
source("Time.Window.R")

#################
## Data Import ##
#################

#import goldfish detections from arrays
detections <- read.csv("HH_Goldfish_22Nov2019.csv")

#arrange detection data
df_Detections <- detections %>% dplyr::select(detection_timestamp_utc,glatos_array,
                                   transmitter_codespace,transmitter_id,sensor_value,
                                   receiver_sn,glatos_project_receiver,station,min_lag)


#import sites identifierr
sites <- read.csv("HH_Station_Groupings_GF.csv")

#import fish info
fish.info <- read.csv("GF meta data_R_Ready_Nov2019.csv")
## maybe drop station WLC-001 b/c coordinates in raw GLATOS export likely incorrect


#merge data with main database
combo <- merge(df_Detections, sites, by = "station", all = T)
combo <- merge(combo,fish.info,by = c("transmitter_id"), all=T)

combo$depth <- combo$Slope*combo$sensor_value+combo$Intercept
combo$depth <- ifelse(combo$depth<0.1,0.1,combo$depth)
## dead fish IDs = 15200 15206
combo<-combo[!(combo$Status=="Drop"),]
combo$DMYHM <- ymd_hms(combo$detection_timestamp_utc) # make this into a date object
combo$transmitter_id <- factor(combo$transmitter_id)
# Redefine factors 
combo$fYear <- factor(year(combo$DMYHM))
combo$fMonth <- factor(month(combo$DMYHM))
sortnames <- c("transmitter_id","DMYHM")
combo<-combo[do.call("order",combo[sortnames]), ] ## sort by ID and Date
combo$jdate<-julian(as.Date(combo[,"DMYHM"])) 
combo$Hour<-hour(combo$DMYHM)
#combo<-subset(combo,DMYHM<"2018-08-01") ## trim to detections between June 29th 2016 and July 31st 2018
combo$Date<-as.Date(combo$DMYHM)

####################
## Basic Analysis ##
####################
## Check range of dates for outliers
min(combo$DMYHM,na.rm=T)
max(combo$DMYHM,na.rm=T)
#combo<-combo[complete.cases(combo[ , 2]),] ## remove where ID=NA

## Assign season (thermally-derived for hamilton harbour)
combo$Season.1<-ifelse((combo$DMYHM > "2015-08-11") & (combo$DMYHM < "2015-10-01"), "summer_15", 
                       ifelse((combo$DMYHM >= "2015-10-01")&(combo$DMYHM < "2015-11-20"), "fall_15",
                              ifelse((combo$DMYHM >= "2015-11-20")&(combo$DMYHM < "2016-04-30"), "winter_15_16",
                                     ifelse((combo$DMYHM >= "2016-04-30") & (combo$DMYHM < "2016-05-31"), "spring_16",
                                            ifelse((combo$DMYHM >= "2016-05-31")&(combo$DMYHM < "2016-09-25"), "summer_16",
                                                   ifelse((combo$DMYHM >= "2016-09-25")&(combo$DMYHM < "2016-11-16"), "fall_16",
                                                          ifelse((combo$DMYHM >= "2016-11-16")&(combo$DMYHM < "2017-04-16"), "winter_16_17",
                                                                 ifelse((combo$DMYHM >= "2017-04-16") & (combo$DMYHM < "2017-06-17"), "spring_17",
                                                                        ifelse((combo$DMYHM >="2017-06-17")&(combo$DMYHM < "2017-10-14"), "summer_17",
                                                                               ifelse((combo$DMYHM >= "2017-10-14")&(combo$DMYHM < "2017-11-24"), "fall_17",
                                                                                      ifelse((combo$DMYHM >= "2017-11-24")&(combo$DMYHM < "2018-04-30"), "winter_17_18",
                                                                                             ifelse((combo$DMYHM >= "2018-04-30") & (combo$DMYHM < "2018-05-31"),  "spring_18",
                                                                                                    ifelse((combo$DMYHM >= "2018-05-31")&(combo$DMYHM < "2018-10-02"), "summer_18",
                                                                                                           ifelse((combo$DMYHM >= "2018-10-02")&(combo$DMYHM < "2018-11-16"), "fall_18",
                                                                                                                  ifelse((combo$DMYHM >= "2018-11-16")&(combo$DMYHM < "2019-04-21"), "winter_18_19",
                                                                                                                         ifelse((combo$DMYHM >= "2019-04-21") & (combo$DMYHM < "2019-06-01"),  "spring_19",
                                                                                                                                ifelse((combo$DMYHM >= "2019-06-01") & (combo$DMYHM < "2019-10-01"),  "summer_19","fall_19")))))))))))))))))

#not syure what this is for
test <- subset(combo,Season.1 == "Flag") ## all issues are 2019 dates
sum(is.na(combo$Season.1))

combo$Season.1<-as.factor(combo$Season.1)

#############################
## Tracking Window Summary ##
#############################
#Tracking Windows 
## Number of detections/individual
time.window(combo)

table(combo$transmitter_id)

####################
##  Counts ##
####################

combo.group <- plyr::count(combo, c("station","transmitter_id","Date")) ## detections/day/ID
combo.group.count<- aggregate(freq ~ station + Date,data = combo.group, length)
plot(freq ~ Date, data = combo.group.count,
     col="black",
     ylab="# Individuals",
     xlab="Date",
     main="Number of Individuals",
     ylim=c(0,max(combo.group.count$freq)))


#############################################
## Define Residency For Station_subregion2 ##
#############################################
combo.Z <- combo ## safety in case need to re-make combo
sortnames <- c("transmitter_id","DMYHM")
combo<-combo[do.call("order",combo[sortnames]), ] ## sort by ID and Date

StationShift2 <- as.character(combo$Station_subregion2[1:(nrow(combo)-1)]) ### Shift stations up one and reintegrate
x <- ""
StationShift2 <- c(x,StationShift2)
combo$Prev.Stat <- StationShift2

StationShift <- as.character(combo$Station_subregion2[2:(nrow(combo))]) ### Shift stations up one and reintegrate
x <- ""
StationShift <- c(StationShift,x)
combo$Next.Stat <- StationShift

ithTime <- combo$DMYHM[2:nrow(combo)] ### Shift time up one and reintegrate
y <- combo$DMYHM[nrow(combo)]
ithTime <- c(ithTime,y)
combo$Next.Time <- ithTime ## Time stamp looks different b/c it shifts from UTC to EDT, time diff calculation is still correct.

transmitter_id_2 <- as.character(combo$transmitter_id[2:(nrow(combo))]) ### Shift ID up one and reintegrate
transmitter_id_2 <- c(transmitter_id_2,x)
combo$Next.transmitter_id <- transmitter_id_2

combo$Time.Diff <- combo$Next.Time-combo$DMYHM ### Calculate time differences

## Remove Singletons and Re-set Database/Time Difference
combo$Flag <- ifelse(combo$Station_subregion2!=combo$Next.Stat&combo$Station_subregion2!=combo$Prev.Stat&combo$Prev.Stat==combo$Next.Stat&combo$Time.Diff<21600,"Flag","Keep") ## select specific cases where there is only a single detection at a receiver and the fish only stays there for <6 hrs 
combo <- combo[!(combo$Flag=="Flag"),] ## Drops singletons that are flagged (N=1)
combo$Flag = NULL
combo$Prev.Stat = NULL
combo$Time.Diff <- combo$Next.Time-combo$DMYHM ### Re-calculate time differences after singletons dropped
StationShift <- as.character(combo$Station_subregion2[2:(nrow(combo))]) ### Shift stations up one and reintegrate
x <- ""
StationShift <- c(StationShift,x)
combo$Next.Stat <- StationShift ## Re-sets movemenets correcting for singletons

## transmitter_identify Movements
combo$Move <- ifelse(combo$transmitter_id==combo$Next.transmitter_id,ifelse(combo$Station_subregion2==combo$Next.Stat,"Stay","Move"),"NewFish") ## Assess behaviour (move/stay)
combo <- combo[!(combo$Move=="NewFish"),] ## Drop rows where there is a switch in the fish transmitter_id
combo$Movements <- paste(combo$Station_subregion2,combo$Next.Stat,sep="-") ## Show type of Movement

## Isolate Residency
combo$Res.Status <- ifelse(combo$Move=="Stay",ifelse(combo$Time.Diff<=21600,"Residence","Reset"),"Move") ##21600 - 6 hrs; 10800 - 3 hrs

Day_Long_Excursion <- subset(combo,Time.Diff>=86400) 
Week_Long_Excursions <- subset(combo,Time.Diff>=604800) 
write.csv(Day_Long_Excursion,file = "HH_Goldfish_DayLongExcursions_Region2_6hrReset_17Dec2019.csv")
write.csv(Week_Long_Excursions,file = "HH_Goldfish_WeekLongExcursions_Region2_6hrReset_17Dec2019.csv")

################################################################
## Summarize Proportional Restransmitter_idency By Indivdiual and Species ##
################################################################
Station.Output.1<-aggregate(Time.Diff~transmitter_id+Season.1+Station_subregion2+Res.Status+Movements,combo,sum) ## adjust "Station_subregion2" based on desired summary
sortnames2<-c("transmitter_id","Season.1","Station_subregion2","Res.Status","Movements")
Station.Output.1<-Station.Output.1[do.call("order",Station.Output.1[sortnames2]),]

## This is where we need to insert zero values for fish alive but not detected on a receiver
DataRep1<-data.frame(Station.Output.1$transmitter_id,Station.Output.1$Season.1)
DataRep1.1<-unique(DataRep1) ## Unique combos of transmitter_id and Season (only seasons where an indivtransmitter_idual was detected)
DataRep2<-unique(Station.Output.1$Station_subregion2) ## length = 5
DataRep1.2<-do.call("rbind", replicate(length(DataRep2), DataRep1.1, simplify = FALSE))
DataRep2.1<-rep(DataRep2,nrow(DataRep1.1))
DataRep2.2<-sort(DataRep2.1)
DataRep3<-data.frame(DataRep1.2,DataRep2.2)
Station.Base<-plyr::rename(DataRep3,c("Station.Output.1.transmitter_id"="transmitter_id","Station.Output.1.Season.1"="Season.1","DataRep2.2"="Station_subregion2"))
Station.Base <- Station.Base[order(Station.Base$transmitter_id, Station.Base$Season.1,Station.Base$Station_subregion2),]
###############################
Station.Output.Res.1<-Station.Output.1[(Station.Output.1$Res.Status=="Residence"),] ## Total Residence Time at Receiver Group by Season
Station.Output.Res.1$Time.Diff<-as.numeric(Station.Output.Res.1$Time.Diff)

## Issue b/c some indivtransmitter_iduals aren't at all present... so need to drop somehow...
Station.Output.Res.1.1<-merge(Station.Base,Station.Output.Res.1,by=c("transmitter_id","Season.1","Station_subregion2"),all=T)
Station.Output.Res.1.1$Time.Diff[is.na(Station.Output.Res.1.1$Time.Diff)] <- 0 ## Set time diff to zero

Total.Time.Station<-aggregate(Time.Diff~transmitter_id+Season.1,Station.Output.Res.1.1,sum)
Station.Output.Res.Merge.1<-merge(Station.Output.Res.1.1,Total.Time.Station,by=c("transmitter_id","Season.1"),all=T)
Station.Output.Res.Merge.1$Prop.Time.Res<-Station.Output.Res.Merge.1$Time.Diff.x/Station.Output.Res.Merge.1$Time.Diff.y ## Indivtransmitter_idual Proportional Restransmitter_idence Time at Receiver Group by Season
Station.Output.Res.Merge.1$Prop.Time.Res[is.na(Station.Output.Res.Merge.1$Prop.Time.Res)] <- 0 ## Set time diff to zero

##################
## New data output
##################

head(Station.Output.Res.Merge.1)
Prop.Res.ID<-Station.Output.Res.Merge.1
Prop.Res.ID$Res.Status<-NULL
Prop.Res.ID$Movements<-NULL
Prop.Res.ID$Time.Diff.x<-NULL
Prop.Res.ID$Time.Diff.y<-NULL

write.csv(Prop.Res.ID,file="Goldfish_SeasonalProp.Res_by_ID_17Dec2019.csv")

Prop.Season.Station.mean<-aggregate(Prop.Time.Res~Season.1+Station_subregion2,Station.Output.Res.Merge.1,mean)
Prop.Season.Station.sd<-aggregate(Prop.Time.Res~Season.1+Station_subregion2,Station.Output.Res.Merge.1,sd)
Prop.Season.Station.N<-aggregate(Prop.Time.Res~Season.1+Station_subregion2,Station.Output.Res.Merge.1,length)
Prop.Season.Station<-merge(Prop.Season.Station.mean,Prop.Season.Station.sd,by=c("Season.1","Station_subregion2"))
Prop.Season.Station<-merge(Prop.Season.Station,Prop.Season.Station.N,by=c("Season.1","Station_subregion2"))

Prop.Season.Station<-plyr::rename(Prop.Season.Station,c("Prop.Time.Res.x"="Mean.Res","Prop.Time.Res.y"="SD.Res","Prop.Time.Res"="N.Fish"))
write.csv(Prop.Season.Station,file="HH_Goldfish_OverallResidency_Subregion.2_6hrReset_17Dec2019.csv") ### Summarized output

##############
## Boxplots ##
##############
Station.Output.Res.Merge.1$Station_subregion2<-factor(Station.Output.Res.Merge.1$Station_subregion2)
boxplot(Prop.Time.Res~Station_subregion2,data=Station.Output.Res.Merge.1,ylab="Proportion Residency",xlab="Region",main="Goldfish - Mean Residency",ylim=c(0,1))

##################
## GLATOS Plots ##
##################
combo.2<-combo
combo.2$detection_timestamp_utc=NULL
combo.2<-plyr::rename(combo.2,c("DMYHM"="detection_timestamp_utc"))
combo.2$Project.Region<-as.character(combo.2$Station_subregion2)
GLATOS.Locations<-c("Cootes","Grindstone","Outer Grindstone","West","Centre","North","East","Southeast","Lake Ontario")


#setwd("C:/Users/Mtransmitter_idwoodJ/Desktop")
xLim <- as.POSIXct(c("2017-06-26", "2019-06-01"), tz = "UTC")
combo.4<-subset(combo.2,transmitter_id=="14186")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14186",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14187")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14187",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14190")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14190",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14191")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14191",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14513")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14513",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14523")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14523",xlim=xLim)
#combo.4<-subset(combo.2,transmitter_id=="15849")
#abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15849",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15851")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15851",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15863")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15863",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15866")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15866",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15873")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15873",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15876")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15876",xlim=xLim)



########################################################
## Define Residency For Station_subStation_subregion3 ##
########################################################
combo.Z<-combo ## safety in case need to re-make combo
sortnames <- c("transmitter_id","DMYHM")
combo<-combo[do.call("order",combo[sortnames]), ] ## sort by ID and Date

StationShift2<-as.character(combo$Station_subregion3[1:(nrow(combo)-1)]) ### Shift stations up one and reintegrate
x<-""
StationShift2<-c(x,StationShift2)
combo$Prev.Stat<-StationShift2

StationShift<-as.character(combo$Station_subregion3[2:(nrow(combo))]) ### Shift stations up one and reintegrate
x<-""
StationShift<-c(StationShift,x)
combo$Next.Stat<-StationShift

ithTime<-combo$DMYHM[2:nrow(combo)] ### Shift time up one and reintegrate
y<-combo$DMYHM[nrow(combo)]
ithTime<-c(ithTime,y)
combo$Next.Time<-ithTime ## Time stamp looks different b/c it shifts from UTC to EDT, time diff calculation is still correct.

transmitter_id_2<-as.character(combo$transmitter_id[2:(nrow(combo))]) ### Shift ID up one and reintegrate
transmitter_id_2<-c(transmitter_id_2,x)
combo$Next.transmitter_id<-transmitter_id_2

combo$Time.Diff<-combo$Next.Time-combo$DMYHM ### Calculate time differences

## Remove Singletons and Re-set Database/Time Difference
combo$Flag<-ifelse(combo$Station_subregion3!=combo$Next.Stat&combo$Station_subregion3!=combo$Prev.Stat&combo$Prev.Stat==combo$Next.Stat&combo$Time.Diff<21600,"Flag","Keep") ## select specific cases where there is only a single detection at a receiver and the fish only stays there for <6 hrs 
combo<-combo[!(combo$Flag=="Flag"),] ## Drops singletons that are flagged (N=1)
combo$Flag=NULL
combo$Prev.Stat=NULL
combo$Time.Diff<-combo$Next.Time-combo$DMYHM ### Re-calculate time differences after singletons dropped
StationShift<-as.character(combo$Station_subregion3[2:(nrow(combo))]) ### Shift stations up one and reintegrate
x<-""
StationShift<-c(StationShift,x)
combo$Next.Stat<-StationShift ## Re-sets movemenets correcting for singletons

## transmitter_identify Movements
combo$Move<-ifelse(combo$transmitter_id==combo$Next.transmitter_id,ifelse(combo$Station_subregion3==combo$Next.Stat,"Stay","Move"),"NewFish") ## Assess behaviour (move/stay)
combo<-combo[!(combo$Move=="NewFish"),] ## Drop rows where there is a switch in the fish transmitter_id
combo$Movements<-paste(combo$Station_subregion3,combo$Next.Stat,sep="-") ## Show type of Movement

## Isolate Residency
combo$Res.Status<-ifelse(combo$Move=="Stay",ifelse(combo$Time.Diff<=21600,"Residence","Reset"),"Move") ##21600 - 6 hrs; 10800 - 3 hrs

Day_Long_Excursion<-subset(combo,Time.Diff>=86400) 
Week_Long_Excursions<-subset(combo,Time.Diff>=604800) 
write.csv(Day_Long_Excursion,file="HH_Goldfish_DayLongExcursions_6hrReset_03Dec2019.csv")
write.csv(Week_Long_Excursions,file="HH_Goldfish_WeekLongExcursions_6hrReset_03Dec2019.csv")

################################################################
## Summarize Proportional Restransmitter_idency By Indivdiual and Species ##
################################################################
Station.Output.1<-aggregate(Time.Diff~transmitter_id+Season.1+Station_subregion3+Res.Status+Movements,combo,sum) ## adjust "Station_subregion3" based on desired summary
sortnames2<-c("transmitter_id","Season.1","Station_subregion3","Res.Status","Movements")
Station.Output.1<-Station.Output.1[do.call("order",Station.Output.1[sortnames2]),]

## This is where we need to insert zero values for fish alive but not detected on a receiver
DataRep1<-data.frame(Station.Output.1$transmitter_id,Station.Output.1$Season.1)
DataRep1.1<-unique(DataRep1) ## Unique combos of transmitter_id and Season (only seasons where an indivtransmitter_idual was detected)
DataRep2<-unique(Station.Output.1$Station_subregion3) ## length = 5
DataRep1.2<-do.call("rbind", replicate(length(DataRep2), DataRep1.1, simplify = FALSE))
DataRep2.1<-rep(DataRep2,nrow(DataRep1.1))
DataRep2.2<-sort(DataRep2.1)
DataRep3<-data.frame(DataRep1.2,DataRep2.2)
Station.Base<-plyr::rename(DataRep3,c("Station.Output.1.transmitter_id"="transmitter_id","Station.Output.1.Season.1"="Season.1","DataRep2.2"="Station_subregion3"))
Station.Base <- Station.Base[order(Station.Base$transmitter_id, Station.Base$Season.1,Station.Base$Station_subregion3),]
###############################
Station.Output.Res.1<-Station.Output.1[(Station.Output.1$Res.Status=="Residence"),] ## Total Residence Time at Receiver Group by Season
Station.Output.Res.1$Time.Diff<-as.numeric(Station.Output.Res.1$Time.Diff)

## Issue b/c some indivtransmitter_iduals aren't at all present... so need to drop somehow...
Station.Output.Res.1.1<-merge(Station.Base,Station.Output.Res.1,by=c("transmitter_id","Season.1","Station_subregion3"),all=T)
Station.Output.Res.1.1$Time.Diff[is.na(Station.Output.Res.1.1$Time.Diff)] <- 0 ## Set time diff to zero

Total.Time.Station<-aggregate(Time.Diff~transmitter_id+Season.1,Station.Output.Res.1.1,sum)
Station.Output.Res.Merge.1<-merge(Station.Output.Res.1.1,Total.Time.Station,by=c("transmitter_id","Season.1"),all=T)
Station.Output.Res.Merge.1$Prop.Time.Res<-Station.Output.Res.Merge.1$Time.Diff.x/Station.Output.Res.Merge.1$Time.Diff.y ## Indivtransmitter_idual Proportional Restransmitter_idence Time at Receiver Group by Season
Station.Output.Res.Merge.1$Prop.Time.Res[is.na(Station.Output.Res.Merge.1$Prop.Time.Res)] <- 0 ## Set time diff to zero

###########
## New data output
head(Station.Output.Res.Merge.1)
Prop.Res.ID<-Station.Output.Res.Merge.1
Prop.Res.ID$Res.Status<-NULL
Prop.Res.ID$Movements<-NULL
Prop.Res.ID$Time.Diff.x<-NULL
Prop.Res.ID$Time.Diff.y<-NULL

write.csv(Prop.Res.ID,file="Goldfish_SeasonalProp_Subgroup3_Res_by_ID_03Dec2019.csv")


Prop.Season.Station.mean<-aggregate(Prop.Time.Res~Season.1+Station_subregion3,Station.Output.Res.Merge.1,mean)
Prop.Season.Station.sd<-aggregate(Prop.Time.Res~Season.1+Station_subregion3,Station.Output.Res.Merge.1,sd)
Prop.Season.Station.N<-aggregate(Prop.Time.Res~Season.1+Station_subregion3,Station.Output.Res.Merge.1,length)
Prop.Season.Station<-merge(Prop.Season.Station.mean,Prop.Season.Station.sd,by=c("Season.1","Station_subregion3"))
Prop.Season.Station<-merge(Prop.Season.Station,Prop.Season.Station.N,by=c("Season.1","Station_subregion3"))

Prop.Season.Station<-plyr::rename(Prop.Season.Station,c("Prop.Time.Res.x"="Mean.Res","Prop.Time.Res.y"="SD.Res","Prop.Time.Res"="N.Fish"))
write.csv(Prop.Season.Station,file="HH_Goldfish_OverallResidency_6hrReset_03Dec2019.csv") ### Summarized output



##############
## Boxplots ##
##############
Station.Output.Res.Merge.1$Station_subregion3<-factor(Station.Output.Res.Merge.1$Station_subregion3)
boxplot(Prop.Time.Res~Station_subregion3,data=Station.Output.Res.Merge.1,ylab="Proportion Residency",xlab="Station",main="Goldfish - Mean Residency",ylim=c(0,1))

##################
## GLATOS Plots ##
##################
combo.2<-combo
combo.2$detection_timestamp_utc=NULL
combo.2<-plyr::rename(combo.2,c("DMYHM"="detection_timestamp_utc"))
combo.2$Project.Region<-as.character(combo.2$Station_subregion3)
GLATOS.Locations<-c("Lake Ontario","Southeast","East",
                    "East Islands","Indian Creek","Centre",
                    "North","West","Piers 5-7",
                    "West NS","Cootes 2","Bayfront",
                    "Outer Carols Bay","Grindstone Outer","Grindstone DS","Grindstone US",
                    "Long Pond","Blackbird Marsh","Osprey Marsh",
                    "Sunfish Pond","Pond 1","Pond 2","Pond 4","Fuck")


#setwd("C:/Users/Mtransmitter_idwoodJ/Desktop")
xLim <- as.POSIXct(c("2017-06-26", "2019-06-01"), tz = "UTC")
combo.4<-subset(combo.2,transmitter_id=="14186")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14186",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14187")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14187",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14190")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14190",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14191")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14191",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14513")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14513",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="14523")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 14523",xlim=xLim)
#combo.4<-subset(combo.2,transmitter_id=="15849")
#abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15849",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15851")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15851",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15863")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15863",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15866")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15866",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15873")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15873",xlim=xLim)
combo.4<-subset(combo.2,transmitter_id=="15876")
abacus_plot(combo.4,location_col="Project.Region",locations=GLATOS.Locations,main="Goldfish 15876",xlim=xLim)




####################
## Depth Analysis ##
####################
#plot(Sensor.Value~DMYHM, data=winter)
#xyplot(-Sensor.Value ~ Date | transmitter_id, combo, pch= 20)

## Depth 
combo$Season.1.Order<- ifelse((combo$DMYHM >="2017-06-17")&(combo$DMYHM < "2017-10-14"), "A_summer_17",
                              ifelse((combo$DMYHM >= "2017-10-14")&(combo$DMYHM < "2017-11-24"), "B_fall_17",
                                     ifelse((combo$DMYHM >= "2017-11-24")&(combo$DMYHM < "2018-04-30"), "C_winter_17_18",
                                            ifelse((combo$DMYHM >= "2018-04-30") & (combo$DMYHM < "2018-05-31"), "D_spring_18",
                                                   ifelse((combo$DMYHM >= "2018-05-31")&(combo$DMYHM < "2018-10-02"), "E_summer_18",
                                                          ifelse((combo$DMYHM >= "2018-10-02")&(combo$DMYHM < "2018-11-16"), "F_fall_18",
                                                                 ifelse((combo$DMYHM >= "2018-11-16")&(combo$DMYHM < "2019-04-21"), "G_winter_18_19",
                                                                        ifelse((combo$DMYHM >= "2019-04-21") & (combo$DMYHM < "2019-06-01"),  "H_spring_19","Drop"))))))))



GF_Depth<-ddply(combo, c("transmitter_id","Season.1.Order"), summarise, Mean = mean(depth), SD = sd(depth),Minimum=min(depth),Maximum=max(depth),NDetect=length(depth))
GF_Depth<-GF_Depth[!(GF_Depth$Season.1.Order=="Drop"),]
boxplot(-Mean~Season.1.Order,data=GF_Depth,ylab="Mean Depth (m)",xlab="Season",ylim=c(-15,0))
boxplot(-depth~Season.1.Order,data=combo,ylab="Depth (m)",xlab="Season")
#boxplot(log(Sensor.Value)~Season.1,data=combo,ylab="Depth (m)",xlab="Season")
write.csv(GF_Depth,file="Goldfish_SeasonalDepth_by_ID_28Nov2019.csv")

combo.ex <- combo %>% dplyr::select(transmitter_id,
                                    station,
                                    DMYHM,
                                    min_lag,
                                    Location,
                                    Region,
                                    Station_subregion,
                                    Station_subregion2,
                                    Northing,
                                    Easting,
                                    length,
                                    mass,
                                    depth,
                                    Season.1,
                                    Season.1.Order,
                                    Move,
                                    Res.Status,
                                    Next.Stat,
                                    Next.Time,
                                    Next.transmitter_id)

write.csv(combo.ex,file="HH_Goldfish_CleanedUp_28Nov2019.csv")


#---------------------------------------------------------------------------------------

#remind station data to main database

stationGroupings <- read.csv("HH_Station_Groupings_GF.csv")


combo.ext <- left_join(combo.ex, stationGroupings, 
                       by = c("station", 
                              "Station_subregion", 
                              "Location",
                              "Region",
                              "Station_subregion2",
                              "Northing",
                              "Easting"
                              )
                       )

write.csv(combo.ext, "HH_Goldfish_CleanedUp_RT.csv")

a <- distinct(combo.ext[,c(2,5:8,21,22)])

write.csv(a, "Site.list.csv")

