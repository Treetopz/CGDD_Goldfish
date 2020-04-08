#Run goldfish1 first before goldfish2

#port in RBG data
#all relevant data are ported into the same folder for this specific project
setwd("I:/Fish Ecology Science/HH/Goldfish CGDD/Data")
# setwd("C:/Users/Destructionmama/Desktop/Goldfish CGDD/Data")
getwd()

#loading packages
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(strucchange)

# install.packages("devtools")
# install_github("twitter/AnomalyDetection")
# # https://github.com/twitter/AnomalyDetection
# library(AnomalyDetection)



#get file list for referrence
fileList <- list.files()
fileList

#import RBG data
fishWayRBG <- read.csv("RBG_FIsh_WQ data.csv")

#arrange dates
fishWayRBG$Date <- dmy(fishWayRBG$Date)
fishWayRBG$Month <- month(fishWayRBG$Date)

#Subset based on species captured
# df.gf <- fishWayRBG[fishWayRBG$Species == "Goldfish", ]
# df.cc <- fishWayRBG[fishWayRBG$Species == "Common Carp",]
# df.ccgr <- fishWayRBG[fishWayRBG$Species == "Common Carp X Goldfish Rudd",]
# df.ccgf <- fishWayRBG[fishWayRBG$Species == "Common Carp x Goldfish",]

#-----------------------------------------------------------------------------------
#port in fishway temperature
fishWayTemp <- fread("FishwayDOT2015_2019.csv")

#Change Use identifier to logial
fishWayTemp$Use <- as.logical(fishWayTemp$Use)

#remove all unrelevant data
fishWayTemp <- fishWayTemp[fishWayTemp$Use == T,]

#only take bottom data
fishWayTemp <- fishWayTemp[fishWayTemp$Suffix == 'bottom', ]

dftemp <- fishWayTemp[fishWayTemp$Serial == 20105539] 

fishWayTemp <- fishWayTemp[fishWayTemp$Serial != 20105539] 


#source multidate to fix dates
source("multilubridate.R")

#format dates
fishWayTemp$Date.Time <- multilubridate(data = fishWayTemp$Date.Time, 
                                          formats = c("dmy_hm", "ymd_hm")
                                         )

fishWayTemp$Date.Time.UTC <- multilubridate(data = fishWayTemp$Date.Time.UTC, 
                                             formats = c("dmy_hm", "ymd_hm")
                                             )


dftemp$Date.Time <- multilubridate(data = dftemp$Date.Time, 
                                        formats = c("mdy_hm")
)

dftemp$Date.Time.UTC <- multilubridate(data = dftemp$Date.Time.UTC, 
                                            formats = c("mdy_hm")
)

#rebind
fishWayTemp <- rbind(fishWayTemp, dftemp)

#get date information
fishWayTemp$Date <- date(fishWayTemp$Date.Time)
fishWayTemp$Hour <- hour(fishWayTemp$Date.Time)

# plot(dftemp$Temp ~ dftemp$Date.Time)
plot(fishWayTemp$Temp ~ fishWayTemp$Date.Time)

#--------------------------------------------------------------------------------------
#Calculate GDCC

#fist calculate daily summary statistics
summaryDOT <- group_by(fishWayTemp, Date) %>% summarise(tempMean = mean(Temp), 
                                                      temp.Max = max(Temp),
                                                      temp.Min = min(Temp), 
                                                      temp.N = length(Temp),
                                                      temp.SD = sd(Temp),                                                      
                                                      DO.Mean = mean(DO.Final), 
                                                      DO.Max = max(DO.Final),
                                                      DO.Min = min(DO.Final), 
                                                      DO.N = length(DO.Final),
                                                      DO.SD = sd(DO.Final)   
                                                      )


#calculate CGDD 
summaryDOT$deltaGDD <- summaryDOT$tempMean - 5

#reset any negative values to 0
summaryDOT$deltaGDD[summaryDOT$deltaGDD <= 0] <- 0

#calculate cumulative sum by year - CGDD
summaryDOT$Year <- year(summaryDOT$Date)

summaryDOT2 <- split(summaryDOT, summaryDOT$Year)


#search through list 
for (i in 1: length(summaryDOT2)){
  
  summaryDOT2[[i]]$CGDD <- cumsum(summaryDOT2[[i]]$deltaGDD)
  
}

summaryDOT3 <- data.frame()

#unlist and combine
for (i in 1:length(summaryDOT2)){
  
  binder <- data.frame(summaryDOT2[[i]])
  
  summaryDOT3 <- rbind(summaryDOT3, binder)
  
}


#merge data
fishwayTemp2 <- left_join(fishWayTemp, summaryDOT2, by = "Date")

#calcualte CGDD Base temperatre set at 5 degree
fishwayTemp2$DD_Base5 <- fishwayTemp2$tempMean

#mean of (max temp -5) by date
DailyMeanSurplus_5 <- aggregate(DD_Base5 ~ Date, data = fishwayTemp2,FUN = mean)
DailyMeanSurplus_5$Year <- year(DailyMeanSurplus_5$Date)
DD_Base5<- aggregate(DailyMeanSurplus_5$DD_Base5,
                    by = list(Date = DailyMeanSurplus_5$Date, Year = DailyMeanSurplus_5$Year),
                    FUN = sum)
colnames(DD_Base5) <- c("Date", "Year", "GDD")




# #calcualte CGDD Base temperatre set at 5 degree
# fishwayTemp2$DD_Base5 <- fishwayTemp2$temp.Max - 5
# 
# #mean of (max temp -5) by date
# DailyMeanSurplus_5 <- aggregate(DD_Base5 ~ Date, data = fishwayTemp2,FUN = mean)
# DailyMeanSurplus_5$Year <- year(DailyMeanSurplus_5$Date)
# DD_Base5<- aggregate(DailyMeanSurplus_5$DD_Base5, 
#                     by = list(Date = DailyMeanSurplus_5$Date, Year = DailyMeanSurplus_5$Year), 
#                     FUN = sum)
# colnames(DD_Base5) <- c("Date", "Year", "GDD")

#cumsum doesn't work when applied in a list, 
# GDD_Base5 <- aggregate(DD_Base5$GDD, by = list(Year = DD_Base5$Year), FUN = cumsum)


GDDfinal_Base5 <- data.frame()
for (i in 1:length(unique(DD_Base5$Year))){
  
  
  #for each year subset data
  GDD_Base5 <- DD_Base5[DD_Base5$Year == unique(DD_Base5$Year)[i],]
  GDD_Base5$CGDD <- cumsum(GDD_Base5$GDD)
  
  #bind data together
  GDDfinal_Base5 <- rbind(GDDfinal_Base5, GDD_Base5)
  
}

plot(GDDfinal_Base5$Date, GDDfinal_Base5$CGDD)

# DD_Base5$CGDD_Base5<- cumsum(DD_Base5$DD_Base5)

fishwayTemp2 <- left_join(fishwayTemp2, GDDfinal_Base5, by = "Date")


final.df <- full_join(GDDfinal_Base5, fishWayRBG,by = "Date")

colnames(final.df) <- c("Date",
                        "Year",
                        "GDD",
                        "CGDD",
                        "Species",
                        "n_count",
                        "Mean.Temp.Mid",
                        "Mean.DO.Mid",
                        "Mean.Depth",
                        "Mean.Secchi.cm",
                        "Mean.Precipitation",
                        "Mean.Turb",
                        "MonthRBG")
                        

#Filter goldfish data only
final.df.GF <- final.df[final.df$Species == "Goldfish" | is.na(final.df$Species) == T,]


#add Goldfish PAs
final.df.GF$PA_Goldfish <- ifelse(is.na(final.df.GF$n_count) == F, 1, 0)
# final.df.GF$PA_Goldfish <- as.factor(final.df.GF$PA_Goldfish)

#add Goldfish PAs with threshold
final.df.GF$PA50_Goldfish <- ifelse(final.df.GF$n_count < 50 | is.na(final.df.GF$n_count) == T, 0, 1)
final.df.GF$PA100_Goldfish <- ifelse(final.df.GF$n_count < 100 | is.na(final.df.GF$n_count) == T, 0, 1)
final.df.GF$PA150_Goldfish <- ifelse(final.df.GF$n_count < 150 | is.na(final.df.GF$n_count) == T, 0, 1)
final.df.GF$PA200_Goldfish <- ifelse(final.df.GF$n_count < 200 | is.na(final.df.GF$n_count) == T, 0, 1)

#reorder data
final.df.GF <- final.df.GF[order(final.df.GF$Date),]

#redo year for unsorted data
final.df.GF$Year <- year(final.df.GF$Date)
final.df.GF$Month <- month(final.df.GF$Date)

#add time difference between pulls
PullDiff <- final.df.GF[final.df.GF$PA_Goldfish == 1,]
PullDiff$lastPullDate <- shift(PullDiff$Date)
PullDiff$pullDiffCalc <- PullDiff$Date - PullDiff$lastPullDate
#bind data
final.df.GF <- left_join(final.df.GF, PullDiff)

#calculate capture/days since last pull
final.df.GF$pullDiffCalc <- as.numeric(final.df.GF$pullDiffCalc)
final.df.GF$nOverDays <- final.df.GF$n_count/final.df.GF$pullDiffCalc

#reattach mean temperature
final.df.GF<- left_join(final.df.GF, summaryDOT, by = "Date") 


#------------------------------------------------------
# exploring breakpoint analysis 
#identify break points using AnomalyDetection package
# https://github.com/twitter/AnomalyDetection

# abNorm <- final.df.GF[,c("CGDD", "nOverDays")]
# abNorm <- final.df.GF[,c("Date", "nOverDays")]
# abNorm <- abNorm[complete.cases(abNorm) == T,]
# # 
# # breakpoint()
# # AnomalyDetectionTs(abNorm, max_anoms=0.02, direction='pos', plot=TRUE)
# time_decompose(abNorm)
#------------------------------------------------------

#define periods
final.df.GF$Period[final.df.GF$CGDD <= 1000] <- "P1"
final.df.GF$Period[final.df.GF$CGDD > 1000] <- "P2"

final.df.GF$Period_B[final.df.GF$CGDD <= 400] <- "P1"
final.df.GF$Period_B[final.df.GF$CGDD > 1200] <- "P2"

write.csv(final.df.GF, "final.df.GF.csv")

# boxplot(final.df.GF$nOverDays ~ final.df.GF$Year)
# plot(final.df.GF$nOverDays ~ final.df.GF$CGDD)
# plot(final.df.GF$nOverDays ~ final.df.GF$GDD)
# 
# #captures per day by year 
# plot(final.df.GF$n_count ~ final.df.GF$pullDiffCalc)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#logistic regressions

#all periods     
#TempMean P1 + P2
logit.tempMean <- glm(as.factor(final.df.GF$PA_Goldfish) ~ final.df.GF$tempMean, family = 'binomial')
summary(logit.tempMean)

plot.logit.tempMean <- ggplot(final.df.GF, aes(tempMean, as.numeric(PA_Goldfish))) + 
                        geom_point(shape = 1, size = 4) + 
                        stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean, filename = "plot.logit.tempMean.png")

#CGDD P1 + P2
logit.CGDD <- glm(as.factor(final.df.GF$PA_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
summary(logit.tempMean)

plot.logit.CGDD <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA_Goldfish))) + 
                    geom_point(shape = 1, size = 4) + 
                    stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD, filename = "plot.logit.CGDD.png")

#-----------------------------------------------------------------
#P1 only
final.df.GF.P1 <- final.df.GF[which(final.df.GF$Period == "P1"),]
# final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$tempMean, final.df.GF.P1$CGDD),]

#TempMean P1                 
logit.tempMean.P1 <- glm(as.factor(final.df.GF.P1$PA_Goldfish) ~ final.df.GF.P1$tempMean, family = 'binomial')
summary(logit.tempMean.P1)

plot.logit.tempMean.P1 <- ggplot(final.df.GF.P1, aes(tempMean, as.numeric(PA_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P1, filename = "plot.logit.tempMean.P1.png")

#CGDD P1                 
logit.CGDD.P1 <- glm(as.factor(final.df.GF.P1$PA_Goldfish) ~ final.df.GF.P1$CGDD, family = 'binomial')
summary(logit.CGDD.P1)

plot.logit.CGDD.P1 <- ggplot(final.df.GF.P1, aes(CGDD, as.numeric(PA_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P1, filename = "plot.logit.CGDD.P1.png")


#-----------------------------------------------------------------
#P2 only
final.df.GF.P2 <- final.df.GF[which(final.df.GF$Period == "P2"),]
# final.df.GF.P2 <- final.df.GF.P2[complete.cases(final.df.GF.P2$tempMean, final.df.GF.P2$CGDD),]

#TempMean P2                 
logit.tempMean.P2 <- glm(as.factor(final.df.GF.P2$PA_Goldfish) ~ final.df.GF.P2$tempMean, family = 'binomial')
summary(logit.tempMean.P2)

plot.logit.tempMean.P2 <- ggplot(final.df.GF.P2, aes(tempMean, as.numeric(PA_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P2, filename = "plot.logit.tempMean.P2.png")

#CGDD P2                 
logit.CGDD.P2 <- glm(as.factor(final.df.GF.P2$PA_Goldfish) ~ final.df.GF.P2$CGDD, family = 'binomial')
summary(logit.CGDD.P2)

plot.logit.CGDD.P2 <- ggplot(final.df.GF.P2, aes(CGDD, as.numeric(PA_Goldfish))) + 
                        geom_point(shape = 1, size = 4) + 
                        stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P2, filename = "plot.logit.CGDD.P2.png")


#---------------------------------------------------------------------------------
#
#all periods 50s  
#TempMean P1 + P2
logit.tempMean.50 <- glm(as.factor(final.df.GF$PA50_Goldfish) ~ final.df.GF$tempMean, family = 'binomial')
summary(logit.tempMean.50)

plot.logit.tempMean.50 <- ggplot(final.df.GF, aes(tempMean, as.numeric(PA50_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.50, filename = "plot.logit.tempMean.50.png")

#CGDD P1 + P2
logit.CGDD.50 <- glm(as.factor(final.df.GF$PA50_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
summary(logit.CGDD.50)

plot.logit.CGDD.50 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA50_Goldfish))) + 
                        geom_point(shape = 1, size = 4) + 
                        stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.50, filename = "plot.logit.CGDD.50.png")
#---------------------------------------------------------------------------------
#all periods 100s  
#TempMean P1 + P2
logit.tempMean.100 <- glm(as.factor(final.df.GF$PA100_Goldfish) ~ final.df.GF$tempMean, family = 'binomial')
summary(logit.tempMean.100)

plot.logit.tempMean.100 <- ggplot(final.df.GF, aes(tempMean, as.numeric(PA100_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.100, filename = "plot.logit.tempMean.100.png")

#CGDD P1 + P2
logit.CGDD.100 <- glm(as.factor(final.df.GF$PA100_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
summary(logit.CGDD.100)

plot.logit.CGDD.100 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA100_Goldfish))) + 
                        geom_point(shape = 1, size = 4) + 
                        stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.100, filename = "plot.logit.CGDD.100.png")

#---------------------------------------------------------------------------------
#all periods 150s  
#TempMean P1 + P2
logit.tempMean.150 <- glm(as.factor(final.df.GF$PA150_Goldfish) ~ final.df.GF$tempMean, family = 'binomial')
summary(logit.tempMean.150)

plot.logit.tempMean.150 <- ggplot(final.df.GF, aes(tempMean, as.numeric(PA150_Goldfish))) + 
                            geom_point(shape = 1, size = 4) + 
                            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.150, filename = "plot.logit.tempMean.150.png")

#CGDD P1 + P2
logit.CGDD.150 <- glm(as.factor(final.df.GF$PA150_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
summary(logit.CGDD.150)

plot.logit.CGDD.150 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA150_Goldfish))) + 
                          geom_point(shape = 1, size = 4) + 
                          stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.150, filename = "plot.logit.CGDD.150.png")

#---------------------------------------------------------------------------------

#-----------------------------------------------------------------
#P1 only
#PA_Goldfish50

final.df.GF.P1.50 <- final.df.GF[which(final.df.GF$Period == "P1"),]
# final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$tempMean, final.df.GF.P1$CGDD),]

#TempMean P1                 
logit.tempMean.P1.50 <- glm(as.factor(final.df.GF.P1$PA50_Goldfish) ~ final.df.GF.P1$tempMean, family = 'binomial')
summary(logit.tempMean.P1.50)

plot.logit.tempMean.P1.50 <- ggplot(final.df.GF.P1, aes(tempMean, as.numeric(PA50_Goldfish))) + 
                                geom_point(shape = 1, size = 4) + 
                                stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P1.50, filename = "plot.logit.tempMean.P1.50.png")

#CGDD P1                 
logit.CGDD.P1.50 <- glm(as.factor(final.df.GF.P1$PA50_Goldfish) ~ final.df.GF.P1$CGDD, family = 'binomial')
summary(logit.CGDD.P1.50)

plot.logit.CGDD.P1.50 <- ggplot(final.df.GF.P1, aes(CGDD, as.numeric(PA50_Goldfish))) + 
                                  geom_point(shape = 1, size = 4) + 
                                  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P1.50, filename = "plot.logit.CGDD.P1.50.png")

#---------------------------------------------------------------------------------------------
#P1_B
#no threshold
final.df.GF.P1B <- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$tempMean, final.df.GF.P1$CGDD),]

#TempMean P1                 
logit.tempMean.P1B <- glm(as.factor(final.df.GF.P1B$PA_Goldfish) ~ final.df.GF.P1B$tempMean, family = 'binomial')
summary(logit.tempMean.P1)

plot.logit.tempMean.P1B <- ggplot(final.df.GF.P1B, aes(tempMean, as.numeric(PA_Goldfish))) + 
                              geom_point(shape = 1, size = 4) + 
                              stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P1B, filename = "plot.logit.tempMean.P1B.png")

#CGDD P1                 
logit.CGDD.P1B <- glm(as.factor(final.df.GF.P1B$PA_Goldfish) ~ final.df.GF.P1B$CGDD, family = 'binomial')
summary(logit.CGDD.P1)

plot.logit.CGDD.P1B <- ggplot(final.df.GF.P1B, aes(CGDD, as.numeric(PA_Goldfish))) + 
                          geom_point(shape = 1, size = 4) + 
                          stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P1B, filename = "plot.logit.CGDD.P1B.png")

#---------------------------------------------------------------------------------------------
#P1_B
#50 threshold
final.df.GF.P1B.50<- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$tempMean, final.df.GF.P1$CGDD),]

#TempMean P1                 
logit.tempMean.P1B.50 <- glm(as.factor(final.df.GF.P1B$PA50_Goldfish) ~ final.df.GF.P1B$tempMean, family = 'binomial')
summary(logit.tempMean.P1.50)

plot.logit.tempMean.P1B.50 <- ggplot(final.df.GF.P1B.50, aes(tempMean, as.numeric(PA50_Goldfish))) + 
                                geom_point(shape = 1, size = 4) + 
                                stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P1B.50, filename = "plot.logit.tempMean.P1B.50.png")

#CGDD P1                 
logit.CGDD.P1B.50 <- glm(as.factor(final.df.GF.P1B.50$PA50_Goldfish) ~ final.df.GF.P1B.50$CGDD, family = 'binomial')
summary(logit.CGDD.P1.50)

plot.logit.CGDD.P1B.50 <- ggplot(final.df.GF.P1B.50, aes(CGDD, as.numeric(PA50_Goldfish))) + 
                          geom_point(shape = 1, size = 4) + 
                          stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P1B.50, filename = "plot.logit.CGDD.P1B.50.png")

#---------------------------------------------------------------------------------------------
#P1_B
#100 threshold
final.df.GF.P1B.100<- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$tempMean, final.df.GF.P1$CGDD),]

#TempMean P1                 
logit.tempMean.P1B.100 <- glm(as.factor(final.df.GF.P1B$PA100_Goldfish) ~ final.df.GF.P1B$tempMean, family = 'binomial')
summary(logit.tempMean.P1B.100)

plot.logit.tempMean.P1B.100 <- ggplot(final.df.GF.P1B.100, aes(tempMean, as.numeric(PA100_Goldfish))) + 
  geom_point(shape = 1, size = 4) + 
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.tempMean.P1B.100, filename = "plot.logit.tempMean.P1B.100.png")

#CGDD P1                 
logit.CGDD.P1B.100 <- glm(as.factor(final.df.GF.P1B.100$PA100_Goldfish) ~ final.df.GF.P1B.100$CGDD, family = 'binomial')
summary(logit.CGDD.P1B.100)

plot.logit.CGDD.P1B.100 <- ggplot(final.df.GF.P1B.100, aes(CGDD, as.numeric(PA100_Goldfish))) + 
  geom_point(shape = 1, size = 4) + 
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(plot.logit.CGDD.P1B.100, filename = "plot.logit.CGDD.P1B.100.png")



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#preliminary figures
# plot(as.numeric(final.df.GF$n_count) ~ final.df.GF$GDD)
# plot(final.df.GF$n_count ~ final.df.GF$CGDD)
# plot(final.df.GF$GDD ~ final.df.GF$n_count)

png("boxplot.NOverDays.Month.png")
boxplot(final.df.GF$nOverDays ~ final.df.GF$Month)
dev.off()

png("plot.NOverDays.CGDD.png")
plot(final.df.GF$nOverDays ~ final.df.GF$CGDD)
dev.off()

library(sm)
library(ggplot2)

#plot temperature trends
png("MeanTemp.png")
plot(final.df.GF$tempMean[final.df.GF$Year >2014] ~ final.df.GF$Date[final.df.GF$Year >2014])
dev.off()

boxplot(final.df.GF$tempMean ~ final.df.GF$Year)

Temp_PA <- ggplot(final.df.GF, aes(x = tempMean, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)
GDD_PA <- ggplot(final.df.GF, aes(x = GDD, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)
CGDD_PA <- ggplot(final.df.GF, aes(x = CGDD, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)

plot( final.df.GF$nOverDays ~ final.df.GF$tempMean)

ggsave(Temp_PA, filename = "Temp_PA.png")
ggsave(GDD_PA, filename = "GDD_PA.png")
ggsave(CGDD_PA, filename = "CGDD_PA.png")


png('Date_GDD.png')
plot(fishwayTemp2$Date, fishwayTemp2$GDD)
dev.off()

png('Date_CGDD.png')
plot(fishwayTemp2$Date, fishwayTemp2$CGDD)
dev.off()


#
uTemp.CGDD.NOD <- ggplot(data= final.df.GF,
                         aes(CGDD, tempMean)) + 
                  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                  scale_colour_gradient2(low = "black", 
                                         mid = "blue",   
                                         high = "red",
                                         midpoint = mean(final.df.GF$nOverDays, na.rm = T))

ggsave(uTemp.CGDD.NOD, filename = "uTemp.CGDD.NOD.png", dpi = 600)


uTemp.CGDD.NOD.E <- ggplot(data= final.df.GF,
                           aes(CGDD, tempMean)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black')

ggsave(uTemp.CGDD.NOD.E, filename = "uTemp.CGDD.NOD.E.png", dpi = 600)


uTemp.CGDD.NOD.ED <- ggplot(data= final.df.GF,
                           aes(CGDD, tempMean)) + 
                    geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                    scale_colour_gradient2(low = "black", 
                                           mid = "blue",   
                                           high = "red",
                                           midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
                    stat_density2d(colour = 'black') + 
                    geom_density(aes(x = CGDD, fill = as.factor(PA_Goldfish)))

ggsave(uTemp.CGDD.NOD.E, filename = "uTemp.CGDD.NOD.E.png", dpi = 600)


# ------------------------------------------------------------------------------


# #########################
# ## Growing Degree Days ## From Jon
# #########################
# combo$Date<-date(combo$DateTime)
# combo$DD_Base5<-combo$HighTemp-5
# DailyMeanSurplus_5<-aggregate(DD_Base5~Date,data=combo,FUN=mean)
# 
# DD_Base5<-aggregate(DD_Base5~Date,data=DailyMeanSurplus_5,FUN=sum)
# DD_Base5$CGDD_Base5<-cumsum(DD_Base5$DD_Base5)
# plot(CGDD_Base5~Date,data=DD_Base5)
# points(combo.2$Date, combo.2$Total.Day, pch=15, col="red")


