#changed version

#port in RBG data
#all relevant data are ported into the same folder for this specific project
setwd("K:/Today22/Goldfish CGDD/Data")
# setwd("C:/Users/Destructionmama/Desktop/Goldfish CGDD/Data")
# setwd("I:/Fish Ecology Science/HH/Goldfish CGDD/Data")
getwd()

#loading packages
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(strucchange)
library(ggridges)

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
summaryDOT <- group_by(fishWayTemp, Date) %>% summarise(temp.Mean = mean(Temp, na.rm = T), 
                                                        temp.Max = max(Temp, na.rm = T),
                                                        temp.Min = min(Temp, na.rm = T), 
                                                        temp.N = length(Temp),
                                                        temp.SD = sd(Temp, na.rm = T),                                                      
                                                        DO.Mean = mean(DO.Final, na.rm = T), 
                                                        DO.Max = max(DO.Final, na.rm = T),
                                                        DO.Min = min(DO.Final, na.rm = T), 
                                                        DO.N = length(DO.Final),
                                                        DO.SD = sd(DO.Final, na.rm = T)   
                                                      )


#calculate CGDD 
summaryDOT$deltaGDD <- summaryDOT$temp.Mean - 5

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

#plot CGDD trends
png(file = "CGDD_Date.png", height = 500, width = 750)
plot(summaryDOT3$CGDD ~ summaryDOT3$Date,
     xlab = "Year",ylab = "CGDD")
dev.off()

#merge data
fishwayTemp2 <- left_join(fishWayTemp, summaryDOT3, by = "Date")

#write fishwayTemp2
write.csv(fishwayTemp2, "fishwayTemp2.csv")


#save
final.df.long <- full_join(fishwayTemp2, fishWayRBG,by = "Date")
colnames(final.df.long) <- c("Index",
                              "Date.Time",
                              "Date.Time.UTC",
                              "DO",
                              "Temp",
                              "DO-Adj",
                              "DO.Corr",
                              "DO.Final",
                              "Use",
                              "Serial",
                              "System.Location",
                              "Site",
                              "Suffix",
                              "Depth",
                              "LHAB_m",
                              "Date",
                              "Hour",
                              "temp.Mean",
                              "temp.Max",
                              "temp.Min",
                              "temp.N",
                              "temp.SD",
                              "DO.Mean",
                              "DO.Max",
                              "DO.Min",
                              "DO.N",
                              "DO.SD",
                              "deltaGDD",
                              "Year",
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
# write final database
write.csv(final.df.long, "final.df.long.csv")


#
final.df <- full_join(summaryDOT3, fishWayRBG, by = "Date")
colnames(final.df) <- c("Date",
                        "temp.Mean",
                        "temp.Max",
                        "temp.Min",
                        "temp.N",
                        "temp.SD",
                        "DO.Mean",
                        "DO.Max",
                        "DO.Min",
                        "DO.N",
                        "DO.SD",
                        "deltaGDD",
                        "Year",
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




write.csv(final.df, "final.df.csv")

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
# final.df.GF$Year <- year(final.df.GF$Date)
# final.df.GF$Month <- month(final.df.GF$Date)

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
# final.df.GF<- left_join(final.df.GF, summaryDOT, by = "Date") 


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


#redo year for unsorted data
final.df.GF$Year <- year(final.df.GF$Date)
final.df.GF$Month <- month(final.df.GF$Date)


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
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#logistic regressions
# setwd("K:/Today22/Goldfish CGDD/Data")
setwd("I:/Fish Ecology Science/HH/Goldfish CGDD/Data")
final.df.GF <- fread("final.df.GF.csv")

final.df.GF$Date <- ymd(final.df.GF$Date)
final.df.GF$JDay <- yday(final.df.GF$Date)


#logistic regression on less than 200 CGDD 2015
temp <- final.df.GF[final.df.GF$CGDD <= 200 & final.df.GF$Year == 2015,]
logit.CGDD2015 <- glm(as.factor(temp$PA_Goldfish) ~ temp$CGDD, family = 'binomial')
summary(logit.CGDD2015)

#plot logistic regression on less than 200 CGDD 2015
CGDD2015 <- ggplot(temp, aes(CGDD, as.numeric(PA_Goldfish))) +
  geom_point(shape = 1, size = 4) +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(CGDD2015, filename = "plot.logit.CGDD2015.png")

#-----------------------------------------------
#logistic regression on less than 200 CGDD 2016
temp <- final.df.GF[final.df.GF$CGDD <= 200 & final.df.GF$Year == 2016,]
logit.CGDD2016 <- glm(as.factor(temp$PA_Goldfish) ~ temp$CGDD, family = 'binomial')
summary(logit.CGDD2016)

#plot logistic regression on less than 200 CGDD 2016
CGDD2016 <- ggplot(temp, aes(CGDD, as.numeric(PA_Goldfish))) +
            geom_point(shape = 1, size = 4) +
            stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(CGDD2016, filename = "plot.logit.CGDD2016.png")

#-----------------------------------------------
#logistic regression on less than 200 CGDD 2017
temp <- final.df.GF[final.df.GF$CGDD <= 200 & final.df.GF$Year == 2017,]
logit.CGDD2017 <- glm(as.factor(temp$PA_Goldfish) ~ temp$CGDD, family = 'binomial')
summary(logit.CGDD2017)

#plot logistic regression on less than 200 CGDD 2017
CGDD2017 <- ggplot(temp, aes(CGDD, as.numeric(PA_Goldfish))) +
  geom_point(shape = 1, size = 4) +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(CGDD2017, filename = "plot.logit.CGDD2017.png")

#-----------------------------------------------

temp <- final.df.GF[final.df.GF$CGDD <= 200 & final.df.GF$Year == 2018,]
logit.CGDD2018 <- glm(as.factor(temp$PA_Goldfish) ~ temp$CGDD, family = 'binomial')
summary(logit.CGDD2018)

CGDD2018 <- ggplot(temp, aes(CGDD, as.numeric(PA_Goldfish))) +
  geom_point(shape = 1, size = 4) +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(CGDD2018, filename = "plot.logit.CGDD2018.png")

#-----------------------------------------------

temp <- final.df.GF[final.df.GF$CGDD <= 200 & final.df.GF$Year == 2019,]
logit.CGDD2019 <- glm(as.factor(temp$PA_Goldfish) ~ temp$CGDD, family = 'binomial')
summary(logit.CGDD2019)

CGDD2019 <- ggplot(temp, aes(CGDD, as.numeric(PA_Goldfish))) +
  geom_point(shape = 1, size = 4) +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
ggsave(CGDD2019, filename = "plot.logit.CGDD2019.png")


# #all periods     
# #temp.Mean P1 + P2
# logit.temp.Mean <- glm(as.factor(final.df.GF$PA_Goldfish) ~ final.df.GF$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean)
# 
# plot.logit.temp.Mean <- ggplot(final.df.GF, aes(temp.Mean, as.numeric(PA_Goldfish))) + 
#                         geom_point(shape = 1, size = 4) + 
#                         stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean, filename = "plot.logit.temp.Mean.png")
# 
# #CGDD P1 + P2
# logit.CGDD <- glm(as.factor(final.df.GF$PA_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
# summary(logit.temp.Mean)
# 
# plot.logit.CGDD <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA_Goldfish))) + 
#                     geom_point(shape = 1, size = 4) + 
#                     stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD, filename = "plot.logit.CGDD.png")
# 
# #-----------------------------------------------------------------
# #P1 only
# final.df.GF.P1 <- final.df.GF[which(final.df.GF$Period == "P1"),]
# # final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$temp.Mean, final.df.GF.P1$CGDD),]
# 
# #temp.Mean P1                 
# logit.temp.Mean.P1 <- glm(as.factor(final.df.GF.P1$PA_Goldfish) ~ final.df.GF.P1$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P1)
# 
# plot.logit.temp.Mean.P1 <- ggplot(final.df.GF.P1, aes(temp.Mean, as.numeric(PA_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P1, filename = "plot.logit.temp.Mean.P1.png")
# 
# #CGDD P1                 
# logit.CGDD.P1 <- glm(as.factor(final.df.GF.P1$PA_Goldfish) ~ final.df.GF.P1$CGDD, family = 'binomial')
# summary(logit.CGDD.P1)
# 
# plot.logit.CGDD.P1 <- ggplot(final.df.GF.P1, aes(CGDD, as.numeric(PA_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P1, filename = "plot.logit.CGDD.P1.png")
# 
# 
# #-----------------------------------------------------------------
# #P2 only
# final.df.GF.P2 <- final.df.GF[which(final.df.GF$Period == "P2"),]
# # final.df.GF.P2 <- final.df.GF.P2[complete.cases(final.df.GF.P2$temp.Mean, final.df.GF.P2$CGDD),]
# 
# #temp.Mean P2                 
# logit.temp.Mean.P2 <- glm(as.factor(final.df.GF.P2$PA_Goldfish) ~ final.df.GF.P2$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P2)
# 
# plot.logit.temp.Mean.P2 <- ggplot(final.df.GF.P2, aes(temp.Mean, as.numeric(PA_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P2, filename = "plot.logit.temp.Mean.P2.png")
# 
# #CGDD P2                 
# logit.CGDD.P2 <- glm(as.factor(final.df.GF.P2$PA_Goldfish) ~ final.df.GF.P2$CGDD, family = 'binomial')
# summary(logit.CGDD.P2)
# 
# plot.logit.CGDD.P2 <- ggplot(final.df.GF.P2, aes(CGDD, as.numeric(PA_Goldfish))) + 
#                         geom_point(shape = 1, size = 4) + 
#                         stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P2, filename = "plot.logit.CGDD.P2.png")
# 
# 
# #---------------------------------------------------------------------------------
# #
# #all periods 50s  
# #temp.Mean P1 + P2
# logit.temp.Mean.50 <- glm(as.factor(final.df.GF$PA50_Goldfish) ~ final.df.GF$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.50)
# 
# plot.logit.temp.Mean.50 <- ggplot(final.df.GF, aes(temp.Mean, as.numeric(PA50_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.50, filename = "plot.logit.temp.Mean.50.png")
# 
# #CGDD P1 + P2
# logit.CGDD.50 <- glm(as.factor(final.df.GF$PA50_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
# summary(logit.CGDD.50)
# 
# plot.logit.CGDD.50 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA50_Goldfish))) + 
#                         geom_point(shape = 1, size = 4) + 
#                         stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.50, filename = "plot.logit.CGDD.50.png")
# #---------------------------------------------------------------------------------
# #all periods 100s  
# #temp.Mean P1 + P2
# logit.temp.Mean.100 <- glm(as.factor(final.df.GF$PA100_Goldfish) ~ final.df.GF$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.100)
# 
# plot.logit.temp.Mean.100 <- ggplot(final.df.GF, aes(temp.Mean, as.numeric(PA100_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.100, filename = "plot.logit.temp.Mean.100.png")
# 
# #CGDD P1 + P2
# logit.CGDD.100 <- glm(as.factor(final.df.GF$PA100_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
# summary(logit.CGDD.100)
# 
# plot.logit.CGDD.100 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA100_Goldfish))) + 
#                         geom_point(shape = 1, size = 4) + 
#                         stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.100, filename = "plot.logit.CGDD.100.png")
# 
# #---------------------------------------------------------------------------------
# #all periods 150s  
# #temp.Mean P1 + P2
# logit.temp.Mean.150 <- glm(as.factor(final.df.GF$PA150_Goldfish) ~ final.df.GF$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.150)
# 
# plot.logit.temp.Mean.150 <- ggplot(final.df.GF, aes(temp.Mean, as.numeric(PA150_Goldfish))) + 
#                             geom_point(shape = 1, size = 4) + 
#                             stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.150, filename = "plot.logit.temp.Mean.150.png")
# 
# #CGDD P1 + P2
# logit.CGDD.150 <- glm(as.factor(final.df.GF$PA150_Goldfish) ~ final.df.GF$CGDD, family = 'binomial')
# summary(logit.CGDD.150)
# 
# plot.logit.CGDD.150 <- ggplot(final.df.GF, aes(CGDD, as.numeric(PA150_Goldfish))) + 
#                           geom_point(shape = 1, size = 4) + 
#                           stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.150, filename = "plot.logit.CGDD.150.png")
# 
# #---------------------------------------------------------------------------------
# 
# #-----------------------------------------------------------------
# #P1 only
# #PA_Goldfish50
# 
# final.df.GF.P1.50 <- final.df.GF[which(final.df.GF$Period == "P1"),]
# # final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$temp.Mean, final.df.GF.P1$CGDD),]
# 
# #temp.Mean P1                 
# logit.temp.Mean.P1.50 <- glm(as.factor(final.df.GF.P1$PA50_Goldfish) ~ final.df.GF.P1$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P1.50)
# 
# plot.logit.temp.Mean.P1.50 <- ggplot(final.df.GF.P1, aes(temp.Mean, as.numeric(PA50_Goldfish))) + 
#                                 geom_point(shape = 1, size = 4) + 
#                                 stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P1.50, filename = "plot.logit.temp.Mean.P1.50.png")
# 
# #CGDD P1                 
# logit.CGDD.P1.50 <- glm(as.factor(final.df.GF.P1$PA50_Goldfish) ~ final.df.GF.P1$CGDD, family = 'binomial')
# summary(logit.CGDD.P1.50)
# 
# plot.logit.CGDD.P1.50 <- ggplot(final.df.GF.P1, aes(CGDD, as.numeric(PA50_Goldfish))) + 
#                                   geom_point(shape = 1, size = 4) + 
#                                   stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P1.50, filename = "plot.logit.CGDD.P1.50.png")
# 
# #---------------------------------------------------------------------------------------------
# #P1_B
# #no threshold
# final.df.GF.P1B <- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# # final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$temp.Mean, final.df.GF.P1$CGDD),]
# 
# #temp.Mean P1                 
# logit.temp.Mean.P1B <- glm(as.factor(final.df.GF.P1B$PA_Goldfish) ~ final.df.GF.P1B$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P1)
# 
# plot.logit.temp.Mean.P1B <- ggplot(final.df.GF.P1B, aes(temp.Mean, as.numeric(PA_Goldfish))) + 
#                               geom_point(shape = 1, size = 4) + 
#                               stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P1B, filename = "plot.logit.temp.Mean.P1B.png")
# 
# #CGDD P1                 
# logit.CGDD.P1B <- glm(as.factor(final.df.GF.P1B$PA_Goldfish) ~ final.df.GF.P1B$CGDD, family = 'binomial')
# summary(logit.CGDD.P1)
# 
# plot.logit.CGDD.P1B <- ggplot(final.df.GF.P1B, aes(CGDD, as.numeric(PA_Goldfish))) + 
#                           geom_point(shape = 1, size = 4) + 
#                           stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P1B, filename = "plot.logit.CGDD.P1B.png")
# 
# #---------------------------------------------------------------------------------------------
# #P1_B
# #50 threshold
# final.df.GF.P1B.50<- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# # final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$temp.Mean, final.df.GF.P1$CGDD),]
# 
# #temp.Mean P1                 
# logit.temp.Mean.P1B.50 <- glm(as.factor(final.df.GF.P1B$PA50_Goldfish) ~ final.df.GF.P1B$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P1.50)
# 
# plot.logit.temp.Mean.P1B.50 <- ggplot(final.df.GF.P1B.50, aes(temp.Mean, as.numeric(PA50_Goldfish))) + 
#                                 geom_point(shape = 1, size = 4) + 
#                                 stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P1B.50, filename = "plot.logit.temp.Mean.P1B.50.png")
# 
# #CGDD P1                 
# logit.CGDD.P1B.50 <- glm(as.factor(final.df.GF.P1B.50$PA50_Goldfish) ~ final.df.GF.P1B.50$CGDD, family = 'binomial')
# summary(logit.CGDD.P1.50)
# 
# plot.logit.CGDD.P1B.50 <- ggplot(final.df.GF.P1B.50, aes(CGDD, as.numeric(PA50_Goldfish))) + 
#                           geom_point(shape = 1, size = 4) + 
#                           stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P1B.50, filename = "plot.logit.CGDD.P1B.50.png")
# 
# #---------------------------------------------------------------------------------------------
# #P1_B
# #100 threshold
# final.df.GF.P1B.100<- final.df.GF[which(final.df.GF$Period_B == "P1"),]
# # final.df.GF.P1 <- final.df.GF.P1[complete.cases(final.df.GF.P1$temp.Mean, final.df.GF.P1$CGDD),]
# 
# #temp.Mean P1                 
# logit.temp.Mean.P1B.100 <- glm(as.factor(final.df.GF.P1B$PA100_Goldfish) ~ final.df.GF.P1B$temp.Mean, family = 'binomial')
# summary(logit.temp.Mean.P1B.100)
# 
# plot.logit.temp.Mean.P1B.100 <- ggplot(final.df.GF.P1B.100, aes(temp.Mean, as.numeric(PA100_Goldfish))) + 
#   geom_point(shape = 1, size = 4) + 
#   stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.temp.Mean.P1B.100, filename = "plot.logit.temp.Mean.P1B.100.png")
# 
# #CGDD P1                 
# logit.CGDD.P1B.100 <- glm(as.factor(final.df.GF.P1B.100$PA100_Goldfish) ~ final.df.GF.P1B.100$CGDD, family = 'binomial')
# summary(logit.CGDD.P1B.100)
# 
# plot.logit.CGDD.P1B.100 <- ggplot(final.df.GF.P1B.100, aes(CGDD, as.numeric(PA100_Goldfish))) + 
#   geom_point(shape = 1, size = 4) + 
#   stat_smooth(method = "glm", method.args = list(family="binomial"), se = FALSE)
# ggsave(plot.logit.CGDD.P1B.100, filename = "plot.logit.CGDD.P1B.100.png")
# 


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
boxplot(final.df.GF$nOverDays ~ final.df.GF$Month,
        xlab = "Month", ylab = "Capture Rate (n / days since last pull)")
dev.off()

png("plot.NOverDays.CGDD.png")
plot(final.df.GF$nOverDays ~ final.df.GF$CGDD,
     xlab = "CGDD", ylab =  "Capture Rate (n / days since last pull)")
dev.off()

library(sm)
library(ggplot2)

#plot temperature trends
png("MeanTemp.png")
plot(final.df.GF$temp.Mean[final.df.GF$Year >2014] ~ final.df.GF$Date[final.df.GF$Year >2014],
     xlab = "Year", ylab = "Mean Daily Temperature (C)")
dev.off()

boxplot(final.df.GF$temp.Mean ~ final.df.GF$Year)

Temp_PA <- ggplot(final.df.GF, aes(x = temp.Mean, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)
# GDD_PA <- ggplot(final.df.GF, aes(x = GDD, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)
CGDD_PA <- ggplot(final.df.GF, aes(x = CGDD, fill = as.factor(PA_Goldfish))) + geom_density(alpha = 0.4)

plot( final.df.GF$nOverDays ~ final.df.GF$temp.Mean)

ggsave(Temp_PA, filename = "Temp_PA.png")
ggsave(GDD_PA, filename = "GDD_PA.png")
ggsave(CGDD_PA, filename = "CGDD_PA.png")

ggplot(final.df.GF, 
       aes(x = temp.Mean, fill = as.factor(PA_Goldfish))) + geom_density(aes(fill = Year,alpha = 0.4))


png('Date_GDD.png')
plot(fishwayTemp2$Date, fishwayTemp2$deltaGDD)
dev.off()


#CGDD vs Daily temp plot
uTemp.CGDD.NOD <- ggplot(data= final.df.GF,
                         aes(CGDD, temp.Mean)) + 
                        geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                        scale_colour_gradient2(low = "black", 
                                               mid = "blue",   
                                               high = "red",
                                               midpoint = mean(final.df.GF$nOverDays, na.rm = T))

ggsave(uTemp.CGDD.NOD, filename = "uTemp.CGDD.NOD.png", dpi = 600)

#JDay vs Daily temp plot
uTemp.JDay.NOD <- ggplot(data= final.df.GF,
                         aes(JDay, temp.Mean)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(final.df.GF$nOverDays, na.rm = T))

ggsave(uTemp.JDay.NOD, filename = "uTemp.JDay.NOD.png", dpi = 600)




uTemp.CGDD.NOD.E <- ggplot(data= final.df.GF,
                            aes(CGDD, temp.Mean)) + 
                                geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                                scale_colour_gradient2(low = "black", 
                                                       mid = "blue",   
                                                       high = "red",
                                                       midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
                                stat_density2d(colour = 'black') + 
                            ylab("Mean Daily Temperature")

ggsave(uTemp.CGDD.NOD.E, filename = "uTemp.CGDD.NOD.E.png", dpi = 600)

uTemp.JDay.NOD.E <- ggplot(data= final.df.GF,
                           aes(JDay, temp.Mean)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black') + 
  ylab("Mean Daily Temperature")

ggsave(uTemp.JDay.NOD.E, filename = "uTemp.JDay.NOD.E.png", dpi = 600)




uTemp.CGDD500.NOD.E <- ggplot(data= final.df.GF,
                                                   aes(CGDD, temp.Mean)) + 
                          geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                          scale_colour_gradient2(low = "black", 
                                                 mid = "blue",   
                                                 high = "red",
                                                 midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
                          stat_density2d(colour = 'black') + 
                          ylab("Mean Daily Temperature") +
                          xlim(0,500)
                        
                        ggsave(uTemp.CGDD500.NOD.E, filename = "uTemp.CGDD500.NOD.E.png", dpi = 600)


                        
uTemp.JDay200.NOD.E <- ggplot(data= final.df.GF,
                            aes(JDay, temp.Mean)) + 
                        geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                        scale_colour_gradient2(low = "black", 
                                               mid = "blue",   
                                               high = "red",
                                               midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
                        stat_density2d(colour = 'black') + 
                        ylab("Mean Daily Temperature") +
                        xlim(50,200)

ggsave(uTemp.JDay200.NOD.E, filename = "uTemp.JDay200.NOD.E.png", dpi = 600)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


#JDay vs Daily temp plot
CGDD.JDay.NOD <- ggplot(data= final.df.GF,
                         aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(final.df.GF$nOverDays, na.rm = T))
ggsave(CGDD.JDay.NOD, filename = "CGDD.JDay.NOD.png", dpi = 600)


CGDD.JDay200.NOD.E <- ggplot(data= final.df.GF,
                              aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(final.df.GF$nOverDays, na.rm = T)) + 
  # stat_density2d(colour = 'black') + 
  ylab("CGDD") +
  xlim(50,200)

ggsave(CGDD.JDay200.NOD.E, filename = "CGDD.JDay200.NOD.E.png", dpi = 600)


temp.data <- final.df.GF[final.df.GF$Year >= 2016 & final.df.GF$Year <=  2018]
CGDD.JDay2016_2018.NOD.E <- ggplot(data= temp.data,
                             aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black') +
  ylab("CGDD") +
  xlim(50,200) +
  ylim(0,1500)

ggsave(CGDD.JDay2016_2018.NOD.E, filename = "CGDD.JDay2016_2018.NOD.E.png", dpi = 600)
# ------------------------------------------------------------------------------
temp.data <- final.df.GF[final.df.GF$Year == 2016]
CGDD.JDay2016.NOD.E <- ggplot(data= temp.data,
                                   aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black') +
  ylab("CGDD") +
  xlim(50,200) +
  ylim(0,1500)

ggsave(CGDD.JDay2016.NOD.E, filename = "CGDD.JDay2016.NOD.E.png", dpi = 600)

temp.data <- final.df.GF[final.df.GF$Year == 2017]
CGDD.JDay2017.NOD.E <- ggplot(data= temp.data,
                              aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black') +
  ylab("CGDD") +
  xlim(50,200) +
  ylim(0,1500)

ggsave(CGDD.JDay2017.NOD.E, filename = "CGDD.JDay2017.NOD.E.png", dpi = 600)

temp.data <- final.df.GF[final.df.GF$Year == 2018]
CGDD.JDay2018.NOD.E <- ggplot(data= temp.data,
                              aes(JDay, CGDD)) + 
  geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
  scale_colour_gradient2(low = "black", 
                         mid = "blue",   
                         high = "red",
                         midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
  stat_density2d(colour = 'black') +
  ylab("CGDD") +
  xlim(50,200) +
  ylim(0,1500)

ggsave(CGDD.JDay2018.NOD.E, filename = "CGDD.JDay2018.NOD.E.png", dpi = 600)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
temp.data <- final.df.GF[final.df.GF$Year >= 2015,]

CGDD_Year_nOverDays <- ggplot(temp.data,
                       aes(x = CGDD, y = as.factor(Year))) + 
                       geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                       scale_colour_gradient2(low = "black", 
                                         mid = "blue",   
                                         high = "red",
                                         midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
                        stat_density2d(colour = 'black') + 
                        ylab("Year")

ggsave(CGDD_Year_nOverDays, filename = "CGDD_Year_nOverDays.png")



#Year by mean Daily temperature by nOverDays
TempMean_Year_nOverDays <- ggplot(temp.data,
                                 aes(x = temp.Mean, y = as.factor(Year))) + 
                            geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                            scale_colour_gradient2(low = "black", 
                                                   mid = "blue",   
                                                   high = "red",
                                                   midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
                            stat_density2d(colour = 'black') + 
                            ylab("Year")+ 
                            xlab("Mean Daily Temperature")

ggsave(TempMean_Year_nOverDays, filename = "TempMean_Year_nOverDays.png")

#Year by CGDD by n_count
CGDD_Year_n_count <- ggplot(temp.data,
                                 aes(x = CGDD, y = as.factor(Year))) + 
                            geom_point(aes(color = n_count, size = n_count),alpha = 0.75) +
                            scale_colour_gradient2(low = "black", 
                                                   mid = "blue",   
                                                   high = "red",
                                                   midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
                            stat_density2d(colour = 'black') + 
                            ylab("Year")
ggsave(CGDD_Year_n_count, filename = "CGDD_Year_n_count.png")
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#Year by CGDD by n_count
temp.data <- final.df.GF[final.df.GF$Year >= 2015,]

CGDD500_Year_n_count <- ggplot(temp.data,
                             aes(x = CGDD, y = as.factor(Year))) + 
                        geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                        scale_colour_gradient2(low = "black", 
                                               mid = "blue",   
                                               high = "red",
                                               midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
                        stat_density2d(colour = 'black') + 
                        xlim(0,500) + 
                        ylab("Year")
ggsave(CGDD500_Year_n_count, filename = "CGDD500_Year_n_count.png")


TempMean10.20_Year_nOverDays <- ggplot(temp.data,
                                     aes(x = temp.Mean, y = as.factor(Year))) + 
                                geom_point(aes(color = nOverDays, size = nOverDays),alpha = 0.75) +
                                scale_colour_gradient2(low = "black", 
                                                       mid = "blue",   
                                                       high = "red",
                                                       midpoint = mean(temp.data$nOverDays, na.rm = T)) + 
                                stat_density2d(colour = 'black') + 
                                xlim(10,20) + 
                                ylab("Year") +
                                xlab("Mean Daily Temperature")
ggsave(TempMean10.20_Year_nOverDays, filename = "TempMean10.20_Year_nOverDays.png")



Month_CGDD <- ggplot(temp.data,aes(temp.data$CGDD, temp.data$Month)) + 
              geom_point(aes(color = Year, alpha = 0.5), size = 5, shape  = 15) +
              ylim(0,12)
ggsave(Month_CGDD, filename = "Month_CGDD.png")

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#density by year CGDD ~ PA 2016-2018
temp.data <- final.df.GF[final.df.GF$PA_Goldfish ==  1 & final.df.GF$Year >= 2016 & final.df.GF$Year <=  2018]

GoldfishPA_Year_CGDD <- ggplot(temp.data, aes(x = CGDD, y = as.factor(Year), fill = as.factor(Year))) + 
                        geom_density_ridges() +
                        ylab("Year") + 
                        xlim(0,3000)

ggsave(GoldfishPA_Year_CGDD, filename = "GoldfishPA_Year_CGDD.png")

#density by year JDAy
temp.data <- final.df.GF[final.df.GF$PA_Goldfish ==  1 & final.df.GF$Year >= 2015]

GoldfishPA_Year_JDay <- ggplot(temp.data, aes(x = JDay, y = as.factor(Year), fill = as.factor(Year))) + 
  geom_density_ridges() +
  ylab("Year") + 
  xlim(0,370)

ggsave(GoldfishPA_Year_JDay, filename = "GoldfishPA_Year_JDay.png")


#density by year JDAy 
temp.data <- final.df.GF[final.df.GF$PA_Goldfish ==  1 & final.df.GF$Year >= 2016 & final.df.GF$Year <=  2018 & final.df.GF$CGDD <= 250,]
temp.data <- temp.data[complete.cases(temp.data$PA_Goldfish),]

GoldfishPA_Year_CGDD300 <- ggplot(temp.data, aes(x = CGDD, y = as.factor(Year), fill = as.factor(Year))) + 
                           geom_density_ridges()  +
                           ylab("Year") +
                           xlim(0,400)

ggsave(GoldfishPA_Year_CGDD300, filename = "GoldfishPA_Year_CGDD300.png")


#density by year JDAy limite to Days 50 to 200 
temp.data <- final.df.GF[final.df.GF$PA_Goldfish ==  1 & final.df.GF$Year >= 2015 & final.df.GF$JDay <= 200 ]
temp.data <- temp.data[complete.cases(temp.data$PA_Goldfish),]

GoldfishPA_Year_JDay200 <- ggplot(temp.data, aes(x = JDay, y = as.factor(Year), fill = as.factor(Year))) + 
  geom_density_ridges()  +
  ylab("Year") +
  xlim(0,200)

ggsave(GoldfishPA_Year_JDay200, filename = "GoldfishPA_Year_JDay200.png")




library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

temp.data <-  final.df.GF[final.df.GF$PA_Goldfish ==  1 & final.df.GF$Year >= 2015,]
ggplot(temp.data, aes(x = CGDD, y = as.factor(Month)), fill = as.factor(Month)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height= 0.01)  + 
    theme_ridges()


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


