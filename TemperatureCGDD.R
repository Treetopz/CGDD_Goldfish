#

library(data.table)
library(lubridate)
library(tidyverse)

setwd("K:/Today23/Goldfish CGDD/Data/DOT for Christine/Binds_For_CGDD")

df <- fread("df.final.csv")

df$Date.Time <- mdy_hm(df$Date.Time)
df$Date.Time.UTC <- mdy_hm(df$Date.Time.UTC)
df <- df[,c(2:8)]
df$Use <- as.logical(df$Use)

#filter data 
dfCGDD <- df
dfCGDD <- dfCGDD[dfCGDD$Use == T,]

#Add extra information
dfCGDD$Date <- date(dfCGDD$Date.Time)
dfCGDD$Month <- month(dfCGDD$Date.Time)
dfCGDD$Year <- year(dfCGDD$Date.Time)

summaryCGDD <- dfCGDD %>% group_by(Site, Date, Year) %>% 
                        summarise(temp.Mean = mean(Temp, na.rm = T), 
                                  temp.Max = max(Temp, na.rm = T),
                                  temp.Min = min(Temp, na.rm = T), 
                                  temp.N = length(Temp),
                                  temp.SD = sd(Temp, na.rm = T))


#calculate CGDD 
summaryCGDD$deltaGDD <- summaryCGDD$temp.Mean - 5

#reset any negative values to 0
summaryCGDD$deltaGDD[summaryCGDD$deltaGDD <= 0] <- 0

#calculate cumulative sum by year - CGDD
summaryCGDD$Year <- year(summaryCGDD$Date)

summaryCGDD2 <- split(summaryCGDD, list(summaryCGDD$Year, summaryCGDD$Site))


#search through list 
#needto fix this line
for (i in 1: length(summaryCGDD2)){
  
  summaryCGDD2[[i]]$CGDD <- cumsum(summaryCGDD2[[i]]$deltaGDD)
  
}

summaryCGDD3 <- data.frame()

#unlist and combine
for (i in 1:length(summaryCGDD2)){
  
  binder <- data.frame(summaryCGDD2[[i]])
  
  summaryCGDD3 <- rbind(summaryCGDD3, binder)
  
}

ggplot(summaryCGDD3, aes(Date, CGDD)) + geom_line(aes(colour = Site))
