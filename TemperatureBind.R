#this script is for binding temperautre data for the CGDD Goldfish project


#load in libraries
library(data.table)
library(dplyr)
library(lubridate)

#source in functions
wd <- "K:/Today23/Goldfish CGDD/R-scripts/CGDD_Goldfish"
setwd(wd)
source("multilubridate.R")

df.final <- data.frame()


#-------------------------------------------------------------------------------------
#set directory
wd <- "K:/Today23/Goldfish CGDD/Data/DOT for Christine/Binds_For_CGDD/Fall 2018/Temp"
setwd(wd)

#generate file list
fileList <- list.files(pattern = "*.csv")

#port in Fall 2018
for (i in 1:length(fileList)){
  
#draw data  
dfTemp <- fread(fileList[i])
colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")

#Format dates
dfTemp$Date.Time <- multilubridate(dfTemp$Date.Time, formats = c("mdy_hm"))
dfTemp$Date.Time.UTC <- multilubridate(dfTemp$Date.Time.UTC, formats = c("mdy_hm"))

write.csv(dfTemp[,2:7],file = fileList[i])
print(fileList[i])
}


fileList <- list.files(pattern = "*.csv")
#binding
for (j in 1:length(fileList)){
  dfTemp <- fread(fileList[j])
  colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")
  df.final <- rbind(df.final, dfTemp)
}

#----------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#set directory
wd <- "K:/Today23/Goldfish CGDD/Data/DOT for Christine/Binds_For_CGDD/Spring 2018/Temp"
setwd(wd)

#generate file list
fileList <- list.files(pattern = "*.csv")

#port in Fall 2018
for (i in 7:length(fileList)){
  
  #draw data  
  dfTemp <- fread(fileList[i])
  colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")
  
  #Format dates
  dfTemp$Date.Time <- multilubridate(dfTemp$Date.Time, formats = c("dmy_hm"))
  dfTemp$Date.Time.UTC <- multilubridate(dfTemp$Date.Time.UTC, formats = c("dmy_hm"))
  
  write.csv(dfTemp[,2:7],file = fileList[i])
  print(fileList[i])
}


fileList <- list.files(pattern = "*.csv")
#binding
for (j in 1:length(fileList)){
  dfTemp <- fread(fileList[j])
  colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")
  df.final <- rbind(df.final, dfTemp)
}

#----------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#set directory
wd <- "K:/Today23/Goldfish CGDD/Data/DOT for Christine/Binds_For_CGDD/Spring 2019/Temp"
setwd(wd)

#generate file list
fileList <- list.files(pattern = "*.csv")

#port in Fall 2018
for (i in 1:length(fileList)){
  
  #draw data  
  dfTemp <- fread(fileList[i])
  colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")
  
  #Format dates
  dfTemp$Date.Time <- multilubridate(dfTemp$Date.Time, formats = c("mdy_hm"))
  dfTemp$Date.Time.UTC <- multilubridate(dfTemp$Date.Time.UTC, formats = c("mdy_hm"))
  
  write.csv(dfTemp[,2:7],file = fileList[i])
  print(fileList[i])
}


fileList <- list.files(pattern = "*.csv")
#binding
for (j in 1:length(fileList)){
  dfTemp <- fread(fileList[j])
  colnames(dfTemp) <- c("Index","Date.Time","Date.Time.UTC","Temp","Use","Site","Serial")
  df.final <- rbind(df.final, dfTemp)
}

#----------------------------------------------------------------------------------

write.csv(df.final, "df.final.csv")
