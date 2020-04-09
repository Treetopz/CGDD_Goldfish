
#This script is for fixing dates with multiple formats in a column, generally when excel is messing things up
#formats are from lubridate instead of your standard R format
#will generate warnings when things failed to parse
#modified code from https://stackoverflow.com/questions/13764514/how-to-change-multiple-date-formats-in-same-column


multilubridate <- function(data, formats){
                  
                  #port in library in case it is not running
                  library(lubridate)
                  
                  #create empty list
                  a <- list()
                  
                  #check loop, apply date/time formats based on NAs until all formats are parsed
                  for(i in 1:length(formats)){
                    a[[i]] <- eval(parse(text = paste(formats[i],"(data)")))
                    a[[1]][!is.na( a[[i]])] <- a[[i]][!is.na(a[[i]])]
                    
                    }
  
  a[[1]]
  
}


#example
#generate data
# data <- c("2019-05-16 11:45", "16-05-2019 11:45:10", "05/12/2020 15:13")
# multidate(data = data, formats = c("ymd_hm","dmy_hms", "mdy_hm"))
