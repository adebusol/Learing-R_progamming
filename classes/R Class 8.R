library(tidyverse)
library(plyr)
#structure of batch processing
# take each file, read it in, process, read in the next one, etc., make a single data frame

#list all your ISIIS files in the 'e3_batch_data' directory (i.e., folder)
dfiles <- list.files(path = "e3_batch_data", full.name=T)

#'adply' function...from library"plyr" 
#For each slice of an array, apply function then combine results into a data frame.
t<- adply(.data = dfiles,.margins = 1, fun = function(x){
  
#read-in the data from each file
d <- read.table(file = x, header = T, sep = "\t", skip = 10, stringsAsFactors = F,
                fileEncoding="ISO-8859-1", check.names=FALSE, encoding = "UTF-8", quote = "\"")

head.d <- names(d)
head.d <- str_replace(string = head.d, pattern = "\\(.*\\)", replacement = "")
#example
#vel <- str_replace(string = head.d, pattern = "Velocity", replacement = "bike")
head.d <- tolower(head.d)
head.d <- str_trim(string = head.d)
head.d <- make.names(head.d)
head.d <- str_replace(string = head.d, pattern = fixed(".."), replacement = ".")
# assign names
names(d) <- head.d

#get a nice date Time format
date <- scan(x, what="character", skip=1, nlines=1, quiet=TRUE)
date <- date[2]
mm <- str_sub(string = date, start = 1, end = 2)
dd <- str_sub(string = date, start = 4, end = 5)
yr <- str_sub(string = date, start = 7, end = 8)

date_pretty <- str_c(paste0("20",yr), mm, dd, sep = "-")

#fix up the time
hr <- str_sub(string = d$time, 1, 2)
min <- str_sub(string = d$time, 4, 5)
sec <- str_sub(string = d$time, start = 7, end = 11)

time <- str_c(hr,min,sec, sep=":")
d$dateTime <- str_c(date_pretty, time, sep =  " ")

#d$dateTime <- as.POSIXct(strptime(x = d$dateTime, format = "%m/%d/%y %H:%M:%OS", tz="America/New_York"))
d$dateTime <- as.POSIXct(strptime(x=d$dateTime, format= "%Y-%m-%d %H:%M:%OS",
                                  tz = "America/New_York"))

date <- NULL

#assign file name to be a new column in data frame 'd'
d$transect <- basename(x)
d$transect <- gsub(pattern = ".txt",replacement = "",x = d$transect)

return(d)
}, .progress= "text", .inform =T)




