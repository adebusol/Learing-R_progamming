#Exercise #8: creating graphs in 'R' using 'ggplot'

#Section 1: process_data ---- 10 POINTS (5 points for each data set)
library(tidyverse)
library(plyr)
#1) read in the text files "NDBC_8764314_Eugene Island_Zeta.txt" & "NDBC_8762484_Frenier Landing_Zeta" - (2 POINT)
# Hint: you may need to skip some lines

#FYI: data sources
#Eugene Island 29.373 N 91.384 W
browseURL("https://www.ndbc.noaa.gov/station_page.php?station=einl1")

#Frenier Landing 30.106 N 90.422 W
browseURL("https://www.ndbc.noaa.gov/station_page.php?station=frel1")
txt1 <- read.table(file = "NDBC_8762484_Frenier Landing_Zeta.txt", stringsAsFactors = F)
head(txt1)
txt2 <- read.table(file = "NDBC_8764314_Eugene Island_Zeta.txt", stringsAsFactors = F)
#2) assign the column names to each data set - (2 POINT)
#Hint: you may need to copy the headers from the text file
names(txt1) <- c("YY", "MM", "DD",  "hh", "mm", 
                 "WDIR", "WSPD", "GST", "WVHT", "DPD",
                 "APD", "MWD", "PRES",  "ATMP",  "WTMP",  
                 "DEWP",  "VIS", "PTDY",  "TIDE")
 names(txt1)                          
head(txt1)
names(txt2) <- c("YY", "MM", "DD",  "hh", "mm", 
                 "WDIR", "WSPD", "GST", "WVHT", "DPD",
                 "APD", "MWD", "PRES",  "ATMP",  "WTMP",  
                 "DEWP",  "VIS", "PTDY",  "TIDE")
head(txt2)
#3) remove the fields with no data (i.e., those with all "MM" in all rows) - (1 POINT)
txt1_new<- txt1[,c(1:8,13,14)]
txt2_new <- txt2[,c(1:8,13,14)]

txt1_new <-txt1_new[ txt1$WSPD!="MM",]
txt1_new<-txt1_new[txt1$GST!="MM",]
txt1_new<- txt1_new[txt1$PRES!="MM",]
txt1_new<- txt1_new[txt1$ATMP!="MM",]

txt2_new <-txt2_new[ txt2$WSPD!="MM",]
txt2_new<-txt2_new[txt2$GST!="MM",]
txt2_new<- txt2_new[txt2$PRES!="MM",]
txt2_new<- txt2_new[txt2$ATMP!="MM",]

#4) use the 'lapply' function to go through both data frames and change the columns with data classed as factors to be numbers.
# To not lose inforamtion, this conversion is a 2-step process requiring first conversion
# from factor to character, and then character to number. Look to the code provided in exercise 'e7_merging_data_sets" for a
# hint on how to do this efficiently using the 'lapply' function - (5 POINTS)
str(txt1_new)
str(txt2_new)
#NOTE: if you wanted to be a even more efficient; you could create a custom function to use in the 'lapply' function
# to reduce the number of steps from 2 (i.e., step1: as.character; step2: as.numeric) to 1 (i.e., fac2num function)
txt1_new[6:10] <- lapply(X = txt1_new[6:10], FUN = as.numeric)
txt2_new[6:10] <- lapply(X = txt2_new[6:10], FUN = as.numeric)

#custom function
fac2num <- function(x){as.numeric(levels(x))[x]}


#5) create a single year-month-day hour:minute field in the data frame that has the date-time class - (1 POINT)

date <- paste(txt1_new$YY, txt1_new$MM, txt1_new$DD, sep="-") 
time <- paste(txt1_new$hh, txt1_new$mm, sep =":")
txt1_new$date_time<- paste(date, time, sep = " ")
txt1_new$date_time <- as.POSIXct(strptime(txt1_new$date_time, format = "%Y-%m-%d %H:%M", tz="America/Chicago"))
str(txt1_new$date_time)

date2 <- paste(txt2_new$YY, txt2_new$MM, txt2_new$DD, sep="-") 
time2 <- paste(txt2_new$hh, txt2_new$mm, sep =":")
txt2_new$date_time<- paste(date2, time2, sep = " ")
txt2_new$date_time <- as.POSIXct(strptime(txt2_new$date_time, format = "%Y-%m-%d %H:%M", tz="America/Chicago"))
#6) subset the data after 10-25-2020 in each data frame.
# Hint: you will need to use this syntax "2020-10-25 00:00" when subsetting the data - (1 POINT)
af1 <- txt1_new[txt1_new$date_time >"2020-10-25 00:00",]
af2 <- txt2_new[txt2_new$date_time >"2020-10-25 00:00",]


#Section 2: create plots ----

#5) Create a scatter plot using two continous variable fields (1 POINT)
p<- ggplot(data = txt1_new, aes(x =hh, y= PRES)) +
  geom_point() 
  
p
#a) change the color of the points to a shade of orange (0.5 POINT)
p<- ggplot(data = txt1_new, aes(x =hh, y= PRES)) +
  geom_point(color = "darkorange")
 p
#b) change the size of the points to 3 (0.5 POINT)
p<- ggplot(data = txt1_new, aes(x =hh, y= PRES)) +
  geom_point(size = 3, color = "darkorange")
p
#c) add a trendline; is there any relationship between the two continous variables? (0.5 POINT)
p<- ggplot(data = txt1_new, aes(x =hh, y= PRES)) +
  geom_point(color = "darkorange")+
  geom_smooth(method = "lm")
p

#6) create a line plot showing WSPD (y) and PRES (x) fields using data from Eugene Island (1 POINT)
 k<- ggplot(data = txt1_new, aes(x= PRES, y= WSPD))+
     geom_line()
     
#a) change the color of the line to a shade of blue (0.5 POINT)
 k<- ggplot(data = txt1_new, aes(x= PRES, y= WSPD))+
  geom_line(color = "steelblue3")
 k
#b) change the width of the line to 2 (0.5 POINT)
 k<- ggplot(data = txt1_new, aes(x= PRES, y= WSPD))+
   geom_line(size = 2, color = "steelblue3")
 k
#c) change the y-axis break points to be every 2 (e.g., 2, 4, 6, 8 etc.);
# change the x-axis break points to be every 5 (e.g., 1000, 1005, 1010, etc.) - (0.5 POINT)
 k<- ggplot(data = txt1_new, aes(x= PRES, y= WSPD))+
   geom_line(size = 2, color = "steelblue3")+
 scale_x_continuous(name= "Pressure", breaks = seq(0,1030,5))+
   scale_y_continuous(name = "Wind speed", breaks = seq(0,26,2))
k

#7) create a line plot with PRES (y) and date-time field (x). Plot both the Eugene Island and
# Frenier Landing on the same plot. Make the lines different colors - (2 POINTS)

t = ggplot() + 
  geom_line(data = txt1_new, aes(x = date_time , y = PRES), color = "blue") +
  geom_line(data = txt2_new, aes(x = date_time , y = PRES), color = "red") 
  
t
#a) Make the lines different colors - (1 POINT)

#b) what does this plot tell you about when Hurricane Zeta arrived at each station? - (0.5 POINT)


#8) compare the wind gust (GST) observations between Eugene Island and Frenier Landing using box-and-whisker plot.

#a) you may need to (1) first assign a station name (e.g. "Frenier Landing') to each data set and
# (2) then combine the rows of GST data into a new, single data frame object before plotting - (2 POINTS)

fren <- txt1_new
eug <- txt2_new

fren$group <- "fren"
eug$group <- "eug"
f_e <- rbind(fren,eug)
bw <-ggplot(f_e,aes(x= factor(group), y=GST))+geom_boxplot()
bw

#b) using a 'theme' geom, remove the 'grey' background color from the panel and add a black line around panel (1 POINT)
bw.theme <- theme(strip.background = element_blank(),
                 strip.text = element_text(family = "Arial", size = 10, face = "bold"),
                 panel.background = element_rect(colour = "black", fill = NULL),
                 panel.spacing = unit(0.5, units = "lines"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.background = element_blank(),
                 legend.text = element_text(size=10),
                 legend.key.height = unit(0.75,"cm"),
                 legend.key.width = unit(0.75,"cm"),
                 axis.ticks.length = unit(0.1,"cm"),
                 axis.ticks.y = element_line(size=1),
                 axis.ticks.x = element_line(size=1),
                 axis.title.x = element_text(face = "bold", size=12),
                 axis.title.y = element_text(face = "bold", size=12),
                 plot.margin = unit(c(1,1,1,1),units = "cm"))
bw <-ggplot(f_e,aes(x= factor(group), y=GST))+geom_boxplot()+
   
  bw.theme
bw
  
#c) apply a different color to each station's boxplot (1 POINT)
bw <-ggplot(f_e,aes(x= factor(group), y=GST))+geom_boxplot(aes(fill=factor(group)))+
  bw.theme+
  scale_y_log10(name="wind gust") 
  bw
#d) change the y-axis scale so that the break-points are every 2 (e.g., 0, 2, 4, 6 etc.) (1 POINT)
  bw <-ggplot(f_e,aes(x= factor(group), y=GST))+geom_boxplot(aes(fill=factor(group)))+
    bw.theme+
    scale_y_log10(name="wind gust") 
  
  bw
#e) using a 'theme' geom, make the 'face' of the y-axis and x-axis titles be bold and a font size = 12 point (1 POINT)
  bw <-ggplot(f_e,aes(x= factor(group), y=GST))+geom_boxplot(aes(fill=factor(group)))+
    bw.theme+
    scale_y_log10(name="wind gust") 

#9) using the "batting.2008" data set, create a bar graph showing the mean number of home runs for all teams in the National League (4 POINTS)
#extra point: order the teams on the x-axis from highest to lowest mean Home Runs (left to right) + 1 POINT
load("test2_data_2020/batting.2008.Rdata")
detach("package:plyr")
NL<- d[d$lgID=="NL",]
hr.stats <- NL %>% group_by(teamID) %>% summarise(mean.hr = mean(HR))

b <- ggplot(data = hr.stats, aes(x = reorder(teamID, mean.hr), y= mean.hr))+
  geom_bar(stat = "identity")
b



