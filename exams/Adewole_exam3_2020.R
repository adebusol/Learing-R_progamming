#Introduction to 'R' Programming
#Fall 2020
#Exam #3

#data set: "ost2014_phy_t.Robj"
library(scales)
library(dplyr)
library(tidyverse)
library(plyr)
#section 1 ---- 5 points

#subset data frame to include only the following fields:
# "cruise"      "transect.id" "haul"        "area"        "tow"         "region"      "dateTime"  "lat" "lon"  "depth"
# "temp"        "salinity"    "pressure"    "sw.density"  "irradiance"

load("test3_data_2020/ost2014_phy_t.Robj")
names(phy_t)
phyt_1 <-phy_t[,c(1:12,15,20,21)]
#sort the data by transect.id (e.g. "OST14-1W-S") and then for each by time so that the last observation is the last one in time
 t<- arrange(phyt_1, transect.id, dateTime)
 
 
 
 
# Section 2 : 10 points ----

  # Plot path the instrument moved through the water for a shallow, mid-depth, and deep tows, using data from the same station (i.e, OST14-1W)
  # Use points; Make the points hollow, outlined dark blue
  # Use 'dateTime' as your x-axis values. Set the x-axis tick interval to be 15 minutes with the label hour:minute
  # Label the plot with the name of the station as the title
  # Add a smoother to the plot
 
 unique(t$tow)
 unique(t$transect.id)
 #used the "OST14-1E" station
 p<- t[t$transect.id==c("OST14-1E-D","OST14-1E-M", "OST14-1E-S"),]
 unique(p$transect.id)
 unique(p$tow)
  
 p1 <- ggplot(data = p, aes(x=dateTime, y= depth))+
   geom_point(color = "dark blue", size = 0.1 )+
   scale_x_datetime(name = "Time", labels = date_format("%H:%M"), date_breaks("15 min"))+
   ggtitle(label = "OST14-1E")+
   geom_smooth(method = "lm")
  p1 

 

#Section 3 : 10 points ----

#You can see from the transect.id values in the data set that there were four station types: Spatial (i.e, transects with W,E,C); Lagrangian (L); Eddy; and DVM
  #Assign the correct 'station' type to each observation using (a) 'ddply' function and (b) apply' function
  #Hint: you may need to create a custom function
  #BONUS!! 10 points if you can also do the same work using a 'for loop' and 'apply' function.
unique(t$transect.id)
  t$stationtype<-NA
  #custom function
  Observ_func <- function(x){
    a<- str_split_fixed( string = x[['transect.id']], pattern = "-", n= 2)
    b<-a[2]
    if(str_detect(string = b, pattern = "E" )){
      stationtype = "Spatial"
     } else if(str_detect(string = b, pattern = "W" )){
        stationtype = "Spatial"
      }else  if(str_detect(string = b, pattern = "C" )){
          stationtype = "Spatial"
      }else  if(str_detect(string = b, pattern = "L" )){
        stationtype = "Lagrangian"
      }else  if(str_detect(string = b, pattern = fixed("Eddy"))){
        stationtype = "Eddy"
      }else  if(str_detect(string = b, pattern = fixed("DVM"))){
        stationtype = "DVM"
        
      } else{}
    return(stationtype)
  }
  
#using apply #worked perfectly
  t$stationtype <- apply(X = t, 1, FUN = Observ_func)
 unique(t$stationtype) 

 #using #ddply
  x<- t[t$transect.id =="OST14-1E-D",]
  
   t3 <- ddply(.data=t, .variable = c("transect.id"),  .fun = function(x){
    a<- str_split_fixed( string =x[['transect.id']] , pattern = "-", n= 2)
    b<-a[2]
    stationtype <- t$stationtype
    if(str_detect(string = b, pattern = "E" )){
      stationtype = "Spatial"
    } else if(str_detect(string = b, pattern = "W" )){
      stationtype = "Spatial"
    }else  if(str_detect(string = b, pattern = "C" )){
      stationtype = "Spatial"
    }else  if(str_detect(string = b, pattern = "L" )){
      stationtype = "Lagrangian"
    }else  if(str_detect(string = b, pattern = fixed("Eddy"))){
      stationtype = "Eddy"
    }else  if(str_detect(string = b, pattern = fixed("DVM"))){
      stationtype = "DVM"
      
    } else{}
    dt<-data.frame(stationtype)
    return(dt)
  }, .progress = "text", .inform = T)
    
#using for loop #couldn't get this to run properly
    for(i in 1:length(t)){
    t[i,]$stationtype <- Observ_func(x= t[i,]) 
    }
   
   
# Section 4 : 5 points -------

# Generate histograms of pressure values for each region of the 'Spatial' study,
# separate the three region-specific plots by faceting.
 sp<-t[t$stationtype=="Spatial",]
 
    y<- ggplot(data = sp, aes(pressure))+
    geom_histogram()+
    facet_wrap(~region)
 y

#section 5 : 10 points -----

#Create box-whisker plot for water temperature for the undulation tows (i.e, 'und' in the transect name) for each of the
#western, central, and eastern regions of the 'Spatial' study.
 wt<- sp[sp$tow== "und",]
 wt<-sp[sp$region != "sof",]
 bw <-ggplot(wt,aes(x= factor(region), y=temp))+
   geom_boxplot()
 bw

#Section 6: 5 points

# Using a bar-plot,compare the temperature in the shallow versus deep tows. #by regions
# in three separate units: Celsius, Fahrenheit, and Kelvin. one bar will be Celsius, another Fahrenheit, etc.
# Plot the values on a log-10 scale.
 detach("package:plyr")
 sha<- t[t$tow =='d',]
 dee<- t[t$tow =='s',]
 
 
#celcius to kelvin
C2K<- function(x){
  y<-(x + 273)
}
sha$temp_k <- NA
dee$temp_k<- NA
sha$temp_k<- C2K(sha$temp)
dee$temp_k <- C2K(dee$temp)

#celcius to fahrenheits
C2F<-function(x){
  y<- (x * 9/5) +32
}
sha$temp_f<-NA
dee$temp_f<-NA
sha$temp_f<-C2F(sha$temp)
dee$temp_f <- C2F(dee$temp)


#meantemps
t.stats_sha<-sha%>%group_by(region)%>%summarise(mean.temp = mean(temp), mean.temp.k = mean(temp_k), mean.temp.f = mean(temp_f))
t.stats_dee<-dee%>%group_by(region)%>%summarise(mean.temp = mean(temp),mean.temp.f = mean(temp_f),mean.temp.k = mean(temp_k))



sha.gather <- gather(data = t.stats_sha, key= variable,value = mean.temp, -region)
dee.gather <- gather(data = t.stats_dee, key= variable,value = mean.temp, -region)


#bar graph
br<-ggplot(sha.gather, aes(x=region, y=mean.temp, fill= variable)) +
  geom_bar(stat = "identity", position = "dodge")+
   ggtitle("Mean Temperature in the Shallow Tow")+ 
   scale_y_log10()

br

br1<-ggplot(dee.gather, aes(x=region, y=mean.temp, fill= variable)) +
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Mean Temperature in the Deep Tow")+
  scale_y_log10()
br1


#section 7: 8 points
#Create a map showing the location of the FOCAL sites (see data provided with exam)
 mp <- t[,c("transect.id","lat","lon","dateTime")]
 library("plyr")
 x<- mp[mp$transect.id =="OST14-1E-D",]
 mp1 <- ddply(.data= mp, .variables = c("transect.id"), function(x){
   x <- x[complete.cases(x),]
   x<- arrange(x, dateTime)
   start.lat <- x[1,]$lat
   start.lon <- x[1,]$lon
   
   end.lat <- x[nrow(x),]$lat
   end.lon <- x[nrow(x),]$lon
   
   df = data.frame(start.lat, start.lon, end.lat, end.lon)
   return(df)
   
 }, .progress = "text", .inform = T)
 
 
 library("rgdal")
 library("ggmap")
 
 f1 <- readOGR(dsn= "test3_data_2020/", layer = "GulfCoastStates_Islands")
 f1 <- spTransform(f1, CRS("+proj=longlat + datum=WGS84"))
 f1 <- fortify(f1)
 
 
 install.packages("mapproj")

 
 mpp <- ggplot() + geom_polygon(data = f1, aes(x=long, y=lat, group = group), 
                              colour = "black", fill="grey70")+
   geom_segment(data = mp1,  aes(x= start.lon, y = start.lat, xend=end.lon, yend= end.lat),
                color= "blue", size=1)+
  coord_map(xlim = c(-82, -80), ylim = c(24,26.5))+
   scale_y_continuous(name = 'degrees N', breaks = seq(24,26.5,0.5))+
   scale_x_continuous(name ='degrees N', breaks = seq(-82,-80,0.5))+
   theme(axis.text.x = element_text(size = 10, color = "black"),
         axis.ticks.y = element_line(size = 0.8, color = "black"),
         axis.ticks.x = element_line(size = 0.8, color = "black"),
         axis.ticks.length = unit(0.1, "cm"),
         panel.background = element_rect(fill='white', size=1, color = "black"),
         panel.grid = element_blank())
mpp

