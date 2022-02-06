#Plotting a map in R
#goal : create a map shwoing transect lines off the coast of florida

#step1 : load the data and cleana it up a bit

load("ost2014_phy_t.Robj")
d1<- phy_t
load("ost2015_phy_t.Robj")
d2 <- phy_t

head(d1)
unique(d1$tow)
#exclude undulation tows
d1 <- d1[d1$tow != 'und',]

#to make a map we really need the latitude and longitude coordinate to draw points, a line or a polygon
d1 <- d1[,c("transect.id","lat","lon", "dateTime")]
head(d1)
#find the beginining and end of each transect by splitting a data frame
#do some work and then put it back together in a single data frame
library("plyr")
x<- d1[d1$transect.id =="OST14-1W-S",]
d14 <- ddply(.data= d1, .variables = c("transect.id"), function(x){
  x <- x[complete.cases(x),]
  x<- arrange(x, dateTime)
  start.lat <- x[1,]$lat
  start.lon <- x[1,]$lon
  
  end.lat <- x[nrow(x),]$lat
  end.lon <- x[nrow(x),]$lon
  
 df = data.frame(start.lat, start.lon, end.lat, end.lon)
  return(df)
  
}, .progress = "text", .inform = T)

#step 2 : making a map
#i get the base map of the area of interest
#install a couple of libraries
library("rgdal")
library("ggmap")

f1 <- readOGR(dsn= "SoFL_terra_map", layer = "SoFL_terra_basemap")
f1 <- spTransform(f1, CRS("+proj=longlat + datum=WGS84"))
f1 <- fortify(f1)


install.packages("mapproj")
#step 3: create the map plot

m <- ggplot() + geom_polygon(data = f1, aes(x=long, y=lat, group = group), 
                             colour = "black", fill="grey70")+
  geom_segment(data = d14,  aes(x= start.lon, y = start.lat, xend=end.lon, yend= end.lat),
               color= "firebrick4", size=2)+
     #zoom in on area of interest
  coord_map(xlim = c(-82, -79), ylim = c(24,26.5))+
   scale_y_continuous(name = 'degrees N', breaks = seq(24,26.5,0.5))+
   scale_x_continuous(name ='degrees N', breaks = seq(-82,-79,0.5))+
   theme(axis.text.x = element_text(size = 10, color = "black"),
         axis.ticks.y = element_line(size = 0.8, color = "black"),
         axis.ticks.x = element_line(size = 0.8, color = "black"),
         axis.ticks.length = unit(0.1, "cm"),
         panel.background = element_rect(fill='white', size=1, color = "black"),
         panel.grid = element_blank())
m

#for d2
d2 <- d2[,c("transect.id","lat","lon", "dateTime")]
head(d2)
#find the beginining and end of each transect by splitting a data frame
#do some work and then put it back together in a single data frame
library("plyr")
x<- d2[d1$transect.id =="OST14-1W-S",]
d14_2 <- ddply(.data= d2, .variables = c("transect.id"), function(x){
  x <- x[complete.cases(x),]
  x<- arrange(x, dateTime)
  start.lat <- x[1,]$lat
  start.lon <- x[1,]$lon
  
  end.lat <- x[nrow(x),]$lat
  end.lon <- x[nrow(x),]$lon
  
  df2 = data.frame(start.lat, start.lon, end.lat, end.lon)
  return(df2)
  
}, .progress = "text", .inform = T)

#step 2 : making a map
#i get the base map of the area of interest
#install a couple of libraries
library("rgdal")
library("ggmap")

f1_2 <- readOGR(dsn= "SoFL_terra_map", layer = "SoFL_terra_basemap")
f1_2 <- spTransform(f1_2, CRS("+proj=longlat + datum=WGS84"))
f1_2 <- fortify(f1_2)


install.packages("mapproj")
#step 3: create the map plot

m <- ggplot() + geom_polygon(data = f1_2, aes(x=long, y=lat, group = group), 
                             colour = "black", fill="grey70")+
  geom_segment(data = d14_2,  aes(x= start.lon, y = start.lat, xend=end.lon, yend= end.lat),
               color= "firebrick4", size=2)+
  #zoom in on area of interest
  coord_map(xlim = c(-82, -79), ylim = c(24,26.5))+
  scale_y_continuous(name = 'degrees N', breaks = seq(24,26.5,0.5))+
  scale_x_continuous(name ='degrees N', breaks = seq(-82,-79,0.5))+
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.ticks.y = element_line(size = 0.8, color = "black"),
        axis.ticks.x = element_line(size = 0.8, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        panel.background = element_rect(fill='white', size=1, color = "black"),
        panel.grid = element_blank())
m


