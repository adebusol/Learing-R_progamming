#How do the variables change with different transect id

library(scales)
library(dplyr)
library(tidyverse)
library(plyr)
library(stringr)

load("test3_data_2020/ost2014_phy_t.Robj")
names(phy_t)
phyt_1 <-phy_t[,c(1:12,15,20,21)]

t<-phyt_1[order(phyt_1$transect.id, phyt_1$dateTime),]

dir.create(path = "plots_ddply")

ddply(.data = t, .variables = c("transect.id"), .fun = function(x){ 
  
  transect <- unique(x$transect.id)
  
  p <- ggplot(data = x, aes(x = dateTime, y=depth)) +
    geom_point() +
    geom_smooth() +
    ggtitle(label = transect)
  p
  
  png(file = paste0("plots_ddply/",transect,".png"), 
      width = 9, height = 14, units = "in", 
      res = 300)
  plot(p) 
  dev.off()
  
}, .progress = "text", .inform = T)

dev.list()
.rs.restartR()


#Custom function(s) - 
#make a bar graph of the mean temperatures(C & K) in the deep waters
#celcius to kelvin
C2K<- function(x){
  y<-(x + 273)
}

detach("package:plyr")
dee<- t[t$tow =='d',]
dee$temp_k<- NA
dee$temp_k <- C2K(dee$temp)
t.stats_dee<-dee%>%group_by(region)%>%summarise(mean.temp = mean(temp),mean.temp.k = mean(temp_k))

#reshape data from wide to long to plot bar graph
dee.gather <- gather(data = t.stats_dee, key= variable,value = mean.temp, -region)

br1<-ggplot(dee.gather, aes(x=region, y=mean.temp, fill= variable)) +
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Mean Temperature in the Deep Tow")
  scale_y_log10()
br1

#histogram
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

#using apply
t$stationtype <- apply(X = t, 1, FUN = Observ_func)
unique(t$stationtype)
#using for loop
for(i in 1:length(t)){
  t[i,]$stationtype <- Observ_func(x= t[i,]) 
}

sp<-t[t$stationtype=="Lagrangian",]

y<- ggplot(data = sp, aes(salinity))+
  geom_histogram()+
  facet_wrap(~region)
y



#map

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

write.csv(t, file="t.csv")
