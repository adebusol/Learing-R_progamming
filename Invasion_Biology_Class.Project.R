#set your working directory
setwd("/Users/nic/Documents/Teaching/InvasionBiology/CourseProject")

#First tried to extract lat/long for a bunch of locality data... it didn't work well because the locations were far off... Would only work if we had good city data
read.csv("CI_nocoods.csv")-> nocoords;
attach(nocoords);
str(nocoords)

library(ggmap)
locations_df <- mutate_geocode(nocoords, locality, stringsAsFactors = FALSE, output="latlong")

is.character(nocoords$locality)
locality2<-as.character(nocoords$locality)
locality2
locality_df <- as.data.frame(locality2,stringsAsFactors = FALSE)

as.character(locality2)
locations_df <- mutate_geocode(locality_df, locality2, output="latlon")


#Lets just work with data that we have lat/long for...
#Read in polished lat long dataset
library(readxl)
read_xlsx(path = "Russian Olive data.xlsx")-> cidata;
attach(cidata);
str(cidata)
require(raster)
require(rgdal)

locations<- SpatialPoints(cidata[,6:5], proj4string=CRS("+proj=longlat +ellps=WGS84"))

#Create initial map to make sure we only have native and invasive populations
#download raster file for map of the world
library(rworldmap)
newmap<- getMap(resolution="low")

plot(newmap)
points(locations, pch=16, cex=.5)


#Extract bioclim values
#Load libraries necessary for working with raster datasets
require(raster)
require(rgdal)

r <- getData("worldclim",var="bio",res=2.5)
bio <- r[[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]]
names(bio) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11","bio12","bio13","bio14","bio15","bio16","bio17", "bio18","bio19")

values <- extract(bio,locations)

#you can print these out
cidata2<-cbind(cidata,values)
write.csv(cidata2, file="cidata2.csv")

#Or load if you already have them downloaded
bio1<- raster('wc2-5/bio1.bil')
bio2<- raster('wc2-5/bio2.bil')
bio3<- raster('wc2-5/bio3.bil')
bio4<- raster('wc2-5/bio4.bil')
bio5<- raster('wc2-5/bio5.bil')
bio6<- raster('wc2-5/bio6.bil')
bio7<- raster('wc2-5/bio7.bil')
bio8<- raster('wc2-5/bio8.bil')
bio9<- raster('wc2-5/bio9.bil')
bio10<- raster('wc2-5/bio10.bil')
bio11<- raster('wc2-5/bio11.bil')
bio12<- raster('wc2-5/bio12.bil')
bio13<- raster('wc2-5/bio13.bil')
bio14<- raster('wc2-5/bio14.bil')
bio15<- raster('wc2-5/bio15.bil')
bio16<- raster('wc2-5/bio16.bil')
bio17<- raster('wc2-5/bio17.bil')
bio18<- raster('wc2-5/bio18.bil')
bio19<- raster('wc2-5/bio19.bil')

bio <- cbind(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11,bio12,bio13,bio14,bio15,bio16,bio17, bio18,bio19)

#Then you can extract to points just as was done above
values <- extract(bio,locations)


#for this you will have to extract each rater 
values <- extract(bio,locations)
cidata2<-cbind(cidata,values)
write.csv(cidata2, file="cidata2.csv")


#make a prettier map

pdf('NA_whiteclover_recipricaltransplant2.pdf')
tempcol2<- colorRampPalette(c("dark blue", "blue","skyblue", "white","light pink", "red", "dark red"))
plot(bio6, xlim=c(-160,-50), ylim=c(10,85), col=tempcol2(100))

#plot your points

points(locations, pch=16, cex=.75)
plot(newmap, add=T)

#Add a scale bar for 1000 km
scalebar(2000, type="bar", below="kilometers", divs=4)
dev.off()

#Europe
pdf('EU_whiteclover_recipricaltransplant.pdf')
plot(bio6, xlim=c(-40,50), ylim=c(25,80), col=tempcol2(100))

points(locations, pch=16, cex=.75)
plot(newmap, add=T)
scalebar(2000, type="bar", below="kilometers", divs=4)
dev.off()



#Okay, the important data to generate is the 

#get the distance between two geographic points
install.packages('geosphere')
library(geosphere)
#Make a spatial points object of the introduction location
#First only want the invasive range, so lets subset our data

InvasiveRange<-cidata[cidata$Range == "Invasive",]
str(InvasiveRange)

#Lets make Spatial Point Objects for our specimens in this subset dataset
IRlocations<-SpatialPoints(InvasiveRange[,6:5], proj4string=CRS("+proj=longlat +ellps=WGS84"))
IRinitiallocations<- SpatialPoints(InvasiveRange[,11:10], proj4string=CRS("+proj=longlat +ellps=WGS84"))

#get distance between each point and the introduction location and bind this to the initial dataset
dist<-distHaversine(IRlocations, IRinitiallocations)
InvasiveRange_new<-cbind(InvasiveRange,dist)

#Lets plot it - what problem do you see here?
rs <- plot(InvasiveRange_new$year, (InvasiveRange_new$dist/1000), pch=16, xlab="Years since Introduction", ylab="Distance from Introduced population (km)")


library(dplyr)
#get maximum distance from starting point
max_values2<- InvasiveRange_new %>% group_by(year) %>% slice(which.max(dist))

#must make into a dataframe to use in dpylr again
maxvalues2<- as.data.frame(max_values2)

#Get the culmulative maximum values
culmmax<-mutate(maxvalues2,culmaxdist= cummax(dist))

#Make into a dataframe again

culmmax2<-as.data.frame(culmmax)

#use mutate to get the slope at each point
diffs<-mutate(culmmax2, D_delta = (((culmaxdist/1000) - lag(culmaxdist/1000))/(year-lag(year))))

diffs$D_delta

diffs$dist/1000
plot(diffs$year, diffs$D_delta, pch=16, xlab="Year", ylab="Velocity of Invasion (km/year)")


#Okay, so we can see how a single point can skew this dataset.
#Lets take those points out and do it again.

read.csv("creeping_indigo_data_2.csv")-> cidata2;
attach(cidata2);
str(cidata2)

InvasiveRange2<-cidata2[cidata2$Range == "Invasive",]
str(InvasiveRange2)

IRlocations2<-SpatialPoints(InvasiveRange2[,6:5], proj4string=CRS("+proj=longlat +ellps=WGS84"))
IRinitiallocations2<- SpatialPoints(InvasiveRange2[,14:13], proj4string=CRS("+proj=longlat +ellps=WGS84"))
dist2<-distHaversine(IRlocations2, IRinitiallocations2)
InvasiveRange_new2<-cbind(InvasiveRange2,dist2)

plot(InvasiveRange_new2$year, (InvasiveRange_new2$dist2/1000), pch=16, xlab="Years since Introduction", ylab="Distance from Introduced population (km)")

max_values2<- InvasiveRange_new2 %>% group_by(year) %>% slice(which.max(dist2))

#must make into a dataframe to use in dpylr again
maxvalues2<- as.data.frame(max_values2)

#Get the culmulative maximum values
culmmax<-mutate(maxvalues2,culmaxdist= cummax(dist2))

#Plot culmulative maximum values
plot(culmmax$year, (culmmax$culmaxdist/1000), pch=16, xlab="Years since Introduction", ylab="Distance from Introduced population (km)")

#add a loess regression
loessMod60 <- loess((culmmax$culmaxdist/1000) ~ culmmax$year, data=culmmax, span=0.6) 
smoothed60 <- predict(loessMod60) 
lines(culmmax$year,smoothed60, col="dark green", lwd=2)


#Make into a dataframe again
culmmax2<-as.data.frame(culmmax)

#use mutate to get the slope at each point
diffs<-mutate(culmmax2, D_delta = (((culmaxdist/1000) - lag(culmaxdist/1000))/(year-lag(year))))
#Plot velocity over time
plot(diffs$year, diffs$D_delta, pch=16, xlab="Year", ylab="Velocity of Invasion (km/year)")

#add a loess regression
loessMod50 <- loess(diffs$D_delta ~ diffs$year, data=diffs, span=0.5) 
smoothed50 <- predict(loessMod50) 
lines(smoothed50, x=diffs$year[2:25], col="dark green", lwd=2)



#delete 
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) 
smoothed10 <- predict(loessMod10) 
lines(smoothed10, x=economics$date, col="red")



#What do we want to use as downstream measures to compare between species?
#Time to max velocity following introduction? Max velocity? slope of introduction?
#what are your independent variables and hypotheses?





 


#Range Maps: https://rspatial.org/raster/cases/3-speciesdistribution.html
#Range Maps a second way:https://rdrr.io/github/manubio13/rangemap/f/README.md#simple-graphical-exploration-of-your-data.





