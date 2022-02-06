#Learn how to make your own functions
#you have to give it arguments to make it work

#makea function convert between units. #Good example

#miles to kilometers

miles2km <- function(x){
 
y<- x*1.6
return(y)
   
}
#return is required (but not always) to get a value 
#what happens inside a function, stays inside a function. you won't see it listed in the global envrionment
#y = 9
#x = 2

miles2km(x=3000)
c <- miles2km(x=3000)
c

#example 2: convert lat and lon
library(stringr)
l <- "30° 20.4050 N"

deg.lat <- function(Lat){
  
  t <- str_split_fixed(string =l, pattern = " ", n= 3)
  dir <- t[,3]
  mult <- ifelse(test= "N", yes = 1, no = -1)
  mult
  
  #changing it to a number. Cos we cant do maths on a character string
  deg <- as.numeric(str_replace(string = t[,1], pattern = "°", replacement = ""))
  
  
  dec.min <- as.numeric(t[,2])
  dec.deg <- (deg + dec.min/60)*mult
  
}

#example 3 watts to microvolts

#ifelse statements ----------
#purpose: return values based on a condition or set of conditions

#basic version---
a <- "horse"
pet.exercise <- ifelse(test = a =="dog", yes = "walk", no = "ride")

#2nd level : using two or more conditions for the test

a<- "goat"
if(a=="cat") {   #this part is our test
  
  #if true, then do this thing (return this value)
  food <- "tuna"
  #if a!=cat, then this value will be skipped and R will check the next test
} else if (a=="horse"){ #test2
  
  food <- "hay"
} else if (a== "goat") { #test 3
  food <- "your laundry"
  
} else {} #if there is no object to return in the curly braces, then nothing will
  
#showing what happens if you have a value in the last 'else{}'
  
a<- "goat"
if(a=="cat") {   #this part is our test
  
  #if true, then do this thing (return this value)
  food <- "tuna"
  #if a!=cat, then this value will be skipped and R will check the next test
} else if (a=="horse"){ #test2
  
  food <- "hay"
} else if (a== "goat") { #test 3
  food <- "your laundry"
  
} else { #if there is no object to return in the curly braces, then nothing will

food <- "money"
}
  
#more practice on if_else statements

y<- seq(2,120,4)

x <- seq(0,600,5)
length (x)<- length (y)
plot(x,y)
library (tidyverse)
ggplot() +geom_point(aes(x = x, y=y))

#work with the x vector  data to set breaks for every 25

#find the maximum value 
m <- max(x)

if(m>100){
  breaks <- seq (from =5,to=m, by=25)
  
} else if(m<=80) {
  breaks <- seq(from = 0, to =m, by =20)
  }else{}
  
#adding custom breaks to the plot
  ggplot() + geom_point (aes(x = x, y=y)) +
  scale_x_continuous(name = "eggs", breaks=breaks)
  
#merging data sets ----
load("fish_data.Rdata")

  #subset/pull out some of the data to make a practice data set that is a little to work with  
f <- fish[fish$transect.id == "OST14-1E-D" & fish$parcel.type == "patch",]
unique (f$transect.id)

#split apart and then put back together (merge or join)
f1 <- f[,c("transect.id","parcel.id","parcel.length.m")]
f2 <- f[,c("transect.id","parcel.id","parcel.start.time")]

#put the data frames f1 and f2 back together
f.merge <- merge(x=f1, y=f2, by = c("transect.id","parcel.id"), all = T)
head(f.merge)


#using the join function
library(dplyr)
inner_join (x = f1, y=f2, "parcel.id")

d <- fish[fish$transect.id == "OST14-1E-D"|fish$transect.id == "OST14-1W-S",]
d <- d[d$fish$parcel.type == "patch",]


