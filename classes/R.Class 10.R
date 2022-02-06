#Warm up
load("fish_data.Rdata")

f1 <- fish[fish$transect.id == "OST14-1W-S" & fish$parcel.type == "patch",]

f2 <- fish[fish$transect.id == "OST14-1W-S" | fish$transect.id =="OST14-3C-D" & fish$parcel.type == "patch",]

f1 <- f1[,c("transect.id", "parcel.id", "parcel.density.m3")] #one transect
f2 <- f2[,c("transect.id", "parcel.id", "parcel.density.m3")] #two transects

#merge the f1 and f2 

f.merge <- merge(x =f1, y=f2, by = c("transect.id", "parcel.id"), all = T)

#example 2

f3 <- fish[fish$transect.id == "OST14-1W-S" | fish$transect.id =="OST14-3C-D" & fish$parcel.type == "patch",]
f3 <-f3[,c("transect.id", "parcel.id", "parcel.length.m")] #two transects

#what happens if you use all.x = T
f.merge2 <- merge(x = f1, y=f3, by =c ("transect.id", "parcel.id"), all.x = T)

#what happens if you use all.y=T
f.merge3 <- merge(x = f1, y=3, by =c ("trnasect.id", "parcel.id"), all.y = T)

#what happens if  the values in the transect.id column do not march at all?

f4 <- fish[fish$transect.id == "OST14-1W-S" & fish$parcel.type == "patch",]

f5 <- fish[fish$transect.id == "OST14-1W-S" | fish$transect.id =="OST14-3C-D" & fish$parcel.type == "patch",]

f4 <- f4[,c("transect.id", "parcel.id", "parcel.length.m")] #one transect
f5 <- f5[,c("transect.id", "parcel.id", "parcel.density.m3")] #two transects

f.merge4 <- merge(x = f4, y= f5, by = c("transect.id", "parcel.id"), all = T) #this keeps everything but tries to



#reshaping your data ------
#functions to learn :
#package 'reshape2' : 'melt' and 'cast' functions
# package 'tidyr' : 'spread' and 'gather' functions
d <- f.merge2 <- merge(x = f1, y=f3, by =c ("transect.id", "parcel.id"), all.x = T)


install.packages("reshape2")
library(reshape2)

#with variable name
d.melt <- melt(data = d, id.vars = c("transect.id", "parcel.id"), measure.vars = c("parcel.density.m3", "parcel.length.m"), variable.name= "parcel.metrics")

#removing the argument variable name
d.melt2 <- melt(data = d, id.vars = c("transect.id", "parcel.id"), measure.vars = c("parcel.density.m3", "parcel.length.m"))

#changing data from "long" format to "wide" format
# use the 'dcast' function from reshape2 package

#when you have multiple values assigned to the same set of idvalues (i.e., transect.id), then you
#will have to aggregate then using a function like mean, sum, min, etc.

dm.cast <- dcast(data = d.melt, formula = transect.id + parcel.id ~ parcel.metrics, fun.aggregate = min)

#note that is you have multiple values assigned to the same set of id.variable values,
#then you do not need to use a function to aggregate

#using the 'gather' and 'spread' function
library(tidyr)
s <- f.merge2

#going from "wide" to long using "gather"
#
s.gather <- gather(data = s, key, value, -transect.id, -parcel.id)

#key and value arguments are used to name the new columns
s.gather2 <- gather(data = s, key = variable, value = metrics, -transect.id, -parcel,id)

#using the 'spread' function to go from long to wide
s.spread <- spread(data = s.gather2, variable, metrics)

#going from wide to long using the spread function
load("fish_data.Rdata")
f<- fish[,c("transect.id","parcel.id", "parcel.type","parcel.length.m","parcel.density.m3")]
f<- f[(f$parcel.type== "patch"),]
#for pratice we will 'melt' the data frame before making it wide again
library(reshape2)
f.melt <- melt(data = f, id.vars =c("transect.id", "parcel.id","parcel.type"), measure.vars =c("parcel.length.m","parcel.density.m3"))

#if you have a long list of coumn names and you dont want to start typing it out you can use a shortcut, the 'names' function
fcolnames<- names(f)
id.vars <- fcolnames[1:3]
f.melt <- melt(data = f, id.vars =c("transect.id", "parcel.id","parcel.type"), measure.vars =c("parcel.length.m","parcel.density.m3"))
               