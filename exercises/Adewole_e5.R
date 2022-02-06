#Exercise #5

#Practice the following skills:
# "adply" function
# functions in the 'apply' family
# summarizing data using 'aggregate' and 'summarize'
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)

#Part 1: 'adply' function ----

#1) Read in and combine all the data in the .Rdata files found in the 'e5_data_sets' folder.
## Return a single data frame containing data from all the files.
efiles<- list.files(path = "e5_data_sets/", pattern= "*.Rdata", full.names = T)
p <- adply(.data = efiles, .margins = 1, .fun = function(x){
  
 load(file= x)

return(indiv.patch.statistics)
  
}, .progress = "text", .inform = T)


  

#2) Read in and combine all the data in the .CSV files found in the 'e5_data_sets' folder.
## Return a single data frame containing data from all the files. Show
cfiles<- list.files(path = "e5_data_sets/", pattern= "*.csv", full.names = T)

k <-adply(.data = cfiles, .margins = 1, .fun = function(x){

  c <- read.csv(file = x, header=T,stringsAsFactors = FALSE)
  
  return(c)
  
}, .progress = "text", .inform = T)




#Part 2: the #apply function family ----
#use when applying a function to a matrix or an array

#3) create a 8x5 matrix using the 'sea.monsters' object and a sequence of numbers that increases by 2 from 2 to 80
sea.monsters <- c("kraken","serpent","sirens","mermaids","nessy")
sea.monsters <- matrix(data = seq(2,80,2), nrow=8, ncol = 5, dimnames = list(c("","","","", "","","",""),c("kraken","serpent","sirens","mermaids","nessy")))
#4) find the standard error for all the columns in matrix created Question 3
std.error <- function(x){sd(x)/sqrt(length(x))}
sea.error <- apply(X = sea.monsters, MARGIN = 2, FUN = function(x){sd(x)/sqrt(length(x))})

#5) find the quantiles for all the columns in matrix created in Question 3

sea.quantile <- apply(X = sea.monsters, MARGIN = 2, FUN = quantile)


#6) find the row sums in matrix created in Question 3
sea.sum <- apply(X = sea.monsters, MARGIN = 1, FUN = sum)

# Generate a 5x7 array with 3 levels
ray <- array(data =1:105, dim = c(5,7,3))

#7) find the z-score of the rows
z.score <- apply(X= ray, MARGIN = 1, FUN=function(x){
  z <- (ray - mean(x))/sd(x)
  return(z)
})


#8) find the 95% confidence interval of the array's columns
CI95 <- apply(X= ray,MARGIN = 2,FUN = function(y){
  error <- qt(0.975, df=length(y)-1)*sd(y)/sqrt(length(y))
  L95 <- list(L95=mean(y) - error)
  U95 <- list(U95=mean(y) + error)
  ci <- list(L95,U95)
  return(ci)
})


#9) #please use the 'little_ost2014_physical.Rdata" data set for the following question
# find the standard error of the columns: temp, salinity, and pressure.
# You may need to subset these columns from the main data frame before summarizing and omit 'NA' values.
load("little_ost2014_physical.Rdata")

p1 <- p[ c("temp","salinity", "pressure")]
p.error <- lapply(X = p1, FUN=function(x){sd(x)/sqrt(length(x))}) 


#Part 2: summarize and aggregate functions -----
#please use the 'little_ost2014_physical.Rdata" data set for the following questions

#10) Find the mean and standard deviation of depth; group by 'transect.id'
f.patch <- fish[fish$parcel.type=="patch",]
unique(f.patch$parcel.type)

p.depth <- p[("depth")]
p.dth <- aggregate(x = p.depth, by = p["transect.id"], FUN = mean)
pdth.std <- aggregate(x = p.depth, by = p["transect.id"], FUN = sd)

#11) use the functions 'group_by' and 'summarise' to generate the following summary statistics for the "temp" in a single data frame.
# Excluding the 'und' tows from your summary, group by 'tow' column.
sum.stats.temp <- group_by(.data = p, tow)

sum.stats.temp1 <- summarise(.data = sum.stats.temp, mean.temp = mean(temp),
                                sd.temp = sd(temp))


pd.stats <- p %>% group_by(tow) %>% summarise(mean.pd = mean(p[p$tow== 'd',"temp"], na.rm = T))

