# Exam 2 | Oct 2020

# The exam will be due Sunday, Oct. 25 at 12pm.

# Please upload to Moodle a zipped (i.e., compressed) folder containing your answers in a R script and the folders
# requested as part of the answer to Question #10.

# ------------

#concepts covered:

# character strings
# for loop
# "adply" function
# functions in the 'apply' family
# summarizing data using 'aggregate' and 'summarize'
# write your own functions
# if_else statements
# merge (base)
# join function family (dplyr)
# ddply

# ------
library(stringr)
#1) read in all the .csv files in the 'test2_data' folder using the adply function. Return a new object that contains
# the data from all the files - (4 POINTS)
cfiles<- list.files(path = "test2_data_2020/", pattern= ".csv", full.names = T)

k <-adply(.data = cfiles, .margins = 1, .fun = function(x){
  
  c <- read.csv(file = x, header=T,stringsAsFactors = FALSE)
  
  return(c)
  
}, .progress = "text", .inform = T)


#2) There are at least three different functions we can use to combine or concatenate character strings in 'R'.
#Please demonstrate, using the questions below, two different functions that will combine the character strings.

#a) - (2 POINTS)
#create a character string object that has the following statement: "My favorite thing about fall is"
#create a character string object that has you answer to that statement
#Combine those objects into a single character string and assign that string to a new object
#using str_c 
cs1 <- "My favorite thing about fall is"
 cs2 <- "the weather"
fav <- str_c( cs1, cs2, sep = " ")
 
#Using the paste function
fav1 <- paste(cs1, cs2)


#b) - (2 POINTS)
#create a character string object that has the following statement: "My least thing about fall is"
#create a character string object that has you answer to that statement
#Combine those objects into a single character string and assign that string to a new object

cs3 <- "My least thing about fall is"
cs4 <- "the pollen"
nfav <- str_c( cs3, cs4, sep = " ")

#Using the paste function
nfav1 <- paste(cs3, cs4)


#3) load("Test_2_data/batting.2008.Rdata") - (3 POINTS)
load("test2_data_2020/batting.2008.Rdata")

#a) extract the year from the field 'debut' and assign those values to a new column in the 'batting' data frame
# if needed, change this new 'year' field to numeric data type.
debut_year <- str_sub(string = d$debut, start = 1,end = 4)
d$debut_year<- debut_year
names(d)

#b) create a new field in the data frame that indicates the age of the player when they started in the Majors.
d$debut_year<- as.integer (d$debut_year)       
age_player<- d$debut_year -d$birthYear 

d$age_player <- age_player

#c) convert the values in the 'debut' field from character string to a 'R' date-time object
d$debut <- as.POSIXct(strptime(x= d$debut, format= "%Y-%m-%d"))

#4) Find the mean number of runs ('R'), hits ('H'), and home runs ('HR') for each team and
# return this summary table as a single object - (2 POINTS)

# BONUS!: if you can use the 'piping' syntax - (+1 POINT)
detach("package:plyr")
t.stats <- d %>% group_by(teamID) %>% summarise(mean.r = mean(R), mean.h = mean(H), mean.hr = mean(HR))
                                                     

#5) Find the row sums in the following matrix - (2 POINTS)

sea.monsters <- c("kraken","serpent","sirens","mermaids","nessy","rogaru")
m <- matrix(data = seq(1,42,1), nrow=7, ncol = 6,
            dimnames = list(c("r1","r2","r3","r4","r5","r6","r7"),
                            sea.monsters))
m.row_sum <- apply(X = m, MARGIN = 1, FUN = sum)  


# 6) Using the 'batting.2008.Rdata', subset the following fields (i.e., 'names(d)[c(12,16:31)]') and
# assign that subset of fields to a new object. - (3 POINTS total)
 names(d)[c(12,16:31)]
 bat2 <- d[,c(12,16:31)]
# use the 'ddply' function to iterate through each 'teamID' (1 POINT) of your subset of data to do the following work:
 library(tidyverse)
 library(plyr)
#a) Find the mean for each team-specific metric.
# Hint: you can only perform the mean function on columns with numeric data, (1 POINT)
 str(bat2)
 bat2[,2:17] <- lapply(bat2[,2:17], as.numeric)
 
my_summary <- ddply(bat2, .variables = c("teamID"),summarise_if,is.numeric, mean)
                                                                                                  
#b) Return the summarized table as a new data frame object. (1 POINT)
my_summary <- as.data.frame(my_summary)
#c) Bonus!: round the values off to the 0.1 decimal place (+1 POINT)

my_summary[,2:17] <- round(my_summary[,2:17],1)
my_summary
#7) create an object that has the names of 3 types of candy. (3 POINTS)
# Write an 'if else' statement that will:

# return a new object 'yum = "Oh Yeah!" ' that indicates the one is your favorite among the three types
# OR returns a new object 'yum = "Meh" ' that indicates you are okay with
# OR returns a new object 'yum = Gross!" ' that indicates your least favorite

# print the object 'yum' at the end to reveal the answer
my_candy<-c("chocolate", "mint", "jelly beans")
if ("chocolate"%in% my_candy){
    yum = "oh yeah!"
    print(yum)
}else if ("mint" %in% my_candy){
  yum = "meh"
} else if ("jellybean" %in% my_candy){
  yum = "gross!"
}else{}

#8) Write an if_else statement testing if you will watch one of two Halloween movies:
# "Texas Chainsaw Massacre" or "Hocus Pocus" - (2 POINTS)
 a <- "halloween"
 if (a=="halloween"){
   movie <- " watch texas chainsaw massacre"
   }else{
     movie = "don't watch hocus pocus"
 }

#9) write a custom function to convert from miles to kilometers - (3 POINTS total)

#a) write function - (2 POINTS)
 mil2km <- function(x){
   
   y <- x*1.6
   return(y)
 }
 
#b) demonstrate its use - (1 POINTS)
 use <- mil2km(x=2500)
use
#10) Create an 'Exam2_teams folder. In that folder, please use 'R' to create subdirectories (i.e., folders) for each
# 'teamID' in the batting.2008.Rdata set using a 'for loop'. - (3 POINTS)
dir.create(path = ("Exam2_teams folder"))
for (i in 1: length(d$teamID)){
  dir.create(path = paste0("Exam2_teams folder/", d$teamID[i]))
}
  
#11)
#a) create an empty 8 row, 5 column matrix. - (1 POINT)
emp <- matrix(nrow = 8, ncol = 5)

#b) Using a nested 'for loop', fill that 8 row by 5 column matrix with the product of row and column index values (e.g., r1*c1 would be 1*1) - (3 POINTS)
for (i in 1:dim(emp)[1]){
  for (j in 1:dim(emp)[2]){
    emp[i,j] <- i * j
  }
}
emp
#12) Create two data frames using the 'batting.2008' data. The first should contain the fields "nameLast", "nameFirst", "weight".
#The second data frame should contain the fields "nameLast", "nameFirst", "height".
# Demonstrate how to join (or merge) these data frames together into a single object that
# contains both the weight and height of each player.
# - (3 POINTS)
df1<- d[,c("nameLast", "nameFirst", "weight")]
df2 <- d[,c("nameLast", "nameFirst", "height")]
df.merge <- merge(x= df1, y= df2,by = c("nameLast", "nameFirst"), all = T)

#13) If you were trying to merge data frames where the column headings differed between then
# (but the values were the same in each), which set of arguments would you use? - (1 POINT)
#using the 'by' argument - by.x or by.y 
