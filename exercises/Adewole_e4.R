#Exercise 3: Working with character strings and dates in 'R'
library(tidyverse)

#Load the following data set
load("data_sets/fish_data.Rdata")
load("fish_data.Rdata")

#1) extract the 'transect.id' column from the data frame
fish$transect.id
#2) Find the unique values of transect.id and assign those to a new object.
##Hint: the function to use is in the question!
u <- unique(fish$transect.id)

#3) Extract the first 5 unique, character strings from transect.id and assign those to a new object
k <- (u[1:5])

#Practing with character strings ----

#4) Each value of transect.id has three components, separated by a dash ('-').
## Break these 3 components apart using the dash as the separator and assign each component to a new object.
s <- (u) 
t <- str_replace(string = s, pattern = "-", replacement = " ")
t2 <- gsub(pattern = "-", x = s, replacement = " ")
split <- str_split_fixed(string = s, pattern = "-", n = 3)
c <- split[,1]
w <- split[,2]
o <- split[,3]

#5) Recombine the three components into a single text string.
## Order the components so that the "D, M, S" component is first in the test string.
##Separate with a underscore ("_").
p <- str_replace(string = s, pattern = "-", replacement = "")
p2 <- gsub(pattern = "-", x = s, replacement = "")
i <- str_replace(string = s, pattern = "-", replacement = "_")
i2 <- gsub(pattern = "-", x = s, replacement = "_")

#6) Using the object with the first 5 unique, character strings from transect.id, replace the dash ("-") with a decimal point ('.')
m <- (k)
w <- str_replace(string = m, pattern = "-", replacement = ".")
w2 <- gsub(pattern = "-", x = k, replacement = ".")
#7) Using the object with the first 5 unique, character strings from transect.id, extract the first 5 values (reading left to right) and assign those values to a new object
ee <- str_sub(string = k, start = 1, end = 5)

# Modifying Column headers ----
#8) Extract the header (i.e., column names) from the fish data frame
head.fish <- names(fish)

#9) Change the case of the letters from lower- to upper-case.
hfish <- toupper(head.fish)


#10) Reassign these new, uppercase headers to be the column names of the fish data frame.
names(fish) <- hfish

#11a) What function would you use to display the FIRST 6 rows of the data frame "fish"?
head(fish, n=6)

#11b) What function would you use to display the LAST 6 rows of the data frame "fish"?
tail(fish, n=6)

#Dates----
#12) Convert the following date to a 'R' date-time object. Set it to be in the West Coast (i.e., Pacific) time zone.
d <- "2020-09-10"
dat <- as.POSIXct(strptime(x = d,format = "%Y-%m-%d", tz = "America/Los_Angeles"))

                  