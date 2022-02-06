#Exercise #6

#Practice the following skills:
# write your own functions
# if_else statements

#Part 1: writing custom functions ---

#1) Generate a function to convert from degrees Fahrenheit to degrees Celcius.
# Demonstrate its utility using an input value of 98.6 deg F.
F2C <- function(x){
  
  y<- (x-30)/2
  return(y)
  
}
c <- F2C(x=98.6) 

#2) Generate a function to convert from degrees Fahrenheit to degrees Kelvin.
# Demonstrate its utility using an input value of 98.6 deg F.
F2K <- function(x){
  
  y<- (x-32) * 5/9 + 273.15
         return(y)
  
}
d <- F2K(x=98.6) 

#3) Scenario: You used an instrument that was not calibrated correctly and its 'blank' value consistent reads 0.05.
# You need to correct your output values using the instruments linear calibration curve and the constant offset value.
# Please write a custom function assuming the calibration equation slope = 3.5. A perfect calibration would have a y-intercept = 0.

# Output values to use:
o <- round(seq(1,20,2^0.5),2)
y= mx + b
calibrate <- function(x){
  y = 3.5*x + 0.05
  return(y)
}

print(o)
calibrate (x=0)

# 4) Using the "little_fish.Rdata" data set included in the files for this assignment, please write a custom function
# to convert the values in the 'parcel.density.m3' column from individuals per cubic meter (current unit) to
# individuals per 1000 cubic meters. Save the converted values as a new column in the 'little fish' data frame.
load("little_fish.Rdata")
cm2 <- function(x){
  y <- (x/1000)
  return(y)
}

cr <- cm2(f$parcel.density.m3)
f$parcel.density.103.m3 <- cr

#5) Using the "little_fish.Rdata" data set, please write a custom function to convert
# the decimal degree values in the following columns to this format

# "deg° min' sec" [NWES]
# 30°40'25" N

# columns:
# parcel.start.lat
# parcel.start.lon
# parcel.end.lat
# parcel.end.lon



library(stringr)
dec.deg <- function(x){
fc <-str_sub(string = x, start = 3, end = 7)
fa <-str_sub(string = x, start = 1, end = 2)
fc<- as.numeric(fc)
deg.min <-fc*60
rdeg.min <- deg.min
m <-round(x = rdeg.min, digits = 0.1)
fg<- str_sub(string = deg.min, start =3, end =6 )
fg<-as.numeric(fg)
deg.sec <- fg*60
s<-round(x = deg.sec, digits = 0.1)
done <- str_c(m,s, sep = "'")
all.done <- str_c(fa, done, sep = "°")
}

cc<- dec.deg(x = f$parcel.start.lon)
cc1 <- dec.deg(x = f$parcel.end.lon)
cc2 <- dec.deg(x = f$parcel.end.lat)
cc4 <- dec.deg(x = f$parcel.end.lon)


#correction

#Part 2: if_else statements-----

# one condition
# 6) generate an if_else statment to test if the value of a numeric object is positive number
q = 6 
test1 <- ifelse(test= q == "-6", yes = "negative number", no = "postive number")

# 7) using the two objects below, please generate an if_else statement that uses logical operator (i.e, !=, ==, >, etc.)
# in the test
x <- 5
y <- 8
test2 <- ifelse(test = (x!=y) == (x=y), yes = "correct", no = "wrong")
# 8) Hamlet's quandry: In Shakespeare's play Hamlet, the lead character has a famous speech "to be or not to be".
browseURL("https://www.poetryfoundation.org/poems/56965/speech-to-be-or-not-to-be-that-is-the-question")
# Write an if_else statement using the "to be' or 'not to be' for outcomes of the 'yes' and 'no' arguments respectively.
a <- "to be"
h.quandry <- ifelse(test = a =="not to be", yes = "no", no = "yes")

#two or more conditions
#9) create an 'if else' statement that returns (in order of preference) your four ideal pizza toppings
e<- "my fav pizza toppings"
if(e=="wings") {  
  
  
  preference <- "bbq, buffalo, garlic pepper"

} else if (e== "my fav pizza toppings") { 
  preference <- "chicken, meatballs, onions, green pepper"
  
} else {}

#two or more conditions joined (new content)
#To join two or more conditions into a single if statement, use logical operators viz. && (and), || (or) and ! (not).

#example:
x <- 7
y <- 5
z <- 2
if(x > y && x > z) {
  print("x is greater")
}

#10) generate your own 'if' statement using multiple conditions in one line
x<- -10
y<- -15
z <- 23
if(x=y || x <z) {
  print("x is a negative number")
}


#New content: understanding a common warning/error message when using if_else.
# The follow set of code will generate a warning message. Explain in a comment what the warning is telling you.
v <- 1:6

if(v %% 2) {
  print("odd")
} else {
  print("even")
}
#because v is a multi-element vector containing a sequence of numbers from 1-16 and we're it using against a single variable function. So R is letting us know that only the first element in our vector will be used.

#solution to the error - you have iterate (or use one value at a time from 'v' we can use a for loop)

#initialize and empty data storage object
o<- c()
for (i in 1: length(v)){
  if(i %% 2) {
    print("odd")
  } else {
    print("even")
  }
  o[i] <- x
}


