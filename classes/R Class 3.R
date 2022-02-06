a <-c(9,3,5,2,4,8)

b <- c(9,3,5,2,4,8, "hello")

d <- c(9,3,5,7,4,8, list(0.25,0.33,0.67), recursive=TRUE)

e <- c(9,3,5,7,4,8, list(0.25,0.33,0.67), recursive=FALSE)

w <- 1:10

length(w) <- 6
length(w) <- 10

pipette <- list(purpose= "DNA", volume=c(50), unit="ml", price=100)

df <- data.frame(a=c(1,2,3,4,5), b=c(20,25,33,47,52))

#matrix -----
#this is an extention of a vector
#it has two dimension
#you can only have data of a single type

m <- matrix(data=1:12, nrow = 4, ncol = 3, dimnames = list(c("r1","r2","r3","r4"),c("c1","c2","c3")))

n <- matrix(data=1:25, nrow = 5, ncol = 4, dinames = list(c("bob","sue","charlie","mary","kevin"),c("c1","c2","c3","c4")))
 
#array ---------

ray <- array(data = 1:24, dim=c(3,4,2))

ray[,,2]
ray[1,4,,2]

big.ray <- array(data = 1:48, dim=c(3,4,4))


#factors --------

hair.color <-c("brown","white","grey","blond","black")

hair.color <-factor(c("brown","white","grey","blond","black"))

hair.color <-factor(levels=c("brown","white","grey","blond","black"), labels = c("Brown", "White", "Grey", "Blond","Black"))

#formulas ------

sample.formula <- as.formula(y~m*x + b)

sample.formula <- as.formula(y~m*x^2 + b)
