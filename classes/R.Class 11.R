
#example doing some actual work

f.names <- c("cat", "otter", "crawfish", "chinchilla")


#example with an array
#initializing our emoty array
ray <- array(dim=c(3,3,3))


#fill the array with values using a for loop
dim(ray)[1]

for(i in 1:dim(ray)[1]){
  for (j in 1:dim(ray)[2]){
    for(k in 1:dim(ray)[3]){
      
      ray[i,j,k] <- i*j*k
    }
      }
}



