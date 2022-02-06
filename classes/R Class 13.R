load("test2_data_2020/batting.2008.Rdata")

#let's make a poin graph
library("tidyverse")

head(d)
 #plot the number of at bats ("AB") versus the home runs ("HR")
#what if we wanted to added a trend line to this plot. use the "+" sign again
p<- ggplot(data = d, aes(x =AB, y = HR)) +
  geom_point() +
  geom_smooth(method = "loess")
p

#change the break points of our x-axis and y-axis of this plot?
p<- ggplot(data = d, aes(x =AB, y = HR)) +
  geom_point() +
  geom_smooth(method = "loess")+
  scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
  scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))
  p
 #how to change the color andd size of the points on this plot? 
 p <- ggplot(data = d, aes(x =AB, y = HR)) +
    geom_point(color = "dodgerblue") +
    geom_smooth(method = "loess")+
    scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
    scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))
  p
  
#how to change the background panel on this plot?
  p <- ggplot(data = d, aes(x =AB, y = HR)) +
    geom_point(color = "dodgerblue") +
    geom_smooth(method = "loess")+
    scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
    scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))+
    theme_bw()
  p
  
  #how remove the gridlines from bacjfround panel on this plot?
  p <- ggplot(data = d, aes(x =AB, y = HR)) +
    geom_point(color = "dodgerblue") +
    geom_smooth(method = "loess")+
    scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
    scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))+
    theme(panel.grid.major= element_blank())
  p
  
  #easy basic plot
  p <- ggplot(data = d, aes(x =AB, y = HR)) +
    geom_point(color = "dodgerblue") +
    geom_smooth(method = "loess")+
    scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
    scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))+
    theme_classic()
  p
  
  #you can change the color of the points to add more information 
  p <- ggplot(data = d, aes(x =AB, y = HR)) +
    geom_point (aes(color = factor(RBI))) +
    geom_smooth(method = "lm")+
    scale_x_continuous(name= "At Bat", breaks = seq(0,700,50))+
    scale_y_continuous(name = "Home Run", breaks = seq(0,50,5))+
    theme_classic()
  p
  
  
  
  
  #what if we eanted to plot data from two different data sets on the same plot?
  #for this examplt i am to split the info in data frame "d" into separate
  #data frames just to demonstate how to input separate data frames into a ggplot
  
  d1 <- d[,c("AB", "HR")]
  d2 <- d[,c("RBI", "H")]
  
  p <- ggplot() +
    geom_point(data = d1,aes(x = AB, y = HR), color = "firebrick") +
    geom_point(data = d2,aes(x = RBI, y = H), color = "dodgerblue2") +
    geom_smooth(data = d1, aes(x = AB, y = HR), method = "lm")+
    geom_smooth(data = d2,aes(x = RBI, y = H), method = "lm")+
    scale_x_continuous(breaks = seq(0,700,50))+
    scale_y_continuous(breaks = seq(0,50,5))+
    theme_classic()
  p
  
  #let's make a line graph
  #let's go back to our original data frame
  d_sea <- d[d$teamID=="SEA",]
  l <- ggplot(data = d_sea, aes(x = AB, y= HR))+
    geom_line()
  l
  #change the thickness of the line
  d_sea <- d[d$teamID=="SEA",]
  l <- ggplot(data = d_sea, aes(x = AB, y= HR))+
    geom_line(size = 2)
  l
  #change the color of the line
  d_sea <- d[d$teamID=="SEA",]
  l <- ggplot(data = d_sea, aes(x = AB, y= HR))+
    geom_line(size = 2, color = "steelblue3")
  l
  #change the transparency of the line
  l <- ggplot(data = d_sea, aes(x = AB, y= HR))+
    geom_line(size = 2, color = "steelblue3", alpha = 0.5) #alpha values range from 0
  l
  
  #plot discrete variables
  b <- ggplot(data = d, aes(x = teamID, y= HR))+
    geom_bar(stat = "identity")
  b

  #change the color of the bars
  b <- ggplot(data = d, aes(x = teamID, y= HR))+
    geom_bar(stat = "identity", fill = "dodgerblue4")
  b
  
  #add labesl to the top bars
  b <- ggplot(data = d, aes(x = teamID, y= HR))+
    geom_bar(stat = "identity", fill = "dodgerblue4")+
    geom_text(aes(label=HR))
  b

  #histogram
  ggplot(data = d, aes(d$RBI))+geom_histogram()
  
  #change the bin size
  ggplot(data = d, aes(d$RBI))+geom_histogram(binwidth = 10)
  
  #change the number of bins
  ggplot(data = d, aes(d$RBI))+geom_histogram(bins = 50)
  b
  
  
  
  
  
  
  