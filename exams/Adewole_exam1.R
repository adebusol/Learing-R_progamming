list.files()

list.files(path = "exercises/", recursive = T)

a <- c(1,2,3, list("red", 4, "blue"), recursive = F)
b <- c(1,2,3, list("red", 4, "blue"), recursive = T)

#importing different file types-----------

#EXCEL files ----------
library(tidyverse)
library(readxl)

x <- read_xlsx(path = "Intro2RProgam/data_sets/Aurelia_SEAMAP_2012-2018_30minCell.xlsx")
y <- readxl::read_excel(path = "Intro2RProgam/data_sets/Aurelia_SEAMAP_2012-2018_30minCell.xlsx")

#csv files ---------

c <- read.csv(file = "Intro2RProgam/data_sets/lg_camera_class_groupings_20170113_phylo_orderd.csv", 
              stringsAsFactors = F)

c2 <- read.csv(file="Intro2RProgam/data_sets/Aurelia_SEAMAP_2012-2018_30minCell.csv", stringsAsFactors = F)


# text files ----------

t <- read.table(file = "Intro2RProgam/data_sets/aurelia_15minCell_statareas.txt", 
                sep = ",", header = T, stringsAsFactors = F)

# reading in 'R' data files ----

load("Intro2RProgam/data_sets/fish_data.Rdata")


# ----- strings

ti <- fish[1:5,]
tid <- ti[,c("transect.id")]
#or
tid2 <- fish[1:5,]$transect.id
tid2

s <- str_split_fixed(string = tid, pattern = "-", n = 3)

s2 <- str_split_fixed(string = tid, pattern = "-", n = 2)

s3 <- str_split_fixed(string = tid, pattern = "-", n = 4)

s4 <- str_split(string = tid, pattern = "-", simplify = T)

ts <- gsub(pattern = "-", replacement = "/", x = tid)

#extract the first 5 characters from a string

ss <- str_sub(string = tid, start = 1, end = 5)
sb <- str_sub(string = tid, start = -10L, end = -6L)
sb2 <- str_sub(string = tid, start = -2L, end = -1L)

#no designation of an integer (i.e, "2L") works okay too!
sb3 <- str_sub(string = tid, start = -2, end = -1)

#dates ------------
 
corona.time <- "2020-09-15 10:04:05"

g <- strptime(x = corona.time, format = "%Y-%m-%d %H:%M:%OS", tz="America/Chicago")
g <- as.POSIXct(g)

#do in one step---
g <- as.POSIXct(strptime(x = corona.time, format = "%Y-%m-%d %H:%M:%OS", tz="America/Chicago"))

#reassigning column names
fish$taxa <- fish$group #made a new column with fish$group data
fish$group <- NULL #removed the old column "fish$group"

fish <- rename(.data = fish, goup = taxa) #new column name goes first, then old name


