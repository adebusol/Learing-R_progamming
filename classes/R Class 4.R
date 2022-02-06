#read csv    ---- 
 #press tab to see a list of files after using this "". Path or file can be used interchangebly to find a file

c <- read.csv(file = "Lab4_import_save/lg_camera_class_groupings_20170113_phylo_orderd.csv", header=T,stringsAsFactors = FALSE)
head(c)
tail(c)
str(c)       
#str - structure of C to see if R made it them look like factors, since it did then we have to use the stringasfactors function and set it to false

#read excel -------
install.packages("readxl")
library(readxl)

e <- read_xlsx(path = "Lab4_import_save/NameTranslator_table20140330.xlsx")

#read txt files -----

i <- read.table( header = T, skip =10, sep = "\t", file = "Lab4_import_save/ISIIS_physical/ISIIS201405281105.txt")

#separator element to separate the rows and columns, "sep", sep = ","

#reading in "R" data files -------
load("fish_data.Rdata")

#grab the first five rows of all the columns
ti <- fish [1:5]
td <-

  #right to left. count backwards so its negative and you start counting backwards,
  
#reassigning ccolumn names

  fish
  

