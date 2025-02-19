#structure of bath processing
#take each

list.files(path = "e3_batch_data/", recursive = F)
dfiles <- list.files(path = "e3_batch_data/", full=T, pattern = "ISIIS")


#red the data in
d<- read.tables(file = dfiles(1), header = T, sep = "\t", skip = 10, stringAsfactors = F, filesEncoding="ISO-8859-1", encoding = "UTF-8", quote = "\"")
d <- read.table(file = dfiles(1) sep="\t", skip=10, header=TRUE, fileEncoding="ISO-8859-1",
                stringsAsFactors=FALSE, quote="\"", check.names=FALSE, encoding="UTF-8",
                na.strings="9999.99")

#
head.d <- names (d)
head.d <- str_replace (head.d, pattern = "\\(.*\\)", replacement = "")



assign names
names(d) <- head

# create a proper date + time format #get a nice date Time format
date <- scan(dfiles(1), what="character", skip=1, nlines=1, quiet=TRUE)
date <- date[2]
mm <- str_sub(string = date, start=1, end = 2)
#you can use str_sub to work either left to right (above; line 25) or right to left (below; line 27)
dd <- str_sub(date,4,5)
dd <- as.numeric(dd)
yy <- str_sub(date,7,8)

# shift by one day when we cross midnight
d$hour <- as.numeric(str_sub(d$time,1,2))
d$date <- date
d$dateTime <- str_c(d$date, d$time, sep=" ")
d$dateTime <- as.POSIXct(strptime(d$dateTime, format="%m/%d/%y %H:%M:%OS", tz="America/New_York"))

d$dateTime <- d$dateTime - time.zone.change * 3600

d$date <-

# code in a transect number. Use the file name as a dummy variable for transect number. Will assign proper transect number later in the pipeline.
d$transect <- basename(file)

# reformat the lat and long in decimal degrees.
names(d)[names(d)=="long"] <- "lon"
d$lat <- to.dec.2015(d$lat)
d$lon <- to.dec.2015(d$lon)

d$lon <- -d$lon

# columns that are all zero are not possible. They are actually missing data. Detect them
totCol <- colSums(d[llply(d, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
d[,allZeroCols] <- NA # replace the content with NAs

# rename some columns
names(d)[names(d)=="horizontal.vel.in.water"] <- "horizontal.vel"
names(d)[names(d)=="irrandiance"] <- "irradiance"

# keep only interesting data
d <- d[,c("transect", "dateTime","depth", "lat", "lon", "temp", "salinity", "pressure", "fluoro", "oxygen", "irradiance", "heading", "horizontal.vel", "vertical.vel", "pitch", "vol.imaged")]

return(d)

}, .inform = T, .progress="text")


# remove adply crap
phy <- phy[,-1]