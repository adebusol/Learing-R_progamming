#read netCDF files---------

#install.packages()
#after installing make sure you load the package by typing 'library()'

#setwd- set working directory

d<- nc_open(filename = "e3_nc_files/T20201532020182.L3m_MO_CHL_chl_ocx_9km.nc")

#print the file's metadata to the working directory

sink('e3_nc_files/T20201532020182.L3m_MO_CHL_chl_ocx_9km.nc')
print(d)
sink()

#get the lat and Lon coordinates for each pixel in the raster file
lon <- ncvar_get(d, "lon")
lat <- ncvar_get(d, "lat", verbose = F)

#read in the chlorophll values and store the, in an array
chl_ocx.array <- ncvar_get(d, "chl_ocx") # store the data in a 2-dimensional array
dim(chl_ocx.array) #2 dimensions are returned

#get the 'no data' value
fillvalue <- ncatt_get(d, "lat", "_fillvalue")
print(fillvalue)

#close the nc file
nc_close(d)

#let's plot the dat, but first we need to clean it ip a bit
## st the fill values to NAs

chl_ocx.array[chl_ocx.array == fillvalue$value]

chl_ocx.array[chl_ocx.array == fillvalue$value] <- NA

lat[lat == fillvalue.lat$value] <- NA

#transform the values so they show up better on the plot
chl_ocx.array.log <- log(chl_ocx.array)
chl_ocx.array.log <- log(chl_ocx.array, base= 10)

library(raster)
r <- raster(t(chl_ocx.array.log), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(r)








