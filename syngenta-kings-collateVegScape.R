#This code was created by Chanuki Seresinhe (cseresinhe@turing.ac.uk) during the Data Study Group held at the Alan Turing Institute in 2016. As this code was written in a rush, please audit this code before using it!

setwd("/Volumes/Chanuki_Data_Files/Sygenta/California-Counties")
library(rgdal)
library(raster)

# Create Shapefile of Farms only
# Farm data downloaded from http://www.conservation.ca.gov/dlrp/fmmp/Pages/Kings.aspx

"kings"
counties <- c("fresno","kern","madera","merced","sanjoaquin","stanislaus","tulare")

for (county in counties){


county_map <- readOGR(dsn = "CA_maps", layer = "kings2012")
#head(county_map@data)
#table(county_map@data$polygon_ty)

#Prime Farmland (P)
#Farmland of Statewide Importance (S)
#Unique Farmland (U)
#Farmland of Local Importance (L)

farm_codes <- c("P","U","S","L")

kings_farm_map <- kings_map[kings_map@data$polygon_ty %in% farm_codes,]
writeOGR(kings_farm_map,"CA_maps","kings2012-farm","ESRI Shapefile")


}



#Upload Farm Shapefile to VegScape and download data

#Read in data from VegScape GeoTiffs

Kings_NDVI_Tifs <- list.files("data/kings_NDVI_DATA_CACHE",
                            full.names = TRUE,
                            pattern = ".tif$")

# Create a time series raster stack
Kings_NDVI_stack <- stack(Kings_NDVI_Tifs)

#Plot 4 of the stacks
plot(Kings_NDVI_stack[[1:4]], 
     nc = 4)

#Note that cells which shouldn't be coloured in are currently green. These are the areas which were outside the Farm shapefile. These need to be set to NA

#We could run this but it seems to require a lot of RAM:
#Kings_NDVI_stack[ Kings_NDVI_stack[] ==255 ] <- NA  #Note that this process take a lot of time!

#Could also do this per stack image:
#Kings_NDVI_stack[[1]][ Kings_NDVI_stack[[1]] ==255 ] <- NA 

#Instead, we process each file one by one:

file <- Kings_NDVI_Tifs[1]

for (file in Kings_NDVI_Tifs){
  print(file)
  Kings_NDVI_tif <- raster(file)
  Kings_NDVI_tif[Kings_NDVI_tif==255]<-NA
  new_filename <- gsub("NDVI_DATA_CACHE","NDVI_DATA_FIXED",file)
  
  writeRaster(Kings_NDVI_tif, filename=new_filename, format="GTiff", overwrite=TRUE)
  
}


#Reload fixed GeoTifs
Kings_NDVI_Tifs <- list.files("data/kings_NDVI_DATA_FIXED",
                              full.names = TRUE,
                              pattern = ".tif$")

# Create a time series raster stack
Kings_NDVI_stack <- stack(Kings_NDVI_Tifs)

#Plot 4 of the stacks
plot(Kings_NDVI_stack[[1:4]], 
     nc = 4)

#We need to rescale the NDVI values
new_scale <- rescale(  seq(1:254) , 0:1)

for (j in 1:length(Kings_NDVI_Tifs)){
  print(j)
for (i in 1:255){
  Kings_NDVI_stack[[j]][ Kings_NDVI_stack[[j]] ==i] <- new_scale[i]
}
}


plot(Kings_NDVI_stack[[326]])


# calculate mean NDVI for each raster
avg_Kings_NDVI <- cellStats(Kings_NDVI_stack,mean)

# convert output array to data.frame
avg_Kings_NDVI_df <- as.data.frame(avg_Kings_NDVI)
names(avg_Kings_NDVI_df) <- "meanNDVI"

#extract dates
avg_Kings_NDVI_df$date<- unlist(lapply(strsplit(x = row.names(avg_Kings_NDVI_df),"_",fixed=TRUE),"[",2))
avg_Kings_NDVI_df$date <- paste(substring(avg_Kings_NDVI_df$date,1,4),substring(avg_Kings_NDVI_df$date,5,6) ,substring(avg_Kings_NDVI_df$date,7,8),sep="-")
avg_Kings_NDVI_df$date  <- as.Date(avg_Kings_NDVI_df$date )
head(avg_Kings_NDVI_df)

library(ggplot2)
ggplot(avg_Kings_NDVI_df, aes(date, meanNDVI)) + geom_line(colour = "SpringGreen4") +
  ggtitle("Daily mean NDVI for all farms in Kings County California") +
  xlab("Dates") + ylab("Mean NDVI") +
  theme(text = element_text(size=20))
ggsave("Kings_NDVI_Plot.png")




