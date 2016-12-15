#This code was created by Chanuki Seresinhe (cseresinhe@turing.ac.uk) during the Data Study Group held at the Alan Turing Institute in 2016. As this code was written in a rush, please audit this code before using it!

setwd("/Volumes/Chanuki_Data_Files/Sygenta/California-Counties")
library(rgdal)
library(raster)
library(maptools)
library(sp)
library(spdep)
library(plotrix)


# Create Shapefile of Farms only
# Farm data downloaded from http://www.conservation.ca.gov/dlrp/fmmp/Pages/county_info.aspx


counties <- c("kern","kings","merced","sanjoaquin","stanislaus","tulare")



#Prime Farmland (P)
#Farmland of Statewide Importance (S)
#Unique Farmland (U)
#Farmland of Local Importance (L)

farm_codes <- c("P","U","S","L")

for (county in counties){

print(county)
map_name <- paste0(county,"2012")
county_map <- readOGR(dsn = "CA_maps", layer = map_name)
#head(county_map@data)
#table(county_map@data$polygon_ty)

map_file_name <- paste0(county,"2012-farm")
county_farm_map <- county_map[county_map@data$polygon_ty %in% farm_codes,]

county_farm_map@data$farm <- rep("farm",nrow(county_farm_map@data))

county_farm_map_sm <- unionSpatialPolygons(county_farm_map, ID=county_farm_map@data$farm)

farm<-NULL
for (i in 1:length(county_farm_map_sm)){
  dt<-county_farm_map_sm@polygons[[i]]@ID
  farm<-rbind(farm, dt)
}

rownames(farm)<-farm[,1]
farm<-data.frame(farm)



new_county_map<-  SpatialPolygonsDataFrame(county_farm_map_sm,data=farm)




writeOGR(new_county_map,"CA_maps",map_file_name,"ESRI Shapefile")


}

###################################################################################################################

#Upload Farm Shapefile to VegScape and download data

###################################################################################################################
#Read in data from VegScape GeoTiffs



for (county in counties) {
NDVI_folder <- paste0("data/",county,"_NDVI_DATA_CACHE")

NDVI_Tifs <- list.files(NDVI_folder,
                            full.names = TRUE,
                            pattern = ".tif$")

# Create a time series raster stack
NDVI_stack <- stack(NDVI_Tifs)


for (file in NDVI_Tifs){
  print(file)
  NDVI_tif <- raster(file)
  NDVI_tif[NDVI_tif==255]<-NA
  new_filename <- gsub("NDVI_DATA_CACHE","NDVI_DATA_FIXED",file)
  
  writeRaster(NDVI_tif, filename=new_filename, format="GTiff", overwrite=TRUE)
  
}
}

for (county in counties) {
  print(county)

NDVI_folder <- paste0("data/",county,"_NDVI_DATA_FIXED")
#Reload fixed GeoTifs
NDVI_Tifs <- list.files(NDVI_folder,
                              full.names = TRUE,
                              pattern = ".tif$")

# Create a time series raster stack
NDVI_stack <- stack(NDVI_Tifs)


#We need to rescale the NDVI values
new_scale <- rescale(  seq(1:254) , 0:1)

for (j in 1:length(NDVI_Tifs)){
  print(j)
for (i in 1:255){
  NDVI_stack[[j]][ NDVI_stack[[j]] ==i] <- new_scale[i]
}
}




# calculate mean NDVI for each raster
avg_NDVI <- cellStats(NDVI_stack,mean)

# convert output array to data.frame
avg_NDVI_df <- as.data.frame(avg_NDVI)
names(avg_NDVI_df) <- "meanNDVI"

#extract dates
avg_NDVI_df$date<- unlist(lapply(strsplit(x = row.names(avg_NDVI_df),"_",fixed=TRUE),"[",2))
avg_NDVI_df$date <- paste(substring(avg_NDVI_df$date,1,4),substring(avg_NDVI_df$date,5,6) ,substring(avg_NDVI_df$date,7,8),sep="-")
avg_NDVI_df$date  <- as.Date(avg_NDVI_df$date )
head(avg_NDVI_df)


filename <- paste0("data/",county,"_NDVI.csv")
write.csv(avg_NDVI_df,filename)

}



save.image("Counties.Rdata")



###################################################################################################################
#Extract weather data

weather_data <- read.csv("data/EightCalifonianCountiesWeatherData.csv", stringsAsFactors=FALSE)
weather_variables <- names(table(weather_data$sername))

weather_data$date<- as.Date(weather_data$date)
class(weather_data$date)

table(weather_data$Loc)
library(car)

weather_data$Loc<-recode(weather_data$Loc,
                         "'Fresno County ,  California'='fresno';
                         'Kern County ,  California'='kern';
'Kings County ,  California'='kings';
'Madera County ,  California'='madera';
'Merced County ,  California'='merced';
'San Joaquin County ,  California'='sanjoaquin';
'Stanislaus County ,  California'='stanislaus';
'Tulare County ,  California'='tulare';
 ")
 

ALL_county_data <- data.frame(meanNDVI=numeric(),
                              date=as.Date(character()),
                              county=character(), 
                              Evapotranspiration_mm_DailySum=numeric(), 
                              GlobalRadiation_Whm2_DailySum=numeric(), 
                              PrecipAmount_mm_DailySum=numeric(),     
                              RelHumidity_DailyAvg=numeric(), 
                              RelHumidity_DailyMax =numeric(),         
                              RelHumidity_DailyMin=numeric(), 
                              TempAir_C_DailyAvg=numeric(),            
                              TempAir_C_DaytimeMax=numeric(), 
                              TempAir_C_NighttimeMin =numeric(),        
                              Windspeed_ms_DailyAvg=numeric(), 
                              WindspeedMax_ms_DailyMax=numeric(), 
                              stringsAsFactors=FALSE)                   





for (county in counties){   
print(county)

NDVI_data <- read.csv(paste0("data/",county,"_NDVI.csv"),stringsAsFactors=FALSE)
NDVI_data$date <- as.Date(NDVI_data$date)
NDVI_data$X <- NULL
#head(NDVI_data)
NDVI_data$county <- rep(county, nrow(NDVI_data))



county_weather_data <- subset(weather_data, weather_data$Loc==county)
#head(county_weather_data)


for (variable in weather_variables){
  print(variable)
  
  county_weather_subset <-  subset(county_weather_data, sername==variable)
  NDVI_data[[variable]]<-county_weather_subset$value[match(NDVI_data$date,county_weather_subset$date)]
 
}
ALL_county_data<- rbind(ALL_county_data,NDVI_data)
print(nrow(ALL_county_data))

}


write.csv(ALL_county_data, "data/ALL_county_data.csv")

###################################################################################################################
#Aggregate data by County and add Yield Data

ALL_county_data <- read.csv("data/ALL_county_data.csv")
ALL_county_data_subset <- ALL_county_data[c("meanNDVI","county",weather_variables)]


data_by_county<-aggregate(ALL_county_data_subset, by=list(ALL_county_data$county), FUN=mean, na.rm=TRUE)
data_by_county$county <- NULL
names(data_by_county)[names(data_by_county)=="Group.1"] <- "county"


#Yield data from grain-corn-2012.pdf gathered from https://www.nass.usda.gov/Quick_Stats/
planted <- c(34300,61500, 81000,93500,45600,130000)
harvested <- c(5000,7600, 11500,53900,2400,9600)
yield <- c(2.83,4.49, 5.63,4.87,3.76,6.7)

#subset data by county as we don't have yield data for Fresno or Madera

data_by_county$planted <- NA
data_by_county$harvested <- NA
data_by_county$yield <- NA

for (i in seq(1:length(planted))){
  county = counties[i]
  print(i)
  print(county)
  data_by_county[which(data_by_county$county==county),]$planted <- planted[i]
  data_by_county[which(data_by_county$county==county),]$harvested <- harvested [i]
  data_by_county[which(data_by_county$county==county),]$yield<- yield[i]
}

data_by_county


###################################################################################################################
# Spatial analysis example
# Note that to run this you actually need data from a lot more counties and this example does not have enough data points for a good analysis and simply shows what is possible

county_map <- readOGR(dsn="County_map","CaliforniaCounty") 
county_map@data$NAME<-recode(county_map@data$NAME,
                         "'Kern'='kern';
                         'Kings'='kings';
'Merced'='merced';
'San Joaquin'='sanjoaquin';
'Stanislaus'='stanislaus';
'Tulare'='tulare';
 ")

county_map@data$NAME


county_map@data=data.frame(county_map@data,data_by_county[match(as.character(county_map@data$NAME), as.character(data_by_county$county)),])
head(county_map@data)


county_map <- county_map[which(county_map@data$county %in% counties), ]


county_map <- county_map[!is.na(county_map@data$yield), ]
county_map.nb <- poly2nb(county_map, row.names = county_map@data$NAME)
 
county_map.listw <- nb2listw(county_map.nb, zero.policy = TRUE, style="S")



model_formula <- yield ~ meanNDVI + Evapotranspiration_mm_DailySum + GlobalRadiation_Whm2_DailySum + PrecipAmount_mm_DailyAvg+ RelHumidity_DailyAvg+ TempAir_C_DailyAvg + Windspeed_ms_DailyAvg + planted 

model_formula <- yield ~ meanNDVI + PrecipAmount_mm_DailySum+ Windspeed_ms_DailyAvg + planted 



# First check result on a linear regression model
model.lm <- lm(model_formula , county_map@data)
summary(model.lm)


# Then, check for Morans I
moran.test.ind <- lm.morantest(model.lm , county_map.listw, zero.policy = TRUE)
out<-capture.output(moran.test.ind)
cat(out,file="results/MoransI.txt",sep="\n",append=FALSE)

# If the results show spatial autocorrelation, then run a Spatial Autoregressive Model model as you won't be able to use a linear regression model
# See http://www.people.fas.harvard.edu/~zhukov/Spatial6.pdf for more information

model.SAR<- spautolm(model_formula, data = county_map@data, listw = county_map.listw, family = "SAR")

sink("results/SAR_model_output.txt", split=T)
summary(model.SAR)
sink()


#plots
library(classInt)
library(ggplot2)
library(RColorBrewer) 

blank_theme = theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), panel.background =  element_rect(fill = NA, colour = NA), axis.line = element_blank(), axis.ticks = element_blank(), axis.text= element_blank(), axis.title= element_blank(), legend.text=element_text(size=12), legend.title=element_text(size=12))


county_map_geom<- fortify(county_map, region="NAME")
county_map_geom<- merge(county_map_geom, county_map@data, by.x="id", by.y="NAME")

county_map_geom <- county_map_geom[order(county_map_geom$order),]
head(county_map_geom )

jenks <- classIntervals(county_map_geom$meanNDVI, 5, style="quantile")
measures <- cut(county_map_geom$meanNDVI, breaks = c(jenks$brks), dig.lab = 2)

pdf("results/Map_Mean_DVI.pdf")

Map <- ggplot() + coord_equal() + labs(fill = "Average Mean DVI")
Map <- Map + scale_fill_brewer(palette="BuPu")+ blank_theme +
  geom_polygon(data=county_map_geom, colour="grey", aes(long, lat,  group=group, fill=measures)) +
  geom_text(data=cnames, aes(long, lat, label = id), size=6)  +
  coord_map()
Map
dev.off()


jenks <- classIntervals(county_map_geom$yield, 5, style="quantile")
measures <- cut(county_map_geom$yield, breaks = c(jenks$brks), dig.lab = 2)

pdf("results/Map_Yield.pdf")

Map <- ggplot() + coord_equal() + labs(fill = "Yield")
Map <- Map + scale_fill_brewer(palette="BuPu")+ blank_theme +
  geom_polygon(data=county_map_geom, colour="grey", aes(long, lat,  group=group, fill=measures)) +
  geom_text(data=cnames, aes(long, lat, label = id), size=6)  +
  coord_map()
Map
dev.off()

jenks <- classIntervals(county_map_geom$PrecipAmount_mm_DailySum , 5, style="quantile")
measures <- cut(county_map_geom$PrecipAmount_mm_DailySum, breaks = c(jenks$brks), dig.lab = 2)

head(county_map_geom)

cnames <- aggregate(cbind(long, lat) ~ id, data=county_map_geom, 
                    FUN=function(x)mean(range(x)))

pdf("results/Map_Precipitation.pdf")

Map <- ggplot() + coord_equal() + labs(fill = "Precipitation")

Map <- Map + scale_fill_brewer(palette="BuPu")+ blank_theme +
  geom_polygon(data=county_map_geom, colour="grey", aes(long, lat,  group=group, fill=measures)) +
  geom_text(data=cnames, aes(long, lat, label = id), size=6)  +
  coord_map()
Map
dev.off()







