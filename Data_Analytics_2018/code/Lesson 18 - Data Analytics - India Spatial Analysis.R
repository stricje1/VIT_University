# ggmap: extends the plotting package ggplot2 for maps 
# rgdal: R's interface to the popular C/C++ spatial data processing library gdal 
# rgeos: R's interface to the powerful vector processing library geos 
# maptools: provides various mapping functions 
# dplyr and tidyr: fast and concise data manipulation packages 
# tmap: a new packages for rapidly creating beautiful maps
# install.packages(x) 
# install.packages(c("rgdal", "maptools", "dplyr", "tidyr", "tmap", "tmaptools", "rgeos"))

# Open the existing "Creating-maps-in-R" project using File -> Open File... on the top menu. 
# The first file we are going to load into R Studio is the "london_sport" shapefile located in the "data" folder of the project. 
# It is worth looking at this input dataset in your file browser before opening it in R. 
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(rgdal) 
library(rgeos)
library(ggplot2)
library(ggmap)
library(raster)

## Import India shape file (.SHP)

mydir<-"C:/Users/jeff/Documents/Crime Analysis/India_data"
ind <- readOGR(dsn = mydir, layer = "INDIA") 

## Import India Population Grid Files (.GRD)

ind_pop<- raster("C:/Users/jeff/Documents/Crime Analysis/India_data/ind_pop.grd")
ind_msk_pop<- raster("C:/Users/jeff/Documents/Crime Analysis/India_data/ind_msk_pop.grd")

# In the second line of code above the readOGR function is used to load a shapefile and assign it to a new spatial object called "lnd"; short for London.
# readOGR is a function which accepts two arguments: dsn which stands for "data source name" and speci???es the directory in which the ???le is stored, and layer which speci???es the ???le name (note that there is no need to include the ???le extention .shp). 
# The file we assigned to the lnd object contains the population of London Boroughs in 2001 and the percentage of the population participating in sporting activities. 

# Take a look at the output created (note the table format of the data and the column names). 
# There are two important symbols at work in the above block of code: the @ symbol in the ???rst line of code is used to refer to the data slot of the lnd object. 
# The $ symbol refers to the Partic_Per column (a variable within the table) in the data slot, which was identi???ed from the result of running the ???rst line of code. 

# The head function in the ???rst line of the code above simply means "show the ???rst few lines of data" (try entering head(lnd@data), see ?head for more details). 
# The second line calculates ???nds the mean sports participation per 100 people for zones in London. 

# Spatial objects like the lnd object are made up of a number of different slots, the key slots being @data (non geographic attribute data) and @polygons (or @lines for line data). 
# The data slot can be thought of as an attribute table and the geometry slot is the polygons that make up the physcial boundaries. 
# Specific slots are accessed using the @ symbol. 
# Let's now analyse the sport object with some basic commands: 
head(ind@data, n = 2) 

#  To check the classes of all the variables in a spatial dataset, you can use the following command: 
sapply(ind@data, class) 
# This shows that, unexpectedly, Pop_2001 is a factor. 
# We can coerce the variable into the correct, numeric, format with the following command: 

# To explore lnd object further, try typing 
nrow(ind)
ncol(ind)
ind@proj4string

# Now we have seen something of the structure of spatial objects in R, let us look at plotting them. 
# Note, that plots use the geometry data, contained primarily in the @polygons slot. 
plot(ind)
plot(ind_pop)

raster::extent(ind)
plot(ind, xlim = c(68.1202, 97.41516), axes = TRUE) 

## Apply Plotting Enhancements

map <- ggplot() + geom_polygon(data = ind, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map + geom_raster(ind_msk_pop)
map + theme_void()
shp_df <- broom::tidy(ind, region = "ST_NAME")
lapply(shp_df, class)
head(shp_df)

## Plot Map with Enhancements

map <- ggplot() + geom_polygon(data = ind, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void() 

## Setup Bounding Box for Indian Map

subscr<-data.frame(lon=c(79.1,79.15,79.2),lat=c(12.85,12.9,12.95), pop=c(58,12,150))
coordinates(subscr)<-~lon+lat
proj4string(subscr)<-CRS("+init=epsg:4326")
lon <- c(78.9,79.3)
lat <- c(12.7,13.2)
map1<-get_map(location = c(lon[1], lat[2], lon[2], lat[1]), zoom=11)

## Setup Bounding Box Plot

p <- ggmap(map1) +
  geom_point(data = as.data.frame(subscr), aes(x = lon, y = lat, size=pop),
             colour = "darkgreen") +
  theme_bw()

## Plot Bounding Box Map

print(p)


## Plot India Map with Grey Fill

- requires (rgeos)

plot(ind, col = "grey")

## Find Geographic centroids for Indian States

cent_ind <- gCentroid(ind[ind$ST_NAME == "Tamil Nadu",]) 
points(cent_ind, cex = 3)
# set 10 km buffer
ind_buffer <- gBuffer(spgeom = cent_ind, width = 10000)
# method2 of subsetting selects only points within the buffer
ind_cents <- SpatialPoints(coordinates(ind),
                           proj4string = CRS(proj4string(ind))) # create spatialpoints
sel <- ind_cents[ind_buffer,] # select points inside buffer
points(sel) # show where the points are located
ind_central <- ind[sel,] # select zones intersecting w. sel

## Plot with Fill and Borders

plot(ind_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(ind_buffer, add = T, border = "red", lwd = 2)

## Selecting Quadrants for India

- The code below should help understand the way spatial data work in R. 
- Find the centre of the India area 

lat <- coordinates(gCentroid(ind))[[1]] 
lng <- coordinates(gCentroid(ind))[[2]]

## Test whether or not a coordinate is east or north of the centre 

east <- sapply(coordinates(ind)[,1], function(x) x > lat) 
north <- sapply(coordinates(ind)[,2], function(x) x > lng)
lnd@data$quadrant[east & north] <- "northeast" 

## Test whether or not a coordinate is east or north of the centre 

east <- sapply(coordinates(ind)[,1], function(x) x > lat) 
south <- sapply(coordinates(ind)[,2], function(x) x < lng)
lnd@data$quadrant[east & south] <- "southeast" 


# Test whether or not a coordinate is east or north of the centre 

west <- sapply(coordinates(ind)[,1], function(x) x < lat) 
north <- sapply(coordinates(ind)[,2], function(x) x > lng)
lnd@data$quadrant[west & north] <- "northwest" 

# Test whether or not a coordinate is east or south of the centre

west <- sapply(coordinates(ind)[,1], function(x) x < lat) 
south <- sapply(coordinates(ind)[,2], function(x) x < lng)
ind@data$quadrant[west & south] <- "southwest" 

## Plot Quadrants with Different Fill Collors

plot(ind)
plot(ind[east & north,], col = "red", add = TRUE) 
plot(ind[east & south,], col = "green", add = TRUE) 
plot(ind[west & north,], col = "blue", add = TRUE) 
plot(ind[west & south,], col = "yellow", add = TRUE)
llgridlines(ind, lty= 3, side ="EN", offset = -0.5)

## Plot SE Quadrants with Red Fill Collors

ind$quadrant[east & north] <- "northest" 
ind$quadrant[!east & !north] <- "southwest"
ind$quadrant[!east & north] <- "Northwest"
plot(ind)
plot(ind[east & !north,], add = TRUE, col = "red" )
llgridlines(ind, lty= 3, side ="EN", offset = -0.5)

# Alongside visualisation and interrogation, a GIS must also be able to create and modify spatial data. 
# R's spatial packages provide a very wide and powerful suite of functionality for processing and creating spatial data. 

## Creating New R Object

- R objects can be created by entering the name of the class we want to make. 
- vector and data.frame objects for example, can be created as follows: 
vec <- vector(mode = "numeric", length = 3) 
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4)) 

## Check the class of these new objects using class(): 

class(vec) 
class(df) 

## Creating New Spatial Data

- The same logic applies to spatial data. 
- The input must be a numeric matrix or data.frame: 

sp1 <- SpatialPoints(coords = df) 

## COmment on reating Spatial Data

- We have just created a spatial points object, one of the fundamental data types for spatial data. 
- (The others arelines, polygons and pixels,which can be created by SpatialLines,SpatialPolygons and SpatialPixels, respectively.) 
- Each type of spatial data has a corollary that can accepts non-spatial data, created by adding DataFrame. 
- SpatialPointsDataFrame(), for example, creates points with an associated data.frame. 
- The number of rows in this dataset must equal the number of features in the spatial object, which in the case of sp1 is 3. 

## Add Data to an Existing Spatial Data File

- The code below extends the pre-existing object sp1 by adding data from df. 
- To see how strict spatial classes are, try replacing df with mat in the above code: it causes an error. 
- All spatial data classes can be created in a similar way, although SpatialLines and SpatialPolygons are much more complicated (Bivand et al. 2013). 
- More frequently your spatial data will be read-in from an externally-created file, e.g. using readOGR(). 
- Unlike the spatial objects we created above, most spatial data comes with an associate 'CRS'.

class(sp1) 
spdf <- SpatialPointsDataFrame(sp1, data = df) 
class(spdf) 

## Projections: setting and transforming CRS in R 

- The Coordinate Reference System (CRS) of spatial objects defines where they are placed on the Earth's surface. 
- You may have noticed 'proj4string 'in the summary of lnd above: the information that follows represents its CRS. 
- Spatial data should always have a CRS. 
- If no CRS information is provided, and the correct CRS is known, it can be set as follow: 

proj4string(ind) <- NA_character_ # remove CRS information from ind 
proj4string(ind) <- CRS("+init=epsg:27700") # assign a new CRS 

## Comments on CRS Changes

- R issues a warning when the CRS is changed. 
- This is so the user knows that they are simply changing the CRS, not reprojecting the data. 
- An easy way to refer to different projections is via EPSG codes.
- Under this system 27700 represents the British National Grid. 
- 'WGS84' (epsg:4326) is a very commonly used CRS worldwide. 

## Find EPSG Codes

- The following code shows how to search the list of available EPSG codes and create a new version of lnd in WGS84:3 

proj4string(ind) 

## Convert the Coordinates

- spTransform converts the coordinates of ind into the widely used WGS84CRS.

CRS("+init=epsg:4326")  
CRS("+init=epsg:3785")  
EPSG <- make_EPSG() # create data frame of available EPSG codes 
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code 
ind84 <- spTransform(ind, CRS("+init=epsg:3785")) # reproject 
ind84$
  
##Saving CRS Formats

- Now we've transformed lnd into a more widely used CRS, it is worth saving it. 
- R stores data effciently in .RData or .Rds formats. 
- The former is more restrictive and maintains the object's name, so we use the latter

saveRDS(object = lnd84, file = "C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/lnd84.Rds") 

## Reset INDIA Shapefile

- To reaffirm our starting point, let's re-load the "INDIA" shapefile as a new object and plot it: 
- Create new object called "ind" from "INDIA" shapefile 

ind <- readOGR(dsn = mydir, "INDIA") 
plot(ind) 
nrow(ind) 


##Joing Non-Spatial and Spatial data

- The non-spatial data we are going to join to the ind object contains records of population in India. 
- This is stored in a comma separated values (.csv) file called "India_pop_2011". 
- If you open the file in a separate spreadsheet application first, we can see each row represents a single city's population. 
- We are going to use a function called aggregate to aggregate the population at the city, ready to join to our spatial ind dataset. 
- A new object called india_pop is created to store this data. 


## India 2011 Population

india_pop <- read.csv("C:/Users/jeff/Documents/Crime Analysis/India_data/India_pop_2011.csv", stringsAsFactors = FALSE)
head(india_pop) # retail price information about key commodities by city


## Joining Tables

- We use left_join because we want the length of the data frame to remain unchanged, with variables from new data appended in new columns (see ?left_join). 
- The *join commands (including inner_join and anti_join) assume, by default, that matching variables have the same name. 
- Here we will specify the association between variables in the two data sets: 


head(ind$ST_NAME) # dataset to add to (results not shown) 
head(india_pop$ST_NAME) # the variables to join
# head(left_join(lnd@data, crime_ag)) # test it works 
ind@data <- left_join(ind@data, india_pop, by = c('ST_NAME' = 'ST_NAME')) 

## Basic Scatterplots

with(india_pop, scatter.smooth(X, Y))

attach(india_pop)
plot(X, Y,  labels=india_pop$Centre)


##India.shp Map Rescaled with lat/long Grids

require(ggplot2)
map <- ggplot() + geom_polygon(data = ind, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map + geom_raster(ind_msk_pop)
map + theme_void()
shp_df <- broom::tidy(ind, region = "ST_NAME")
lapply(shp_df, class)
head(shp_df)

## India.shp Map with States

map <- ggplot() + geom_polygon(data = ind, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void() 

## India.shp Map with Cities

ggplot(india_pop, aes(X,Y)) + geom_point() + geom_text(aes(label=Centre)) + 
  geom_polygon(data = ind, aes(x = long, y = lat, group = group), colour = "black", fill = NA)



## India Map with Sates (colored)

library(tmap) # load tmap package (see Section IV) 
qtm(ind, "Price per Kg") # plot the basic map 

library(tmap) 
library(tmaptools)
ind_wgs = spTransform(ind, CRS("+init=epsg:4326"))
osm_tiles = read_osm(bbox(ind_wgs))
ind_wgs$ST_NAME <- ind$ST_NAME 
tm1<-tm_shape(osm_tiles) +
  tm_raster() +
  tm_shape(ind_wgs) +
  tm_fill("ST_NAME", legend.show=TRUE) +
  tm_layout(legend.position = c(0.99,0.02))
tm1

tm2<-tm_shape(ind_msk_pop) +
  tm_raster() 
tm2

## Define Popups for Leaflet Map

india_pop$popup <- paste("<b>Rank #: </b>", india_pop$Rank, 
                         "<br>", "<b>City: </b>", india_pop$Centre,
                         "<br>", "<b>Region: </b>", india_pop$Region,
                         "<br>", "<b>State: </b>", india_pop$ST_NAME,
                         "<br>", "<b>Coountry: </b>", india_pop$Country,
                         "<br>", "<b>Date: </b>", india_pop$Date,
                         "<br>", "<b>Population: </b>", india_pop$Pop_2011,
                         "<br>", "<b>Longitude: </b>", india_pop$X,
                         "<br>", "<b>Latitude: </b>", india_pop$Y)

## India Leaflet Population Map

library(leaflet)

leaflet(india_pop, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~X, lat = ~Y, popup = india_pop$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
