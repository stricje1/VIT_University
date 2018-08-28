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
mydir<-"C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data"
lnd <- readOGR(dsn = mydir, layer = "london_sport") 
lnd_b <- readOGR(dsn = mydir, layer = "LondonBoroughs") 
lnd_s <- readOGR(dsn = mydir, layer = "ukbord")
# In the second line of code above the readOGR function is used to load a shape???le and assign it to a new spatial object called "lnd"; short for London.
# readOGR is a function which accepts two arguments: dsn which stands for "data source name" and speci???es the directory in which the ???le is stored, and layer which speci???es the ???le name (note that there is no need to include the ???le extention .shp). 
# The ???le we assigned to the lnd object contains the population of London Boroughs in 2001 and the percentage of the population participating in sporting activities. 

# Take a look at the output created (note the table format of the data and the column names). 
# There are two important symbols at work in the above block of code: the @ symbol in the ???rst line of code is used to refer to the data slot of the lnd object. 
# The $ symbol refers to the Partic_Per column (a variable within the table) in the data slot, which was identi???ed from the result of running the ???rst line of code. 

# The head function in the ???rst line of the code above simply means "show the ???rst few lines of data" (try entering head(lnd@data), see ?head for more details). 
# The second line calculates ???nds the mean sports participation per 100 people for zones in London. 

# Spatial objects like the lnd object are made up of a number of different slots, the key slots being @data (non geographic attribute data) and @polygons (or @lines for line data). 
# The data slot can be thought of as an attribute table and the geometry slot is the polygons that make up the physcial boundaries. 
# Specific slots are accessed using the @ symbol. 
# Let's now analyse the sport object with some basic commands: 
head(lnd@data, n = 2) 
mean(lnd$Partic_Per) # short for mean(lnd@data$Partic_Per) 

#  To check the classes of all the variables in a spatial dataset, you can use the following command: 
sapply(lnd@data, class) 
# This shows that, unexpectedly, Pop_2001 is a factor. 
# We can coerce the variable into the correct, numeric, format with the following command: 
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001)) 

# To explore lnd object further, try typing 
nrow(lnd)
ncol(lnd)
lnd@proj4string

# Now we have seen something of the structure of spatial objects in R, let us look at plotting them. 
# Note, that plots use the geometry data, contained primarily in the @polygons slot. 
plot(lnd)

# Inputting another object such as plot(lnd@data) will generate an entirely di???erent type of plot.
plot(lnd@data)

# R has powerful subsetting capabilities that can be accessed very concisely using square brackets, as shown in the following example: # select rows of lnd@data where sports participation is less than 15 
lnd@data[lnd$Partic_Per < 15, ] 

# The above line of code asked R to select only the rows from the lnd object, where sports participation is lower than 15, in this case rows 17, 21 and 32, which are Harrow, Newham and the city centre respectively. 
# The square brackets work as follows: anything before the comma refers to the rows that will be selected, anything after the comma refers to the number of columns that should be returned. 
# For example if the data frame had 1000 columns and you were only interested in the ???rst two columns you could specify 1:2 after the comma.
# The ":" symbol simply means "to", i.e. columns 1 to 2. 
# Try experimenting with the square brackets notation (e.g. guess the result of lnd@data[1:2, 1:3] and test it). 

# So far we have been interrogating only the attribute data slot (@data) of the lnd object, but the square brackets can also be used to subset spatial objects, i.e. the geometry slot. 
# Using the same logic as before try to plot a subset of zones with high sports participation. # Select zones where sports participation is between 20 and 25% 
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25 
plot(lnd[sel, ]) # output not shown here 
head(sel) # test output of previous selection (not shown) 

# This plot is quite useful, but it only displays the areas which meet the criteria. 
# To see the sporty areas in context with the other areas of the map simply use the add = TRUE argument after the initial plot. 
# (add = T would also work, but we like to spell things out in this tutorial for clarity). 
# What do you think the col argument refers to in the below block? (see Figure 5). 
# If you wish to experiment with multiple criteria queries, use &. 
plot(lnd, col = "lightgrey") # plot the london_sport object 
sel <- lnd$Partic_Per > 25 
plot(lnd[sel, ], col = "turquoise", add = TRUE) # add selected zones to map

# Congratulations! You have just interrogated and visualised a spatial object: where are areas with high levels of sports participation in London? 
# The map tells us. 
library(rgeos)
plot(lnd, col = "grey")
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000)
# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Selecting quadrants 
# The code below should help understand the way spatial data work in R. 
# Find the centre of the london area 
lat <- coordinates(gCentroid(lnd))[[1]] 
lng <- coordinates(gCentroid(lnd))[[2]]
# arguments to test whether or not a coordinate is east or north of the centre 
east <- sapply(coordinates(lnd)[,1], function(x) x > lat) 
north <- sapply(coordinates(lnd)[,2], function(x) x > lng)
# test if the coordinate is east and north of the centre 
lnd@data$quadrant[east & north] <- "northeast" 

# arguments to test whether or not a coordinate is east or north of the centre 
east <- sapply(coordinates(lnd)[,1], function(x) x > lat) 
south <- sapply(coordinates(lnd)[,2], function(x) x < lng)
# test if the coordinate is east and south of the centre 
lnd@data$quadrant[east & south] <- "southeast" 

west <- sapply(coordinates(lnd)[,1], function(x) x < lat) 
north <- sapply(coordinates(lnd)[,2], function(x) x > lng)
# test if the coordinate is east and north of the centre 
lnd@data$quadrant[west & north] <- "northwest" 

# arguments to test whether or not a coordinate is east or north of the centre 
west <- sapply(coordinates(lnd)[,1], function(x) x < lat) 
south <- sapply(coordinates(lnd)[,2], function(x) x < lng)
# test if the coordinate is east and south of the centre 
lnd@data$quadrant[west & south] <- "southwest" 

plot(lnd)
plot(lnd[east & north,], col = "red", add = TRUE) 
plot(lnd[east & south,], col = "green", add = TRUE) 
plot(lnd[west & north,], col = "blue", add = TRUE) 
plot(lnd[west & south,], col = "yellow", add = TRUE)  

lnd$quadrant[!east & north] <- "northwest"
lnd$quadrant[east & !north] <- "southeast"
lnd$quadrant[!east & !north] <- "southwest"
plot(lnd)
plot(lnd[east & north,], add = TRUE, col = "red" )
llgridlines(lnd, lty= 3, side ="EN", offset = -0.5)

lnd_disolved = rgeos::gUnaryUnion(spgeom = lnd, id = lnd$quadrant)
# Alongside visualisation and interrogation, a GIS must also be able to create and modify spatial data. 
# R's spatial packages provide a very wide and powerful suite of functionality for processing and creating spatial data. 

# Creating new spatial data 
# R objects can be created by entering the name of the class we want to make. 
# vector and data.frame objects for example, can be created as follows: 
vec <- vector(mode = "numeric", length = 3) 
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4)) 
# We can check the class of these new objects using class(): 
class(vec) 
class(df) 
# The same logic applies to spatial data. The input must be a numeric matrix or data.frame: 
sp1 <- SpatialPoints(coords = df) 

# We have just created a spatial points object, one of the fundamental data types for spatial data. 
# (The others arelines,polygonsandpixels,whichcanbecreatedbySpatialLines,SpatialPolygonsandSpatialPixels, respectively.) 
# Each type of spatial data has a corollary that can accepts non-spatial data, created by adding DataFrame. 
# SpatialPointsDataFrame(), for example, creates points with an associated data.frame. 
# The number of rows in this dataset must equal the number of features in the spatial object, which in the case of sp1 is 3. 
class(sp1) 
spdf <- SpatialPointsDataFrame(sp1, data = df) 
class(spdf) 

# The above code extends the pre-existing object sp1 by adding data from df. 
# To see how strict spatial classes are, try replacing df with mat in the above code: it causes an error. 
# All spatial data classes can be created in a similar way, although SpatialLines and SpatialPolygons are much more complicated (Bivand et al. 2013). 
# More frequently your spatial data will be read-in from an externally-created ???le, e.g. using readOGR(). 
# Unlike the spatial objects we created above, most spatial data comes with an associate 'CRS'.

## Projections: setting and transforming CRS in R 
# The Coordinate Reference System (CRS) of spatial objects de???nes where they are placed on the Earth's surface. 
# You may have noticed 'proj4string 'in the summary of lnd above: the information that follows represents its CRS. 
# Spatial data should always have a CRS. 
# If no CRS information is provided, and the correct CRS is known, it can be set as follow: 
proj4string(lnd) <- NA_character_ # remove CRS information from lnd 
proj4string(lnd) <- CRS("+init=epsg:27700") # assign a new CRS 

# R issues a warning when the CRS is changed. 
# This is so the user knows that they are simply changing the CRS, not reprojecting the data. 
# An easy way to refer to di???erent projections is via EPSG codes.

# Under this system 27700 represents the British National Grid. 
# 'WGS84' (epsg:4326) is a very commonly used CRS worldwide. 
# The following code shows how to search the list of available EPSG codes and create a new version of lnd in WGS84:3 
EPSG <- make_EPSG() # create data frame of available EPSG codes 
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code 
lnd84 <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject 

# Above, spTransform converts the coordinates of lnd into the widely used WGS84CRS.
# Now we've transformed lnd into a more widely used CRS, it is worth saving it. 
# R stores data e???ciently in .RData or .Rds formats. 
# The former is more restrictive and maintains the object's name, so we use the latter. 
# Save lnd84 object (we will use it in Part IV) 
saveRDS(object = lnd84, file = "C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/lnd84.Rds") 

# Now we can remove the lnd84 object with the rm command. It will be useful later. 
# (In RStudio, notice it also disappears from the Environment in the top right panel.) 
rm(lnd84) # remove the lnd object 
# we will load it back in later with readRDS(file = "data/lnd84.Rds")

## Attribute joins 
# Attribute joins are used to link additional pieces of information to our polygons. 
# In the lnd object, for example, we have 4 attribute variables - that can be found by typing names(lnd). 
# But what happens when we want to add more variables from an external source? 
# We will use the example of recorded crimes by London boroughs to demonstrate this. 
# To rea???rm our starting point, let's re-load the "london_sport" shape???le as a new object and plot it: 
library(rgdal) # ensure rgdal is loaded 
# Create new object called "lnd" from "london_sport" shapefile 
lnd <- readOGR(dsn = mydir, "london_sport") 
plot(lnd) 
nrow(lnd) 

# The non-spatial data we are going to join to the lnd object contains records of crimes in London. 
# This is stored in a comma separated values (.csv) ???le called "mps-recordedcrime-borough". 
# If you open the ???le in a separate spreadsheet application ???rst, we can see each row represents a single reported crime. 
# We are going to use a function called aggregate to aggregate the crimes at the borough level, ready to join to our spatial lnd dataset. 
# A new object called crime_data is created to store this data. 

# Create and look at new crime_data object 
crime_data <- read.csv("C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/mps-recordedcrime-borough.csv", stringsAsFactors = FALSE)
head(crime_data$CrimeType) # information about crime type
# Extract "Theft & Handling" crimes and save 
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ] 
head(crime_theft, 2) # take a look at the result (replace 2 with 10 to see more rows)
# Calculate the sum of the crime count for each district, save result 
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft) 
# Show the first two rows of the aggregated crime data 
head(crime_ag, 2)

# You should not expect to understand all of this upon ???rst try: simply typing the commands and thinking brie???y about the outputs is all that is needed at this stage. 
# Here are a few things that you may not have seen before that will likely be useful in the future:
# In the ???rst line of code when we read in the ???le we specify its location (check in your ???le browser to be sure). 
# The == function is used to select only those observations that meet a speci???c condition i.e. where it is equal to, in this case all crimes involving "Theft and Handling". 
# The ~ symbol means "by": we aggregated the CrimeCount variable by the district name. 

# Now that we have crime data at the borough level, the challenge is to join it to the lnd object. 
# We will base our join on the Borough variable from the crime_ag object and the name variable from the lnd object. 
# It is not always straight-forward to join objects based on names as the names do not always match. 
# Let's see which names in the crime_ag object match the spatial data object, lnd: 

# Compare the name column in lnd to Borough column in crime_ag to see which rows match. 
lnd$name %in% crime_ag$Borough 

# Return rows which do not match 
lnd$name[!lnd$name %in% crime_ag$Borough] 

# The ???rst line of code above uses the %in% command to identify which values in lnd$name are also contained in the Borough names of the aggregated crime data. 
# The results indicate that all but one of the borough names matches. 
# The second line of code tells us that it is 'City of London'. 
# This does not exist in the crime data. This may be because the City of London has it's own Police Force.4 
# (The borough name in the crime data does not match lnd$name is 'NULL'. 
# Check this by typing 
crime_ag$Borough[!crime_ag$Borough %in% lnd$name] 

#Having checked the data found that one borough does not match, we are now ready to join the spatial and non-spatial datasets. 
# It is recommended to use the left_join function from the dplyr package but the merge function could equally be used. 
# Note that when we ask for help for a function that is not loaded, nothing happens, indicating we need to load it: 

library(dplyr) # load dplyr

# We use left_join because we want the length of the data frame to remain unchanged, with variables from new data appended in new columns (see ?left_join). 
# The *join commands (including inner_join and anti_join) assume, by default, that matching variables have the same name. 
# Here we will specify the association between variables in the two data sets: 

head(lnd$name) # dataset to add to (results not shown) 
head(crime_ag$Borough) # the variables to join
# head(left_join(lnd@data, crime_ag)) # test it works 
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough')) 

# Take a look at the new lnd@data object. 
# You should see new variables added, meaning the attribute join was successful. Congratulations! 
# You can now plot the rate of theft crimes in London by borough (see Fig 8). 

library(tmap) # load tmap package (see Section IV) 
qtm(lnd, "CrimeCount") # plot the basic map 

library(tmap) 
library(tmaptools)
lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326"))
osm_tiles = read_osm(bbox(lnd_wgs))
lnd_wgs$Thefts <- lnd$CrimeCount / 10000 
tm_shape(osm_tiles) +
  tm_raster() +
  tm_shape(lnd_wgs) +
  tm_fill("Thefts", fill.title = "Thefts\n(10000)", scale = 0.8, alpha = 0.5) +
  tm_layout(legend.position = c(0.89,0.02))
## Clipping and spatial joins 
# In addition to joining by attribute (e.g. Borough name), it is also possible to do spatial joins in R. 
# We use transport infrastructure points as the spatial data to join, with the aim of ???nding out about how many are found in each London borough. 

library(rgdal) 
library(sf)
# create new stations object using the "lnd-stns" shapefile. 
stations <- readOGR(dsn = mydir, layer = "lnd-stns") 
# stations = read_shape("data/lnd-stns.shp") # from tmap 
proj4string(stations) # this is the full geographical detail. 
proj4string(lnd) # what's the coordinate reference system (CRS)
bbox(stations) # the extent, 'bounding box' of stations 
bbox(lnd) # return the bounding box of the lnd object 

# The proj4string() function shows that the Coordinate Reference System (CRS) of stations di???ers from that of our lnd object. 
# OSGB 1936 (or EPSG 27700) is the o???cial CRS for the UK, so we will convert the 'stations' object to this: 

# Create reprojected stations object 
stations <- spTransform(stations, CRSobj = CRS(proj4string(lnd))) 
plot(lnd) # plot London 
points(stations) # overlay the station points

# Note the stations points now overlay the boroughs but that the spatial extent of stations is greater than that of lnd. 
# To clip the stations so that only those falling within London boroughs are retained we can use sp::over, or simply the square bracket notation for subsetting tabular data 
#(enter ?gIntersects to ???nd out another way to do this): 

stations <- stations[lnd, ] 
plot(stations) # test the clip succeeded

# The above line of code says: "output all `stations` within the `lnd` object bounds", a concise way of clipping that is consistent with R's syntax for non-spatial clipping. 
# To prove it worked, only stations within the London boroughs appear in the plot. 
# gIntersects can achieve the same result, but with more lines of code (see www.rpubs.com/RobinLovelace for more on this).

#It may seem confusing that two different functions can be used to generate the same result. 
#However, this is a common issue in R; the question is finding the most appropriate solution. 

# In its less concise form (without use of square brackets), `over` takes two main input arguments: the target layer (the layer to be altered) and the source layer by which the target layer is to be clipped. 
# The output of `over` is a data frame of the same dimensions as the original object (in this case `stations`), except that the points which fall outside the zone of interest are set to a value of `NA` ("no answer"). 
# We can use this to make a subset of the original polygons, remembering the square bracket notation described in the first section. 
# We create a new object, `sel` (short for "selection"), containing the indices of all relevant polygons: 

sel <- over(stations_backup, lnd) 
stations2 <- stations_backup[!is.na(sel[,1]),] 

# Typing `summary(sel)` should provide insight into how this worked: it is a data frame with 1801 NA values, representing zones outside of the London polygon. 
# Note that the preceding two lines of code is equivalent to the single line of code, `stations <- stations[lnd, ]`. 
# The next section demonstrates spatial aggregation, a more advanced version of spatial subsetting. 

## Spatial aggregation

# As with R's very terse code for spatial subsetting, the base function `aggregate` (which provides summaries of variables based on some grouping variable) also behaves differently when the inputs are spatial objects. 

stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length) 
head(stations_agg@data) 

#The above code performs a number of steps in just one line:

#- `aggregate` identifies which `lnd` polygon (borough) each `station` is located in and groups them accordingly. The use of the syntax `stations["CODE"]` tells R that we are interested in the spatial data from `stations` and its `CODE` variable (any variable could have been used here as we are merely counting how many points exist). 
#- It counts the number of `stations` points in each borough, using the function `length`. 
#- A new spatial object is created, with the same geometry as `lnd`, and assigned the name `stations_agg`, the count of stations. 

# It may seem confusing that the result of the aggregated function is a new shape, not a list of numbers --- this is because values are assigned to the elements within the `lnd` object. 
# To extract the raw count data, one could enter `stations_agg$CODE`. 
# This variable could be added to the original `lnd` object as a new field, as follows: 

lnd$n_points <- stations_agg$CODE

# As shown below, the spatial implementation of `aggregate` can provide summary statistics of variables, as well as simple counts. 
# In this case we take the variable `NUMBER` and find its mean value for the stations in each ward.^
# [See the  miniature Vignette 'Clipping and aggregating spatial data with gIntersects' for more information on this: http://rpubs.com/RobinLovelace/83834 .] 

lnd_n <- aggregate(stations["NUMBER"], by = lnd, FUN = mean)

# For an optional advanced task, let us analyse and plot the result.

# fig.cap="Choropleth map of mean values of stations in each borough

q <- cut_number(lnd_n$NUMBER,4) # a nice little function from ggplot2 
q <- factor(q, labels = c("red", "green", "yellow", "blue")) 
summary(q)
qc <- as.character(q) # convert to character class to plot 
plot(lnd_n, col = qc) # plot (not shown in printed tutorial) 
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright") 
areas <- sapply(lnd_n@polygons, function(x) x@area) 

# This results in a simple choropleth map and a new vector containing the area of each borough (the basis for Figure 11). 
# As an additional step, try comparing the mean area of each borough with the mean value of `stations` points within it: `plot(lnd_n$NUMBER, areas)`. 

## Adding different symbols for tube stations and train stations* 

# Imagine now that we want to display all tube and train stations on top of the previously created choropleth map.
# How would we do this? 
# The shape of points in R is determined by the `pch` argument, as demonstrated by the result of entering the following code: `plot(1:10, pch=1:10)`. 
# To apply this knowledge to our map, try adding the following code to the chunk above (output not shown): 

levels(stations$LEGEND) # see A roads and rapid transit stations (RTS) (not shown) 
sel <- grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting  
sym <- as.integer(stations$LEGEND[sel]) # symbols 
points(stations[sel,], pch = sym) 
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))

# fig.cap="Symbol levels for train station types in London

# The above block of code first identifies which types of transport points are present in the map with `levels` (this command only works on factor data). 
# Next we select a subset of `stations` using a new command, `grepl`, to determine which points we want to plot. 
# Note that `grepl`'s first argument is a text string (hence the quote marks) and the second is a factor (try typing `class(stations$LEGEND)` to test this).
# grepl` uses *regular expressions* to match whether each element in a vector of text or factor names match the text pattern we want. 
# In this case, because we are only interested in roundabouts that are A roads and Rapid Transit systems (RTS). 
# Note the use of the vertical separator `|` to indicate that we want to match `LEGEND` names that contain either "A Road" *or* "Rapid". 
# Based on the positive matches (saved as `sel`, a vector of `TRUE` and `FALSE` values), we subset the stations. 
# Finally we plot these as points, using the integer of their name to decide the symbol and add a legend. 
# (See the documentation of `?legend` for detail on the complexities of legend creation in R's base graphics.) 

## Part IV: Making maps with tmap, ggplot2 and lea???et

## tmap 
# tmap was created to overcome some of the limitations of base graphics and ggmap. 
# A concise introduction to tmap can be accessed (after the package is installed) by using the vignette function: 

# install.packages("tmap") # install the CRAN version 
library(tmap) 
vignette("tmap-getstarted") 

# A couple of basic plots show the package's intuitive syntax and attractive default parameters. 
qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues") # not shown 
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = "Blues", nrow = 1)

# The plot above shows the ease with which tmap can create maps next to each other for di???erent variables. 
# The plot produced by the following code chunk (not shown) demonstrates the power of the tm_facets command. 
# Note that all the maps created with the qtm function can also be created with tm_shape, followed by tm_fill (or another tm_ function). 

tm_shape(lnd) + 
  tm_fill("Pop_2001", thres.poly = 0) + 
  tm_facets("name", free.coords = TRUE, drop.units = TRUE) 

# To create a basemap with tmap, you can use the read_osm function, from the tmaptools package as follows. 
# Note that you must ???rst transform the data into a geographical CRS: 

# Transform the coordinate reference system 
library(OpenStreetMap)
lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326")) 
osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM 

tm_shape(osm_tiles) + tm_raster() + tm_shape(lnd_wgs) + 
  tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) + 
  tm_layout(legend.position = c(0.89,0.02)) 

# Another way to make tmap maps have a basemap is by entering tmap_mode("view"). 
# This will make the maps appear on a zoomable webmap powered by lea???et. 
# There are many other intuitive and powerful functions in tmap. 
# Check the documentation to ???nd out more: 

?tmap # get more info on tmap

## ggmap 
# ggmap is based on the ggplot2 package, an implementation of the Grammar of Graphics (Wilkinson 2005). 
# ggplot2 can replace the base graphics in R (the functions you have been plotting with so far). 
# It contains default options that match good visualisation practice and is well-documented: http://docs.ggplot2.org/ current/ .

# As a ???rst attempt with ggplot2 we can create a scatter plot with the attribute data in the lnd object created previously: 
library(ggplot2) 
p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001)) 
# The real power of ggplot2 lies in its ability to add layers to a plot. 
# In this case we can add text to the plot. 
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) + 
  geom_text(size = 4, aes(label = name)) 
# This idea of layers (or geoms) is quite di???erent from the standard plot functions in R, 
# but you will ???nd that each of the functions does a lot of clever stu??? to make plotting much easier (see the documentation for a full list).
# In the following steps we will create a map to show the percentage of the population in each London Borough who regularly participate in sports activities. 
# ggmap requires spatial data to be supplied as data.frame, using fortify(). 
# The generic plot() function can use Spatial* objects directly; ggplot2 cannot. 
# Therefore we need to extract them as a data frame. 
# The fortify function was written speci???cally for this purpose. 
# For this to work, either the maptools or rgeos packages must be installed. 
library(rgeos) 
lnd_f <- fortify(lnd) ## Regions defined for each Polygons 
# This step has lost the attribute information associated with the lnd object. 
# We can add it back using the left_join function from the dplyr package (see ?left_join).

head(lnd_f, n = 2) # peak at the fortified data 
lnd$id <- row.names(lnd) # allocate an id variable to the sp data 
head(lnd@data, n = 2) # final check before join (requires shared variable name) 
lnd_f <- left_join(lnd_f, lnd@data) # join the data 

# The newlnd_f object contains coordinates alongside the attribute information associated with each London Borough. 
# It is now straightforward to produce a map with ggplot2. 
# coord_equal() is the equivalent of asp = T in regular plots with R:

map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) + 
  geom_polygon() + coord_equal() + 
  labs(x = "Easting (m)", y = "Northing (m)", 
       fill = "% Sports\nParticipation") + 
  ggtitle("London Sports Participation")

# Entering map should result in your ???rst ggplot-made map of London. 
# The default colours are really nice but we may wish to produce the map in black and white, which should produce a map like the one shown below.
# Try changing the colours and saving plots with ggsave(). 	

map + scale_fill_gradient(low = "white", high = "blue")

## Creating interactive maps with lea???et 
# Lea???et is the world's premier web mapping system, serving hundreds of thousands of maps worldwide each day. 
# The JavaScript library actively developed at github.com/Lea???et/Lea???et, has a strong user community. 
# It is fast, powerful and easy to learn. 

# The lea???et package creates interactive web maps in few lines of code. 
# One of the exciting things about the package is its tight integration with the R package for interactive on-line visualisation, shiny. 
# Used together, these allow R to act as a complete map-serving platform, to compete with the likes of GeoServer! 
# For more information on rstudio/lea???et, see rstudio.github.io/lea???et/ and the following on-line tutorial: robinlovelace.net/r/2015/02/01/lea???et-r-package.html.

#install.packages("leaflet") 
library(leaflet)
lnd84 <- readRDS('C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/lnd84.Rds')
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = lnd84)

## Advanced Task: Faceting for Maps 
# The below code demonstrates how to read in the necessary data for this task and 'tidy' it up. 
# The data ???le contains historic population values between 1801 and 2001 for London, again from the London data store. 
# We tidy the data so that the columns become rows. 
# In other words, we convert the data from '???at' to 'long' format. 
# This is the form required by ggplot2 for faceting graphics: 
# the date of the population survey becomes a variable in its own right, rather than being strung-out over many columns. 

london_data <- read.csv("C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/census-historic-population-borough.csv") 
# install.packages("tidyr") 
library(tidyr) # if not install it, or skip the next two steps 
library(dplyr)
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name) 
head(ltidy, 2) # check the output 

# In the above code we take the london_data object and create the column names 'date' (the date of the record, previously spread over many columns) and 'pop' (the population which varies). 
# The minus (-) symbol in this context tells gather not to include the Area.Name and Area.Code as columns to be removed. 
# In other words, "leave these columns be". 
# Data tidying is an important subject: more can be read on the subject in Wickham (2014) or in a vignette about the package, accessed from within R by entering vignette("tidy-data"). 

# Merge the population data with the London borough geometry contained within our lnd_f object, using the left_join function from the dplyr package:

head(lnd_f, 2) # identify shared variables with ltidy 

ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable 
lnd_f <- left_join(lnd_f, ltidy) 

# Rename the date variable (use ?gsub and Google 'regex' to ???nd out more).

lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)

# We can now use faceting to produce one map per year: 
library(ggplot2)
ggplot(data = lnd_f, # the input data 
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables 
  geom_polygon() + # plot the boroughs 
  geom_path(colour="black", lwd=0.05) + # borough borders 
  coord_equal() + # fixed x and y scales 
  facet_wrap(~ date) + # one plot per time slice 
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors 
                       midpoint = 150, name = "Population\n(thousands)") + # legend options 
  theme(axis.text = element_blank(), # change the theme options 
        axis.title = element_blank(), # remove axis titles 
        axis.ticks = element_blank()) # remove axis ticks
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure

# There is a lot going on here so explore the documentation to make sure you understand it. 
# Try out di???erent colour values as well. 