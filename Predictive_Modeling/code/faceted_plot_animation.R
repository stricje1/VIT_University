## Animate Faceted plot of the distribution of London's population over time
pkgs = c("ggmap", "sp", "tmap", "rgeos", "maptools", "dplyr", "rgdal")
lapply(pkgs, library, character.only = TRUE)

dta<-"C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/london_sport.shp"
mydir<-"C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data"

# Below, the same layer of the london_sport shape file (SHP) is read with readOGR() and put into a data frame with the fortify() function.
# Then we do a left join with the data frame lnd_f ('f' for fortified), and the nongeographic attribute London sport data, data, lnd@data.
lnd <- readOGR(dsn = mydir, layer = "london_sport")
lnd_f <- fortify(lnd, region = "ons_label") # only for visualisation
lnd_f = rename(lnd_f, ons_label = id)
lnd_f = left_join(lnd_f, lnd@data)

# REad London census data to populate data with burough populations from 1810-2001 in 10-year increments.
# Area.Code will be renamed (mapped to) ons_label as a common key with ltidy below. 
# Once we "tidy" the data, it is checked by showing the headings with two rows of data. 
# We then do another left join with the lnd_f data frame and the "tidy'd" data.
# Finally, we rename the date column using the gsub() function.
london_data = read.csv("C:/Users/jeff/Documents/Crime Analysis/Creating-maps-in-R-master/data/census-historic-population-borough.csv")
library(tidyr) # if not install it, or skip the next two steps
ltidy = gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2) # check the output (not shown)
head(lnd_f, 2) # identify shared variables with ltidy 
ltidy = rename(ltidy, ons_label = Area.Code) # rename Area.code variable
lnd_f = left_join(lnd_f, ltidy)
lnd_f$date = gsub(pattern = "Pop_", replacement = "", lnd_f$date)

# We are now ready to perform the plots for year of the years from 1810-2001 in 10-year increments.
# This will yield 21 frames or plots for the animation.

frames <- 21 #number of frames or plots

# Next, we generate a function for creating file name with leading zeros.
# This makes it easier to process the plots sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}
# Using a for-loop, we generate a plot for each year with index i and also use the index to name the files and affix the dates to the plots.
# The latter is done using facet_wrap().
# We also label the plots with longitude as teh x-axis and latitude as the y-axis, and add a legend entitled "population/1000."
# The buroughs are added as before with polygons and we save the plots as a .png files in the working directory
dates = unique(lnd_f$date)
i = dates[3]
for(i in dates){
  name <- rename(i)    
  print(i)
  lnd_tmp = lnd_f[lnd_f$date == i,]
  p = ggplot(data = lnd_tmp, # the input data
             aes(x = long, y = lat, fill = pop/1000, group = group, frame = i)) + # define variables
    geom_polygon() + # plot the boroughs
    geom_path(colour="black", lwd=0.05) + # borough borders
    facet_wrap(~ date) +
    coord_equal()
  ggsave(paste0("ggplots-", i, ".png"), p)
}
# Finally, we run GraphicsMagick to create the animation (GIF file) with one second (or 100 hundredths (100x100) of a second) delay between each frame
# The parameters needed for GraphicsMagick are mapped to my_command and then used with the system() function.
# The resulting animation will be in the R working directory. 
# If you do not remeber your working directory, type the getwd() function in Rstudio.
# Figure X shows one frame of the resulting animation.
my_command <- 'gm convert *.png -delay 100x100 -loop 0 animation.gif'
system(my_command)

## ----fig.cap="Faceted plot of the distribution of London's population over time", fig.height=6, fig.width=6----