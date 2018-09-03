library(radiant.data)

## Summary method for Pivot Tables with pivotr() 

pivotr(diamonds, cvars = "cut") %>% summary(chi2 = TRUE) 
pivotr(diamonds, cvars = "cut", tabsort = "desc(n_obs)") %>% summary() 
pivotr(diamonds, cvars = "cut", tabfilt = "n_obs > 700") %>% summary() 
pivotr(diamonds, cvars = "cut:clarity", nvar = "price") %>% summary()

## Summary method for the explore function explore()

## Launch a Radiant data window in R Studio

radiant.data_window()
radiant.data_viewer()
radiant.data() 

## pivotr: Create a pivot table

pivotr(diamonds, cvars = "cut") %>% str() 
pivotr(diamonds, cvars = "cut")$tab 
pivotr(diamonds, cvars = c("cut","clarity","color"))$tab 
pivotr(diamonds, cvars = "cut:clarity", nvar = "price")$tab 
pivotr(diamonds, cvars = "cut", nvar = "price")$tab 
pivotr(diamonds, cvars = "cut", normalize = "total")$tab

## plot.pivotr: Plot method for the pivotr function

pivotr(diamonds, cvars = "cut") %>% plot() 
pivotr(diamonds, cvars = c("cut", "clarity")) %>% plot() 
pivotr(diamonds, cvars = c("cut", "clarity", "color")) %>% plot()

## explore: Explore and summarize data

explore(diamonds, c("price", "carat")) %>% str() 
explore(diamonds, "price:x")$tab 
explore(diamonds, c("price","carat"), byvar = "cut", fun = c("n_missing", "skew"))$tab

## dtab.pivotr: Make an interactive pivot table

pivotr(diamonds, cvars = "cut") %>% dtab() 
pivotr(diamonds, cvars = c("cut","clarity")) %>% dtab(format = "color_bar") 
pivotr(diamonds, cvars = c("cut","clarity"), normalize = "total") %>% dtab(format = "color_bar", perc = TRUE)

## dtab.data.frame: Create an interactive table to view, search, sort, and ???lter data

dtab(mtcars)

## combine_data: Combine datasets using dplyr's bind and join functions

avengers %>% combine_data(superheroes, type = "bind_cols") 
combine_data(avengers, superheroes, type = "bind_cols") avengers %>% 
combine_data(superheroes, type = "bind_rows") avengers %>% 
combine_data(superheroes, add = "publisher", type = "bind_rows")

## choose_files: Choose ???les interactively

choose_files("csv")

## visualize: Visualize data using ggplot2 http://ggplot2.tidyverse.org

visualize(diamonds, xvar = "cut", yvar = "price", type = "bar", facet_row = "cut", fill = "cut")
visualize(diamonds, xvar = "price:carat", custom = TRUE) %>% gridExtra::grid.arrange(grobs = ., top = "Histograms", ncol = 2) 
visualize(diamonds, yvar = "price", xvar = "carat", type = "scatter", custom = TRUE) + labs(title = "A scatterplot", x = "price in $") 
visualize(diamonds, yvar = "price", xvar = "carat", type = "scatter", size = "table", custom = TRUE) + scale_size(range=c(1,10), guide = "none") 
visualize(diamonds, yvar = "price", xvar = c("cut","clarity"), type = "line", fun = "max")
visualize(diamonds, yvar = "price", xvar = c("cut","clarity"), type = "bar", fun = "median") 
visualize(diamonds, "carat:cut", yvar = "price", type = "scatter", pointcol = "blue", fun = c("mean", "median"), linecol = c("red","green")) 
visualize(diamonds, "price:cut", type = "dist", fillcol = "red") 

## view_data: View data in a shiny-app

view_data(mtcars)

## table2data: Create data.frame from a table

df<-data.frame(price = c("$200","$300"), sale = c(10, 2)) %>% table2data()

head(DD)

library(readr)
library(plyr)
path <- "c:\\Users\\jeff\\Documents\\Crime Analysis\\India_data\\chennai_crimes.csv"
df <- read_csv(path)
head(df)
summary(df$Resolution)
list(df$Resolution)

## Manipulate datafram
### Remove columns from dataframe
df$Address<-NULL
df$Descript<-NULL
df$PdId<-NULL

### Change data in a column of a dataframe
df2<-mutate(df, X = X-164.6)
df3<-mutate(df2, Y = Y+20.6)
head(df3)

### Plot new data
plot(df3$X,df3$Y)
plot(c(min(df3$X),max(df3$X),min(df3$Y),max(df3$Y)))

### Change dat from all-caps to proper case
library(dplyr)
proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl=TRUE))
}
df3 <- df3 %>% mutate(Category = proper_case(Category),
                      Descript = proper_case(Descript),
                      PdDistrict = proper_case(PdDistrict),
                      Resolution = proper_case(Resolution),
                      Time = as.character(Time))
df_sub <- df3[1:100,]  # display the first 100 rows
datatable(df_sub, options = list(pageLength = 5,scrollX='400px'))

### Make a new data column by transforming an existing column   
data("baseball")
head(baseball)
mutate(baseball, avg_ab = ab / g)
transform(baseball, avg_ab = ab / g)
BB<-baseball
head(BB)

### Difference between mutate and transform
system.time(mutate(baseball, avg_ab = ab / g))
system.time(transform(baseball, avg_ab = ab / g))


#!/users/jeff/Documents/VIT_University/tidy_text/Rscript_Indian_Philosophy.r
# Load necessary libraries
library(broom)
library(dplyr)
library(tm)
library(tidyverse)
library(tidytext)
library(tidyr)
library(tmap)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(reshape2)
library(wordcloud)
library(wordcloud2)

Which of the above libraries are required to execute the code below
getwd()
setwd("users/jeff/Documents/")
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

ind_text <- read_folder("users/jeff/Documents/VIT_Cource_Material/data/ind_phil")
summary(ind_text)
