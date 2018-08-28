
## Plot Cars Data

require(stats) # for lowess, rpois, rnorm
plot(cars)
lines(lowess(cars))

## Plot Sine Function

plot(sin, -pi, 2*pi)

## Discrete Distribution Plot

plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

## Simple quantiles/ECDF

plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")

## Rich Colors

m <- abs(matrix(1:120+rnorm(120), nrow=15, ncol=8))
opar <- par(bg="gray", mfrow=c(1,2))
matplot(m, type="l", lty=1, lwd=3, col=rich.colors(8))
matplot(m, type="l", lty=1, lwd=3, col=rich.colors(8,"blues"))
par(opar)
barplot(rep(1,100), col=rich.colors(100), space=0, border=0, axes=FALSE)
barplot(rep(1,20), col=rich.colors(40)[11:30]) # choose subset
plot(m, rev(m), ylim=c(120,0), pch=16, cex=2,
     col=rich.colors(200,"blues",alpha=0.6)[1:120]) # semitransparent
rich.colors(100, plot=TRUE)  # describe rgb recipe
par(mfrow=c(2,2))
barplot(m, col=heat.colors(15), main="\nheat.colors")
barplot(m, col=1:15, main="\ndefault palette")
barplot(m, col=rich.colors(15), main="\nrich.colors")
barplot(m, col=rainbow(15), main="\nrainbow")
par(opar)

## Rich Color Bar Plot

barplot(rep(1,100), col=rich.colors(100), space=0, border=0, axes=FALSE)

## Choose subset

barplot(rep(1,20), col=rich.colors(40)[11:30]) # choose subset

## Semitransparent

plot(m, rev(m), ylim=c(120,0), pch=16, cex=2,
     col=rich.colors(200,"blues",alpha=0.6)[1:120]) # semitransparent

## Describe rgb recipe

rich.colors(100, plot=TRUE)  # describe rgb recipe

## Barplots – Various Color Schemes

barplot(m, col=heat.colors(15), main="\nheat.colors")
barplot(m, col=1:15, main="\ndefault palette")
barplot(m, col=rich.colors(15), main="\nrich.colors")
barplot(m, col=rainbow(15), main="\nrainbow")

## The rgl package

file.show(system.file("NEWS", package = "rgl")) 

## Enhancing a 3d Plot

plot3d(rnorm(100), rnorm(100), rnorm(100)) 
abclines3d(0, 0, 0, a = diag(3), col = "gray")
open3d() 
y <- subdivision3d(tetrahedron3d(col = "red"), depth = 3) 
shade3d(y) # No normals 
y <- addNormals(y) 
shade3d(translate3d(y, x = 1, y = 0, z = 0)) # With normals

## Bounding Arrow in A Box

saveopts <- options(rgl.useNULL = TRUE)
theta <- seq(0, 4*pi, len=100) 
xyz <- cbind(sin(theta), cos(theta), sin(theta/2)) 
lineid <- plot3d(xyz, type="l", alpha = 0, lwd = 5, col = "blue")["data"]
widget <- rglwidget() %>% 
playwidget(ageControl(births = theta,
		ages = c(-4*pi, -4*pi, 1-4*pi, 0, 0, 1), 
		objids = lineid, 
		alpha = c(0, 1, 0, 0, 1, 0)), 
	start = 0, stop = 4*pi, 
	step = 0.1, rate = 4) 
if (interactive()) 
	widget 
options(saveopts)

## Loading Libraries (Packages)

#!/users/jeff/Documents/VIT_University/tidy_text/Rscript_Indian_Philosophy.r
# Load necessary libraries
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
library(tibble)
library(reshape2)
library(wordcloud)
library(wordcloud2)
# tidyverse and tidytext work well together, so I loaded both. The stringr package is useful for filtering out the LaTeX specific code and also for dropping words that have numbers in them (like jefferson1776 as a reference or 0.05).

## Analyzes Approximately 750,000 words

# put/convert ind texts to a data frame
ind_words <- tibble(file = 
	paste0("~/VIT_University/tidy_text/",                              
	c("Indian_Philosophy_Part_I.txt", 
	"Indian_Philosophy_Part_II.txt"))) %>%
  	mutate(text = map(file, read_lines))
ind_words

# The resulting tibble has a variable file that is the name of the file that created that row and a list-column of the text of that file.

## Running a script – Preprogrammed Code

## 3D Plot of Half of a Torus

# 3D Plot of Half of a Torus 
par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(1, 1)) 
R <- 3 
r <- 2 
x <- seq(0, 2*pi,length.out=50) 
y <- seq(0, pi,length.out=50) 

M <- mesh(x, y)   
alpha <- M$x 
beta <- M$y     
surf3D(x = (R + r*cos(alpha)) * cos(beta),
      y = (R + r*cos(alpha)) * sin(beta), 
      z = r * sin(alpha), 
      colkey=FALSE, 
      bty="b2", 
      main="Half of a Torus")