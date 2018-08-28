install.packages("psych")
library(devtools)
library(MASS)
library(car)
library(psych)
library(flipMultivariates)
lda <- LDA(class ~ ., data = vehicles)

install_github("Displayr/flipStandardCharts")
# Load the data
wine<-read.csv("C:/Users/jeff/Documents/VIT_University/data/wine.csv")
wine
scatterplotMatrix(wine[2:6])
# Perform LDA
wine.lda <- lda(V1 ~ V2 +  V3  +  V4 +  V5 + V6  + V7  + V8  + V9 + V10 +  V11 + V12 + V13 + V14,data=wine)
wine.lda
# For convenience, the value for each discriminant function (eg. the first discriminant function) are scaled so that their mean value is zero
wine.lda$scaling[,1]

# To calculate the values of the first discriminant function, we can define our own function: calclda()
calclda <- function(variables,loadings)
  {
   # find the number of samples in the data set
   as.data.frame(variables)
   numsamples <- nrow(variables)
   # make a vector to store the discriminant function
   ld <- numeric(numsamples)
   # find the number of variables
   numvariables <- length(variables)
   # calculate the value of the discriminant function for each sample
   for (i in 1:numsamples)
   {
      valuei <- 0
      for (j in 1:numvariables)
      {
         valueij <- variables[i,j]
         loading <- loadings[j]
          valuei <- valuei + (valueij * loading)
       }
       ld[i] <- valuei
   }
   # standardize the discriminant function so that its mean value is 0:
   ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
   ld <- ld[[1]]
   return(ld)
  }
calclda(wine[2:14], wine.lda$scaling[,1])


# To calculate the values of the first discriminant function, we can define our own function: calclda()
meanlda <- function(variables,loadings)
  {
   # find the number of samples in the data set
   as.data.frame(variables)
   numsamples <- nrow(variables)
   # make a vector to store the discriminant function
   ld <- numeric(numsamples)
   # find the number of variables
   numvariables <- length(variables)
   # calculate the value of the discriminant function for each sample
   for (i in 1:numsamples)
   {
      valuei <- 0
      for (j in 1:numvariables)
      {
         valueij <- mean(variables[i,j])
         loading <- loadings[j]
          valuei <- valuei + (valueij * loading)
       }
       ld[i] <- valuei
   }
   # standardize the discriminant function so that its mean value is 0:
   ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
   ld <- ld[[1]]
   return(ld)
  }
meanlda(wine[2:14], wine.lda$scaling[,1])


# Predict the values of the first lienar discriminant function
wine.lda.values <- predict(wine.lda)
wine.lda.values$x[,1] 
ldahist(data = wine.lda.values$x[,1], g=wine$x0)
ldahist(data = wine.lda.values$x[,2], g=wine$x0)

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$x0,cex=0.7,pos=4,col="red") # add labels

printMeanAndSdByGroup <- function(variables,groupvariable) 
{ 
	# find out how many variables we have 
	variables <- as.data.frame(variables) 
	numvariables <- length(variables) 
	# find out how many values the group variable can take 
	groupvariable2 <- as.factor(groupvariable[[1]]) 
	levels <- levels(groupvariable2) 
	numlevels <- length(levels) 
	for (i in 1:numlevels) 
	{ 
		leveli <- levels[i] 
		levelidata <- variables[groupvariable==leveli,] 
		groupsize <- nrow(levelidata) 
		print(paste("Group",leveli,"Group size:",groupsize)) 
		print(paste("Group",leveli,"Means:")) 
		print(mean(levelidata))
		print(paste("Group",leveli,"Standard Deviations:")) 
		print(sd(levelidata))
}}

calcWithinGroupsVariance <- function(variable,groupvariable) 
      {
         # find out how many values the group variable can take
         groupvariable2 <- as.factor(groupvariable[[1]])
         levels <- levels(groupvariable2)
         numlevels <- length(levels)
         # get the mean and standard deviation for each group:
         numtotal <- 0
         denomtotal <- 0
         for (i in 1:numlevels)
         {
            leveli <- levels[i]
            levelidata <- variable[groupvariable==leveli,]
            levelilength <- length(levelidata)
            # get the mean and standard deviation for group i:
            meani <- mean(levelidata)
            sdi <- sd(levelidata)
            numi <- (levelilength - 1)*(sdi * sdi)
            denomi <- levelilength
            numtotal <- numtotal + numi
            denomtotal <- denomtotal + denomi 
         } 
         # calculate the within-groups variance
         Vw <- numtotal / (denomtotal - numlevels) 
         return(Vw)
      } 

groupStandardize <- function(variables, groupvariable)
  {
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-Standardized version of each variable
    for (i in 1:numvariables)
     {
     variablei <- variables[i]
     variablei_name <- variablenames[i]
     variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
     variablei_mean <- mean(variablei)
     variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
     data_length <- nrow(variablei)
     if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
     variables_new['variablei_name'] <- variablei_new
     }
  return(variables_new)
  }
groupstandardizedconcentrations <- groupStandardize(wine[2:14], wine[1])
    error.bars.by(wine.lda[2:3],wine.lda$V1,bars=TRUE,labels=c("CONC2","CONC3"),
    main="Concentration 2 and Concentration 3 by Wine Type",ylim=c(0,800),colors=c("red","blue"),
    legend=5,v.labels=c("CONC2","CONC3"))

wine.lda[2:3]

# OUTPUT
$counts
 1  2  3 
59 71 48 

$means
   wine$V2  wine$V3  wine$V4  wine$V5  wine$V6  wine$V7   wine$V8  wine$V9 wine$V10 wine$V11  wine$V12 wine$V13
1 13.74475 2.010678 2.455593 17.03729 106.3390 2.840169 2.9823729 0.290000 1.899322 5.528305 1.0620339 3.157797
2 12.27873 1.932676 2.244789 20.23803  94.5493 2.258873 2.0808451 0.363662 1.630282 3.086620 1.0562817 2.785352
3 13.15375 3.333750 2.437083 21.41667  99.3125 1.678750 0.7814583 0.447500 1.153542 7.396250 0.6827083 1.683542
   wine$V14
1 1115.7119
2  519.5070
3  629.8958	

Call:
lda(x0 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
    x12 + x13, data = wines)

Prior probabilities of groups:
        1         2         3 
0.3314607 0.3988764 0.2696629 

Group means:
        x1       x2       x3       x4       x5       x6        x7       x8       x9      x10
1 13.74475 2.010678 2.455593 17.03729 106.3390 2.840169 2.9823729 0.290000 1.899322 5.528305
2 12.27873 1.932676 2.244789 20.23803  94.5493 2.258873 2.0808451 0.363662 1.630282 3.086620
3 13.15375 3.333750 2.437083 21.41667  99.3125 1.678750 0.7814583 0.447500 1.153542 7.396250
        x11      x12       x13
1 1.0620339 3.157797 1115.7119
2 1.0562817 2.785352  519.5070
3 0.6827083 1.683542  629.8958

Coefficients of linear discriminants:
             LD1           LD2
x1  -0.403399781  0.8717930699
x2   0.165254596  0.3053797325
x3  -0.369075256  2.3458497486
x4   0.154797889 -0.1463807654
x5  -0.002163496 -0.0004627565
x6   0.618052068 -0.0322128171
x7  -1.661191235 -0.4919980543
x8  -1.495818440 -1.6309537953
x9   0.134092628 -0.3070875776
x10  0.355055710  0.2532306865
x11 -0.818036073 -1.5156344987
x12 -1.157559376  0.0511839665
x13 -0.002691206  0.0028529846

Proportion of trace:
   LD1    LD2 
0.6875 0.3125 

# This means that the first discriminant function is a linear combination of the variables: -0.403*V2 + 0.165*V3 – 0.369*V4 + 0.155*V5 – 0.002*V6 + 0.618*V7 – 1.661*V8 – 1.496*V9 + 0.134*V10 + 0.355*V11 – 0.818*V12 – 1.158*V13 – 0.003*V14, where V2, V3, … V14 are the concentrations of the 14 chemicals found in the wine samples. For convenience, the value for each discriminant function (eg. the first discriminant function) are scaled so that their mean value is zero (see below).