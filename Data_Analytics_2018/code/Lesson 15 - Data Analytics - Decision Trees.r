#*****************************************************************************
# Classification Tree with rpart
#*****************************************************************************
#Let’s use the data frame kyphosis to predict a type of deformation (kyphosis) after surgery, from age in months (Age), number of vertebrae involved (Number), and the highest vertebrae operated on (Start).
#In R, call the rpart library. Recursive partitioning for classification, regression and survival trees. 
#---------------------------------------------------
library(rpart)
# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start, method= "class", data=kyphosis)
#---------------------------------------------------
#where kyphosis is the response, with variables Age, Number, and Start. Class in the method and kyphosis is the data set. Next, we display the results.
#---------------------------------------------------
# display the results
printcp(fit)
# visualize cross-validation results 
plotcp(fit)
# detailed summary of splits
summary(fit)
#---------------------------------------------------
# plot tree 
plot(fit, uniform=TRUE, main= "Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
#---------------------------------------------------
# create attractive postscript plot of tree 
post(pfit, file = "C:/Users/Jeff/Documents/VIT_University/tree.ps", title = "Classification Tree for Kyphosis")
#*****************************************************************************
#---------------------------------------------------
# prune the tree 
pfit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])
# plot the pruned tree 
pfit<-prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#------------------------------------------------------------------
#In this example we will predict car mileage from price, country, reliability, and car type. The data frame is cu.summary. 
#---------------------------------------------------
post(pfit, file = "C:/Users/Jeff/Documents/VIT_University/ptree2.ps", title = "Pruned Regression Tree for Mileage")

#******************************************************************************
# Regression Tree Example
#******************************************************************************
library(rpart)
#---------------------------------------------------
 # grow tree 
fit2 <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
#---------------------------------------------------
printcp(fit2) # display the results 
plotcp(fit2) # visualize cross-validation results 
summary(fit2) # detailed summary of splits
#---------------------------------------------------
 # create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit2) # visualize cross-validation results   
#---------------------------------------------------
 # plot tree 
plot(fit2, uniform=TRUE, main= "Regression Tree for Mileage")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)
#---------------------------------------------------
 # create attractive postcript plot of tree 
post(fit2, file = "C:/Users/Jeff/Documents/VIT_University/ptree4.ps", title = "Regression Tree for Mileage")
#---------------------------------------------------
# prune the tree 
pfit2<- prune(fit2, cp=0.01160389) # from cptable   
#---------------------------------------------------
 # plot the pruned tree 
plot(pfit2, uniform=TRUE, main= "Pruned Regression Tree for Mileage")
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)
post(pfit2, file = "C:/Users/Jeff/Documents/VIT_University/ptree5.ps", title = "Pruned Regression Tree for Mileage")

#*****************************************************************************
#Regression Trees
#*****************************************************************************

#Regression Trees like say linear regression, outputs an expected value given a certain output. For demonstration purpose, we will train a regression model based on the California housing prices data from the 1990 Census. 
#---------------------------------------------------
library(tree)
real.estate <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/cadata.csv", header=TRUE)
tree.model <- tree(log(HomeValue) ~ Longitude + Latitude, data=real.estate)
plot(tree.model)
text(tree.model, cex=.75)
#---------------------------------------------------
#Notice that the leaf values represent the log of the price, since that was the way we represented the formula in the tree() function.
#(note: Knitr seems to output the wrong values above, check the results yourself in R)
#We can compare the predictions with the dataset (darker is more expensive) which seem to capture the global price trend:
#---------------------------------------------------
tree.model <- tree(HomeValue ~ Longitude + Latitude, data=real.estate)
price.deciles <- quantile(real.estate$HomeValue, 0:10/10)
cut.prices    <- cut(real.estate$HomeValue, price.deciles, include.lowest=TRUE)
plot(real.estate$Longitude, real.estate$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude",ylab="Latitude")
partition.tree(tree.model, ordvars=c("Longitude","Latitude"), add=TRUE)
summary(tree.model)
#---------------------------------------------------
#Deviance means here the mean squared error.
#---------------------------------------------------
#The flexibility of a tree is basically controlled by how many leaves they have, since that’s how many cells they partition things into. The tree fitting function has a number of controls settings which limit how much it will grow | each node has to contain a certain number of points, and adding a node has to reduce the error by at least a certain amount. The default for the latter, min.dev, is 0:01; let’s turn it down and see what happens:
#---------------------------------------------------
tree.model2 <- tree(log(HomeValue) ~ Longitude + Latitude, data=real.estate, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)
summary(tree.model2)
#---------------------------------------------------
#It’s obviously much finer-grained than the previous example (68 leafs against 12), and does a better job of matching the actual prices (lower error).
#Also, we can include all the variables, not only the latitude and longitude:
#---------------------------------------------------
tree.model3 <- tree(log(HomeValue) ~ ., data=real.estate)
plot(tree.model3)
text(tree.model3, cex=.75)
summary(tree.model3)

#Prune model 3
library(tree)
pfit3<-prune.tree(tree.model3, k = 0.1, best = 10, method = "deviance", eps = 1e-3)
plot(pfit3, type = "proportional", main="Pruned Regression Tree for Mileage")
text(pfit3,  all=TRUE, cex=.8)
#-----------------------------------------------------------------------
#Classification Trees
#---------------------------------------------------
#Classification trees output the predicted class for a given sample.
#Let’s use here the iris dataset (and split it into train and test sets):
#---------------------------------------------------
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(iris), alpha * nrow(iris))
train.set <- iris[inTrain,]
test.set  <- iris[-inTrain,]
#---------------------------------------------------
#There are two options for the output: + Point prediction: simply gives the predicted class + Distributional prediction: gives a probability for each class
#---------------------------------------------------
library(tree)
tree.model <- tree(Species ~ Sepal.Width + Petal.Width, data=train.set)
tree.model
summary(tree.model)
#---------------------------------------------------
# Distributional prediction
my.prediction <- predict(tree.model, test.set) # gives the probability for each class
head(my.prediction)
#---------------------------------------------------
# Point prediction
# Let's translate the probability output to categorical output
maxidx <- function(arr) {
    return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('setosa', 'versicolor', 'virginica')[idx]
table(prediction, test.set$Species)
plot(tree.model)
text(tree.model)
# Another way to show the data:
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
partition.tree(tree.model, label="Species", add=TRUE)
legend("topright",legend=unique(iris$Species), col=unique(as.numeric(iris$Species)), pch=19)
summary(tree.model)
#---------------------------------------------------
#We can prune the tree to prevent overfitting. The next function prune.tree() allows us to choose how many leafs we want the tree to have, and it returns the best tree with that size.
#The argument newdata accepts new input for making the prune decision. If new data is not given, the method uses the original dataset from which the tree model was built.
#For classification trees we can also use argument method="misclass" so that the pruning measure should be the number of misclassifications.
#---------------------------------------------------
pruned.tree <- prune.tree(tree.model, best=4)
plot(pruned.tree)
text(pruned.tree)
#---------------------------------------------------
pruned.prediction <- predict(pruned.tree, test.set, type="class") # give the predicted class
table(pruned.prediction, test.set$Species)
#---------------------------------------------------
#This package can also do K-fold cross-validation using cv.tree() to find the best tree:
# here, let's use all the variables and all the samples
#---------------------------------------------------
tree.model <- tree(Species ~ ., data=iris)
summary(tree.model)
#---------------------------------------------------
cv.model <- cv.tree(tree.model)
plot(cv.model)
#---------------------------------------------------
cv.model$dev  # gives the deviance for each K (small is better)
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size
#---------------------------------------------------
# let's refit the tree model (the number of leafs will be no more than best.size)
#---------------------------------------------------
cv.model.pruned <- prune.misclass(tree.model, best=best.size)
summary(cv.model.pruned)
#---------------------------------------------------
#The misclassification rate has just slighty increased with the pruning of the tree.
#
#******************************************************************************
# Purity and Entropy
#******************************************************************************
#
#We define a subset to be completely pure if it contains only a single class. For example, if a subset contains only poisonous mushrooms, it is completely pure. In R, assuming that the last column contains the class (i.e. the category to be predicted), this can be written as:
#-------------------------------------------------------------------
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}
#The entropy is a measure of the purity of a dataset.
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}
# If a dataset is completely pure, then it has entropy 0:
Entropy(c(10, 0))
Entropy(c(0, 10))
# If a set contains 5 poisonous and 5 edible mushrooms, the entropy becomes 1, as the purity is at its lowest:
Entropy(c(5, 5))
# We can plot the entropy as a function of the number of edible mushrooms in a set of, say, 100 mushrooms:
entropy <- function(edible) Entropy(c(edible, 100 - edible))
entropy <- Vectorize(entropy)
curve( entropy, from = 0, to = 100, xname = 'edible')
#
#*****************************************************************************
# Information Gain
#*****************************************************************************
#
#Mathematically, the information gain IG is defined as:
#IG(T,a)=H(T)=SUM_{v in vals(a)}((abs({x in T|x_a = v})/abs(T))*H({x in T|x_a = v}))
#In words, the information gain measures the difference between the entropy before the split, and the weighted sum of the entropies after the split:
#So, let’s rewrite that in R:
#----------------------------------------------------------
InformationGain<-function( tble ) {
tble <- as.data.frame.matrix(tble)
entropyBefore <- Entropy(colSums(tble))
s <- rowSums(tble)
entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}
#-----------------------------------------------------------
#For example, using the mushroom data set:
library(data.tree)
data(mushroom)
tble <- table(mushroom[,c('color', 'edibility')])
tble

InformationGain(tble)
InformationGain(table(mushroom[,c('size', 'edibility')]))
InformationGain(table(mushroom[,c('points', 'edibility')]))

#****************************************************************************
# ID3 Algorithm
#****************************************************************************

TrainID3 <- function(node, data) {
    
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
            function(x) InformationGain(
              table(data[,x], data[,ncol(data)])
              )
            )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }  
  }
}
#
#***************************************************************************
# End TrainID3
#***************************************************************************
#
#Training with data
tree <- Node$new("mushroom")
TrainID3(tree, mushroom)
print(tree, "feature", "obsCount")
#-------------------------------------------------------------------
#Predict Function
#-------------------------------------------------------------------
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}
#Predicting some classes
Predict(tree, c(color = 'red', 
                size = 'large', 
                points = 'yes')
        )
#		
#************************************************************************
# World PopulationTreeMap
#************************************************************************
#
### convert a data.frame to a data.tree structure
### navigate a tree and locate specific nodes
### use Aggregate and Cumulate
### manipulate an existing tree, e.g. by using the Prune method
#-------------------------------------------------------------------
library(treemap)
data(GNI2014)
treemap(GNI2014,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")
#-------------------------------------------------------------------
# Convert from data.frame
#-------------------------------------------------------------------
# First, let’s convert the population data into a data.tree structure:	  
#------------------------------------------------------------------- 
library(data.tree)
GNI2014$continent <- as.character(GNI2014$continent)
GNI2014$pathString <- paste("world", GNI2014$continent, GNI2014$country, sep = "/")
tree <- as.Node(GNI2014[,])
print(tree, pruneMethod = "dist", limit = 20)
#-------------------------------------------------------------------
#We can also navigate the tree to find the population of a specific country. Luckily, RStudio is quite helpful with its code completion (use CTRL + SPACE):
#-------------------------------------------------------------------
tree$Europe$Switzerland$population
#-------------------------------------------------------------------
#Or, we can look at a sub-tree:
#-------------------------------------------------------------------
northAm <- tree$`North America`
northAm$Sort("GNI", decreasing = TRUE)
print(northAm, "iso3", "population", "GNI", limit = 12)

#-------------------------------------------------------------------#Or, we can find out what is the country with the largest GNI:
#-------------------------------------------------------------------
maxGNI <- Aggregate(tree, "GNI", max)
#same thing, in a more traditional way:
maxGNI <- max(sapply(tree$leaves, function(x) x$GNI))

tree$Get("name", filterFun = function(x) x$isLeaf && x$GNI == maxGNI)
#-------------------------------------------------------------------
# Aggregate and Cumulate
#-------------------------------------------------------------------
#We aggregate the population. For non-leaves, this will recursively iterate through children, and cache the result in the population field.
#-------------------------------------------------------------------
tree$Do(function(x) {
        x$population <- Aggregate(node = x,
        attribute = "population",
        aggFun = sum)
        }, 
     traversal = "post-order")
#-------------------------------------------------------------------
# Next, we sort each node by population:
#-------------------------------------------------------------------
tree$Sort(attribute = "population", decreasing = TRUE, recursive = TRUE)	 
#-------------------------------------------------------------------
#Finally, we cumulate among siblings, and store the running sum in an attribute called cumPop:
#-------------------------------------------------------------------
tree$Do(function(x) x$cumPop <- Cumulate(x, "population", sum))
#-------------------------------------------------------------------
# The tree now looks like this:
#-------------------------------------------------------------------
print(tree, "population", "cumPop", pruneMethod = "dist", limit = 20)
#-------------------------------------------------------------------
# Prune
#-------------------------------------------------------------------
#The previous steps were done to define our threshold: big countries should be displayed, while small ones should be grouped together. This lets us define a pruning function that will allow a maximum of 7 countries per continent, and that will prune all countries making up less than 90% of a continent’s population.
#We would like to store the original number of countries for further use:
#-------------------------------------------------------------------
tree$Do(function(x) x$origCount <- x$count)
#-------------------------------------------------------------------
# We are now ready to prune. This is done by defining a pruning function, returning ‘FALSE’ for all countries that should be combined:
#-------------------------------------------------------------------
myPruneFun <- function(x, cutoff = 0.9, maxCountries = 7) {
  if (isNotLeaf(x)) return (TRUE)
  if (x$position > maxCountries) return (FALSE)
  return (x$cumPop < (x$parent$population * cutoff))
}
#-------------------------------------------------------------------
# We clone the tree, because we might want to play around with different parameters:
#-------------------------------------------------------------------
treeClone <- Clone(tree, pruneFun = myPruneFun)
print(treeClone$Oceania, "population", pruneMethod = "simple", limit = 20)
#-------------------------------------------------------------------
# Finally, we need to sum countries that we pruned away into a new “Other” node:
#-------------------------------------------------------------------
treeClone$Do(function(x) {
  missing <- x$population - sum(sapply(x$children, function(x) x$population))
  other <- x$AddChild("Other")
  other$iso3 <- paste0("OTH(", x$origCount, ")")
  other$country <- "Other"
  other$continent <- x$name
  other$GNI <- 0
  other$population <- missing
},
filterFun = function(x) x$level == 2
)


print(treeClone$Oceania, "population", pruneMethod = "simple", limit = 20)
#-------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------
#Plotting the treemap
#n order to plot the treemap, we need to convert the data.tree structure back to a data.frame:

df <- ToDataFrameTable(treeClone, "iso3", "country", "continent", "population", "GNI")

treemap(df,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value")
#-------------------------------------------------------------------		
# Plot as dendrogram
#-------------------------------------------------------------------
#Just for fun, and for no reason other than to demonstrate conversion to dendrogram, we can plot this in a very unusual way:

plot(as.dendrogram(treeClone, heightAttribute = "population"))	

#**************************************************************************
# Portfolio Breakdown (finance)
#**************************************************************************

# In this example, we show how to display an investment portfolio as a hierarchic breakdown into asset classes. You’ll see:
### how you can re-use a traversal
### advanced use of Aggregate
### how to add default attribute formatters to your tree
#Convert from data.frame
#-------------------------------------------------------------------
fileName <- system.file("extdata", "portfolio.csv", package="data.tree")
pfodf <- read.csv(fileName, stringsAsFactors = FALSE)
head(pfodf)

# Let us convert the data.frame to a data.tree structure. Here, we use again the path string method. For other options, see ?as.Node.data.frame

pfodf$pathString <- paste("portfolio", 
                          pfodf$AssetCategory, 
                          pfodf$AssetClass, 
                          pfodf$SubAssetClass, 
                          pfodf$ISIN, 
                          sep = "/")
pfo <- as.Node(pfodf)

# Aggregate
#To calculate the weight per asset class, we use the Aggregate method:

t <- Traverse(pfo, traversal = "post-order")
Do(t, function(x) x$Weight <- Aggregate(node = x, attribute = "Weight", aggFun = sum))

# We now calculate the WeightOfParent,

Do(t, function(x) x$WeightOfParent <- x$Weight / x$parent$Weight)

# Duration is a bit more complicated, as this is a concept that applies only to the fixed income asset class. Note that, in the second statement, we are reusing the traversal from above.

pfo$Do(function(x) x$Duration <- ifelse(is.null(x$Duration), 0, x$Duration), filterFun = isLeaf)
Do(t, function(x) x$Duration <- Aggregate(x, function(x) x$WeightOfParent * x$Duration, sum))

# Formatters
#We can add default formatters to our data.tree structure. Here, we add them to the root, but we might as well add them to any Node in the tree.

SetFormat(pfo, "WeightOfParent", function(x) FormatPercent(x, digits = 1))
SetFormat(pfo, "Weight", FormatPercent)

FormatDuration <- function(x) {
  if (x != 0) res <- FormatFixedDecimal(x, digits = 1)
  else res <- ""
  return (res)
}

SetFormat(pfo, "Duration", FormatDuration)

# These formatter functions will be used when printing a data.tree structure.
# Print
print(pfo, 
      "Weight", 
      "WeightOfParent",
      "Duration",
      filterFun = function(x) !x$isLeaf)
	  
#**************************************************************************
# Jenny Lind (decision tree, plotting)
#**************************************************************************

# This demo calculates and plots a simple decision tree. It demonstrates the following:
###how to read a yaml file into a data.tree structure
###how to calculate a decision tree
###how to plot a data.tree with the data.tree plotting facility
#Load YAML file
#YAML is similar to JSON, but targeted towards humans (as opposed to computers). It’s consise and easy to read. YAML can be a neat format to store your data.tree structures, as you can use it across different software and systems, you can edit it with any text editor, and you can even send it as an email.

#This is how our YAML file looks:

fileName <- system.file("extdata", "jennylind.yaml", package="data.tree")
cat(readChar(fileName, file.info(fileName)$size))	  

# Let’s convert the YAML into a data.tree structure. First, we load it with the yaml package into a list of lists. Then we use as.Node to convert the list into a data.tree structure:

library(data.tree)
library(yaml)
lol <- yaml.load_file(fileName)
jl <- as.Node(lol)
print(jl, "type", "payoff", "p")
#-------------------------------------------------------------------
# Calculate
#-------------------------------------------------------------------
# Next, we define our payoff function, and apply it to the tree. Note that we use post-order traversal, meaning that we calculate the tree from leaf to root:

payoff <- function(node) {
  if (node$type == 'chance') node$payoff <- sum(sapply(node$children, function(child) child$payoff * child$p))
  else if (node$type == 'decision') node$payoff <- max(sapply(node$children, function(child) child$payoff))
}

jl$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)
# The decision function is the next step. Note that we filter on decision nodes:

decision <- function(x) {
  po <- sapply(x$children, function(child) child$payoff)
  x$decision <- names(po[po == x$payoff])
}

jl$Do(decision, filterFun = function(x) x$type == 'decision')
#-------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------
#Plot with the data.tree plotting facility
#The data tree plotting facility uses GraphViz / Diagrammer. You can provide a function as a style:

GetNodeLabel <- function(node) switch(node$type, 
                                      terminal = paste0( '$ ', format(node$payoff, scientific = FALSE, big.mark = ",")),
                                      paste0('ER\n', '$ ', format(node$payoff, scientific = FALSE, big.mark = ",")))

GetEdgeLabel <- function(node) {
  if (!node$isRoot && node$parent$type == 'chance') {
    label = paste0(node$name, " (", node$p, ")")
  } else {
    label = node$name
  }
  return (label)
}

GetNodeShape <- function(node) switch(node$type, decision = "box", chance = "circle", terminal = "none")


SetEdgeStyle(jl, fontname = 'helvetica', label = GetEdgeLabel)
SetNodeStyle(jl, fontname = 'helvetica', label = GetNodeLabel, shape = GetNodeShape)
#Note that the fontname is inherited as is by all children, whereas e.g. the label argument is a function, it’s called on each inheriting child node.

#Another alternative is to set the style per node:

jl$Do(function(x) SetEdgeStyle(x, color = "red", inherit = FALSE), 
      filterFun = function(x) !x$isRoot && x$parent$type == "decision" && x$parent$decision == x$name)
#Finally, we direct our plot from left-to-right, and use the plot function to display:

SetGraphStyle(jl, rankdir = "LR")
plot(jl)

#****************************************************************************
# File Explorer (system utilities)
#****************************************************************************

# In this example, we print the files that exist in the folder structure of the file system. As a special goodie, we’ll show code that lets you build your own R File Explorer, an interactive tree / list widget that lets you expand folders and browse through your file system.
#-------------------------------------------------------------------
#Print
#-------------------------------------------------------------------
#First, let’s read the files in a directory tree into R. In this example, the root path “..” is the parent of the vignettes folder, i.e. the data.tree package folder itself:

path <- ".."
files <- list.files(path = path, 
                    recursive = TRUE,
                    include.dirs = FALSE) 

df <- data.frame(
      filename = sapply(files, 
                        function(fl) paste0("data.tree","/",fl)
      ), 
      file.info(paste(path, files, sep = "/")),
      stringsAsFactors = FALSE
    )
 
print(head(df)[-c(3,5,6)], row.names = FALSE)
#-------------------------------------------------------------------
# We now convert this into a data.tree:
#-------------------------------------------------------------------
fileStructure <- as.Node(df, pathName = "filename")
fileStructure$leafCount / (fileStructure$totalCount - fileStructure$leafCount)

print(fileStructure, "mode", "size", limit = 25)
#-------------------------------------------------------------------
# Listviewer html widget
#-------------------------------------------------------------------
#Finally, we can display the files by timelyportfolio’s listviewer. As it’s not on CRAN, we only display a screenshot of the widget in in this vignette. This is not half as fun as the interactive widget, of course. So please try it out for yourself to see it in action.

#This requires listviewer, which is available only on github
#devtools::install_github("timelyportfolio/listviewer")
#-------------------------------------------------------------------
library(listviewer)

l <- ToListSimple(fileStructure)
jsonedit(l)

#****************************************************************************
# Bubble Chart (visualisation)
#****************************************************************************

#In this example, we will replicate Mike Bostock’s bubble example. See here for details: http://bl.ocks.org/mbostock/4063269.
#We use Joe Cheng’s bubbles package. All of this is inspired by Timelyportfolio, the king of htmlwidgets.

#You’ll learn how to convert a complex JSON into a data.frame, and how to use this to plot hierarchic visualisations.
#-------------------------------------------------------------------
#Load JSON file
#-------------------------------------------------------------------
#The data represents the Flare class hierarchy, which is a code library for creating visualizations. The JSON is long, deeply nested, and complicated.
#-------------------------------------------------------------------
fileName <- system.file("extdata", "flare.json", package="data.tree")
flareJSON <- readChar(fileName, file.info(fileName)$size)
cat(substr(flareJSON, 1, 300))

# So, let’s convert it into a data.tree structure:

library(jsonlite)
flareLoL <- fromJSON(file(fileName),
                     simplifyDataFrame = FALSE
                     )

flareTree <- as.Node(flareLoL, mode = "explicit")
flareTree$fieldsAll

print(flareTree, "size", limit = 30)

# Finally, we can convert it into a data.frame. The ToDataFrameTable only converts leafs, but inherits attributes from ancestors:

flare_df <- ToDataFrameTable(flareTree, 
                             className = function(x) x$parent$name, 
                             packageName = "name", 
                             "size")
head(flare_df)

# This does not look spectacular. But take a look at this stack overflow question to see how people struggle to do this type of operation.
#-------------------------------------------------------------------
#Here, it was particularly simple, because the underlying JSON structure is regular. If it were not (e.g. some nodes contain different attributes than others), the conversion from JSON to data.tree would still work. And then, as a second step, we could modify the data.tree structure before converting it into a data.frame. For example, we could use Prune and Remove to remove unwanted nodes, use Set to remove or add default values, etc.
#-------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------
#What follows has nothing to do with data.tree anymore. We simply provide the bubble chart printing for your enjoyment. In order to run it yourself, you need to install the bubbles package from github:
#-------------------------------------------------------------------
#devtools::install_github("jcheng5/bubbles@6724e43f5e")
library(scales)
library(bubbles)
library(RColorBrewer)
bubbles(
  flare_df$size,
  substr(flare_df$packageName, 1, 2),
  tooltip = flare_df$packageName,
  color = col_factor(
    brewer.pal(9,"Set1"),
    factor(flare_df$className)
  )(flare_df$className),
  height = 800,
  width = 800
)