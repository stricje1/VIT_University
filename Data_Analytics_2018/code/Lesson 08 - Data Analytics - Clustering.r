#******************************************************************
# Hierarchical Clustering
#******************************************************************
## First load the package.
## votes.repub is a data frame with the percent of votes given to the republican candidate in presidential elections from 1856 to 1976. Rows represent the 50 states, and columns the 31 elections.
#---------------------------------------------------
#Agglomerative Nesting (Hierarchical Clustering) using agnes function computes agglomerative hierarchical clustering of the dataset.
#---------------------------------------------------
library(cluster)
data(votes.repub)
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)
#---------------------------------------------------
op <- par(mfrow=c(2,2))
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)
#---------------------------------------------------
## alpha = 0.625 ==beta = -1/4 is "recommended" by some
#---------------------------------------------------
agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.625)
plot(agnS)
#---------------------------------------------------
## "show" equivalence of three "flexible" special cases
#---------------------------------------------------
#daisy computes all the pairwise dissimilarities (distances) between observations in the data set. The original variables may be of mixed types. In that case, or whenever metric = "gower" is set, a generalization of Gower's formula is used.
#---------------------------------------------------
d.vr <- daisy(votes.repub)
a.wgt <- agnes(d.vr, method = "weighted")
a.sing <- agnes(d.vr, method = "single")
a.comp <- agnes(d.vr, method = "complete")
iC <- -(6:7) # not using 'call' and 'method' for comparisons
stopifnot(all.equal(a.wgt [iC], agnes(d.vr, method="flexible",par.method = 0.5)[iC]), all.equal(a.sing[iC], agnes(d.vr, method="flex", par.method= c(.5,.5,0, -.5))[iC]), all.equal(a.comp[iC], agnes(d.vr, method="flex", par.method= c(.5,.5,0, +.5))[iC]))
#---------------------------------------------------
# The class "dendrogram" provides general functions for handling tree-like structures. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.
#---------------------------------------------------
(d2 <- as.dendrogram(agn2)) # two main branches 'dendrogram' with 2 branches and 50 members total, at height 281.9508
d2[[1]] # the first branch 'dendrogram' with 2 branches and 8 members total, at height 116.7048 
d2[[2]] # the 2nd one { 8 + 42 = 50 } 'dendrogram' with 2 branches and 42 members total, at height 178.4119 
d2[[1]][[1]]# first sub-branch of branch 1 .. and shorter form 'dendrogram' with 2 branches and 6 members total, at height 72.92212
identical(d2[[c(1,1)]], d2[[1]][[1]])
## a "textual picture" of the dendrogram :
str(d2)

#***********************************************************************
plot(agnes(agriculture), ask = TRUE)
data(animals)
aa.a <- agnes(animals) # default method = "average"
aa.ga <- agnes(animals, method = "gaverage")
op <- par(mfcol=1:2, mgp=c(1.5, 0.6, 0), mar=c(.1+ c(4,3,2,1)),cex.main=0.8)
plot(aa.a, which.plot = 2)
#---------------------------------------------------

#---------------------------------------------------
#**********************************************************************
# k-Means Clustering
#**********************************************************************
#The wine dataset contains the results of a chemical analysis of wines grown in a specific area of Italy. Three types of wine are represented in the 178 samples, with the results of 13 chemical analyses recorded for each sample. The Type variable has been transformed into a categoric variable.
#---------------------------------------------------
library(rattle)
library(rattle.data)
data(wine)
head(wine)
#---------------------------------------------------
#A fundamental question is how to determine the value of the parameter k. If we looks at the percentage of variance explained as a function of the number of clusters, one should choose a number of clusters so that adding another cluster doesn’t give much better modeling of the data. More precisely, if one plots the percentage of variance explained by the clusters against the number of clusters, the first clusters will add much information (explain a lot of variance), but at some point the marginal gain will drop, giving an angle in the graph. The number of clusters is chosen at this point, hence the “elbow criterion”.
#---------------------------------------------------
df <- scale(wine[-1])
wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}
wssplot(df)
#---------------------------------------------------
#NbClust package provides 30 indices for determining the number of clusters and proposes to user the best clustering scheme from the different results obtained by varying all combinations of number of clusters, distance measures, and clustering methods.
#---------------------------------------------------
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
#---------------------------------------------------
# The Hubert index is a graphical method of determining the number of clusters.
#In the plot of Hubert index, we seek a significant knee that corresponds to a significant increase of the value of the measure i.e. the significant peak in Hubert index second differences plot. 
# The D index is a graphical method of determining the number of clusters. 
#In the plot of D index, we seek a significant knee (the significant peak in Dindex second differences plot) that corresponds to a significant increase of the value of the measure. 
#All 178 observations were used. 
#---------------------------------------------------
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
#---------------------------------------------------
library(cluster)
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
clusplot(wine, fit.km$cluster, color=TRUE, shade=TRUE,  labels=2, lines=0)
#---------------------------------------------------
#Hierarchical methods use a distance matrix as an input for the clustering algorithm. The choice of an appropriate metric will influence the shape of the clusters, as some elements may be close to one another according to one distance and farther away according to another.
d <- dist(wine, method = "euclidean") # Euclidean distance matrix.
#We use the Euclidean distance as an input for the clustering algorithm (Ward’s minimum variance criterion minimizes the total within-cluster variance):
H.fit <- hclust(d, method="ward.D")
H2.fit<- hclust(d, method="ward.D2")
#he clustering output can be displayed in a dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 
plot(H2.fit) # display dendogram
groups <- cutree(H2.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H2.fit, k=3, border="blue") 
#*********************************************************************
#Social Network Clustering Analysis
#*********************************************************************
#For this analysis, we will be using a dataset representing a random sample of 30.000 U.S. high school students who had profiles on a well-known Social Network in from 2006 to 2009.
#From the top 500 words appearing across all pages, 36 words were chosen to represent five categories of interests, namely extracurricular activities, fashion, religion, romance, and antisocial behavior. The 36 words include terms such as football, sexy, kissed, bible, shopping, death, and drugs. The final dataset indicates, for each person, how many times each word appeared in the person’s SNS profile.
#-------------------------------------------------------------------
social<-read.csv(file="C:/Users/Jeff/Documents/VIT_University/data/social_net.csv",header = T)
head(social,3)
dim(social)
#-------------------------------------------------------------------
#Let’s also take a quick look at the specifics of the data. The first several lines of the str() output are as follows:
#-------------------------------------------------------------------
str(social)
#-------------------------------------------------------------------
#As we had expected, the data include 30,000 teenagers with four variables indicating personal characteristics and 36 words indicating interests. Note that there are some NA’s in the variable gender.
#-------------------------------------------------------------------
summary(social$age)
#-------------------------------------------------------------------
#We will skip all the data with missing values:
#-------------------------------------------------------------------
social = na.omit(social)
dim(social)
#-------------------------------------------------------------------
#We’ll start our cluster analysis by considering only the 36 features that represent the number of times various interests appeared on the SNS profiles of teens. For convenience, let’s make a data frame containing only these features:
#-------------------------------------------------------------------
interests <- social[5:40]
#-------------------------------------------------------------------
#To apply z-score standardization to the interests data frame, we can use the scale() function with lapply(), as follows:
#-------------------------------------------------------------------
interests_z <- as.data.frame(lapply(interests, scale))
#-------------------------------------------------------------------
#To divide teens into five clusters, we can use the following command:
#-------------------------------------------------------------------
social_clusters <- kmeans(interests_z, 5)
#number of examples falling in each of the groups. If the groups are too large or too small, then they are not likely to be very useful. To obtain the size of the kmeans() clusters, use the teen_clusters$size component as follows:
#-------------------------------------------------------------------
social_clusters$size
#-------------------------------------------------------------------
#The cluster characterization can be obtained with pie charts:
#-------------------------------------------------------------------
par(mfrow=c(2,2))
pie(colSums(interests[social_clusters$cluster==1,]),cex=0.5)
pie(colSums(interests[social_clusters$cluster==2,]),cex=0.5)
pie(colSums(interests[social_clusters$cluster==3,]),cex=0.5)
pie(colSums(interests[social_clusters$cluster==4,]),cex=0.5)
#********************************************************************
#Customer Segmentation
#********************************************************************
#Customer segmentation is as simple as it sounds: grouping customers by their characteristics - and why would you want to do that? To better serve their needs!
#Our example is to do with e-mail marketing.
#-------------------------------------------------------------------
offers<-read.table("C:/Users/Jeff/Documents/VIT_University/data/offers.csv", sep = ',', header=T)
head(offers)
transactions<-read.table("C:/Users/Jeff/Documents/VIT_University/data/transactions.csv", sep = ',', header=T)
head(transactions)
#-------------------------------------------------------------------
#Step 1: Organizing the information
#-------------------------------------------------------------------
#We have two data sets: one for the offers and the other for the transactions. First what we need to do is create a transaction matrix. That means, we need to put the offers we mailed out next to the transaction history of each customer. This is easily achieved with a pivot table.
# Create transaction matrix (a pivot table like in Excel way!)
#-------------------------------------------------------------------
library(reshape)
pivot<-melt(transactions[1:2])#Using CustomerLastName as id variables
pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
pivot<-cbind(offers,pivot[-1])
# write.csv(file="pivot.csv",pivot) # to save your data
cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)
head(cluster.data)
#-------------------------------------------------------------------
#In the clustering data set, rows represents costumers and columns are different wine brands/types.
#-------------------------------------------------------------------
#Step 2: Distances and Clusters
#-------------------------------------------------------------------
#We will use k=4 indicating that we will use 4 clusters. This is somewhat arbitrary, but the number you pick should be representative of the number of segments you can handle as a business. So 100 segments does not make sense for an e-mail marketing campaign.
#We need to calculate how far away each customer is from the cluster’s mean. To do this we could use many distances/dissimilarity index, one of which is the Gower dissimilarity.
#-------------------------------------------------------------------
library(cluster)
D=daisy(cluster.data, metric='gower')
#-------------------------------------------------------------------
#After the creation of a distance matrix, we implement a Ward’s hierarchical clustering procedure:
#-------------------------------------------------------------------
H.fit <- hclust(D, method="ward.D2")
plot(H.fit) # display dendrogram
groups <- cutree(H.fit, k=4) # cut tree into 4 clusters
#-------------------------------------------------------------------
# draw dendogram with red borders around the 4 clusters
#-------------------------------------------------------------------
rect.hclust(H.fit, k=4, border="red") 
#-------------------------------------------------------------------
# 2D representation of the Segmentation:
#-------------------------------------------------------------------
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')
 
#****************************************************************************
#Generally, the way K-Means algorithms work is via an iterative refinement process:
##Each data point is randomly assigned to a cluster (number of clusters is given before hand).
##Each cluster’s centroid (mean within cluster) is calculated.
##Each data point is assigned to its nearest centroid (iteratively to minimise the within-cluster variation) until no major differences are found.
#-------------------------------------------------------------------
#Let’s have a look at an example in R using the Chatterjee-Price Attitude Data from the library(datasets) package. The dataset is a survey of clerical employees of a large financial organization. The data are aggregated from questionnaires of approximately 35 employees for each of 30 (randomly selected) departments. The numbers give the percent proportion of favourable responses to seven questions in each department. For more details, see ?attitude.
#-------------------------------------------------------------------
# Load necessary libraries
library(datasets)
# Inspect data structure
str(attitude)
# Summarise data
summary(attitude)
#-------------------------------------------------------------------
#As we’ve seen, this data gives the percent of favourable responses for each department. For example, in the summary output above we can see that for the variable privileges among all 30 departments the minimum percent of favourable responses was 30 and the maximum was 83. In other words, one department had only 30% of responses favourable when it came to assessing ‘privileges’ and one department had 83% of favorable responses when it came to assessing ‘privileges’, and a lot of other favourable response levels in between.
#In light of the example, we’ll take a subset of the attitude dataset and consider only two variables in our K-Means clustering exercise. So imagine that we would like to cluster the attitude dataset with the responses from all 30 departments when it comes to ‘privileges’ and ‘learning’ and we would like to understand whether there are commonalities among certain departments when it comes to these two variables.
#-------------------------------------------------------------------
# Subset the attitude data
dat = attitude[,c(3,4)]
# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =2)
#-------------------------------------------------------------------
#With the data subset and the plot above we can see how each department’s score behave across Privilege and Learning compare to each other. In the most simplistic sense, we can apply K-Means clustering to this data set and try to assign each department to a specific number of clusters that are “similar”.
#-------------------------------------------------------------------
#Let’s use the kmeans function from R base stats package:
# Perform K-Means with 2 clusters
#-------------------------------------------------------------------
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)
# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)
#-------------------------------------------------------------------
#As mentioned before, one of the key decisions to be made when performing K-Means clustering is to decide on the numbers of clusters to use. In practice, there is no easy answer and it’s important to try different ways and numbers of clusters to decide which options is the most useful, applicable or interpretable solution.

#In the plot above, we randomly chose the number of clusters to be 2 for illustration purposes only.

#However, one solution often used to identifiy the optimal number of clusters is called the Elbow method and it involves observing a set of possible numbers of clusters relative to how they minimise the within-cluster sum of squares. In other words, the Elbow method examines the within-cluster dissimilarity as a function of the number of clusters. Below is a visual representation of the method:

# Check for the optimal number of clusters given the data
#-------------------------------------------------------------------
mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#-------------------------------------------------------------------
# We can say that after 6 clusters the observed difference in the within-cluster dissimilarity is not substantial. Consequently, we can say with some reasonable confidence that the optimal number of clusters to be used is 6.

# Perform K-Means with the optimal number of clusters identified from the Elbow method
#-------------------------------------------------------------------
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2
# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
#-------------------------------------------------------------------
#From the results above we can see that there is a relatively well defined set of groups of departments that are relatively distinct when it comes to answering favourably around Privileges and Learning in the survey. It is only natural to think the next steps from this sort of output. One could start to devise strategies to understand why certain departments rate these two different measures the way they do and what to do about it. But we will leave this to another exercise.

#****************************************************************************
#Iris kmeans #1
iris2 <- iris
iris2$Species <- NULL
mydata <- iris2
#How many clusters?
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#K-Means	 
(kmeans.result <- kmeans(iris2, 3))
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)
#-------------------------------------------------------
#Iris kmeans #2
library(fpc)
pamk.result <- pamk(iris2)
pamk.result$nc
table(pamk.result$pamobject$clustering, iris$Species)
layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1)) # change back to one graph per page
#-------------------------------------------------------
library(fpc)
pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species) 
layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pam.result)
layout(matrix(1)) # change back to one graph per page
#-------------------------------------------------------
#Iris kmeans #3
library(ggplot2)
set.seed(20)
irisCluster <- kmeans(iris2[, 3:4], 3, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
#-----------------------------------------------------
#Hierarchical Clustering
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)
#-----------------------------------------------------
#Density-based Clustering
library(fpc)
iris2 <- iris[-5] # remove class tags
ds <- dbscan(iris2, eps=0.42, MinPts=5)
# compare clusters with original class labels
table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])
plotcluster(iris2, ds$cluster)
# create a new dataset for labeling
set.seed(435)
idx <- sample(1:nrow(iris), 10)
newData <- iris[idx,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
# label new data
myPred <- predict(ds, iris2, newData)
# plot result
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
# check cluster labels
table(myPred, iris$Species[idx])
