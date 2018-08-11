# Clustering Models in R
# The functions in the vegan package contain tools for diversity analysis, ordination methods and tools for the analysis of dissimilarities. Together with the labdsv package, the vegan package provides most standard tools of descriptive community analysis.
# The dune meadow vegetation data, dune, has cover class values of 30 species on 20 sites. The corresponding environmental data frame dune.env has following entries:
## A1: a numeric vector of thickness of soil A1 horizon.
## Moisture: an ordered factor with levels: 1 < 2 < 4 < 5.
## Management: a factor with levels: BF (Biological farming), HF (Hobby farming), NM (Nature Conservation Management), and SF (Standard Farming).
## Use: an ordered factor of land-use with levels: Hayfield < Haypastu < Pasture.
## Manure: an ordered factor with levels: 0 < 1 < 2 < 3 < 4.
# install.packages("vegan")
library(vegan) 
library(vegdata) 
#Example 1: h-cluster
data(dune)
dis <- vegdist(dune) 
# Function hclust provides several alternative clustering strategies. In community ecology, most popular are single linkage a.k.a. nearest neighbour, complete linkage a.k.a. furthest neighbour, and various brands of average linkage methods. 
clus <- hclust(dis, "single") 
plot(clus)
 
cluc <- hclust(dis, "complete") 
plot(cluc)
 
clua <- hclust(dis, "average") 
plot(clua)
range(dis)

# Cophenetic correlation measures the similarity between original dissimilarities and dissimilarities estimated from the tree
cor(dis, cophenetic(clus)) 
cor(dis, cophenetic(cluc))
cor(dis, cophenetic(clua))

# Cutting 
plot(cluc)
rect.hclust(cluc, 3)
grp <- cutree(cluc, 3)

#The only continuous variable in the Dune data is the thickness of the A1 horizon
data(dune.env)
boxplot(A1 ~ grp, data=dune.env, notch = TRUE)

#The clustering results can be displayed in ordination diagrams. All usual vegan functions for factors can be used: ordihull, ordispider, and ordiellipse. 
ord <- cca(dune) 
plot(ord, display = "sites")
ordihull(ord, grp, lty = 2, col = "red")

#The vegan package has function ordicluster to overlay hclust tree in an ordination
plot(ord, display="sites") 
ordicluster(ord, cluc, col="blue")

# Example 2
## reorder by water content of soil
data(mite, mite.env)
hc <- hclust(vegdist(wisconsin(sqrt(mite))))
ohc <- with(mite.env, reorder(hc, WatrCont))
plot(hc)
plot(ohc)

# Example 3
data(dune)

d <- vegdist(dune)
par(mfrow=c(1,3))
par(mfrow=c(3,1))
par(mfrow=c(1,1))
par(mar=c(3,4,1,1)+.1) 
csin <- hclust(d, method="single") 
csin

plot(csin)
plot(csin, hang=-1) 
ccom <- hclust(d, method="complete") 
plot(ccom, hang=-1) 
caver <- hclust(d, method="aver") 
plot(caver, hang=-1)

vegemite(dune, caver)

plot(csin, hang=-1) 
rect.hclust(csin, 3) 
plot(ccom, hang=-1) 
rect.hclust(ccom, 3)

plot(caver, hang=-1) 
rect.hclust(caver, 3)

cl <- cutree(ccom, 3) 
cl
 
table(cl)

table(cl, cutree(csin, 3)) 

table(cl, cutree(caver, 3)) # Classical tree pruning
   
ord <- cmdscale(d) # Classical (Metric) Multidimensional Scaling

ordiplot(ord)


ordihull(ord, cl, lty=3) 
ordispider(ord, cl, col="blue", label=TRUE) 
ordiellipse(ord, cl, col="red")

ordiplot(ord, dis="si")
ordihull(ord, cutree(caver, 3)) 
ordiplot(ord, dis="si") 
ordicluster(ord, csin)

ordiplot(ord, dis="si") 
ordicluster(ord, caver)

ordiplot(ord, dis="si") 
ordicluster(ord, caver, prune=2)

den <- as.dendrogram(caver)

x <- scores(ord, display = "sites", choices = 1) 
oden <- reorder(den, x)

par(mfrow=c(2,1)) 
plot(den) 
plot(oden) 
par(mfrow=c(1,1))

vegemite(dune, oden)

tabasco(dune, caver)

tabasco(dune, caver, Rowv = FALSE) 
tabasco(dune, oden, Rowv = FALSE)

mst <- spantree(d)

ordiplot(ord, dis="si") 
lines(mst, ord)

plot(mst, type="t")


plot(d, cophenetic(csin), asp=1) 
abline(0, 1) 
plot(d, cophenetic(ccom), asp=1) 
abline(0, 1)
plot(d, cophenetic(caver), asp=1) 
abline(0, 1)
cor(d, cophenetic(csin)) 
cor(d, cophenetic(ccom)) 
cor(d, cophenetic(caver))

cl <- factor(cl)

Moist <- with(dune.env, as.numeric(as.character(Moisture)))


data(dune.env)

Moist <- with(dune.env, as.numeric(as.character(Moisture)))
with(dune.env, as.numeric(Moisture))

boxplot(Moist ~ cl, notch=TRUE)

anova(lm(Moist ~ cl))

anova(rda(Moist ~ cl))

with(dune.env, table(cl, Management))


install.packages("labdsv")

library(labdsv)



const(dune, cl)

importance(dune, cl)
 
mod <- indval(dune, as.numeric(cl))


names(mod)

summary(mod)

summary(mod, type = "long")
 
ckm <- kmeans(decostand(dune, "hell"), 3) 
ckm$cluster

ordiplot(ord, dis="si")
ordihull(ord, ckm$cluster, col="red")

install.packages("mclust")

library(mclust)           # load mclust library


x = faithful[,1]          # get the first column of the faithful data set
y = faithful[,2]          # get the second column of the faithful data set
plot(x,y)                 # plot the spread points before the clustering
model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)
data = faithful           # get the data set 
plot(model, faithful)     # plot the clustering results


model <- Mclust(faithful)
data = faithful
plot(model, faithful)

model <- mclust(faithful)


install.packages("mclust")


x = faithful[,1]          # get the first column of the faithful data set
y = faithful[,2]          # get the second column of the faithful data set
plot(x,y)                 # plot the spread points before the clustering
model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)

data = faithful           # get the data set 
plot(model)               # plot the clustering results

model <- mclust(faithful)

library(mclust)           # load mclust library

x = faithful[,1]          # get the first column of the faithful data set
y = faithful[,2]          # get the second column of the faithful data set
plot(x,y)                 # plot the spread points before the clustering
model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)
data = faithful           # get the data set 
plot(model, faithful)     # plot the clustering results

mydata = USArrests
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata <- scale(mydata) # standardize variables
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
# The "ward" method has been renamed to "ward.D"; note new "ward.D2"
plot(fit) # display dendogram
k1 = 2 # eyeball the no. of clusters
# cut tree into k1 clusters
groups <- cutree(fit, k=k1)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=k1, border="red")

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #


# Use optimal no. of clusters in k-means #
k1=2
# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution
# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
fit # view solution summary

fit$BIC # lookup all the options attempted

classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows


# Use only if you want to save the output
write.table(mydata1,file.choose())#save output

fit$BIC # lookup all the options attempted
 
classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows


# Use only if you want to save the output
write.table(mydata1,file.choose())#save output
fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)
# get cluster means
cmeans=aggregate(mydata.orig,by=list(classif),FUN=mean); cmeans
 
# Read offers and transaction data
offers <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/OfferInformation.csv")
  View(offers)
  plot(offers)
transactions <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/transactions_india.csv")
  View(transactions)
  plot(transactions)
#Load Libraries
  library(ClusterR)
  library(reshape2)
  library(reshape)
  pivot<-melt(transactions[1:2])
  View(pivot)
  ## Using CustomerLastName as id variables
  pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
  pivot<-cbind(offers,pivot[-1])
  
  # write.csv(file="pivot.csv",pivot) # to save your data
  
  cluster.data<-pivot[,3:length(pivot)]
  fit<-KMeans_rcpp(cluster.data, clusters = 5, num_init = 5, max_iters = 100, initializer = 'kmeans++', verbose = F)
    fit
  head(cluster.data)
  
  #In the clustering data set, rows represents costumers and columns are different wine brands/types.
  #Step 2: Distances and Clusters
  #We will use 
  k=4
  #indicating that we will use 4 clusters. This is somewhat arbitrary, but the number you pick should be representative of the number of segments you can handle as a business. So 100 segments does not make sense for an e-mail marketing campaign.
  #We need to calculate how far away each customer is from the cluster's mean. To do this we could use many distances/dissimilarity index, one of which is the Gower dissimilarity.
  library(cluster)
  D=daisy(cluster.data, metric='gower')
  ## Warning: binary variable(s) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
  ## 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32
  ## treated as interval scaled
 # After the creation of a distance matrix, we implement a Ward's hierarchical clustering procedure:
    H.fit <- hclust(D, method="ward.D2")
  ## The "ward" method has been renamed to "ward.D"; note new "ward.D2"
  plot(H.fit) # display dendrogram
  
  groups <- cutree(H.fit, k=6) # cut tree into 4 clusters
  
  # draw dendogram with red borders around the 4 clusters
  rect.hclust(H.fit, k=6, border="red") 
  
  # 2D representation of the Segmentation:
  clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
           labels=2, lines=0, main= 'Customer segments')
  
  #Top get the top deals we will have to do a little bit of data manipulation. First we need to combine our clusters and transactions. Notably the lengths of the 'tables' holding transactions and clusters are different. So we need a way to merge the data . so we use the merge() function and give our columns sensible names:
  # Merge Data
  head(transactions)
  head(offers)
  head(offers2)
  cluster.deals<-merge(transactions[1:2], groups, by.x = "CustomerLastName", by.y = "row.names")
  colnames(cluster.deals)<-c("Name","Offer","Cluster")
  head(cluster.deals)
  View(cluster.deals)  
  
  #We then want to repeat the pivoting process to get Offers in rows and clusters in columns counting the total number of transactions for each cluster. Once we have our pivot table we will merge it with the offers data table like we did before:
  # Get top deals by cluster
  cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
  cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
  cluster.topDeals<-cbind(offers,cluster.pivot[-1])
  head(cluster.topDeals)
  
  plot(cluster.topDeals)
  View(cluster.deals)
  plot(cluster.deals$Cluster,cluster.deals$Name)
  cluster.deals$Cluster
  head(cluster.deals)
  new_list <- cluster.deals[order(cluster.deals$Cluster),]
  View(new_list)
  
  plot(cluster.deals$Cluster,cluster.deals$Name,)
  with(cluster.deals, text(Name~Cluster, labels = Name, pos = 2))
  
