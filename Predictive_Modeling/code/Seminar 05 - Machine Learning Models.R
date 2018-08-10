#*****************************************************************************
# Naive Bayes Classifiers
#*****************************************************************************
#---------------------------------------------------------
#The 'pairs' command creates a scatterplot. Each dot is a data point and its position is determined by the values that data point has for a pair of variables. The class determines the color of the data point. From the plot note that Setosa irises have smaller petals than the other two species.
#We will use the e1071 R package to build a Naïve Bayes classifier. 
#---------------------------------------------------------
#load library
library(e1071)
library(naivebayes)
library(crimelinkage)
data(crimes)
head(crimes)
pairs = t(combn(crimes$crimeID[1:8],m=2)) 
varlist = list( spatial = c("X", "Y"), 
                temporal = c("DT.FROM","DT.TO"), 
                categorical = c("MO1", "MO2", "MO3")) 
compareCrimes(pairs,crimes,varlist,binary=TRUE)
fit <- crimeClust_bayes(crimeID="crimeID", 
                        spatial=s, 
                        Xcat=pairs, 
                        maxcriminals=12,
                        iters=500,burn=100,update=100)

if(require(fields,quietly=TRUE)){
  fields::image.plot(1:12,1:12,fit$p.equal, 
                     xlab="Crime",ylab="Crime", 
                     main="Probability crimes are from the same criminal") }
# Extract the crimes with the largest posterior probability 
bayesPairs(fit$p.equal) 
bayesProb(fit$p.equal[1,])


crimeID The crime ID number 
X,Y Spatial 
coordinates MO1 A categorical 
MO variable that takes values 1,...,31 
MO2 A categorical MO variable that takes values a,...,h 
MO3 A categorical MO variable that takes values A,...,O 
DT.FROM The earliest possible Date-time of the crime. 
DT.TO The latest possible Date-time of the crime

#-- Load the package and get the example crime data
library(crimelinkage)
data(crimes)               # some example crime incident data
data(offenders)            # some example crime offender data
seriesData = makeSeriesData(crimedata=crimes,offenderTable=offenders)
#-- Make Crime Pairs for Case Linkage
set.seed(1)         # set random seed for replication
allPairs = makePairs(seriesData,thres=365,m=40)

#-- Make Evidence Variables for Case Linkage
varlist = list( spatial = c("X", "Y"), 
                temporal = c("DT.FROM","DT.TO"), 
                categorical = c("MO1",  "MO2", "MO3"))    # crime variables list
X = compareCrimes(allPairs,crimedata=crimes,varlist=varlist,binary=TRUE) # Evidence data
Y = ifelse(allPairs$type=='linked',1,0)      # Linkage indicator. 1=linkage, 0=unlinked


#-- Get Training Data
set.seed(3)                                        # set random seed for replication
train = sample(c(TRUE,FALSE),nrow(X),replace=TRUE,prob=c(.7,.3))  # assign pairs to training set
test = !train
D.train = data.frame(X[train,],Y=Y[train])          # training data

#-- Fit naive Bayes model and make estimateBF() function 
vars = c("spatial","temporal","tod","dow","MO1","MO2","MO3") 
fmla.all = as.formula(paste("Y ~ ", paste(vars, collapse= "+")))
NB = naiveBayes(fmla.all,data=D.train,weights=weight,df=10,nbins=15,partition='quantile')

estimateBF <- function(X){           # estimateBF() returns the estimated log Bayes factor
  predict(NB,newdata=X)
}
NB$spatial
NB$temporal
NB$MO1
NB$MO2$BF
#-- Get unsolved crimes
unsolved = subset(crimes, !crimeID %in% seriesData$crimeID)

#-- Run agglomerative hierarchical crime clustering
tree = crimeClust_hier(unsolved,varlist,estimateBF,linkage='average', binary=TRUE)

#-- Plot results in dendrogram using plot_hcc()
plot_hcc(tree,yticks=seq(-2,6,by=2),type="triangle",hang=.05,main="Average Linkage") 

#-- Examine crimes C:431 and C:460 
subset(crimes,crimeID %in% c('C:431','C:460'))
#-- Find path info for crime C:429
cp = clusterPath('C:429',tree)
cp[cp$logBF>0,]                 # only return path for scores > 0
# To give an example, extract the solved and unsolved crimes from the crimes data.
solved = subset(crimes, crimeID %in% seriesData$crimeID)
unsolved = subset(crimes, !crimeID %in% seriesData$crimeID)
# The function seriesID() can be used to find the most similar crime series to the unsolved crime.
crime = unsolved[2,]             # use the 2nd unsolved crime C:392
crime
results = seriesID(crime,solved,seriesData,varlist,estimateBF)
head(results$score)
# This shows that the unsolved crime is most similar to the crime(s) in crime group 12 with an average linkage log Bayes factor of 4.06. To get the crimes and offenders associated with these groups, just use the subset() function with the groups object:
subset(results$groups,group=='12')      # most similar crime series
subset(results$groups,group=='154')     # 2nd most similar series
subset(results$groups,group=='9')       # a series with multiple crimes
# We can do this for another unsolved crime
crime4 = unsolved[4,]             # use the 4th unsolved crime
results4 = seriesID(crime4,solved,seriesData,varlist,estimateBF)
head(results4$score)
# Because the scores are so low (log Bayes factors around 1), this unsolved crime is not very similar to any other solved crimes in the crime database. Perhaps this is the start of a new crime series?
# It is also possible to compare a crime to all unsolved crimes to detect potential unsolved crime series.
#- using crime C:394 (the 4th unsolved crime)
pairs = data.frame(i1=unsolved$crimeID[4],i2=unique(unsolved$crimeID[-4]))  
X = compareCrimes(pairs,unsolved,varlist,binary=TRUE)     # Evidence data
score = data.frame(pairs,logBF=estimateBF(X))  
head(score[order(-score$logBF),])
# There are no unsolved crimes that are very similar to this one - probably not enough evidence to link this crime to any others.
# This approach also gives similar results to what was obtained from the hierarchical clustering path approach:
C429 = which(unsolved$crimeID %in% 'C:429')       # now use crime C:429
pairs = data.frame(i1=unsolved$crimeID[C429],i2=unique(unsolved$crimeID[-C429]))  
X = compareCrimes(pairs,unsolved,varlist,binary=TRUE)     # Evidence data
score = data.frame(pairs,logBF=estimateBF(X))  
head(score[order(-score$logBF),])      
#-- results from hierarchical clustering
cp = clusterPath('C:429',tree)
cp[cp$logBF>0,]   
# The function crimeClust_bayes() is used for the Bayesian model-based clustering approach. 
# Because it uses both solved and unsolved crimes, the labels (crime group) for the solved crimes is also passed into the function. 
# (Note: this function will take 20+ minutes to run.)
#-- Make the crime group labels for each crime (NA for unsolved crimes)
seriesData$CG = makeGroups(seriesData,method=2)      # method=2 uses unique co-offenders
group_info = unique(seriesData[,c('crimeID','CG')])  # extract the group info
A = merge(crimes,group_info,by="crimeID",all.x=TRUE) # add group info to crimes
A = A[order(A$CG),]                                  # order by crime group

#-- Run MCMC
fit = crimeClust_bayes(A$CG, spatial=A[,c('X','Y')],t1=A$DT.FROM,t2=A$DT.TO,
                       Xcat=A[,c("MO1","MO2","MO3")],maxcriminals=1000,
                       iters=3000,burn=1000,update=100,seed=5)
summary(fit)

#-- Extract pairwise probabilities
pp = fit$p.equal    # probability that crime i is linked to crime j
diag(pp) = NA
summary(pp)

# The matrix pp contains the pairwise estimated probability that two crime are linked (share a common offender). We can use this information for crime series identification.
# Using the image.plot() from the fields package, we can see how strongly the unsolved crimes are linked to the existing (solved) crime series.
library(fields) # if not installed, type: install.packages("fields")

#-- Get index of unsolved crimes
ind.unsolved = which(is.na(A$CG))          # index of unsolved crimes
n = nrow(A)                                # number of crimes

#-- Image plot of linkage probabilities
fields::image.plot(1:n,ind.unsolved,pp[1:n,ind.unsolved],
                   xlab="Crime",ylab="Unsolved Crime",
                   main="Probability crimes are linked")

# We see that some unsolved crimes are linked to solved crimes with a posterior probability above 0.25. These crimes may be worth further investigations.
# Here we plot the maximum posterior probability that an unsolved crime is linked to another crime (solved or unsolved).
#-- Find strongest linkages
unsolved.probs = apply(pp[ind.unsolved,],1,max,na.rm=TRUE)  # maximum probability
plot(ind.unsolved,unsolved.probs,xlab="unsolved crime",ylab='maximum probability of linkage')
abline(h=0.25)

ind = ind.unsolved[unsolved.probs > 0.25]
investigate = as.character(A$crimeID[ind])       # crimeIDs for crimes with strongest linkage
investigate

# This shows that C:408, C:417, C:464 are the crimes with the strongest linkages (posterior probabilities greater that 0.25). A particular crime can be investigated in more detail with the function bayesProb():
bp = bayesProb(pp[A$crimeID %in% "C:417"])
bp$crimeID = A$crimeID[bp$index]
bp$CG = A$CG[bp$index]
head(bp)

# For this example, our model provides a list of the most likely crimes associated with the unsolved crime C:417. The first two crimes (C:15 and C:26) are solved crimes indicating that the offender(s) responsible for these crimes may also be responsible for C:417. The next two crimes, C:459 and C:446 do not have a group ID. This means that they are unsolved crimes. By providing the posterior probabilities, crime analysts may choose to investigate further only if the linkage is strong enough.
#****************************************************************************
# HouseVotes84
#****************************************************************************

