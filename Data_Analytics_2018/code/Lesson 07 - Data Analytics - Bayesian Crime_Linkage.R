````
title: "Crime Linkage"
author: "Dr. Jeffrey Strickland"
date: "8/16/2018"
output: powerpoint_presentation
``
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## R Markdown

This is an R Markdown presentation. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

## Introduction to Crime Series Linkage

- Statistical linkage and clustering of criminal events
- Pairwise case linkage attempts to establish if list of crimes share a common offender
- create of lists of potential suspects for an unsolved crime
- identify groups of crimes committed by the same individuals or group of individuals
- for offender profiling
- predicting future events

## Hierarchical Based Approaches

- Algorithmic in nature and involve creating measures of similarity between sets of crimes
- Pairwise (crime-crime) similarity is a function of the Bayes factors obtained from case linkage
- Group (series-series) similarity scores based on linkage method from hierarchical cluster analysis
- Require having a model for estimating the log Bayes factor for a crime pair
- For details, see the vignette Statistical Methods for Crime Series Linkage. 

## Preliminaries

```{r crime01, echo = TRUE}
#-- Load the package and get the example crime data
library(crimelinkage)
data(crimes)               # some example crime incident data
data(offenders)            # some example crime offender data
seriesData = makeSeriesData(crimedata=crimes,offenderTable=offenders)
```
crimelinkage::seriesData creates a data frame with index to crimedata and offender information. It is used to generate the linkage data.

## Make Crime Pairs for Case Linkage
:::::::::::::: {.columns}
::: {.column}
- These functions generate a set of crimeIDs for linked and unlinked crime pairs
- Linked pairs are assigned a weight according to how many crimes are in the crime series
:::
  ::: {.column}
```{r crime02, echo = TRUE}
set.seed(1)  # set random seed for replication
allPairs = makePairs(seriesData,thres=365,m=40)
```
:::
  ::::::::::::::
  
  ## Make Evidence Variables for Case Linkage
  ```{r crime03, echo = TRUE}
varlist = list(spatial = c("X", "Y"),
               temporal = c("DT.FROM","DT.TO"),
               categorical = c("MO1",  "MO2", "MO3"))
X = compareCrimes(allPairs,crimes,varlist,binary=TRUE)  # Evidence data
Y = ifelse(X$type=='linked',1,0)      # Linkage indicator. 1=linkage, 0=unlinked
```
crimelinkage::compareCrimes calculates spatial and temporal distance, difference in categorical, and absolute value of numerical crime variables

## Get Training Data

```{r crime04, echo = TRUE}
set.seed(3)                 # set random seed for replication
train = sample(c(TRUE,FALSE),nrow(X), replace=TRUE,
               prob=c(.7,.3))  # assign pairs to training set
test = !train
D.train = data.frame(X[train,],Y=Y[train])    # training data
```
We used "X" and "Y" from the previous step

## Fit naive Bayes model and make estimateBF() function

```{r crime05, echo = TRUE}
vars = c("spatial","temporal","tod","dow","MO1","MO2","MO3") 
fmla.all = as.formula(paste("Y ~ ", paste(vars, collapse= "+")))
NB = naiveBayes(fmla.all,data=D.train,weights=weight,df=10,nbins=15,partition='quantile')
### estimateBF() returns the estimated log Bayes factor
estimateBF <- function(X){           
  predict(NB,X)
}
```
After binning, this adds pseudo counts to each bin count to give df approximate degrees of freedom

## Naive Bayes Factors Plot

```{r naiveBayes}
plot(NB)
```

## Estimated predictions

```{r crime06, echo = TRUE}
D.test = data.frame(X[test,],Y=Y[test])    # testing data
est <- estimateBF(D.test)
```

## Naive Bayes Estimation Plot

```{r Estimates}
plot(est)
```

## Agglomerative Hierarchical Crime Series Clustering

- Algorithmic approach to crime series cluster analysis that sequentially forms a hierarchy of cluster solutions
- Starts with every observation (e.g., crime incident) in its own cluster
- Then it sequentially merges the two closest clusters to form a new larger cluster
- This process is repeated until all observations are in the same cluster
- This algorithm requires two similarity measures to be specified: 
  - the pairwise similarity between two observations 
- the similarity between two groups of observations. 

Get unsolved crimes

```{r crime06A, echo = TRUE}
#-- 
unsolved = subset(crimes, !crimeID %in% seriesData$crimeID)
head(unsolved)
```


## Run agglomerative hierarchical crime clustering

```{r crime07, echo = TRUE}
tree = crimeClust_hier(unsolved,varlist,estimateBF,linkage='average')
tree
```

## Plot results in dendrogram using plot_hcc()

```{r Hierarchical Plot}
plot_hcc(tree,yticks=seq(-2,4,by=2),type="triangle",hang=.05,main="Average Linkage") 
```

The dendrogram (using average linkage) only merges 5 crime pairs with a log Bayes factor of more than 4.

## Examine crimes C:429 and C:469

```{r crime08, echo = TRUE}
subset(crimes,crimeID %in% c('C:429','C:469'))
```

## Find path info for crime C:425

```{r crime09, echo = TRUE}
cp = clusterPath('C:429',tree)
cp[cp$logBF>0,]    # only return path for scores > 0
```

## Hierarchical Based Crime Series Linkage

- This approach to crime series identification 
- compares an unsolved crime to every crime in criminal incident database
- calculates its similarity as the log Bayes factor (
- aggregates the similarity scores over the crime groups using single, complete, or average linkage
- Single linkage uses the largest score (most similar crime) from each group
- complete linkage uses the smallest score (least similar crime) from each group
- average linkage uses the average score as the group score.
  
## Extract solved and unsolved crimes
  
```{r crime10, echo = TRUE}
solved = subset(crimes, crimeID %in% seriesData$crimeID)
unsolved = subset(crimes, !crimeID %in% seriesData$crimeID)
```
  
## Find similar crime series to the unsolved crime
  
```{r crime11, echo = TRUE}
crime = unsolved[2,]             # use the 2nd unsolved crime
crime
```

## Examine the Results
```{r crime11A, echo = TRUE}
 results = seriesID(crime,solved,seriesData,varlist,estimateBF)
head(results$score)
```

The function seriesID() can be used to find the most similar crime series to the unsolved crime
This shows that the unsolved crime is most similar to the crime(s) in crime group 25 with an average linkage log Bayes factor of 3.63.
  
## Get the crimes and offenders associated with these groups
  
```{r crime12, echo = TRUE}
subset(results$groups,group=='25')      # most similar crime series
subset(results$groups,group=='93')      # 2nd most similar series
subset(results$groups,group=='9')       # a series with multiple crimes
```
  
## We can do this for another unsolved crime
  
```{r crime13, echo = TRUE}
crime4 = unsolved[4,]             # use the 4th unsolved crime
crime4
```
  
## Review the Results
```{r crime13A, echo = TRUE}  
results4 = seriesID(crime4,solved,seriesData,varlist,estimateBF)
head(results4$score)
```
Because the scores are so low (log Bayes factors around 1), this unsolved crime is not similar to any other solved crimes in the crime database. Perhaps this is the start of a new crime series?
    
## Compare a crime to all unsolved crimes to detect potential unsolved crime series.
    
```{r crime14, echo = TRUE}
pairs = data.frame(i1=unsolved$crimeID[4],i2=unique(unsolved$crimeID[-4]))  
X = compareCrimes(pairs,unsolved,varlist,show.pb=FALSE)
score = data.frame(pairs,logBF=estimateBF(X))  
head(score[order(-score$logBF),])
```
  
## Similar results to the hierarchical clustering path approach
  
```{r crime15, echo = TRUE}
C429 = which(unsolved$crimeID %in% 'C:429')
pairs = data.frame(i1=unsolved$crimeID[C429],i2=unique(unsolved$crimeID[-C429]))  
X = compareCrimes(pairs,unsolved,varlist,show.pb=FALSE)
score = data.frame(pairs,logBF=estimateBF(X))  
head(score[order(-score$logBF),])
```
  
## Bayesian Model-Based Approaches
  
- Partially-supervised Bayesian model-based clustering approach to crime series linkage
- This approach is partially-supervised because 
- the offender is known for a subset of the events
- utilizes spatiotemporal crime locations as well as crime features describing the offenders modus operandi.
- The hierarchical model naturally handles complex features often seen in crime data, including.
- missing data, 
- interval censored event times
- a mix of discrete and continuous variables
- also provide uncertainty assessments for all model parameters
- produces posterior clustering probabilities which allow analysts to act on model output only as warranted.

## Make the crime group labels for each crime

```{r crime16, echo = TRUE}
seriesData$CG = makeGroups(seriesData,method=2)      # method=2 uses unique co-offenders
group_info = unique(seriesData[,c('crimeID','CG')])  # extract the group info
A = merge(crimes,group_info,by="crimeID",all.x=TRUE) # add group info to crimes
A = A[order(A$CG),]               
```

## Run MCMC

```{r crime17, echo = TRUE, eval = FALSE}
fit = crimeClust_bayes(A$CG, s=A[,c('X','Y')],t1=A$DT.FROM,t2=A$DT.TO,
                       Xcat=A[,c("MO1","MO2","MO3")],maxcriminals=1000,
                       iters=3000,burn=1000,update=100)

```
## Solution Iterations
:::::::::::::: {.columns}
::: {.column}
![prediction model flow](model_fig4.png)
![prediction model flow](model_fig3.png)
:::
::: {.column}
![prediction model flow](model_fig2.png)
![prediction model flow](model_fig1.png)
:::
::::::::::::::

## Extract pairwise probabilities

```{r crime18, echo = TRUE, eval = FALSE}
pp = fit$p.equal    # probability that crime i is linked to crime j
diag(pp) = NA       
```

## Get index of unsolved crimes

```{r crime19, echo = TRUE}
ind.unsolved = which(is.na(A$CG))          # index of unsolved crimes
n = nrow(A)                                # number of crimes
```

## Image plot of linkage probabilities

```{r crime20, echo = TRUE, eval = FALSE}
fields::image.plot(1:n,ind.unsolved,pp[1:n,ind.unsolved],
           xlab="Crime",ylab="Unsolved Crime",
           main="Probability crimes are linked")
```
## Probability Plot

![prediction model flow](model_fig5.png)

We see that some unsolved crimes are linked to solved crimes with a posterior probability above 0.35. This is a fairly strong linkage and may be worth further investigations.

## Find strongest linkages

```{r crime21, echo = TRUE, eval = FALSE}
unsolved.probs = apply(pp[ind.unsolved,],1,max,na.rm=TRUE)  # maximum probability
plot(ind.unsolved,unsolved.probs,xlab="unsolved crime",ylab='maximum probability of linkage')
abline(h=0.30)
```

## Find strongest linkages

```{r crime22, echo = TRUE, eval = FALSE}
ind = ind.unsolved[unsolved.probs > 0.30]
investigate = as.character(A$crimeID[ind])       # crimeIDs for crimes with strongest linkage
investigate

```

## More Detailed Investigation

```{r crime23, echo = TRUE, eval = FALSE}
bp = bayesProb(pp[A$crimeID %in% "C:417"])
bp$crimeID = A$crimeID[bp$index]
bp$CG = A$CG[bp$index]
head(bp)
```

## Slide with Plot

```{r bp, eval = FALSE}
plot(bp)
```

