#*****************************************************************************
# Naive Bayes Classifiers
#*****************************************************************************

# The Iris dataset is pre-installed in R, since it is in the standard datasets package. To access its documentation, click on 'Packages' at the top-level of the R documentation, then on 'datasets' and then on 'iris'. As explained, there are 150 data points and 5 variables. Each data point concerns a particular iris flower and gives 4 measurements of the flower: Sepal.Length, Sepal.Width, Petal.Length and Petal.Width together with the flower’s Species. The goal is to build a classifier that predicts species from the 4 measurements, so species is the class variable. 
#---------------------------------------------------------
data(iris)
pairs(iris[1:4],main="Iris Data (red=setosa,green=versicolor,blue=virginica)", pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
summary(iris)
iris
#---------------------------------------------------------
#The 'pairs' command creates a scatterplot. Each dot is a data point and its position is determined by the values that data point has for a pair of variables. The class determines the color of the data point. From the plot note that Setosa irises have smaller petals than the other two species.
#We will use the e1071 R package to build a Naïve Bayes classifier. 
#---------------------------------------------------------
#load library
library(e1071)
library(naivebayes)
#build a Naïve Bayes classifier 
classifier<-naiveBayes(iris[,1:4], iris[,5]) 
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
#get prior probabilities
classifier$apriori
#get posterior probabilities are sued for predictions
classifier$tables$Petal.Length
#plot the distributions
	plot(function(x) dnorm(x, 3.428, 0.3790644), 0, 8, col="red", main="Petal Width distribution for the 3 different species")
	curve(dnorm(x, 2.770, 0.3137983), add=TRUE, col="blue")
	curve(dnorm(x, 2.974, 0.3224966), add=TRUE, col="green")
#
nb <- naive_bayes(Species ~ ., data = iris) 
plot(nb)
nb_kernel <- naive_bayes(x = iris[-5], y = iris[ ,5], usekernel = TRUE) 
plot(nb_kernel)
vars <- 10 
rows <- 500000 
y <- sample(c("a", "b"), rows, TRUE)
X1 <- as.data.frame(matrix(sample(letters[5:9], vars * rows, TRUE), ncol = vars)) 
nb_cat <- naive_bayes(x = X1, y = y) 
nb_cat 
system.time(pred2 <- predict(nb_cat, X1))



X2 <- as.data.frame(matrix(rnorm(vars * rows), ncol = vars)) 
nb_num <- naive_bayes(x = X2, y = y) 
nb_num 
system.time(pred2 <- predict(nb_num, X2))	

iris2 <- cbind(iris, New = sample(letters[1:3], 150, TRUE)) 
nb <- naive_bayes(Species ~ ., data = iris2) 
plot(nb, ask = TRUE) 
plot(nb, which = c(1, 2), ask = TRUE, arg.num = list(col = 1:3, lty = 1, main = "Naive Bayes Plot")) 
plot(nb, which = "New", arg.cat = list(color = 4:7))

nb <- naive_bayes(Species ~ ., data = iris) 
tables(nb, "Sepal.Length") 
tables(nb, 1:2)

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

#To introduce the Naive Bayes algorithm, I will use the HouseVotes84 dataset, which contains US congressional voting records for 1984. The data set is in the mlbench package which is not part of the base R installation. You will therefore need to install it if you don’t have it already.  Package installation is a breeze in RStudio – just go to Tools > Install Packages and follow the prompts.
#The HouseVotes84 dataset describes how 435 representatives voted – yes (y), no (n) or unknown (NA) – on 16 key issues presented to Congress.  The dataset also provides the party affiliation of each representative – democrat or republican.
#Load mlbench, get the dataset and get some summary stats on it. 
#load mlbench library
#---------------------------------------------------------
library(mlbench)
#set working directory if needed (modify path as needed)
setwd("C:/Users/Strickland/Documents/VIT University")
#load HouseVotes84 dataset
votes<- read.csv("C:/Users/Strickland/Documents/VIT University/HouseVotes84.csv", header=FALSE)
names(votes) <- c("Issue01","Issue02","Issue03","Issue04","Issue05","Issue06","Issue07","Issue08","Issue09","Issue10","Issue11","Issue12","Issue13","Issue14","Issue15","Issue16","Party")
summary(votes)
#data("HouseVotes84")
#---------------------------------------------------------
#It is good to begin by exploring the data visually.  To this end, let’s do some bar plots using the basic graphic capabilities of R:
#---------------------------------------------------------
#barplots for specific issue
plot(as.factor(votes[,1]))
title(main="Votes cast for issue", xlab="vote", ylab="# reps")
#by party
plot(as.factor(votes[votes$Party=='republican',1]))
title(main="Republican votes cast for issue 1", xlab="vote", ylab="# reps")
plot(as.factor(votes[votes$Party=='democrat',1]))
title(main="Democrat votes cast for issue 1", xlab="vote", ylab="# reps")
#---------------------------------------------------------
#We need to impute NA values for a given issue and party by looking at how other representatives from the same party voted on the issue. This is very much in keeping with the Bayesian spirit: we infer unknowns based on a justifiable belief – that is, belief based on the evidence.
#To do this I write two functions: one to  compute the number of NA values for a given issue (vote) and class (party affiliation), and the other to calculate the fraction of yes votes for a given issue (column) and class (party affiliation).
#Functions needed for imputation
#function to return number of NAs by vote and class (democrat or republican)
#---------------------------------------------------------
na_by_col_class <- function (col,cls){return(sum(is.na(votes[,col]) & votes$Party==cls))}
title(main="Votes cast for issue", xlab="vote", ylab="# reps")
#---------------------------------------------------------
#function to compute the conditional probability that a member of a party will cast a ‘yes’ vote for
#a particular issue. The probability is based on all members of the party who #actually cast a vote on the issue (ignores NAs).
#---------------------------------------------------------
p_y_col_class <- function(col,cls){
sum_y<-sum(votes[,col]=='y' & votes$Party==cls,na.rm = TRUE)
sum_n<-sum(votes[,col]=='n' & votes$Party==cls,na.rm = TRUE)
return(sum_y/(sum_y+sum_n))}
#Check that functions work!
p_y_col_class(2,'democrat')
p_y_col_class(2,'republican')
na_by_col_class(2,'democrat')
na_by_col_class(2,'republican')
#---------------------------------------------------------
#We can now impute the NA values based on the above. We do this by randomly assigning values ( y or n) to NAs, based on the proportion of members of a party who have voted y or n. In practice, we do this by invoking the uniform distribution and setting an NA value to y if the random number returned is less than the probability of a yes vote and to n otherwise.
#---------------------------------------------------------
#impute missing values.
for (i in 2:ncol(votes)) 
{
	if(sum(is.na(votes[,i])>0)) {
		c1 <- which(is.na(votes[,i])& votes$Party=='democrat',arr.ind = TRUE)
		c2 <- which(is.na(votes[,i])& votes$Party=='republican',arr.ind = TRUE)
	votes[c1,i] <-
		ifelse(runif(na_by_col_class(i,'democrat'))<p_y_col_class(i,'democrat'),'y','n')
	votes[c2,i] <-
		ifelse(runif(na_by_col_class(i,'republican'))<p_y_col_class(i,'republican'),'y','n')}
}
#---------------------------------------------------------
#The next step is to divide the available data into training and test datasets. The former will be used to train the algorithm and produce a predictive model. The effectiveness of the model will then be tested using the test dataset.
#divide into test and training sets
#create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
#---------------------------------------------------------
votes["train"] <- ifelse(runif(nrow(votes))<0.80,1,0)
#---------------------------------------------------------
#get col number of train / test indicator column (needed later)
#---------------------------------------------------------
trainColNum <- grep("train",names(votes))
#---------------------------------------------------------
#separate training and test sets and remove training column before modeling
trainvotes <- votes[votes$train==1,-trainColNum]
testvotes <- votes[votes$train==0,-trainColNum]
#---------------------------------------------------------
#Now we’re finally good to build our Naive Bayes model 
#The code to train the model is anticlimactically simple:
#load e1071 library and invoke naiveBayes method
#---------------------------------------------------------
library(e1071)
nb_model <- naiveBayes(Party~.,data = trainvotes)
nb_model
summary(nb_model)
str(nb_model)
#---------------------------------------------------------
#Here we’ve invoked the naiveBayes method from the e1071 package. 
#Now that we have a model, we can do some predicting. We do this by feeding our test data into our model and comparing the predicted party affiliations with the known ones. The latter is done via the wonderfully named confusion matrix – a table in which true and predicted values for each of the predicted classes are displayed in a matrix format.
#---------------------------------------------------------
nb_test_predict <- predict(nb_model,testvotes[,-1])
#---------------------------------------------------------
#confusion matrix
table(pred=nb_test_predict,true=testvotes$Party)
#---------------------------------------------------------
#In the confusion matrix (as defined above), the true values are in columns and the predicted values in rows. So, the algorithm has correctly classified 38 out of 43 (i.e. 38+5) Democrats and 22 out of 25 Republicans (i.e. 22+3). 
#A simple measure of efficacy would be the fraction of predictions that the algorithm gets right. For the training/testing set above, this is simply 60/68 (see the confusion matrix above). The simplest way to calculate this in R is:
#---------------------------------------------------------
#fraction of correct predictions
mean(nb_test_predict==testvotes$Party)
#---------------------------------------------------------
#A natural question to ask at this point is: how good is this prediction. This question cannot be answered with only a single run of the model; we need to do many runs and look at the spread of the results. 
#---------------------------------------------------------
#function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
fraction_correct <- rep(NA,n)
for (i in 1:n)
{
votes[,"train"] <- ifelse(runif(nrow(votes))<train_fraction,1,0)
trainColNum <- grep("train",names(votes))
trainvotes <- votes[votes$train==1,-trainColNum]
testvotes <- votes[votes$train==0,-trainColNum]
nb_model <- naiveBayes(Party~.,data = trainvotes)
nb_test_predict <- predict(nb_model,testvotes[,-1])
fraction_correct[i] <- mean(nb_test_predict==testvotes$Party)
}
return(fraction_correct)
}
#---------------------------------------------------------
#Let’s do 20 runs with the same training fraction (0.8) as before:
#20 runs, 80% of data randomly selected for training set in each run
#---------------------------------------------------------
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions
#summary of results
summary(fraction_correct_predictions)
#standard deviation
sd(fraction_correct_predictions)
#---------------------------------------------------------
#We see that the outcome of the runs are quite close together, in the 0.85 to 0.95 range with a standard deviation of 0.025. This tells us that Naive Bayes does a pretty decent job with this data.
#****************************************************************************
#Use HairEyeColor Dataset
HairEyeColor
mosaicplot(HairEyeColor)
#The counts we need to compute the parameters for a Naïve Bayes classifier 
margin.table(HairEyeColor,3)
margin.table(HairEyeColor,c(1,3))
#Note that Sex is variable 3, and Hair is variable 1.

#*****************************************************************************
# Tweet Classification with Naive Bayes Classifier: R
#*****************************************************************************

#Mandrill is a transactional email platform from MailChimp.
#Step 1: Data Cleaning
#We start off with our vanilla data set separated into 3 different files:
###Mandrill Tweets at http://www.salemmarafi.com/wp-content/uploads/2014/05/Mandrill.csv
###Other Tweets at http://www.salemmarafi.com/wp-content/uploads/2014/05/Other.csv 
###Test Tweets at http://www.salemmarafi.com/wp-content/uploads/2014/05/Test.csv 
#Download those files into your working directory and fire up R.
#The first thing we need to do is load up the libraries we will be using and read our data. We also add the appropriate columns for “App” and “Other”:
# Libraries
library(tm)
 
# Collect data
tweets.mandrill<-read.csv('C:/Users/Strickland/Documents/VIT University/Tweet_Mandrill.csv',header=T)
tweets.other<-read.csv('C:/Users/Strickland/Documents/VIT University/Tweet_Other.csv',header=T)
tweets.test<-read.csv('C:/Users/Strickland/Documents/VIT University/Tweet_Test.csv',header=T)
 
# Add "App" to mandrill tweets, and "Other" to others. 
tweets.mandrill["class"]<-rep("App",nrow(tweets.mandrill))
tweets.other["class"]<-rep("Other",nrow(tweets.mandrill))

#We then need a helper function to replace all the stuff we do not want in the tweets. To do this we use a function called gsub() within our helper function.
# Helper Function
replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  x
}

#Let us do the text transformations on our tweets now:
# Do our punctuation stuff
tweets.mandrill$Tweet <- replacePunctuation(tweets.mandrill$Tweet)
tweets.other$Tweet <- replacePunctuation(tweets.other$Tweet)
tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)

#Step 2: Word Counting
#We do not need to count words for the R version.

#Step 3: Tokenization
#We are going to use the ‘tm’ library to tokenize our training and test data. First we will create a Corpus – which is just a funky word for a set of text stuff … in this case tweets!
# Create corpus
tweets.mandrill.corpus <- Corpus(VectorSource(as.vector(tweets.mandrill$Tweet)))
tweets.other.corpus <- Corpus(VectorSource(as.vector(tweets.other$Tweet)))
tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))
#Great! Now we want to get a term document matrix – another fancy phrase for a table that shows how many times each word was mentioned in each document.
# Create term document matrix
tweets.mandrill.matrix <- t(TermDocumentMatrix(tweets.mandrill.corpus,control = list(wordLengths=c(4,Inf))));
tweets.other.matrix <- t(TermDocumentMatrix(tweets.other.corpus,control = list(wordLengths=c(4,Inf))));
tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(4,Inf))));
#Great! That’s just what we need; we can count words in documents and totals later!
#We can move on to building our model now.
#Step 4: Model Building
#We want to create the probabilities table.
#We start by counting the number of times each word has been used
#We then need to do the “additive smoothing” step
#We then take then calculate the probabilities using the smoothed frequencies
#Lastly we need to take the natural log of the probabilities
#To save us time, we will write a little helper function that will do this for any document matrix we might have.
probabilityMatrix <-function(docMatrix)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  # Add one
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  # Calculate the natural log of the probabilities
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Add pretty names to the columns
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

#Great, now we can just use that for our training sets “App” and “Other”.
tweets.mandrill.pMatrix<-probabilityMatrix(tweets.mandrill.matrix)
tweets.other.pMatrix<-probabilityMatrix(tweets.other.matrix)
#We can output these to files and compare them to the Excel file:
write.csv(file="mandrillProbMatrix.csv",tweets.mandrill.pMatrix)
write.csv(file="otherProbMatrix.csv",tweets.mandrill.pMatrix)

#Step 5: Using Bayes and the MAP model
#We now want to use the MAP model to compare each of the test tweets with the two probability matrices.
#We will write another function to help us out here. We start thinking about what we want to do with each tweet:
#Get each test tweets characters and look for them in a probability matrix
#We want to know how many words were not found.
#We want to sum up the probabilities of the found words
#For the words we do not find, we want to add ln(1/sum of the smoothed words) each time
#Sum up the probabilities from the last 2 steps
#Lets go:

getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
  # Count how many words were not found in the mandrill matrix
  charactersNotFound<-length(testChars)-length(charactersFound)
  # Add the normalized probabilities for the words founds together
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  # We use ln(1/total smoothed words) for words not found
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  #This is our probability
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}

#Now we can use this function for every tweet we have with a for loop. Lets see how this is done:
# Get the matrix
tweets.test.matrix<-as.matrix(tweets.test.matrix)
# A holder for classification 
classified<-NULL
 
for(documentNumber in 1:nrow(tweets.test.matrix))
{
  # Extract the test words
  tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
  # Get the probabilities
  mandrillProbability <- getProbability(tweets.test.chars,tweets.mandrill.pMatrix)
  otherProbability <- getProbability(tweets.test.chars,tweets.other.pMatrix)
  # Add it to the classification list
  classified<-c(classified,ifelse(mandrillProbability>otherProbability,"App","Other"))
}
#Brilliant we can take a look at our classification alongside our tweets:
View(cbind(classified,tweets.test$Tweet))
#Very much like our Excel model, we are off by 1/20.
#Entire Code
write.csv(file="PredProbMatrix.csv",(cbind(classified,tweets.test$Tweet)))
write.csv(file="mandrillProbMatrix.csv",tweets.mandrill.pMatrix)
write.csv(file="PredProbMatrix.csv",tweets.test.matrix)

#*****************************************************************
###           Text Analytics using the koRpus-package          ###
#*****************************************************************
#--------------------------------------------------------
#Example 1
#--------------------------------------------------------
library(koRpus)
library(tm)
library(NLP)
tagged.text <- tokenize("C:/Users/Strickland/Documents/VIT University/Data_Analytics.txt", lang="en")
taggedText(tagged.text)
head(taggedText(tagged.text),n=20)
#--------------------------------------------------------
#Example 2
#--------------------------------------------------------
library(koRpus)
library(tm)
library(NLP)
tagged.text <- tokenize("C:/Users/Strickland/Documents/VIT University/texts/Predictive1.txt", lang="en")
taggedText(tagged.text)
head(taggedText(tagged.text),n=15)

