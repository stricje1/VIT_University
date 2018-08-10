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
tweets.mandrill<-read.csv('C:/Users/Jeff/Documents/VIT_University/data/Tweet_Mandrill.csv',header=T)
tweets.other<-read.csv('C:/Users/Jeff/Documents//VIT_University/data/Tweet_Other.csv',header=T)
tweets.test<-read.csv('C:/Users/Jeff/Documents//VIT_University/data/Tweet_Test.csv',header=T)
 
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