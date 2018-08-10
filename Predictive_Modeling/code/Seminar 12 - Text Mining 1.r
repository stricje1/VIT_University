# Load the R package for text mining and then load your texts into R.
library(wordcloud2)
library(yaml)
library(NLP)
library(tm) 
library(SnowballC)
library(ggplot2) 
 
#On a PC, save the folder to your C: drive and use the following code chunk:
cname <- file.path("C:/Users/jeff/Documents/VIT_University", "texts")   
cname   
dir(cname)   
cdocs <- Corpus(DirSource(cname))   
summary(docs)
inspect(docs)

#Removing punctuation:

# If necesasry, you can remove special characters.
docs <- tm_map(cdocs, removePunctuation) 
for(j in seq(docs))   
   {   
     docs[[j]] <- gsub("/", " ", docs[[j]])
     docs[[j]] <- gsub("â", " ", docs[[j]])
     docs[[j]] <- gsub("o", " ", docs[[j]])
     docs[[j]] <- gsub("???", " ", docs[[j]])
     docs[[j]] <- gsub("T", " ", docs[[j]])
     docs[[j]] <- gsub("@", " ", docs[[j]])   
     docs[[j]] <- gsub("\\|", " ", docs[[j]])   
  }   
inspect(docs[1]) # You can check a document (in this case the first) to see if it worked. 

#Remove unnecessary words & convert to lowercase:
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)     
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("can", "should", "would", "figure", "using", "will", "use", "now", "see", "may", "given", "since", "want", "next", "like", "new", "one", "might", "without"))   
   
# Combine words that should stay together
for (j in seq(docs))
{
docs[[j]] <- gsub("data analytics", "data_analytics", docs[[j]])
docs[[j]] <- gsub("predictive models", "predictive_models", docs[[j]])
docs[[j]] <- gsub("predictive analytics", "predictive_analytics", docs[[j]])
docs[[j]] <- gsub("data science", "data_science", docs[[j]])
docs[[j]] <- gsub("operations research", "operations_research", docs[[j]])
docs[[j]] <- gsub("chi-square", "chi_square", docs[[j]])
}

#Remove common word endings  
#Strip unnecesary whitespace from your documents:
docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, stripWhitespace)   
# inspect(docs[3]) # Check to see if it worked.   
writeLines(as.character(docs[[20]])) 

#Create term-document matrix 
tdm <- TermDocumentMatrix(docs) 
#view it 
tdm 
#Create document-term matrix 
dtm <- DocumentTermMatrix(docs) 
#print a summary 
dtm

#organizes the terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)     
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")   

# Remov3 sparse terms. This makes a matrix that is 10% empty space, maximum.     
dtms <- removeSparseTerms(dtm, 0.1) 
inspect(dtms)  

# Next, we check some of the frequency counts. There are a lot of terms, so for now, we just check out some of the most and least frequently occurring words, as well as check out the frequency of frequencies.
freq[head(ord)]   
freq[tail(ord)]   
head(table(freq), 50)  
tail(table(freq), 50)   
freq <- colSums(as.matrix(dtms)) 
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)   
findFreqTerms(dtm, lowfreq=150) 
# Plot words that appear at least 50 times.
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)    
p <- ggplot(subset(wf, freq>300), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

# Find correlations
findAssocs(dtm, c("question" , "analysis"), corlimit=0.98) # specifying a correlation limit of 0.98   

findAssocs(dtms, "contrast", corlimit=0.90) # specifying a correlation limit of 0.95  

# Plot words that occur at least 50 times.
wordcloud2(subset(wf, freq>50))

#convert dtm to matrix 
m<-as.matrix(dtm)
#write as csv file 
write.csv(m,file="dtmEight2Late.csv")
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                     substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between documentvectors 
d <- dist(m)
#run hierarchical clustering using Ward's method 
groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree 
plot(groups, hang=-1)
#cut into 2 subtrees (experiment) - try 2,3,4,5,6 
rect.hclust(groups,2)

#kmeans clustering

#kmeans - run with nstart=100 and k=2,3,5 to compare results with hclust 
kfit <- kmeans(d, 5, nstart=100)
#plot - need library cluster 
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit 
print(kfit)
#print cluster sizes 
kfit$size
#print clusters (members) 
kfit$cluster
#write clusters to csv file 
write.csv(kfit$cluster,file="KMClustGroups2.csv")
#ss between cluster centers 
kfit$betweenss
#what about a simple scatter plot??

#kmeans - how to determine optimal number of clusters?
#look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k 
wss <- 2:19
for (i in 2:19) wss[i] <- sum(kmeans(d, centers=i,nstart=15)$withinss) 
plot(2:19, wss[2:19], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

clusplot(as.matrix(d), kfit$centers, color=TRUE, shade=TRUE,  labels=2, lines=0)

