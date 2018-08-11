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
docs <- Corpus(DirSource(cname))   
summary(docs)
inspect(docs)

#Removing punctuation:

# If necesasry, you can remove special characters.
docs <- tm_map(docs, removePunctuation) 
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
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

#Remove common word endings (e.g., "ing", "es", "s")  
#Strip unnecesary whitespace from your documents:
docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, stripWhitespace)   
# inspect(docs[3]) # Check to see if it worked.   

#This tells R to treat your preprocessed documents as text documents.
docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   

# To proceed, create a document term matrix.


#You'll also need a transpose of this matrix. Create it using:


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