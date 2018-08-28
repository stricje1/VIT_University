##################################################################################################
###                                     BLOG TOPIC ANALYSIS                                    ###
##################################################################################################

#load text mining library
library(tm)

#set working directory (modify path as needed)
setwd("C:/Users/Jeff/Documents/VIT_University/blog_corpus")

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern="*.txt")

#read files into a character vector
files <- lapply(filenames,readLines)

#create corpus from vector
docs <- Corpus(VectorSource(files))

#inspect a particular document in corpus
writeLines(as.character(docs[[3]]))

#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "⢩")
docs <- tm_map(docs, toSpace, "â€ ")
docs <- tm_map(docs, toSpace, "mâ€™")


#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)

#fix up 1) differences between us and aussie english 2) general errors
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "henry", replacement = "henry ")
#define and eliminate all custom stopwords
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","‘ve ",
                 "‘re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot")
docs <- tm_map(docs, removeWords, myStopwords)
#inspect a document as a check
writeLines(as.character(docs[[17]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq.csv")

#load topic models library
library(topicmodels)
 
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
 
#Number of topics
k <- 5
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
 
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
 
#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
 
#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
 
#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
 
#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
 
#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))

##################################################################################################
###                                 2012 USA PRESIDENTIAL DEBATE                               ###
##################################################################################################

[ [source files available on GitHub](https://github.com/pedrosan/TheAnalyticsEdge) ]

## PRELIMINARIES

Libraries needed for data processing and plotting:
    ```{r load_packages, cache = FALSE, echo = TRUE, message = FALSE, warning = FALSE, tidy = FALSE}
setwd("C:/Users/jeff/Documents/VIT_University")
## Install/Load Tools & Data
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_gh("trinker/gofastr")
#pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2, sentimentr)
library(scales)
library(Rcpp)
library(tm)
library(topicmodels)
library(dplyr)
library(tidyr)
library(igraph)
library(devtools)
library(LDAvis)
library(ggplot2)
library(sentimentr)
```
## Get External Scripts
Source external script with my own handy functions definitions:
    ```{r load_my_functions}
source("data/baseball_defs.R")
```
The content of this external file is included in the Appendix at the end of this report.

## Source topicmodels2LDAvis & optimal_k functions
invisible(lapply(
    file.path(
        "https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions", 
        c("topicmodels2LDAvis.R", "optimal_k.R")
    ),
    devtools::source_url
))
## Load the Data
```{r sec2-subset, eval = TRUE, cache = TRUE}
data(presidential_debates_2012)
head(presidential_debates_2012,5)
````
## Generate Stopwords 
```{r sec2-subset, eval = TRUE, cache = TRUE}
stops <- c(
        tm::stopwords("english"),
        tm::stopwords("SMART"),    
        "governor", "president", "mister", "obama","romney"
    ) %>%
    gofastr::prep_stopwords() 
```

## Create the DocumentTermMatrix
```{r sec2-subset, eval = TRUE, cache = TRUE}
doc_term_mat <- presidential_debates_2012 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops, stem=TRUE) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documenheads() 
```

## Control List
```{r sec2-subset, eval = TRUE, cache = TRUE}
control <- list(burnin = 500, iter = 1000, keep = 100)
```
### Determine Optimal Number of Topics
```{r sec2-subset, eval = TRUE, cache = TRUE}
(k <- optimal_k(doc_term_mat, 40, control = control))
```

## Harmonic Mean of Log Likelihood
![Number of Topics](Electplot01.png)### Run the Model

```{r sec2-subset, eval = TRUE, cache = TRUE}
control[["seed"]] <- 100
lda_model <- topicmodels::LDA(doc_term_mat, k=as.numeric(k), method = "Gibbs", 
    control = control)
```

## Plot the Topics Per Person & Time
```{r sec2-subset, eval = TRUE, cache = TRUE}
topics <- topicmodels::posterior(lda_model, doc_term_mat)[["topics"]]
topic_dat <- dplyr::add_rownames(as.data.frame(topics), "Person_Time")
colnames(topic_dat)[-1] <- apply(terms(lda_model, 10), 2, paste, collapse = ", ")

tidyr::gather(topic_dat, Topic, Proportion, -c(Person_Time)) %>%
    tidyr::separate(Person_Time, c("Person", "Time"), sep = "_") %>%
    dplyr::mutate(Person = factor(Person, 
        levels = c("OBAMA", "ROMNEY", "LEHRER", "SCHIEFFER", "CROWLEY", "QUESTION" ))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() +
        ggplot2::facet_grid(Person~Time) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::xlab("Proportion")
```

## Plot the Topics Matrix as a Heatmap 
```{r sec2-subset, eval = TRUE, cache = TRUE}
heatmap(topics, scale = "none")
```
## Heatmap of Topics
![Topics by Candidate](Electplot02.png)

## Network of the Word Distributions Over Topics
```{r sec2-subset, eval = TRUE, cache = TRUE}
post <- topicmodels::posterior(lda_model)

cor_mat <- cor(t(post[["terms"]]))
cor_mat[ cor_mat < .05 ] <- 0
diag(cor_mat) <- 0
```
## Strength Between Topics Based On Word Probabilities
```{r sec2-subset, eval = TRUE, cache = TRUE}
graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
graph <- delete.edges(graph, E(graph)[ weight < 0.05])

E(graph)$edge.width <- E(graph)$weight*20
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 15

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
    edge.color = "orange", vertex.color = "orange", 
    vertex.frame.color = NA, vertex.label.color = "grey30")
title("Strength Between Topics Based On Word Probabilities", cex.main=.8)
```
## Word Distributions over Topics
![Topics by Candidate](Electplot03.png)

## Network of the Topics Over Documents
```{r sec2-subset, eval = TRUE, cache = TRUE}
minval <- .1
topic_mat <- topicmodels::posterior(lda_model)[["topics"]]

graph <- graph_from_incidence_matrix(topic_mat, weighted=TRUE)
graph <- delete.edges(graph, E(graph)[ weight < minval])

E(graph)$edge.width <- E(graph)$weight*17
E(graph)$color <- "blue"
V(graph)$color <- ifelse(grepl("^\\d+$", V(graph)$name), "grey75", "orange")
V(graph)$frame.color <- NA
V(graph)$label <- ifelse(grepl("^\\d+$", V(graph)$name), paste("topic", V(graph)$name), gsub("_", "\n", V(graph)$name))
V(graph)$size <- c(rep(10, nrow(topic_mat)), colSums(topic_mat) * 20)
V(graph)$label.color <- ifelse(grepl("^\\d+$", V(graph)$name), "red", "grey30")

par(mar=c(0, 0, 3, 0))
set.seed(365)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
    vertex.color = adjustcolor(V(graph)$color, alpha.f = .4))
title("Topic & Document Relationships", cex.main=.8)
```
## Word Topics over Documents
![Topics by Candidate](Electplot03.png)

```{r sec2-subset, eval = TRUE, cache = TRUE}
## LDAvis of Model
lda_model %>%
    topicmodels2LDAvis() %>%
    LDAvis::serVis()
```

## Fitting New Data 

```{r sec2-subset, eval = TRUE, cache = TRUE}
library(gofastr)
## Create the DocumentTermMatrix for New Data
doc_term_mat2 <- partial_republican_debates_2015 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, location, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops, stem=TRUE) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documents() 
```

## Run the Model for New Data
```{r sec2-subset, eval = TRUE, cache = TRUE}
control2 <- control
control2[["estimate.beta"]] <- FALSE

lda_model2 <- topicmodels::LDA(doc_term_mat2, k = as.numeric(k), model = lda_model, 
    control = control2)
```

## Plot the Topics Per Person & Location for New Data
```{r sec2-subset, eval = TRUE, cache = TRUE}
topics2 <- topicmodels::posterior(lda_model2, doc_term_mat2)[["topics"]]
topic_dat2 <- dplyr::add_rownames(as.data.frame(topics2), "Person_Location")
colnames(topic_dat2)[-1] <- apply(terms(lda_model2, 10), 2, paste, collapse = ", ")

tidyr::gather(topic_dat2, Topic, Proportion, -c(Person_Location)) %>%
    tidyr::separate(Person_Location, c("Person", "Location"), sep = "_") %>%
    ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() +
        ggplot2::facet_grid(Person~Location) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::xlab("Proportion")
````

## LDAvis of Model for New Data
lda_model2 %>%
    topicmodels2LDAvis() %>%
    LDAvis::serVis()