###################################################################################
###                          BLOG TOPIC ANALYSIS USING LDA                      ###
###################################################################################

Topic modeling focuses on the problem of classifying sets of documents into themes, either manually or automatically (preferred). It is a way of identifying patterns in a corpus-technically, this is text mining. We take our corpus and group words across the corpus into 'topics,' using a text mining algorithm or tool.

In natural language processing, latent Dirichlet allocation (LDA) is a generative statistical model that allows sets of observations to be explained by unobserved groups that explain why some parts of the data are similar. A topic model which discovers underlying topics in a collection of documents and infers word probabilities in topics

We will use a collection of 20 posts from my LinkedIn blog as an example corpus, call "blog_corpus." The corpus can be downloaded at https://github.com/stricje1/VIT_University/tree/master/Data_Analytics_2018/data.

```{r echo = TRUE}
#load text mining library
library(tm)
```

## Set working Directory

see your working dierct(modify path as needed)

```{r echo = TRUE}
setwd("C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/code/blog_corpus")

```

## load files into corpus

```{r echo = TRUE}
get listing of .txt files in directory
filenames <- list.files(getwd(),pattern="*.txt")
```

## Read files into a character vector

```{r echo = TRUE}
files <- lapply(filenames,readLines)
```

## create corpus from vector

```{r echo = TRUE}
docs <- Corpus(VectorSource(files))
```

#inspect a particular document in corpus

```{r echo = TRUE}
writeLines(as.character(docs[[3]]))
```

## Start Preprocessing

The next preprocessing steps include converting to lower case, removing special characters/symbols, removing punctuation, stripping digits, removing stop words,  abd removing white spaces.

First, we transform the corpus to lower case.

```{r echo = TRUE}
docs <-tm_map(docs,content_transformer(tolower))
```

Second, we remove punctuation.

```{r echo = TRUE}
docs <- tm_map(docs, removePunctuation)
```

Third, we strip digits from the corpus.

```{r echo = TRUE}
docs <- tm_map(docs, removeNumbers)
```

Fourth, we remove stopwords.

```{r echo = TRUE}
docs <- tm_map(docs, removeWords, stopwords("english"))
```

Finally, we remove whitespaces.

```{r echo = TRUE}
docs <- tm_map(docs, stripWhitespace)
```

It is good practice to check the document preprocessing every now and then.

```{r echo = TRUE}
writeLines(as.character(docs[[20]]))
```

## Stem document

(fix up 1) differences between us and aussie english 2) general errors

```{r echo = TRUE}
docs <- tm_map(docs,stemDocument)
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
``

At this point, it is a good idea to inspect a document as a check.

```{r echo = TRUE}
writeLines(as.character(docs[[17]]))
```

## Create document-term matrix

This code chunk generates the document-term matrix for our corpus, and the next several code chucks set up the document-term matrix for our analysis.

```{r echo = TRUE}
dtm <- DocumentTermMatrix(docs)
```

This code chunk converts rownames to filenames

```{r echo = TRUE}
rownames(dtm) <- filenames
```

This code chunk collapses matrix by summing over columns

```{r echo = TRUE}
freq <- colSums(as.matrix(dtm))
```

This code chunk determines the length, which should be total number of terms.

```{r echo = TRUE}
length(freq)
```

This code chunk creates the sort order (descending by frequency) for our terms and writes it to our disk.

```{r echo = TRUE}
ord <- order(freq,decreasing=TRUE)
freq[ord]
write.csv(freq[ord],"word_freq.csv")
```

## Load Topic Models Library

Here we load the R-package "topicmodels" that we will be using in our analysis.

```{r echo = TRUE}
library(topicmodels)
```

 
## Number of Topics

Now we set a rough guess for the number of topics as five. Later, we will use an user function to make a better guess at the optimal number of topics, k.

```{r echo = TRUE}
k <- 5
```

## Set Parameters for Gibbs Sampling

In this code chunk we set the parameters for the LDA using Gibbs sampling that we will implement below. This is just a matter of convienance.

```{r echo = TRUE}
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
```

## Run LDA using Gibbs sampling

Using the parameter we set above, we now implment the LDA model using Gibbs sampling.

```{r echo = TRUE}
control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin)
ldaOut <-LDA(dtm,k, method="Gibbs", control=control)
```

## Write out Results

Now we write the documents to topics:

  ```{r echo = TRUE}
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
```

## Top 6 Terms per Topic

The next code chunk provides the top six terms for each topic.

```{r echo = TRUE}
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
```

## Topic Probabilities

Next, we calculate the probabilities associated with each topic assignment.

```{r echo = TRUE}
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
```

#Find relative importance of top 2 topics

```{r echo = TRUE}
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
```

#Find relative importance of second and third most important topics

```{r echo = TRUE}
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
```

#write to file

```{r echo = TRUE}
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
```

##################################################################################
###                  2012 USA PRESIDENTIAL DEBATE USING LDA                    ###
##################################################################################

[[source files available on GitHub](https://github.com/pedrosan/TheAnalyticsEdge)]

## PRELIMINARIES

Here, we install and load libraries needed for data processing and plotting:
  
```{r load_packages, cache = FALSE, echo = TRUE, message = FALSE, warning = FALSE, tidy = FALSE}
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2, sentimentr)
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

Next, we call external script containing my own optimal_k functions definitions, using the source function call.
  
```{r load_my_functions}
source("C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/code/optimal_k.R")
```

The content of this external file is included in the Appendix at the end of this document.

## Load the Data

Now, we load the data.

```{r sec2-subset, eval = TRUE, cache = TRUE}
data(presidential_debates_2012)
head(presidential_debates_2012,5)
````

In this code chunk we generate user-defined stopwords that are relevant to our case.

```{r sec2-subset, eval = TRUE, cache = TRUE}
stops <- c(
        tm::stopwords("english"),
        tm::stopwords("SMART"),    
        "governor", "president", "mister", "obama","romney"
    ) %>%
    gofastr::prep_stopwords() 
```

The next code chunk creates the document-term matrix.

```{r sec2-subset, eval = TRUE, cache = TRUE}
doc_term_mat <- presidential_debates_2012 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops, stem=TRUE) %>%                                                    
    gofastr::filter_tf_idf() %>%
    #gofastr::filter_documenheads() 
```

This code chunk established the control list we will use to find optimum_k.

```{r sec2-subset, eval = TRUE, cache = TRUE}
control <- list(burnin = 500, iter = 1000, keep = 100)
```

## Determine Optimal Number of Topics

Using our external optimal_k function code, we determine the optimal number of topics, which is a little btter than a random guess.

```{r sec2-subset, eval = TRUE, cache = TRUE}
if (!require("vlad")) install.packages("vlad")
library(vlad)
k <- optimal_k(doc_term_mat, 40, control = control)
```

## Harmonic Mean of Log Likelihood
![Number of Topics](Electplot01.png)

## Run the Model

Now, we implement he LDA model using Gibbs sampling and the parameters we have already established.

```{r sec2-subset, eval = TRUE, cache = TRUE}
control[["seed"]] <- 100
lda_model <- topicmodels::LDA(doc_term_mat, k=as.numeric(k), method = "Gibbs", 
    control = control)
```

## Plot the Topics Per Person & Time

This code chunk is used to generate a plot of the topics per person and time.

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

Now, we make a heatmap of our results.

```{r sec2-subset, eval = TRUE, cache = TRUE}
heatmap(topics, scale = "none")
```
## Heatmap of Topics
![Topics by Candidate](Electplot02.png)

## Network of the Word Distributions Over Topics

Next, we generate a network of word distributions over topics.

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

This code chunk generates a network of the topics over documents

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


## LDAvis of Model

```{r sec2-subset, eval = TRUE, cache = TRUE}
source("C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/code/topicmodels2LDAvis.R")
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
control = list(nstart=5, seed = list(2003,5,63,100001,765), best=TRUE, burnin = 4000, iter = 2000, thin=500)
control2  = list(burnin = 500, iter = 1000, keep = 100)
#control2[["estimate.beta"]] <- FALSE
k <- optimal_k1(doc_term_mat2,40,control=control2)
plot.optimal_k1(optimal_k1(doc_term_mat2,40,control=control2))
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