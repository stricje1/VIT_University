## Loading Libraries
## Sourcing Data
### Processing Data
#### Tokenization

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidytext)
library(tm)
library(sentimentr)
library(wordcloud)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotrix)
library(igraph)
library(ggraph)

setwd("C:/Users/jeff/Documents/VIT_University/IsisFanboy/")
getwd()

tweets<-read.csv("AboutIsis.csv",stringsAsFactor=FALSE)

str(tweets)

# Glimpse of data 
tweets$date<-as.Date(tweets$time, "%d/%m/%Y %H:%M:%S")
head(tweets)
  
#### Tokenization  
tweets$date<-as.Date(tweets$time, "%d/%m/%Y %H:%M:%S")
tidy_tweets<-tweets %>%group_by(name,username,tweetid)%>%mutate(ln=row_number())%>%unnest_tokens(word,tweets)%>%ungroup()
head(tidy_tweets,5)

t_wrd<-tidy_tweets %>%count(word,sort=TRUE)
head(t_wrd,5)

# tidy_tweets contains all words like 'the','is','are' etc.. So lets take only the sentimental words by joining with lexicons.
# combining with lexicons to get the sentiment words  

tweets_sentiment<-tidy_tweets%>% inner_join(get_sentiments("bing"))
# WordCloud of all words in ISIS tweets  
tot_wrd<-tweets_sentiment%>%count(word,sort=TRUE)
wordcloud(tot_wrd$word,tot_wrd$n, min.freq =5, scale=c(5, .2), random.order = FALSE, random.color = FALSE,colors = brewer.pal(8, "Dark2"))

pos_neg<-tweets_sentiment %>%count(word,sentiment,sort=TRUE)

# Plotting the most occuring positive and negative words.  
pos_neg %>% filter(sentiment=='positive')%>%head(20) %>%ggplot(aes(x=word,y=n))+geom_bar(stat="identity",fill="green4")+
  theme(axis.text.x=element_text(angle=90))+labs(title="Most occuring Positive Words",y="count")
pos_neg %>% filter(sentiment=='negative')%>%head(20) %>%ggplot(aes(x=word,y=n))+geom_bar(stat="identity",fill="red4")+
  theme(axis.text.x=element_text(angle=90))+labs(title="Most occuring Negative Words",y="count")
 

# Percenatage of positive and negative words in the tweets.  
perc<-tweets_sentiment%>%count(sentiment)%>%mutate(total=sum(n))%>%group_by(sentiment)%>%mutate(percent=round(n/total,2)*100)%>%ungroup()

label <-  
  c( paste(perc$percent[1],'%','-',perc$sentiment[1],sep=''),
     paste(perc$percent[2],'%','-',perc$sentiment[2],sep=''))

pie3D(perc$percent,labels=label,labelcex=1.1,explode=0.1 	)

# Wordcloud on Positive and Negative words  
pos<-pos_neg %>% filter(sentiment=='positive')
neg<-pos_neg %>% filter(sentiment=='negative')
wordcloud(pos$word,pos$n, min.freq =5, scale=c(5, .2), random.order = FALSE, random.color = FALSE,colors = brewer.pal(9,"Spectral"))
wordcloud(neg$word,neg$n, min.freq =5, scale=c(5, .2), random.order = FALSE, random.color = FALSE,colors = brewer.pal(9, "Spectral"))

# Top 10 words contributing to different sentiments  
tidy_tweets%>%inner_join(get_sentiments("nrc")) %>%count(word,sentiment) %>% group_by(sentiment)%>%top_n(10)%>%
  ungroup() %>%mutate(word=reorder(word,n))%>%ggplot(aes(x=word,y=n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# Positive & Negative Words over time  
sentiment_by_time <- tidy_tweets %>%
  mutate(dt = floor_date(date, unit = "month")) %>%
  group_by(dt) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc"))

sentiment_by_time %>%
  filter(sentiment %in% c('positive','negative')) %>%
  count(dt,sentiment,total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  ggplot(aes(x=dt,y=percent,col=sentiment,group=sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)

demo_bigrams <- unnest_tokens(tweets, input = tweets, output = bigram, token = "ngrams", n=2)
demo_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- demo_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "holy") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()


not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"holy\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)
## Network of bigrams
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)