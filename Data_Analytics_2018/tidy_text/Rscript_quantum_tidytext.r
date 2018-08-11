#!/users/jeff/Documents/VIT_University/tidy_text/Rscript_quantum_tidytext.r
# In 2010, I wrote Quantum Paith as an apologetic on agreement of science and creation. I had not planned to write two more in the series, but Quantum Hope and Quantum Love seemed to naturally follow after 1st Corinthians 13.
# I wondered how my sentiment changes across the three books. To do this, I’ll use the tidytext package. Let’s import the relevant packages now.
# Load necessary libraries
	library(tm)
	library(tidyverse)
	library(tidytext)
	library(stringr)
	library(tibble)
	library(reshape2)
    library(wordcloud)
	library(wordcloud2)
# tidyverse and tidytext work well together, so I loaded both. The stringr package is useful for filtering out the LaTeX specific code and also for dropping words that have numbers in them (like jefferson1776 as a reference or 0.05).

# put/convert quantum texts to a data frame
quantum_words <- tibble(file = paste0("~/VIT_University/tidy_text/", 
                         c("quantum_phaith.txt", "quantum_hope.txt", "quantum_love.txt"))) %>%
  mutate(text = map(file, read_lines))
quantum_words

# The resulting tibble has a variable file that is the name of the file that created that row and a list-column of the text of that file.
# We want to unnest() that tibble, remove the lines that are LaTeX crude (either start with \[A-Z] or \[a-z], like \section or \figure) and compute a line number.

quantum_words <- quantum_words %>%
  unnest() %>%
  filter(text != "%!TEX root = quantum.tex") %>%
  filter(!str_detect(text, "^(\\\\[A-Z,a-z])"),
         text != "") %>%
  mutate(line_number = 1:n(),
         file = str_sub(basename(file), 1, -5))
quantum_words$file <- forcats::fct_relevel(quantum_words$file, c("quantum_phaith",
                                                  "quantum_hope",
                                                  "quantum_love"))

# Now we have a tibble with file giving us the chapter, text giving us the line of text from the text files and line_number giving a counter of the number of lines since the start of the quantum texts.
# Now we want to tokenize (strip each word of any formatting and reduce down to the root word, if possible). This is easy with unnest_tokens(). I played around with the results and came up with some other words that needed to be deleted (stats terms like ci or p, LaTeX terms like _i or tabular and references/numbers).												  

quantum_words <- quantum_words %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[0-9]"),
         word != "fismanreview",
         word != "multicolumn",
         word != "p",
         word != "_i",
         word != "c", 
         word != "ci",
         word != "al",
         word != "dowellsars",
         word != "h",
         word != "tabular",
         word != "t",
         word != "ref",
         word != "cite",
         !str_detect(word, "[a-z]_"),
         !str_detect(word, ":"),
         word != "bar",
         word != "emph",
         !str_detect(word, "textless"))
quantum_words

# Now to compute the sentiment using the words written per line in the quantum texts. tidytext comes with three sentiment lexicons, affin, bing and nrc. affin provides a score ranging from -5 (very negative) to +5 (very positive) for 2,476 words. bing provides a label of “negative” or “positive” for 6,788 words. nrc provides a label (anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise or trust) for 13,901 words. None of these account for negation (“I’m not sad” is a negative sentiment, not a positive one).
# Using the nrc lexicon, let’s see how the emotions of my words change over the three quantum texts.

quantum_words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(index = line_number %/% 25, file, sentiment) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = index, y = n, fill = file)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ sentiment, ncol = 5)

# All three texts are more positive than negative, and all three representedd trust fairly well. It looks like "disgust" and "sadness" are minimized.
# We can use the bing and afinn lexicons to look at how the sentiment of the words changed over the course of the thesis.  
quantum_words %>% 
  left_join(get_sentiments("bing")) %>%
  left_join(get_sentiments("afinn")) %>%
  group_by(index = line_number %/% 25, file) %>%
  summarize(afinn = mean(score, na.rm = TRUE), 
            bing = sum(sentiment == "positive", na.rm = TRUE) - sum(sentiment == "negative", na.rm = TRUE)) %>%
  gather(lexicon, lexicon_score, afinn, bing) %>% 
  ggplot(aes(x = index, y = lexicon_score, fill = file)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ lexicon, scale = "free_y") +
    scale_x_continuous("Location in quantum", breaks = NULL) +
    scale_y_continuous("Lexicon Score")
quantum_words
# Looking at the two lexicon’s scoring of my books, the affin lexicon seems a little more stable if we assume local correlation of sentiments is likely. The scores show that all three text are much more positive than negative.
# Filter for negative words
bingnegative <- get_sentiments("bing") %>% 
    filter(sentiment == "negative")
# Get a word count
wordcounts <- quantum_words %>%
    group_by(index = line_number %/% 25, file) %>%
    summarize(words = n())
wordcounts
# Build	a cloud chart
quantum_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
# Build a contrasting cloud chart.
quantum_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)  
#
#
#
#
# To compare the sentiment expressed by my writing in general, I took six of my recent LinkedIn post and analyzed them.
paper_words <- data_frame(file = paste0("~/VIT_University/tidy_text/", 
                         c("Lifelong_Learning.txt", "Why_Stand_Many_Have_Fallen.txt", "An_Exercise_in_Rhetoric.txt", "Confessions_of_a_Career_Consultant.txt", "Where_did_all_the_Teaching_Go.txt", "Where_did_all_the_Thinking_Go.txt"))) %>%
  mutate(text = map(file, read_lines)) %>%
  unnest() %>%
  group_by(file = str_sub(basename(file), 1, -5)) %>%
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) 
paper_words

# "Where_did_all_the_Teaching_Go" expresses a complete negative sentiment. "An_Exercise_in_Rhetoric"  ends negative. "Why_Stand_Many_Have_Fallen" ends with positive sentiment, and "Where_did_all_the_Thinking_Go" also ends with positive sentiment.
paper_sentiment <- inner_join(paper_words, get_sentiments("bing")) %>%
  count(file, index = round(line_number / max(line_number) * 100 / 5) * 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)
paper_sentiment

paper_sentiment %>% ggplot(aes(x = index, y = net_sentiment, fill = file)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ file) + 
  scale_x_continuous("Location in paper (percent)") + 
  scale_y_continuous("Bing Net Sentiment")
# Execute the script
# source("Rscript_quantum_tidytext.r", echo=T)