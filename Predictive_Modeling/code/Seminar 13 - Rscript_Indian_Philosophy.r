#!/users/jeff/Documents/VIT_University/tidy_text/Rscript_Indian_Philosophy.r
# Load necessary libraries
	library(tm)
	library(tidyverse)
	library(tidytext)
	library(stringr)
	library(tibble)
	library(reshape2)
  library(wordcloud)
	library(wordcloud2)
	library(dplyr)
	library(tidyr)
	library(purrr)
	library(readr)
	library(janeaustenr)
	
# tidyverse and tidytext work well together, so I loaded both. The stringr package is useful for filtering out the LaTeX specific code and also for dropping words that have numbers in them (like jefferson1776 as a reference or 0.05).

# put/convert ind texts to a data frame
ind_words <- tibble(file = paste0("~/VIT_University/tidy_text/", 
    c("Indian_Philosophy_Part_I.txt", "Indian_Philosophy_Part_II.txt"))) %>%
	mutate(text = map(file, read_lines))
ind_words

# The resulting tibble has a variable file that is the name of the file that created that row and a list-column of the text of that file.
# We want to unnest() that tibble, remove the lines that are LaTeX crude (either start with \[A-Z] or \[a-z], like \section or \figure) and compute a line number.

ind_words <- ind_words %>%
  unnest() %>%
  filter(text != "%!TEX root = ind.tex") %>%
  filter(!str_detect(text, "^(\\\\[A-Z,a-z])"),
         text != "") %>%
  mutate(line_number = 1:n(),
         file = str_sub(basename(file), 1, -5))
# ind_words$file <- forcats::fct_relevel(ind_words$file, c("Indian_Philosophy_Part_I.txt", "Indian_Philosophy_Part_II.txt"))

# Now we have a tibble with file giving us the chapter, text giving us the line of text from the text files and line_number giving a counter of the number of lines since the start of the ind texts.
# Now we want to tokenize (strip each word of any formatting and reduce down to the root word, if possible). This is easy with unnest_tokens(). I played around with the results and came up with some other words that needed to be deleted (stats terms like ci or p, LaTeX terms like _i or tabular and references/numbers).												  

ind_words <- ind_words %>%
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
ind_words

# Now to compute the sentiment using the words written per line in the ind texts. tidytext comes with three sentiment lexicons, affin, bing and nrc. affin provides a score ranging from -5 (very negative) to +5 (very positive) for 2,476 words. bing provides a label of “negative” or “positive” for 6,788 words. nrc provides a label (anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise or trust) for 13,901 words. None of these account for negation (“I’m not sad” is a negative sentiment, not a positive one).
# Using the nrc lexicon, let’s see how the emotions of my words change over the three ind texts.

ind_words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(index = line_number %/% 25, file, sentiment) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = index, y = n, fill = file)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ sentiment, ncol = 5)

# All three texts are more positive than negative, and all three representedd trust fairly well. It looks like "disgust" and "sadness" are minimized.
# We can use the bing and afinn lexicons to look at how the sentiment of the words changed over the course of the thesis.  
ind_words %>% 
  left_join(get_sentiments("bing")) %>%
  left_join(get_sentiments("afinn")) %>%
  group_by(index = line_number %/% 25, file) %>%
  summarize(afinn = mean(score, na.rm = TRUE), 
            bing = sum(sentiment == "positive", na.rm = TRUE) - sum(sentiment == "negative", na.rm = TRUE)) %>%
  gather(lexicon, lexicon_score, afinn, bing) %>% 
  ggplot(aes(x = index, y = lexicon_score, fill = file)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ lexicon, scale = "free_y") +
    scale_x_continuous("Location in ind", breaks = NULL) +
    scale_y_continuous("Lexicon Score")
ind_words
# Looking at the two lexicon’s scoring of my books, the affin lexicon seems a little more stable if we assume local correlation of sentiments is likely. The scores show that all three text are much more positive than negative.
# Filter for negative words
bingnegative <- get_sentiments("bing") %>% 
    filter(sentiment == "negative")
# Get a word count
wordcounts <- ind_words %>%
    group_by(index = line_number %/% 25, file) %>%
    summarize(words = n())
wordcounts
# Build	a cloud chart
ind_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
# Build a contrasting cloud chart.
ind_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)  
# END OF SCRIPT
