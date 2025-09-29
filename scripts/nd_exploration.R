# exploration

library(igraph)
library(ggraph)
library(ggplot2)
library(stringr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(reshape2)
library(forcats)
library(topicmodels)
library(tm)

# combining four months to experiment
setwd("~/Research/text_mining/neurodiversity/data/raw_data/2022")
jan <- read.csv("2022-Jan.csv", stringsAsFactors=F)
feb <- read.csv("2022-Feb.csv", stringsAsFactors=F)
mar <- read.csv("2022-Mar.csv", stringsAsFactors=F)
apr <- read.csv("2022-Apr.csv", stringsAsFactors=F)

# create month row for each
jan$month <- "jan"
feb$month <- "feb"
mar$month <- "mar"
apr$month <- "apr"

full.df <- rbind(jan, feb, mar, apr) 

write.csv(full.df, "2022-Jan-Apr.csv")

# filtering out spam bot. this may not generalize
full.df %<>%
  filter(user_username != "TheCryptoWow")


# CHOOSE STOP WORDS ----
stop_words <- read.csv("", header=T)

# can also use tidy stop word lists
get_stopwords(source = "smart")
get_stopwords(source = "snowball")


# CREATE TOKENIZE FUNCTION ----
# cleans, tokenizes, and removes stop words
tokenize <- function(df, stop_words){
  # run data through preprocessing function
  df <- clean(df)
  # transform stop word list into tidytext readable format
  stop_words <- tibble(stop_words)
  # tokenize and remove stop words
  df <- df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
}



# TRANSFORM DATA ----
# to tidytext format 

# use mutate to annotate a linenumber quantity to keep track of 
# lines in the original format
nd.df <- full.df %>% 
  group_by(month) %>% 
  mutate(
    linenumber=row_number(),
    tweet=cumsum(
      str_detect(
        text, month))) %>% 
  ungroup()


# RUN TOKENIZE FUNCTION ----
# note* make sure your preprocessing function is loaded
df <- tokenize(nd.df, stop_words)


# BASIC EXPLORATION ----

## frequency count ----
df %>% 
  count(word, sort=T) %>% 
  print(n = 120)


## plot word frequencies ----
df %>%
  count(word, sort=T) %>%
  filter(n > 1300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


## calculate frequencies by month ----
freq <- df %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(month, word) %>%
  group_by(month) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = month, values_from = proportion) %>%
  pivot_longer(`jan`:`apr`,
               names_to = "month", values_to = "proportion")


# SENTIMENT ANALYSIS ----

# some available lexicons: nrc, bing, AFINN

## 'nrc' lexicon ----
## categorizes in binary fashion of positive, negative, anger
## anticipation, digust, fear, job, sadness, surprise, and trust
get_sentiments("nrc")

## 'bing' lexicon ----
## categorizes in binary fashion into pos/neg
get_sentiments("bing")

## 'AFINN' lexicon ----
## assigns words with a score between -5 and 5, ranging from neg to pos
get_sentiments("afinn")

## example - filter for joy words
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

df %>%
  inner_join(nrc_joy) %>%
  count(word, sort=T)

## note: sentiment lexicons have column named word
## so using "word" is convenient

## count how many pos and neg words using bing lexicon
nd.sent <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(month, index = linenumber %/% 20, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

## plot the sentiment scores
ggplot(nd.sent, aes(index, sentiment, fill = month)) +
  geom_col(show.legend = F) +
  facet_wrap(~month, ncol = 2, scales = "free_x") +
  labs(title = "bing lexicon scores by month")


## comparing the three lexicons ----

afinn <- df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  df %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "BING"),
  df %>% 
    inner_join(get_sentiments("nrc") %>%                  
                 filter(sentiment %in% c("positive", 
                                         "negative"))    
               ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# plot
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# compare nrc with bing
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive","negative")) %>%
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


## most common positive and negative words ----

# having a data frame with both pos and neg - you can count
bing_word_counts <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
bing_word_counts

# plot
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend=F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title="sentiment frequencies with bing",     
       x = "contribution to sentiment",  
       y = NULL)


# WORDCLOUDS ----

# plot word cloud
df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# "neurodiversity" is dominating, removing. 
# "amp" comes from "Culture Amp". removing for clarity
# leave "we" out for now until n-gram analysis
custom_stop_words <- bind_rows(
  tibble(word = c("neurodiversity","amp","we"),                                       
         lexicon = c("custom")),                                
  stop_words)

# plot again
df %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# compare - use sentiment analysis to tag pos and neg words using inner_join
# first sent to comparison.cloud

library(reshape2)

df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("darkred", "navy"),
                   max.words = 100)


# ANALYZING WORD AND DOCUMENT FREQUENCY: TF-IDF ----

## inverse document frequency (idf) decreases the weight for commonly
## used words and increases weight for words that are not used very much
## this can be combined with the term frequency to calculate a term's tf-idf
## (the two are multiplied together)

## term frequency ----

## most common terms
words <- df %>% 
  count(word, sort=T)

## words by month
words <- df %>% 
  count(word, month, sort=T)

tot.words <- words %>% 
  group_by(month) %>% 
  summarize(total=sum(n))

words <- left_join(words, tot.words)

ggplot(words, aes(n/sum(n))) +
  geom_histogram(show.legend=F) +
  xlim(NA, 0.0009) 
  # facet_wrap(~month, ncol = 2, scales = "free_y")


## tf-idf ----
### the bind_tf_idf() function ----

## the idea is to find the important words of each document by decreasing
## the weight for commonly used words and increasing the weight for words
## that are not used very much. this attempts to find words that are 
## important (i.e., common), but not too common

# the common words are zeros
tf.idf <- words %>% 
  bind_tf_idf(word, month, n)
tf.idf


# terms with high tf-idf:
tf.idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

# i would imagine "nfts" are spam. at any rate, it's an irrelevant term


library(forcats)

## visualize tf-idf ----
tf.idf %>%
  group_by(month) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = month)) +
  geom_col(show.legend=F) +
  facet_wrap(~month, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# very strange output. it looks like "thecryptowow" dominated
# in march, along with nfts. most of this is garbage - this suggests
# preprocessing should be re-addressed using a custom spell check list.
# each month is very different, suggesting that custom lists may not generalize


# N-GRAMS ----

## you get n-grams by adding token = "ngrams" option to unnest_tokens()

# create clean dataset
clean <- clean(full.df)


## convert data to bigrams ----
nd.bi <- clean %>%
  unnest_tokens(
    bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

## frequency count
nd.bi %>% 
  count(bigram, sourt=T)

### seperate words into separate columns ----
nd.bi_sep <- nd.bi %>%
  separate(bigram, c("word1", "word2"), sep = " ")


### create bigram stop word list ----
## removing "neurodiversity" and "amp"
bigram_stop_words <- bind_rows(
  tibble(word = c("neurodiversity","amp","we", "uk", "nfts", 
                  "our", "pm", "gmt", "nd", "herts"),                                       
         lexicon = c("custom")),                                
  stop_words) 


### filter out stop words ----
nd.bi_filtered <- nd.bi_sep %>%
  filter(!word1 %in% bigram_stop_words$word) %>%
  filter(!word2 %in% bigram_stop_words$word)

## new bigram counts
nd.bi_counts <- nd.bi_filtered %>% 
  count(word1, word2, sort=T)

nd.bi_counts

## after getting counts, adding the following to stop list:
## "we", "uk", "nfts", "our", "pm", "gmt", "nd", "herts"
## note: pm = "Project Management" (an organization), gmt = greenwich mean time,
## "nib" = "Neurodiversity in Business", "nd" i assume stands for neurodiversity
## "amp" = "Culture Amp", "hert" = "Hertfordshire Mind Nerwork"
## making note of "mock elon", and "openly mock" bigrams


### recombine bigram columns ----
bigrams_united <- nd.bi_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


### tf-idf for bigrams ----
bigram_tf.idf <- bigrams_united %>%
  count(month, bigram) %>%
  bind_tf_idf(bigram, month, n) %>%
  arrange(desc(tf_idf))

bigram_tf.idf


### visualize bigram tf-idf ----
bigram_tf.idf %>%
  group_by(month) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = month)) +
  geom_col(show.legend=F) +
  facet_wrap(~month, ncol = 2, scales = "free") +
  labs(
    title = "tf-idf graph for bigrams",
    x = "tf-idf", 
    y = NULL)


## network analysis of bigrams ----

### filter for only relatively common combinations ----
bigram_graph <- nd.bi_counts %>%
  filter(n > 25) %>%
  graph_from_data_frame()

bigram_graph


###  visualization ----
library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


### nicer visualization ----
set.seed(2010)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend=F,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# i played around with the seed to get a combo that wasn't too cluttered

## trigrams ----
## this time i'm leaving in "neurodiversity", "we", and "our"


### create trigrams stop words list ----
trigram_stop_words <- bind_rows(
  tibble(word = c("amp", "uk", "nfts", 
                   "pm", "gmt", "nd", "herts"),                                       
         lexicon = c("custom")),                                
  stop_words) 


### generate trigrams ----
trigram <- clean %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% trigram_stop_words$word,
         !word2 %in% trigram_stop_words$word,
         !word3 %in% trigram_stop_words$word) %>%
  count(word1, word2, word3, sort=T)

trigram


### trigram counts ----
nd.tri_counts <- trigram %>% 
  count(word1, word2, word3, sort=T)

nd.tri_counts


### recombine trigram columns ----
trigrams_united <- trigram %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united


# TOPIC MODELING ----

library(topicmodels)

## create document term matrix ----

library(tm)

# using bigram stop word list because it was the most comprehensive
nd.dtm <- clean %>%
  unnest_tokens(word, text) %>%
  count(X, word) %>%
  anti_join(bigram_stop_words) %>% 
  cast_dtm(X, word, n)
  
terms <- Terms(nd.dtm)
head(terms, 300)


## latent dirichlet allocation ----
## using LDA() from topipcs model package
## i created a function 


topic_model <- function(dtm, topic_num, n){
  
  # set a seed so that the output of the model is predictable
  dtm <- LDA(
    dtm, 
    k = topic_num, 
    control = list(seed = 1234))
  
  ## get word topic probabilities 
  dtm_topics <- tidy(
    dtm, matrix = "beta")
  
  ## isolate representative terms 
  top_terms <- dtm_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = n) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ## model visualization
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend=F) +
    facet_wrap(~ topic, scales = "free") +
    labs(title = sprintf("%d-topic LDA model", topic_num)) +
    scale_y_reordered()
  
}

topic_model(nd.dtm, 8, 10)


## some words are common within both topics - this is an advantage of topic
## as opposed to "hard clustering" methods; topics in natural language could
## have some overlap

## alternative is consider the terms that had the greatest difference in beta
## between topics one and two

beta_wide <- nd_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide









