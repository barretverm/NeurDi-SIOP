# DESCRIPTIVES
# top hashtags and tagged users

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# TOP HASHTAGS ----
top_hashtags <- function(df, year, n = 20, exclude = NULL, unique_per_tweet = FALSE) {
  out <- df %>%
    filter(is.na(tweet_category) | tweet_category != "retweet") %>%   # drop RTs
    select(tweet_id, extracted_hashtags) %>%
    mutate(extracted_hashtags = str_squish(extracted_hashtags)) %>%
    separate_rows(extracted_hashtags, sep = "\\s+") %>%
    filter(!is.na(extracted_hashtags), extracted_hashtags != "") %>%
    mutate(hashtag = tolower(str_remove(extracted_hashtags, "^#")))
  
  if (!is.null(exclude)) out <- out %>% filter(!hashtag %in% tolower(exclude))
  if (unique_per_tweet)  out <- out %>% distinct(tweet_id, hashtag)
  
  out %>%
    count(hashtag, sort = TRUE) %>%
    slice_head(n = n) %>%
    mutate(year = as.integer(year), .before = 1)   # add the year column
}

# ---- HASHTAG PLOTS ----

# 2020
top20_2020 <- top_hashtags(tweets_2020, 2020, n = 10, exclude = "neurodiversity")
ggplot(top20_2020, aes(x = fct_reorder(paste0("#", hashtag), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2020 Top 10 Hashtags (Excluding Retweets & #neurodiversity)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2021
top20_2021 <- top_hashtags(tweets_2021, 2021, n = 10, exclude = "neurodiversity")
ggplot(top20_2021, aes(x = fct_reorder(paste0("#", hashtag), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2021 Top 10 Hashtags (Excluding Retweets & #neurodiversity)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2022
top20_2022 <- top_hashtags(tweets_2022, 2022, n = 10, exclude = "neurodiversity")
ggplot(top20_2022, aes(x = fct_reorder(paste0("#", hashtag), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2022 Top 10 Hashtags (Excluding Retweets & #neurodiversity)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2023
top20_2023 <- top_hashtags(tweets_2023, 2023, n = 10, exclude = "neurodiversity")
ggplot(top20_2023, aes(x = fct_reorder(paste0("#", hashtag), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2023 Top 10 Hashtags (Excluding Retweets & #neurodiversity)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# all
top20_all <- top_hashtags(all_tweets, year = "All", n = 10, exclude = "neurodiversity")
ggplot(top20_all, aes(x = fct_reorder(paste0("#", hashtag), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Top 10 Hashtags Total (Excluding Retweets & #neurodiversity)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)


# TOP MENTIONS (tagged users) ----
top_mentions <- function(df, year, n = 20, exclude = NULL, unique_per_tweet = FALSE) {
  out <- df %>%
    filter(is.na(tweet_category) | tweet_category != "retweet") %>%   # drop RTs
    select(tweet_id, tagged) %>%
    mutate(tagged = str_squish(tagged)) %>%
    separate_rows(tagged, sep = "\\s+") %>%
    filter(!is.na(tagged), tagged != "") %>%
    mutate(mention = tolower(str_remove(tagged, "^@")))
  
  if (!is.null(exclude)) out <- out %>% filter(!mention %in% tolower(exclude))
  if (unique_per_tweet)  out <- out %>% distinct(tweet_id, mention)
  
  out %>%
    count(mention, sort = TRUE) %>%
    slice_head(n = n) %>%
    mutate(year = as.character(year), .before = 1)   # keep text-friendly for "All"
}

## ---- TAGGED PLOTS ----

# 2020
topm_2020 <- top_mentions(tweets_2020, 2020, n = 10)
ggplot(topm_2020, aes(x = fct_reorder(paste0("@", mention), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2020 Top 10 Mentions (Excluding Retweets)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2021
topm_2021 <- top_mentions(tweets_2021, 2021, n = 10)
ggplot(topm_2021, aes(x = fct_reorder(paste0("@", mention), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2021 Top 10 Mentions (Excluding Retweets)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2022
topm_2022 <- top_mentions(tweets_2022, 2022, n = 10)
ggplot(topm_2022, aes(x = fct_reorder(paste0("@", mention), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2022 Top 10 Mentions (Excluding Retweets)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# 2023
topm_2023 <- top_mentions(tweets_2023, 2023, n = 10)
ggplot(topm_2023, aes(x = fct_reorder(paste0("@", mention), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "2023 Top 10 Mentions (Excluding Retweets)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

# All years combined
topm_all <- top_mentions(all_tweets, year = "All", n = 10)
ggplot(topm_all, aes(x = fct_reorder(paste0("@", mention), n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Top 10 Mentions (All Years, Excluding Retweets)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 12)

