##### PARSE DATASETS #####
library(dplyr)
library(lubridate)

# ---- READ IN DATA ----
# See "combine_datasets" function - it's set to "cleaned_data" directory

# to import all:
# all_tweets  <- do.call(rbind, lapply(2020:2023, read_year))

tweets_2020 <- read_year(2020)
tweets_2021 <- read_year(2021)
tweets_2022 <- read_year(2022)
tweets_2023 <- read_year(2023)



# ---- REORDER CHRONOLOGICALLY ----
# 2020
tweets_2020 <- tweets_2020 %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC")  
  ) %>%
  arrange(created_at)

# 2021
tweets_2021 <- tweets_2021 %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC")  
  ) %>%
  arrange(created_at)

# 2022
tweets_2022 <- tweets_2022 %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC") 
  ) %>%
  arrange(created_at)

# 2023
tweets_2023 <- tweets_2023 %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC")  
  ) %>%
  arrange(created_at)

# COMBINE ALL
all_tweets <- bind_rows(tweets_2020, tweets_2021, tweets_2022, tweets_2023)



# ---- PARSE DATASETS BY RETWEETS, QUOTES, REPLIES, ORIGINAL ----
## retweets ----
retweets_2020 <- tweets_2020[tweets_2020$tweet_category == "retweet", ]
retweets_2021 <- tweets_2021[tweets_2021$tweet_category == "retweet", ]
retweets_2022 <- tweets_2022[tweets_2022$tweet_category == "retweet", ]
retweets_2023 <- tweets_2023[tweets_2023$tweet_category == "retweet", ]
retweets_all  <- all_tweets[all_tweets$tweet_category   == "retweet", ]

## quotes ----
quotes_2020 <- tweets_2020[tweets_2020$tweet_category == "quote", ]
quotes_2021 <- tweets_2021[tweets_2021$tweet_category == "quote", ]
quotes_2022 <- tweets_2022[tweets_2022$tweet_category == "quote", ]
quotes_2023 <- tweets_2023[tweets_2023$tweet_category == "quote", ]
quotes_all  <- all_tweets[all_tweets$tweet_category   == "quote", ]
# check for duplicates
sum(duplicated(quotes_all$tweet_id))
sum(duplicated(quotes_all$text))
# duplicates by a combo (same user + same text)
sum(duplicated(quotes_all[c("author_id","text")]))

## replies ----
replies_2020 <- tweets_2020[tweets_2020$tweet_category == "reply", ]
replies_2021 <- tweets_2021[tweets_2021$tweet_category == "reply", ]
replies_2022 <- tweets_2022[tweets_2022$tweet_category == "reply", ]
replies_2023 <- tweets_2023[tweets_2023$tweet_category == "reply", ]
replies_all  <- all_tweets[all_tweets$tweet_category   == "reply", ]
# check for duplicates
sum(duplicated(replies_all$tweet_id))
sum(duplicated(replies_all$text))
# duplicates by a combo (same user + same text)
sum(duplicated(replies_all[c("author_id","text")]))

## original ----
original_2020 <- tweets_2020[tweets_2020$tweet_category == "original", ]
original_2021 <- tweets_2021[tweets_2021$tweet_category == "original", ]
original_2022 <- tweets_2022[tweets_2022$tweet_category == "original", ]
original_2023 <- tweets_2023[tweets_2023$tweet_category == "original", ]
original_all  <- all_tweets[all_tweets$tweet_category   == "original", ]
# check for duplicates
sum(duplicated(original_all$tweet_id))
sum(duplicated(original_all$text))
# duplicates by a combo (same user + same text)
sum(duplicated(original_all[c("author_id","text")]))



# ---- REMOVE DUPLICATES ----
# duplicating by author AND text inside each category. 
# this keeps one copy per account per message but still counts when different 
# people post the same message
dedupe_author_text <- function(df) {
  # normalize text (case + whitespace) for matching
  txt <- tolower(trimws(gsub("\\s+", " ", ifelse(is.na(df$text), "", df$text))))
  key <- paste(df$author_id, txt)              # one per (author, text)
  keep <- !duplicated(key)
  df[keep, , drop = FALSE]
}

## quotes ----
quotes_2020_dedup <- dedupe_author_text(quotes_2020)
quotes_2021_dedup <- dedupe_author_text(quotes_2021)
quotes_2022_dedup <- dedupe_author_text(quotes_2022)
quotes_2023_dedup <- dedupe_author_text(quotes_2023)
quotes_all_dedup <- rbind(quotes_2020_dedup, quotes_2021_dedup,
                          quotes_2022_dedup, quotes_2023_dedup)

## replies ----
replies_2020_dedup <- dedupe_author_text(replies_2020)
replies_2021_dedup <- dedupe_author_text(replies_2021)
replies_2022_dedup <- dedupe_author_text(replies_2022)
replies_2023_dedup <- dedupe_author_text(replies_2023)
replies_all_dedup <- rbind(replies_2020_dedup, replies_2021_dedup,
                           replies_2022_dedup, replies_2023_dedup)

## original ----
original_2020_dedup <- dedupe_author_text(original_2020)
original_2021_dedup <- dedupe_author_text(original_2021)
original_2022_dedup <- dedupe_author_text(original_2022)
original_2023_dedup <- dedupe_author_text(original_2023)
original_all_dedup <- rbind(original_2020_dedup, original_2021_dedup,
                            original_2022_dedup, original_2023_dedup)

# recombine
all_no_rt_dedup <- rbind(quotes_all_dedup, replies_all_dedup, original_all_dedup)
all_no_rt_dedup <- all_no_rt_dedup[order(all_no_rt_dedup$created_at), ]
