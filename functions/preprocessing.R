##### PREPROCESSING #####

library(tidyverse)        
library(tidytext)         
library(stringi)          
library(magrittr)         
library(qdapDictionaries) 
library(qdap)             
library(textstem)         
library(SnowballC)        
library(tm)               
library(stringr)          

# lexicon conversion: british/canadian -> american
lex <- read.csv("dictionaries/lexicon_conversion.csv", stringsAsFactors = FALSE)
british  <- trimws(lex$british_lexicon)   # clean spaces
american <- trimws(lex$american_lexicon)

# abbreviations
abr <- read.csv("dictionaries/abbreviations.csv", stringsAsFactors = FALSE)
abbreviate   <- trimws(abr$abbreviate)
abbreviation <- trimws(abr$abbreviation)


# PREPROCESSING FUNCTION ----
clean_text <- function(x){
  
  # --- Classify tweet type (priority: RT > quote > reply > original) ---
  stype <- tolower(x$sourcetweet_type)
  x <- x %>%
    dplyr::mutate(
      tweet_category = dplyr::case_when(
        stype == "retweeted"                ~ "retweet",
        stype == "quoted"                   ~ "quote",
        stype == "replied_to"               ~ "reply",
        !is.na(in_reply_to_user_id)         ~ "reply",
        TRUE                                ~ "original"
      )
    )
  
  # filter out other languages
  x <- dplyr::filter(x, lang == "en")
  
  # remove URLs
  x$text <- gsub("https?://\\S+", " ", x$text)
  
  # remove standalone RT marker (not inside words)
  x$text <- gsub("(?i)\\bRT\\b:?\\s*", " ", x$text, perl = TRUE)
  
  # robust HTML entity cleanup
  x$text <- gsub("&(?:amp|gt|lt);?", " ", x$text)
  
  # remove "amp" artefact
  x$text <- gsub("\\bamp\\b", " ", x$text, ignore.case = TRUE)
  
  # apostrophe-less contractions common on Twitter (before lemmatization)
  x$text <- gsub("(?i)\\bim\\b",    "i am",    x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bive\\b",   "i have",  x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bdont\\b",  "do not",  x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bcant\\b",  "can not", x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bwont\\b",  "will not",x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bisnt\\b",  "is not",  x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bdoesnt\\b","does not",x$text, perl=TRUE)
  x$text <- gsub("(?i)\\bdidnt\\b", "did not", x$text, perl=TRUE)
  
  # normalize text
  x$text <- tolower(x$text)
  x$text <- gsub("\n", " ", x$text)
  x$text <- stringr::str_replace_all(x$text, "[\u2019\u2018\u02BC\u2032\u00B4\uFF07`´]", "'")
  x$text <- stringi::stri_replace_all_regex(x$text, "[\\p{Cf}]", "")
  
  # expand apostrophe’d contractions
  x$text <- qdap::replace_contraction(x$text, ignore.case = FALSE, sent.cap = FALSE)
  
  # extract & remove hashtags
  hashtags <- stringr::str_extract_all(x$text, "#\\w+")
  x$text   <- gsub("#\\w+", " ", x$text)
  x$extracted_hashtags <- sapply(hashtags, function(z) paste(z, collapse = " "))
  
  # extract & remove mentions
  tagged <- stringr::str_extract_all(x$text, "@\\w+")
  x$text <- gsub("@\\w+", " ", x$text)
  x$tagged <- sapply(tagged, function(z) paste(z, collapse = " "))
  
  # digits -> space, then drop bare ordinal suffixes (e.g., "5th" → " th" → removed)
  x$text <- gsub("[[:digit:]]", " ", x$text)
  x$text <- gsub("\\b(?:st|nd|rd|th)\\b", " ", x$text)
  
  # punctuation, lemmatize, negation glue
  x$text <- gsub("[[:punct:]]", " ", x$text)
  x$text <- textstem::lemmatize_strings(x$text)
  negation <- c("not ", "never ", "no ", "nobody ", "nor ", "neither ")
  negation_fixed <- c("not_", "never_", "no_", "nobody_", "nor_", "neither_")
  x$text <- stringi::stri_replace_all_regex(x$text, pattern = negation,
                                            replacement = negation_fixed, vectorize_all = FALSE)
  
  # british/canadian → american; abbreviations
  x$text <- stringi::stri_replace_all_regex(x$text, pattern = british,
                                            replacement = american, vectorize_all = FALSE)
  abbrev_patterns <- paste0("\\b", abbreviate, "\\b")
  x$text <- stringi::stri_replace_all_regex(x$text, pattern = abbrev_patterns,
                                            replacement = abbreviation, vectorize_all = FALSE)
  
  # tidy up white spaces
  x$text <- gsub("\\s+", " ", x$text)
  x$text <- trimws(x$text)
  
  # create variable to flag duplicates
  x <- x %>%
    dplyr::mutate(text_norm = stringr::str_squish(tolower(text))) %>%
    dplyr::add_count(text_norm, name = "dup_n") %>%
    dplyr::group_by(text_norm) %>%
    dplyr::mutate(dup_rank = dplyr::row_number(),
                  is_dup_text = dup_rank > 1) %>%
    dplyr::ungroup()
  
  x
}
