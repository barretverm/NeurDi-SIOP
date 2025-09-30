##### PREPROCESSING #####

library(tidyverse)        # data manipulation
library(tidytext)         # tidy text processing
library(stringi)          # string operations, regex
library(magrittr)         # pipes (%>% / %<>%)
library(qdapDictionaries) # contraction/negation dictionaries
library(qdap)             # text cleaning (replace_contraction)
library(textstem)         # lemmatization
library(SnowballC)        # stemming (not directly used)
library(tm)               # stopword/cleaning tools
library(stringr)          # string helpers

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
  
  ## handle NA values so they don't break filters
  x$sourcetweet_type[is.na(x$sourcetweet_type)] <- "999"
  x$lang[is.na(x$lang)] <- "999"
  
  # filter out retweets, quotes, and non-English
  x <- x %>%
    dplyr::filter(sourcetweet_type != "retweeted" &
                    sourcetweet_type != "quoted") %>%
    dplyr::filter(lang == "en")
  
  # remove links and duplicates
  x$text <- gsub("\\bhttp\\S*\\s*", " ", x$text)
  x <- x[!duplicated(x$text), ]
  
  # normalize text
  x$text <- tolower(x$text)                                         # lowercase
  x$text <- gsub("\n", " ", x$text)                                 # remove newlines
  x$text <- stringr::str_replace_all(
    x$text, "[\u2019\u2018\u02BC\u2032\u00B4\uFF07`´]", "'") # normalize apostrophes
  x$text <- stringi::stri_replace_all_regex(x$text, "[\\p{Cf}]", "") # strip zero-width chars
  
  # expand contractions
  x$text <- replace_contraction(x$text, ignore.case = FALSE, sent.cap = FALSE)
  x$text <- gsub("\\bim\\b", "i am", x$text, perl = TRUE)   # fix apostrophe-less "im"
  
  ## extract hashtags
  hashtags <- stringr::str_extract_all(x$text, "#\\w+")
  x$text   <- gsub("#\\w+", " ", x$text)
  x$extracted_hashtags <- sapply(hashtags, function(z) paste(z, collapse = " "))
  
  ## extract tagged users
  tagged <- stringr::str_extract_all(x$text, "@\\w+")
  x$text <- gsub("@\\w+", " ", x$text)
  x$tagged <- sapply(tagged, function(z) paste(z, collapse = " "))
  
  # general cleaning
  x$text <- gsub("[[:punct:]]", " ", x$text)  # strip other punctuation 
  x$text <- gsub("[[:digit:]]", " ", x$text)  # remove digits
  x$text <- lemmatize_strings(x$text)         # lemmatize
  
  ## handle negations
  negation <- c("not ", "never ", "no ", "nobody ", "nor ", "neither ")
  negation_fixed <- c("not_", "never_", "no_", "nobody_", "nor_", "neither_")
  x$text <- stringi::stri_replace_all_regex(
    x$text,
    pattern = negation,
    replacement = negation_fixed,
    vectorize_all = FALSE
  )
  
  # british/canadian -> american
  x$text <- stringi::stri_replace_all_regex(
    x$text,
    pattern = british,
    replacement = american,
    vectorize_all = FALSE
  )
  
  # replace abbreviations (e.g., nd -> neurodiversity)
  abbrev_patterns <- paste0("\\b", abbreviate, "\\b")  # add word boundaries
  x$text <- stringi::stri_replace_all_regex(
    x$text,
    pattern = abbrev_patterns,
    replacement = abbreviation,
    vectorize_all = FALSE
  )
  
  # remove HTML artefacts
  x$text <- gsub("&amp", " ", x$text)   
  x$text <- gsub("amp",  " ", x$text) 
  x$text <- gsub(" s ",  " is ", x$text) 
  
  # tidy whitespace
  x$text <- gsub("\\s+", " ", x$text)
  x$text <- trimws(x$text)
  
  return(x)
}

# test
test <- clean_text(data)
View(test)
