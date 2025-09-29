##### NEURODIVERSITY PREPROCESSING #####

library(tidyverse)
library(tidytext)
library(stringi)
library(magrittr)
library(qdapDictionaries)
library(qdap)
library(textstem)
library(SnowballC)
library(tm)

# read in csv (replace with your file path)
setwd('')
data <- read.csv('', stringsAsFactors=F)

# import list of british to american lexicon conversion
lex <- read.csv("~/NeurDi/custom_lists/lexicon_conversion.csv",
                stringsAsFactors=F)
british <- lex$british_lexicon 
american <- lex$american_lexicon                   


# PREPROCESSING FUNCTION ----
clean <- function(x){
  
  ## change NA to 999 to remove retweets and quotes ----
  x %<>% 
    mutate_all(~ifelse(is.na(.),999,.)) %>%
    filter(sourcetweet_type != "retweeted"
           & sourcetweet_type != "quoted") %>% 
    filter(lang=="en")                           ## filter out non-english ----
  
  # a lot of the duplicates were tweets with different links at the end
  # this helps filter them out
  x$text <- gsub("\\bhttp\\S*\\s*","", x$text)   ## remove links ----
  x <- x[!duplicated(x$text),]                   ## remove duplicates ----
  
  x$text <- tolower(x$text)                      ## convert to lowercase ----
  # remove newline characters from the "text" column
  x$text <- gsub("\n", "", x$text)
  
  ## replace all "’" with "'" ----
  # so that contractions are interpreted correctly
  x$text <- str_replace_all(x$text,"’","'")
  ## expand contractions ----
  x$text <- replace_contraction(
    x$text, ignore.case=F, sent.cap=F)             
  
  ## hashtags ----
  ### extract hashtags from the text column ----
  hashtags <- str_extract_all(x$text, "#\\w+")
  ### remove hashtags from the "text" column ----
  x$text <- gsub("#\\w+", "", x$text)
  ### create a new column with the extracted hashtags ----
  x$extracted_hashtags <- sapply(
    hashtags, function(x) paste(x, collapse = " "))
  
  ## tagged users ----
  ### extract tagged users from the text column ----
  tagged <- str_extract_all(x$text, "@\\w+")
  ### remove tags from the "text" column ----
  x$text <- gsub("@\\w+", "", x$text)
  ### Create a new column with the extracted tags ----
  x$tagged <- sapply(
    tagged, function(x) paste(x, collapse = " "))
  
  x$text <- gsub("[[:punct:]]", "", x$text)      ## remove punctuation ----
  x$text <- gsub("[[:digit:]]", "", x$text)      ## remove numbers ----
  x$text <- lemmatize_strings(x$text)            ## lemmatize ----
  
  ## negations ----
  # based on the negation words in `qdapDictionaries::negation.words`,
  # create a vector with the negation words and one with the the
  # negation words followed by '_'. Then apply the handle negation.
  negation <- c(
    "not ", "never ", "no ", "nobody ", "nor ", "neither ") 
  negation_fixed <- c(
    "not_", "never_", "no_", "nobody_", "nor_", "neither_")  
  # apply
  x$text <- stringi::stri_replace_all_regex(
    str = x$text, pattern = negation, replacement = negation_fixed, 
    stringi::stri_opts_fixed(case_insensitive=F))
  
  # replace all bloody british spellings with american
  # apply
  x$text <- stringi::stri_replace_all_regex(
    str = x$text, pattern = british, replacement = american, 
    stringi::stri_opts_fixed(case_insensitive=F))
  
  ## remove stop words ----
  # x$text <- removeWords(x$text, words = stopwords::stopwords("en"))
  # x$text <- gsub("[ |\t]{2,}", " ", x$text)      ## remove tabs ----
  # x$text <- stri_trim(x$text)                    ## remove extra white space ----
  
  # was getting messy results from removeWords - using tidytext instead
  # this also addresses white spaces
  
  return(x)
}

cleaned <- clean(data)
write.csv(cleaned, "", row.names=F)

############### NEED TO ADDRESS WORK-RELATED ACRONYMS (LIKE HR)