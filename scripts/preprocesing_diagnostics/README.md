# Preprocessing Diagnostics

This document reflects a fluid working process of preprocessing diagnostics. There were a number of idiosyncratic artifacts, overlooked linguistic and lexical properties, and other characteristics left following preliminary preprocessing. The purpose of this process is to identify and address features of the Twitter data that were compromising the fidelity of (structural) topic modeling and sentiment analysis analyses. The .csv files reflect the output of the following steps for the month of March, 2021. The original preprocessing script is stored as a function.

The tweet data is has been retrieved/downloaded using R scripts. The data is stored in [raw_data](raw_data) in .csv format organized by year and month.

## Process

The data was separated into two datasets: one including verified users only, and the other including all users. The purpose of this split is to address the issue of bots. There are a number of duplicates still present after removing retweets and quotes that were artificially inflating word frequencies, which are likely bots/spam. By filtering for verified users only, duplicates are reduced to practically zero. However, the sample size for verified users is 497 users. The full sample size before preprocessing is 11,904.

In the full dataset, when we removed duplicates, we retained one copy of each. Removing duplicates is challenging because many of them mention different users and include different URLs, making them difficult to identify and eliminate. One approach that helped was to remove URLs before dealing with duplicates, but a significant number still remained. One potential solution is to search for tweets that share at least 5 identical words in the same order, although this method is arbitrary.

> See (duplicates.csv) and (duplicates_verified.csv).

## Steps

```
library(tidyverse)
library(tidytext)
library(stringi)
library(magrittr)
library(qdapDictionaries)
library(qdap)
library(textstem)
library(SnowballC)
library(tm)
```

**[Step 1] (not written to CSV):** 
- Exclude quoted and retweets
- Exclude non-verified (for verified dataset only)
- Include english only (lang = "en")

```
x %<>% 
    mutate_all(~ifelse(is.na(.),999,.)) %>%
    filter(sourcetweet_type != "retweeted"
         & sourcetweet_type != "quoted") %>% 
    filter(lang=="en")
```

**[Step 2] `(step_2_non-ver.csv; step_2_ver.csv)`:** 
- Remove links 
- Remove duplicates (not removing for verified data set)

```
x$text <- gsub("\\bhttp\\S*\\s*","", x$text)
x <- x[!duplicated(x$text),] 
```

**[Step 3] `(step_3_non-ver.csv; step_3_ver.csv)`:**
- Convert to lowercase

```
x$text <- tolower(x$text) 
```

**[Step 4] `(step_4_non-ver.csv; step_4_ver.csv)`:** 
- Remove hashtags and put in new column

```
hashtags <- str_extract_all(x$text, "#\\w+")
x$text <- gsub("#\\w+", "", xtext)
x$extracted_hashtags <- sapply(
  hashtags, function(x) paste(x, collapse = " "))
```

**[Step 5] `(step_5_non-ver.csv; step_5_ver.csv)`:**
- Remove tagged users and put in new column

```
tagged <- str_extract_all(x$text, "@\\w+")
x$text <- gsub("@\\w+", "", x$text)
x$tagged <- sapply(
  tagged, function(x) paste(x, collapse = " "))
```

**[Step 6] `(step_6_non-ver.csv; step_6_ver.csv)`:**
- Expand contractions

```
x$text <- str_replace_all(x$text,"’","'")
x$text <- replace_contraction(
vx$text, ignore.case=F, sent.cap=F)  
```

**[Step 7] `(step_7_non-ver.csv; step_7_ver.csv)`:** 
- Remove punctuation

```
x$text <- gsub("[[:punct:]]", "", x$text)
```

**[Step 8] `(step_8_non-ver.csv; step_8_ver.csv)`:** 
- Remove numbers

```
x$text <- gsub("[[:digit:]]", "", x$text) 
```

**[Step 9] `(step_9_non-ver.csv; step_9_ver.csv)`:** 
- Lemmatize

```
x$text <- lemmatize_strings(x$text)
```

**[Step 10] `(step_10_non-ver.csv; step_10_ver.csv)`:**
- Handle negations

```
negation <- c(
    "not ","never ","no ","nobody ","nor ","neither ","cannot ",) 
negation_fixed <- c(
    "not_", "never_", "no_", "nobody_", "nor_", "neither_")  
x$text <- stringi::stri_replace_all_regex(
    str = xtext, pattern = negation, replacement = negation_fixed, 
    stringi::stri_opts_fixed(case_insensitive=F))
```

**Step 11 `(step_11_non-ver.csv; step_11_ver.csv)`:**
- Remove stop words

```
x$text <- removeWords(x$text, words = stopwords::stopwords("en"))
```
> Something goes wrong here, and a lot of words are clumped together. For example, "taco tuesday" becomes "tacotuesday". 

**Alternative attempt using tidytext `(:**

```
x %<>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
```

> I used tidytext to remove stop words, but it converted into a tidytext single-token-per-row format. I tried converting it back (the revert CSVs) but now there are fewer rows than before, and only two columns. `See (step_non_ver-revert.csv; step_ver-revert.csv)`:

```
  x %<>% 
    group_by(tweet_id) %>%
    summarize(text = str_flatten(word, " "))
```

## Transformations
**Custom stop word list**
- I created different a custom stop word list for analysis with n-grams. The reasoning behind this is that some stop words may have contextual significance, and therefore should be retained. The list being modified is the "SMART" stop word list included in the tidytext package. 



```
get_stopwords(source="smart")
```

Retained for n-gram analysis:
- able
- after
- against
- allow
- alone
- always
- among
- am
- anywhere
- appropriate
- before
- cause
- being
- believe
- better
- despite
- different
- my
- myself
- need
- needs
- our
- ours
- ourselves
- serious
- their
- theirs
- them
- themselves
- they
- together
- we
- welcome

The logic behind the n-gram retention list is to capture in-group/out-group rhetoric, and to potentially capture related valence. 

**Lexical transformations**

There are many instances of UK and American english lexicons biasing frequency results. See below for link, and notable examples.

[Lexical Conversion List] ()

American lexicon
- behavior
- defense
- honor
- humor
- labor
- vigor

British lexicon
- behaviour
- defence
- honour
- humour
- labour
- vigour

**Numerical values**

Exclude years:
- 1900:2023

**Acronyms**
Acronyms, including organization-related: *NEEDS TO BE EXPANDED*
- bc = because
- hr = human resources
- ...

**Obvious spam bots**
- "TheCryptoWow"
