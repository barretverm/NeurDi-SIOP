####### STRUCTURAL TOPIC MODELING

library(tidytext)
library(magrittr)
library(dplyr)
library(tm)
library(stm)

# unpackage data from cleaned csv file


setwd('')
df.stm <- read.csv('', 
                        header = T, 
                        stringsAsFactors = F)

#> create integer vector for time - the three months are in chronological 
#> order and rename

df.stm %<>%
  mutate(time = row_number()) %>% 
  rename("doc_id" = "X")



########### NEED TO INCORPORATE INTO MAIN PREPROCESSING FUNCTION
# Use gsub() to replace "" with ""
df.stm$text <- gsub("", "", df.stm$text)


# re-order 
df.stm_ro <- df.stm[,c("doc_id", "text", "month")]

# stm requires metadata in df or matrix format
metadata <- df.stm_ro[,c("month", "text")]


# Prep corpus
out <- textProcessor(
  df.stm_ro$text,
  metadata = metadata,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  ucp = FALSE,
  stem = TRUE,
  wordLengths = c(3, Inf),
  sparselevel = 1,
  lower.thresh = 5,
  upper.thresh = Inf,
  language = "en",
  verbose = TRUE,
  onlycharacter = FALSE,
  striphtml = FALSE,
  customstopwords = c("im","just","ive","heres"),
  custompunctuation = NULL,
  v1 = FALSE
)


out_d <- prepDocuments(
  documents = out$documents,
  vocab = out$vocab,
  meta = metadata,
  lower.thresh = 5,
  upper.thresh = Inf,
  verbose = TRUE
  )


# search number of topics
searchK(
  documents,
  vocab,
  K = ,
  init.type = "Spectral",
  N = floor(0.1 * length(documents)),
  proportion = 0.5,
  heldout.seed = NULL,
  M = 10,
  cores = 1,
  ...
)



search <- searchK(  
  documents = out_d$documents, 
  vocab = out_d$vocab, 
  K = c(5:15), 
  data=metadata,
  set.seed(2023),
  verbose = T
  )
        
plot(search)

# fit models and effect estimates
stm1 <- stm(documents = out$documents,
            vocab = out$vocab,
            data = out$meta,
            K = 8,
            emtol=0.00005
)
  
# Analyze appropriate number of topics (K)
# set.seed(2023)
# K<-c(5,10,15,20)
# stm_search <- searchK(documents = out$documents, vocab = out$vocab, K = 5:20,
#                       init.type = "Spectral", data = out$meta, verbose=TRUE)

# Run STM
# stm_5 <- stm (documents = out$documents, vocab = out$vocab,  K = 5,data = out$meta, prevalence=~is_after, emtol=0.0001)
# stm_10 <- stm (documents = out$documents, vocab = out$vocab,  K = 10, data = out$meta, prevalence=~is_after, emtol=0.0001)
stm_15 <- stm (documents = out$documents, 
               vocab = out$vocab,  
               K = 15, 
               data = out$meta, 
               prevalence=~is_after, 
               emtol=0.00005)
# stm_20 <- stm (documents = out$documents, vocab = out$vocab,  K = 20, data = out$meta, prevalence=~is_after, emtol=0.0001)

# Display topics 1-30's 10 highest ranked words
# labelTopics(stm, topics = 1:30, n = 10)


varEffects <- estimateEffect(
  1:3 ~ month,
  stm1,
  out$meta
)

setwd("~/Research/text_mining/quiet_quitting/data")
save.image('stm_anaylsis.RData')

