library(tm)
library(stm)

classes = c('numeric', 'numeric', 'character', 'integer')
df <- read.csv( 'hashtags.csv',
    header = TRUE,
    colClasses = classes)

# Discard useless ID
df$X <- NULL

meta <- df[,c('tweet_id', 'days_after')]

out <- textProcessor(
  df$hashtags,
  metadata = meta,
  verbose = TRUE,
  lowercase = FALSE,
  removepunctuation = FALSE,
  removestopwords = FALSE,
  removenumbers = FALSE,
  stem = FALSE
)

stm1 <- stm(
    documents = out$documents,
    vocab = out$vocab,
    data = out$meta,
    prevalence = ~days_after,
    K = 8,
    emtol=0.0001
)

varEffects <- estimateEffect(1:8 ~ days_after, stm1, out$meta)

save.image('hashtag_stm.Rdata')