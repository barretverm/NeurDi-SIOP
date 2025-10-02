# one simple helper function that returns docs, vocab, and meta ready for STM
make_stm_inputs <- function(df,
                            lower_thresh   = 5,
                            custom_stop    = c("neurodiversity")) {
  
  tp <- textProcessor(
    documents         = df$text,
    metadata          = df,
    lowercase         = FALSE,   # already lowercased 
    removestopwords   = TRUE,
    removenumbers     = FALSE,   # digits already removed 
    removepunctuation = FALSE,
    ucp               = TRUE,
    stem              = FALSE,
    wordLengths       = c(2, Inf),
    customstopwords   = custom_stop
  )
  
  prep <- prepDocuments(
    documents   = tp$documents,
    vocab       = tp$vocab,
    meta        = tp$meta,
    lower.thresh = lower_thresh
    # upper.thresh = 0.99  # <- add to drop near-universal terms
  )
  
  # Minimal covariates (optional but handy for prevalence formulas)
  meta <- prep$meta
  if (!inherits(meta$created_at, "POSIXct")) {
    meta$created_at <- ymd_hms(meta$created_at, tz = "UTC", quiet = TRUE)
  }
  meta$date_num <- as.numeric(meta$created_at)
  meta$tweet_category <- stats::relevel(factor(meta$tweet_category), ref = "original")
  
  list(docs = prep$documents, vocab = prep$vocab, meta = meta)
}