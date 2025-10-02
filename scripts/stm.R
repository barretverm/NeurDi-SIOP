########### STM ###########

library(stm)
library(dplyr)
library(lubridate)

# ALL YEARS (no retweets, deduped)
all_no_rt_dedup <- bind_rows(
  quotes_all_dedup,
  replies_all_dedup,
  original_all_dedup
)

# BY YEAR (no retweets, deduped)
no_rt_2020_dedup <- bind_rows(quotes_2020_dedup, replies_2020_dedup, original_2020_dedup)
no_rt_2021_dedup <- bind_rows(quotes_2021_dedup, replies_2021_dedup, original_2021_dedup)
no_rt_2022_dedup <- bind_rows(quotes_2022_dedup, replies_2022_dedup, original_2022_dedup)
no_rt_2023_dedup <- bind_rows(quotes_2023_dedup, replies_2023_dedup, original_2023_dedup)




# ---- 2020 ----
seed(123)
stm_processed_2020_dedup <- make_stm_inputs(no_rt_2020_dedup)
# search for number of topics
kgrid <- c(18, 19, 20, 21, 22, 23, 24)
ksearch_2020 <- searchK(
  documents = docs, vocab = vocab, data = meta,
  K = kgrid,
  prevalence = ~ tweet_category + s(date_num),
  init.type = "Spectral",
  verbose = TRUE
)

plot(ksearch_2020)
plot_ksearch(ksearch_2020)

# stm
set.seed(123)
K <- 20  
mod_2020 <- stm(
  documents  = docs, vocab = vocab, data = meta,
  K          = K,
  prevalence = ~ tweet_category + s(date_num),
  init.type  = "Spectral",
  max.em.its = 75,
  verbose    = TRUE
)

labelTopics(mod_2020, n = 10)       # top words per topic
plot(mod_2020, type = "summary")    # topic prevalence barplot


# ---- 2021 ----
stm_processed_2021_dedup <- make_stm_inputs(no_rt_2021_dedup)
# search for number of topics
kgrid <- c(15:25)
ksearch_2021 <- searchK(
  documents = docs, vocab = vocab, data = meta,
  K = kgrid,
  prevalence = ~ tweet_category + s(date_num),
  init.type = "Spectral",
  verbose = TRUE
)

plot(ksearch_2021)
plot_ksearch(ksearch_2021)

# stm
set.seed(123)
K <- 22  
mod_2021_k22 <- stm(
  documents  = docs, vocab = vocab, data = meta,
  K          = K,
  prevalence = ~ tweet_category + s(date_num),
  init.type  = "Spectral",
  max.em.its = 75,
  verbose    = TRUE
)

labelTopics(mod_2021_k22, n = 10)       # top words per topic
plot(mod_2021_k22, type = "summary")    # topic prevalence barplot



# ---- 2022 ----
stm_processed_2022_dedup <- make_stm_inputs(no_rt_2022_dedup)
# search for number of topics
kgrid <- c(15:25)
ksearch_2022 <- searchK(
  documents = docs, vocab = vocab, data = meta,
  K = kgrid,
  prevalence = ~ tweet_category + s(date_num),
  init.type = "Spectral",
  verbose = TRUE
)

plot(ksearch_2022)
plot_ksearch(ksearch2022)

# stm
set.seed(123)
K <- 22
mod_2022_k22 <- stm(
  documents  = docs, vocab = vocab, data = meta,
  K          = K,
  prevalence = ~ tweet_category + s(date_num),
  init.type  = "Spectral",
  max.em.its = 75,
  verbose    = TRUE
)

labelTopics(mod_2022_k22, n = 10)       # top words per topic
plot(mod_2022_k22, type = "summary")    # topic prevalence barplot



# ---- 2023 ----
stm_processed_2023_dedup <- make_stm_inputs(no_rt_2023_dedup)
# search for number of topics
kgrid <- c(19:23)
ksearch_2023 <- searchK(
  documents = docs, vocab = vocab, data = meta,
  K = kgrid,
  prevalence = ~ tweet_category + s(date_num),
  init.type = "Spectral",
  verbose = TRUE
)

plot(ksearch_2023)
plot_ksearch(ksearch_2023)

# stm
set.seed(123)
K <- 22
mod_2023_k22 <- stm(
  documents  = docs, vocab = vocab, data = meta,
  K          = K,
  prevalence = ~ tweet_category + s(date_num),
  init.type  = "Spectral",
  max.em.its = 75,
  verbose    = TRUE
)

labelTopics(mod_2023_k22, n = 10)       # top words per topic
plot(mod_2023_k22, type = "summary")    # topic prevalence barplot


# ---- all ----
stm_processed_all_dedup <- make_stm_inputs(all_no_rt_dedup)
# search for number of topics
kgrid <- c(18:24)
ksearch_all <- searchK(
  documents = docs, vocab = vocab, data = meta,
  K = kgrid,
  prevalence = ~ tweet_category + s(date_num),
  init.type = "Spectral",
  verbose = TRUE
)

plot(ksearch_all)
plot_ksearch(ksearch_all)

# stm
set.seed(123)
K <- 22  
mod_all_k22 <- stm(
  documents  = docs, vocab = vocab, data = meta,
  K          = K,
  prevalence = ~ tweet_category + s(date_num),
  init.type  = "Spectral",
  max.em.its = 75,
  verbose    = TRUE
)

labelTopics(mod_all_k22, n = 10)       # top words per topic
plot(mod_all_k22, type = "summary")    # topic prevalence barplot
estimateEffect(mod_all_k22)


