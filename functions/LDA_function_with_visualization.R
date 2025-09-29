library(topicmodels)
library(tm)
library(ggplot2)


# dtm = your document term matrix, topic_num = number of topics
# n = top terms representative of each topic

topic_model <- function(dtm, topic_num, n){
  
  # set a seed so that the output of the model is predictable
  dtm <- LDA(
    dtm, 
    k = topic_num, 
    control = list(seed = 1234))
  
  ## get word topic probabilities 
  dtm_topics <- tidy(
    dtm, matrix = "beta")
  
  ## isolate representative terms 
  top_terms <- dtm_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = n) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ## model visualization
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend=F) +
    facet_wrap(~ topic, scales = "free") +
    labs(title = sprintf("%d-topic LDA model", topic_num)) +
    scale_y_reordered()
  
}
