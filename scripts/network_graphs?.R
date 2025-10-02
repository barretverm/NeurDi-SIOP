library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(ggraph)     # ggplot2-like plotting for graphs
library(ggplot2)

# Optional: a lookup from author_id -> username (for pretty labels)
# Build this once from any/all tweet data you have in memory
users_lookup <- bind_rows(tweets_2020, tweets_2021, tweets_2022, tweets_2023) %>%
  distinct(author_id, user_username) %>%
  mutate(author_id = as.character(author_id))

retweet_edges <- function(df, users_lu = NULL) {
  df %>%
    filter(tweet_category == "retweet", !is.na(sourcetweet_author_id)) %>%
    transmute(from_id = as.character(author_id),
              to_id   = as.character(sourcetweet_author_id)) %>%
    count(from_id, to_id, name = "weight") %>%
    { if (is.null(users_lu)) . else
      . %>% 
        left_join(users_lu, by = c("from_id" = "author_id")) %>%
        rename(from = user_username) %>%
        left_join(users_lu, by = c("to_id" = "author_id")) %>%
        rename(to = user_username) %>%
        mutate(from = coalesce(from, from_id),
               to   = coalesce(to,   to_id)) %>%
        select(from, to, weight)
    }
}

reply_edges <- function(df, users_lu = NULL) {
  df %>%
    filter(tweet_category == "reply", !is.na(in_reply_to_user_id)) %>%
    transmute(from_id = as.character(author_id),
              to_id   = as.character(in_reply_to_user_id)) %>%
    count(from_id, to_id, name = "weight") %>%
    { if (is.null(users_lu)) . else
      . %>%
        left_join(users_lu, by = c("from_id" = "author_id")) %>%
        rename(from = user_username) %>%
        left_join(users_lu, by = c("to_id" = "author_id")) %>%
        rename(to = user_username) %>%
        mutate(from = coalesce(from, from_id),
               to   = coalesce(to,   to_id)) %>%
        select(from, to, weight)
    }
}

quote_edges <- function(df, users_lu = NULL) {
  df %>%
    filter(tweet_category == "quote", !is.na(sourcetweet_author_id)) %>%
    transmute(from_id = as.character(author_id),
              to_id   = as.character(sourcetweet_author_id)) %>%
    count(from_id, to_id, name = "weight") %>%
    { if (is.null(users_lu)) . else
      . %>%
        left_join(users_lu, by = c("from_id" = "author_id")) %>%
        rename(from = user_username) %>%
        left_join(users_lu, by = c("to_id" = "author_id")) %>%
        rename(to = user_username) %>%
        mutate(from = coalesce(from, from_id),
               to   = coalesce(to,   to_id)) %>%
        select(from, to, weight)
    }
}

mention_edges <- function(df) {
  df %>%
    filter(is.na(tweet_category) | tweet_category != "retweet") %>%  # optional: drop RTs
    select(author = user_username, tagged) %>%
    mutate(tagged = str_squish(tagged)) %>%
    separate_rows(tagged, sep = "\\s+") %>%
    filter(!is.na(tagged), tagged != "") %>%
    mutate(mention = tolower(str_remove(tagged, "^@"))) %>%
    count(author, mention, name = "weight") %>%
    rename(from = author, to = mention)
}

hashtag_cooc_edges <- function(df, min_weight = 2) {
  df %>%
    filter(is.na(tweet_category) | tweet_category != "retweet") %>%  # optional
    select(tweet_id, extracted_hashtags) %>%
    mutate(extracted_hashtags = str_squish(extracted_hashtags)) %>%
    separate_rows(extracted_hashtags, sep = "\\s+") %>%
    filter(!is.na(extracted_hashtags), extracted_hashtags != "") %>%
    mutate(tag = tolower(str_remove(extracted_hashtags, "^#"))) %>%
    group_by(tweet_id) %>%
    summarise(pairs = list(t(combn(sort(unique(tag)), 2))), .groups = "drop") %>%
    unnest_wider(pairs, names_sep = "_", simplify = FALSE) %>%
    unnest(cols = c(pairs_1, pairs_2), names_repair = "minimal") %>%  # cols = tag1/tag2
    rename(tag1 = pairs_1, tag2 = pairs_2) %>%
    count(tag1, tag2, name = "weight", sort = TRUE) %>%
    filter(weight >= min_weight)
}


library(dplyr)

retweet_edges <- function(df, users_lu = NULL) {
  edges <- df %>%
    filter(tweet_category == "retweet", !is.na(sourcetweet_author_id)) %>%
    transmute(from_id = as.character(author_id),
              to_id   = as.character(sourcetweet_author_id)) %>%
    count(from_id, to_id, name = "weight")
  
  if (!is.null(users_lu)) {
    edges <- edges %>%
      left_join(users_lu, by = c("from_id" = "author_id")) %>%
      rename(from_user = user_username) %>%
      left_join(users_lu, by = c("to_id" = "author_id")) %>%
      rename(to_user   = user_username) %>%
      mutate(from = coalesce(from_user, from_id),
             to   = coalesce(to_user,   to_id)) %>%
      select(from, to, weight)
  }
  edges
}
edges_rt20   <- retweet_edges(tweets_2020, users_lookup)
class(edges_rt20)                 # now "tbl_df" / "data.frame"
edges_rt20_f <- dplyr::filter(edges_rt20, weight >= 2)

g_rt20 <- graph_from_data_frame(edges_rt20_f, directed = TRUE)

V(g_rt20)$indeg <- degree(g_rt20, mode = "in")

set.seed(42)
ggraph(g_rt20, layout = "fr") +
  geom_edge_link(aes(alpha = pmin(weight, 5)), show.legend = FALSE) +
  geom_node_point(aes(size = indeg)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  labs(title = "Retweet Network (2020) — edges ≥ 2",
       subtitle = "Directed: retweeter → original author",
       x = NULL, y = NULL) +
  theme_void()


######################################################################
# pick top 30 nodes by in-degree (adjust k)
k <- 30
top_labels <- names(sort(V(g_rt20)$indeg, decreasing = TRUE))[seq_len(k)]


set.seed(42)
ggraph(g_rt20, layout = "fr") +
  geom_edge_link(aes(alpha = pmin(weight, 5)), show.legend = FALSE) +
  geom_node_point(aes(size = indeg)) +
  geom_node_text(
    aes(label = ifelse(name %in% top_labels, name, NA_character_)),
    repel = TRUE, max.overlaps = Inf, size = 3
  ) +
  scale_size_continuous(range = c(1, 10)) +
  labs(
    title = "Retweet Network (2020) — edges ≥ 2",
    subtitle = "Directed: retweeter → original author (labels = top 30 by in-degree)",
    x = NULL, y = NULL
  ) +
  theme_void()


# keep only heavier edges
edges_rt20_f <- dplyr::filter(edges_rt20, weight >= 3)
g_rt20 <- igraph::graph_from_data_frame(edges_rt20_f, directed = TRUE)

# keep the giant component only
comp <- igraph::components(g_rt20)
g_rt20 <- igraph::induced_subgraph(g_rt20, which(comp$membership == which.max(comp$csize)))

# shrink labels further: top by betweenness instead of in-degree
V(g_rt20)$btw <- igraph::betweenness(g_rt20, directed = TRUE, normalized = TRUE)
top_labels <- names(sort(V(g_rt20)$btw, decreasing = TRUE))[1:25]

