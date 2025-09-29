########### SPLITTING BY VERIFIED/NON-VERIFIED

ver <- function(x){
  user_ver <- x %>% 
    filter(user_verified=="TRUE") %>% 
    unnest_tokens(word,text) %>% 
    anti_join(stop_words) %>% 
    count(word,sort=T) %>% 
    arrange(desc(n))
  
  return(user_ver)
  
}

non_ver <- function(x){
  user_ver <- x %>% 
    filter(user_verified=="FALSE") %>% 
    unnest_tokens(word,text) %>% 
    anti_join(stop_words) %>% 
    count(word,sort=T) %>% 
    arrange(desc(n))
  
  return(user_ver)
  
}


ver <- ver(data)
non_ver <- non_ver(data)

ver_h <- head(ver,40)
non_ver_h <- head(non_ver,40)

df <- data.frame(ver_h, non_ver_h)
write.csv()

# Nest tokens
token_ver <- x %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
token_ver$text

# Count words
non_ver<-token_ver %>%
  count(word, sort=T) %>%
  arrange(desc(n))

# Visualize most common words
token_ver %>%
  count(word, sort=T) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y=NULL)

head(ver,30)
head(non_ver,30)

write.csv(ver, file="ver.csv")
write.csv(non_ver, file="non_ver.csv")
