
# Lightweight testing object
# test <- data[1:500,]
#---------------------------------------------

# Spell check ----
check <- unnest_tokens(data, word, text)
words <- unique(check$word)
head(words)
length(words)

## Find bad words ----
bad.words <- hunspell(words)
bad.words <- unique(unlist(bad.words))
length(bad.words)
bad.words[1:10]

## Find suggestions ----
# This takes a minute
suggs <- hunspell_suggest(bad.words)
head(suggs)

## Select first suggestions ----
suggs <- unlist(lapply(suggs, function(x) x[1]))

## Keep occurences > 6 ----
# Manually check the words with at least 6 occurrences
# Count the frequency of each word, then keep the words in the bad.words list
# using inner_join
word.list <- as.data.frame(cbind(bad.words, suggs))
freq <- count(check, word)
freq <- inner_join(freq, word.list, by = c(word="bad.words"))
freq

## Export CSV ----
# Save to csv - open in Excel and check the words manually to determine which 
# words need correction, and which suggestions were incorrect. Provide 
# alternatives for 
write.csv(freq, "word.list.freq.csv", row.names=F)

data_ver <- data %>% 
  filter(user_verified=="TRUE")

data_non_ver <- data %>% 
  filter(user_verified=="FALSE")
