
################# TWITTER MINING FOR NEURODIVERSITY ##################

library(academictwitteR)
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(ggthemr)

# SET TWITTER BEARER IN .Renviron FILE AND RESTART .rs.restartR()
set_bearer()
# AFTER RESTART, GET BEARER
get_bearer()


# PRELIMINARY COUNTS----
# NOTE: N IS THE NUMBER OF UNITS IN GRANULARITY. FOR EXAMPLE, IF "MONTHS",
# THEN JANUARY THROUGH FEBRUARY IS N = 2. IF 31 DAYS, THEN GRANULARITY = 'DAY',
# AND N = 31
count <- count_all_tweets(
  query= 'neurodiversity', '#neurodiversity',
  start_tweets = '2023-03-01T00:00:00Z',
  end_tweets = '2023-03-31T00:00:00Z',
  bearer_token = get_bearer(),
  n = 31,
  granularity= 'day',
  verbose= T,
  lang= 'en',
  remove_promoted= T
)
count
sum(count$tweet_count)

# monthly counts into data frames for full years
mar2023 <- count
df_2023 <- bind_rows(
  )

head(df_2023)
tail(df_2023)

# export counts for each year to csv
setwd('~/UCF/Research/Neurodiversity/NeurDi')
write.csv(df_2023, '2023-count.csv')

# combine all years into master count and export to csv
df <- bind_rows(df_2020, df_2021, df_2022, df_2023)
head(df)
tail(df)
write.csv(df, '2020-2023.csv')

# COUNTS PER MONTH----
# May 2020 - present
## 2020----
# May 2020 (1-31) - 7465
# Jun 2020 (1-30) - 10512
# Jul 2020 (1-31) - 11524
# Aug 2020 (1-31) - 12054
# Sep 2020 (1-30) - 13562
# Oct 2020 (1-31) - 17773
# Nov 2020 (1-30) - 10939
# Dec 2020 (1-31) - 11038
## 2021----
# Jan 2021 (1-31) - 26346
# Feb 2021 (1-28) - 14136
# Mar 2021 (1-31) - 24893
# Apr 2021 (1-30) - 23209
# May 2021 (1-31) - 16054
# Jun 2021 (1-30) - 13985
# Jul 2021 (1-31) - 16077
# Aug 2021 (1-31) - 17720
# Sep 2021 (1-30) - 18293
# Oct 2021 (1-31) - 17523
# Nov 2021 (1-30) - 19486
# Dec 2021 (1-31) - 14819
## 2022----
# Jan 2022 (1-31) - 17902
# Feb 2022 (1-28) - 15016
# Mar 2022 (1-31) - 33335
# Apr 2022 (1-30) - 23786
# May 2022 (1-31) - 18963
# Jun 2022 (1-30) - 18010
# Jul 2022 (1-31) - 23179
# Aug 2022 (1-31) - 18518
# Sep 2022 (1-30) - 16338
# Oct 2022 (1-31) - 23342
# Nov 2022 (1-30) - 24323
# Dec 2022 (1-31) - 17209
##2023----
# Jan 2023 (1-31) - 19207
# Feb 2023 (1-28) - 17275
# Mar 2023 (1-31) - 43502

# TWEET RETRIEVAL----
get_all_tweets(
  query= 'neurodiversity', '#neurodiversity',
  start_tweets = '2023-02-01T00:00:00Z',
  end_tweets = '2023-02-28T00:00:00Z',
  bearer_token = get_bearer(),
  n = 50000,
  data_path = '~/UCF/Research/Neurodiversity/NeurDi/2023-Feb',
  export_query = T,
  bind_tweets = F,
  page_n = 500,
  context_annotations = T,
  verbose = T,
)

# BIND TWEETS----
# specify folder containing JSON files
setwd('~/UCF/Research/Neurodiversity/NeurDi/CSV/2023')

bind <- bind_tweets(
  '~/UCF/Research/Neurodiversity/NeurDi/2023-Feb', 
  verbose = T, 
  output_format = 'tidy')

# verify range in time
glimpse(bind)
tail(bind$created_at)

# export csv
write.csv(bind, '2023-Feb.csv')


# prepare data for plot ----
# vector for 1 - 9, reflecting number of months - this will be used
# to assign month labels to on the plot
num <- 1:1030
df <- data.frame(df, num)
tail(df)

# frequency line plot ----
ggthemr('fresh', text_size=18)
ggplot(
  df,(aes(num, tweet_count, color='fresh')))+
  geom_point(size=.1)+ 
  geom_line()+
  #  geom_smooth(se=F)+
  labs(x='', y='',
       title='Number of Tweets',
       scale_y_continuous(limits=c(0,35000)))+
  # scale_x_discrete(limits=m)+
  theme(legend.title = element_blank(),
        legend.position = 'none')
