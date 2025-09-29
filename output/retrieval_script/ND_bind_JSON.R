### BINDING TWITTER JSON FILES

library(academictwitteR)
library(tidyverse)
library(tidyselect)


### SPECIFY FOLDER DIRECTORY. THIS FUNCTION WILL BIND THE JSON FILES
### CONTAINED WITHIN THE FOLDER.
AUG.NOV.2020 <- bind_tweets(
  '~/data/Twitter_API/neurodiversity_data/2020-Aug-31_2020-Nov-30', 
  verbose = T, 
  output_format = 'tidy')

### VERIFY DATA. LOOK USE created_at COLUMN TO MAKE SURE YOU HAVE THE 
### CORRECT RANGE IN TIME
glimpse(AUG.NOV.2020)
tail(AUG.NOV.2020$created_at)

### EXPORT CSV
setwd()
write.csv(AUG.NOV.2020, '2020-Aug-31_2020-Nov-30.csv')