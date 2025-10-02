library(lubridate)
library(dplyr)

# IMPORT AND COMBINE DATASETS
# for use in "parse_datasets.R"
read_year <- function(year, root = "data/cleaned_data"){
  dir <- file.path(root, as.character(year))
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  do.call(rbind, lapply(files, function(p) read.csv(p, stringsAsFactors = FALSE)))
}

