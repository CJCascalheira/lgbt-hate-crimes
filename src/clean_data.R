# Dependencies
library(tidyverse)

# Import data
files <- list.files(path = "data/table-13_hate-crimes", pattern = "*.csv")

for (i in seq_along(files)) {
  year <- str_extract(files[i], "([0-9]){4}")
  df <- read_csv(paste0("data/table-13_hate-crimes/", files[i]))
  assign(paste0("hate_", year), df)
}

# CLEAN -------------------------------------------------------------------
