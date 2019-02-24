# Dependencies
library(tidyverse)

# Find all files
files <- list.files(path = "data/table-13_hate-crimes", pattern = "*.csv")

# Import individual data frames
for (i in seq_along(files)) {
  year <- str_extract(files[i], "([0-9]){4}")
  df <- read_csv(paste0("data/table-13_hate-crimes/", files[i]))
  assign(paste0("hate_", year), df)
}

# CLEAN -------------------------------------------------------------------

# List of all data frames
df_list <- list(hate_2006, hate_2007, hate_2008, hate_2009, hate_2010, hate_2011,
                hate_2012, hate_2013, hate_2014, hate_2015, hate_2016, hate_2017)

# Remove missing values and useless column
df_list2 <- lapply(df_list, function (df) {
  df %>%
    select(-place) %>%
    filter(total == "Total")
})

# Extract data frames from list
for (i in seq_along(df_list)) {
  year <- str_extract(files[i], "([0-9]){4}")
  assign(paste0("hate_", year), df_list2[[i]])
}
