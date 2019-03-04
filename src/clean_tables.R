# Dependencies
library(tidyverse)
library(fiftystater)

# Find all files
files <- list.files(path = "data/table-13_hate-crimes", pattern = "*.csv")

# Import individual data frames
for (i in seq_along(files)) {
  years <- str_extract(files[i], "([0-9]){4}")
  df <- read_csv(paste0("data/table-13_hate-crimes/", files[i]))
  assign(paste0("hate_", years), df)
}

# REMOVE NA & USELESS COLUMN ----------------------------------------------

# List of all data frames
df_list <- list(hate_2006, hate_2007, hate_2008, hate_2009, hate_2010, hate_2011,
                hate_2012, hate_2013, hate_2014, hate_2015, hate_2016, hate_2017)

# Remove missing values and useless column
df_list2 <- lapply(df_list, function (df) {
  df %>%
    filter(total == "Total") %>%
    select(-place, -total)
})

# Extract data frames from list
for (i in seq_along(df_list)) {
  years <- str_extract(files[i], "([0-9]){4}")
  assign(paste0("hate_", years), df_list2[[i]])
}

# COMBINE RACE & ETHNICITY ------------------------------------------------

# List all data frames with race and ethnicity as separate variables
race_list <- list(hate_2006, hate_2007, hate_2008, hate_2009, hate_2010,
                  hate_2011, hate_2012, hate_2013, hate_2014)

# Combine race and ethnicity
race_list2 <- lapply(race_list, function (df) {
  df %>%
    mutate(race = race + ethnicity) %>%
    select(-ethnicity)
})

# Extract data frames from list
years <- vector(mode = "character", length = length(race_list2))

for (i in seq_along(race_list2)) {
  years[i] <- str_extract(files[i], "([0-9]){4}")
  assign(paste0("hate_", years[i]), race_list2[[i]])
}

# FIX NAMES & FIND MISSING STATES -----------------------------------------

# Uniform state names (violation of DRY)
hate_2006$state <- str_to_title(hate_2006$state)
hate_2007$state <- str_to_title(hate_2007$state)
hate_2008$state <- str_to_title(hate_2008$state)
hate_2009$state <- str_to_title(hate_2009$state)
hate_2010$state <- str_to_title(hate_2010$state)
hate_2011$state <- str_to_title(hate_2011$state)
hate_2012$state <- str_to_title(hate_2012$state)
hate_2013$state <- str_to_title(hate_2013$state)
hate_2014$state <- str_to_title(hate_2014$state)
hate_2015$state <- str_to_title(hate_2015$state)
hate_2016$state <- str_to_title(hate_2016$state)
hate_2017$state <- str_to_title(hate_2017$state)

# Vector of all states
all_states <- str_to_title(unique(fifty_states$id))

# Fill missing state 2006
all_states[!(all_states %in% hate_2006$state)]

ms_2006 <- data.frame(state = "Mississippi",
           race = 0,
           religion = 0,
           sexual_orientation = 0,
           disability = 0)

hate_2006 <- bind_rows(hate_2006, ms_2006)

# Fill missing state 2007
all_states[!(all_states %in% hate_2007$state)]

ms_2007 <- data.frame(state = "Mississippi",
                      race = 0,
                      religion = 0,
                      sexual_orientation = 0,
                      disability = 0)

hate_2007 <- bind_rows(hate_2007, ms_2007)

# Fill missing state 2012
all_states[!(all_states %in% hate_2012$state)]

nj <- data.frame(state = "New Jersey",
                      race = 0,
                      religion = 0,
                      sexual_orientation = 0,
                      disability = 0)

hate_2012 <- bind_rows(hate_2012, nj)

# Fill missing state 2015
all_states[!(all_states %in% hate_2015$state)]

ms_2015 <- data.frame(state = "Mississippi",
                      race = 0,
                      religion = 0,
                      sexual_orientation = 0,
                      disability = 0,
                      gender = 0,
                      gender_identity = 0)

hate_2015 <- bind_rows(hate_2015, ms_2015)

# Create Hawaii observations
hi_5 <- data.frame(state = "Hawaii",
                   race = 0,
                   religion = 0,
                   sexual_orientation = 0,
                   disability = 0)

hi_7 <-data.frame(state = "Hawaii",
                  race = 0,
                  religion = 0,
                  sexual_orientation = 0,
                  disability = 0,
                  gender = 0,
                  gender_identity = 0)

# Add Hawaii to all data frames (violation of DRY)
hate_2006 <- bind_rows(hate_2006, hi_5)
hate_2007 <- bind_rows(hate_2007, hi_5)
hate_2008 <- bind_rows(hate_2008, hi_5)
hate_2009 <- bind_rows(hate_2009, hi_5)
hate_2010 <- bind_rows(hate_2010, hi_5)
hate_2011 <- bind_rows(hate_2011, hi_5)
hate_2012 <- bind_rows(hate_2012, hi_5)
hate_2013 <- bind_rows(hate_2013, hi_7)
hate_2014 <- bind_rows(hate_2014, hi_7)
hate_2015 <- bind_rows(hate_2015, hi_7)
hate_2016 <- bind_rows(hate_2016, hi_7)
hate_2017 <- bind_rows(hate_2017, hi_7)

# MERGE INTO MASTER DATA FRAME --------------------------------------------

# Add year column (major violation of DRY)
hate_2006 <- hate_2006 %>%
  mutate(year = rep(2006, 51))

hate_2007 <- hate_2007 %>%
  mutate(year = rep(2007, 51))

hate_2008 <- hate_2008 %>%
  mutate(year = rep(2008, 51))

hate_2009 <- hate_2009 %>%
  mutate(year = rep(2009, 51))

hate_2010 <- hate_2010 %>%
  mutate(year = rep(2010, 51))

hate_2011 <- hate_2011 %>%
  mutate(year = rep(2011, 51))

hate_2012 <- hate_2012 %>%
  mutate(year = rep(2012, 51))

hate_2013 <- hate_2013 %>%
  mutate(year = rep(2013, 51))

hate_2014 <- hate_2014 %>%
  mutate(year = rep(2014, 51))

hate_2015 <- hate_2015 %>%
  mutate(year = rep(2015, 51))

hate_2016 <- hate_2016 %>%
  mutate(year = rep(2016, 51))

hate_2017 <- hate_2017 %>%
  mutate(year = rep(2017, 51))

# List of all data frames
df2_list <- list(hate_2006, hate_2007, hate_2008, hate_2009, hate_2010, hate_2011,
                hate_2012, hate_2013, hate_2014, hate_2015, hate_2016, hate_2017)

# Convert from wide to long format
df2_list2 <- lapply(df2_list, function (df) {
  df %>%
    gather(key = "bias", value = "incidents", -c(state, year))
})

# Melt into single data frame
hate_crimes <- bind_rows(df2_list2)

# Check for NA
which(is.na(hate_crimes$incidents))

# Save data frame as CSV
write_csv(hate_crimes, path = "data/hate_crimes.csv")


# CLEAN TABLE 4 -----------------------------------------------------------

# Import data
crimes <- read_csv("data/table-4_crime-type/table-4_crime-type.csv")

# Long form
crimes2 <- crimes %>%
  # Only sexual orientation
  filter(bias_group %in% c("sexual_orientation", "gender_identity")) %>%
  gather(key = crime, value = n, -c(year, bias, bias_group))

# Save data frame as CSV
write_csv(crimes2, path = "data/crime_type.csv")
