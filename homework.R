#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ----------
# 1. Check the type of Year with glimpse
# 2. Convert Year to numeric, reassign to ds
# 3. Verify conversion with typeof
glimpse(ds)
ds <- ds %>%
  mutate(Year = as.numeric(Year))
typeof(ds$Year)

### Question 2 ----------
# Make all variable names lowercase
ds <- ds %>%
  rename_with(tolower)

### Question 3 ----------
# Create new variable 'decade'
# e.g., 1971 -> 1970, 2001 -> 2000
ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)

### Question 4 ----------
# Sort dataset by rank so that 1 is at top
ds <- ds %>%
  arrange(rank)

### Question 5 ----------
# Create tibble 'top10' with just artist, song for top 10
top10 <- ds %>%
  filter(rank <= 10) %>%
  select(artist, song)

### Question 6 ----------
# Summarize earliest, most_recent, average release year of all songs
ds_sum <- ds %>%
  summarize(
    earliest    = min(year, na.rm = TRUE),
    most_recent = max(year, na.rm = TRUE),
    average     = mean(year, na.rm = TRUE)
  )
ds_sum

### Question 7 ----------
# Filter for earliest, most recent, "average-ist" years (rounded)
# One filter command only, then sort by year
ds %>%
  filter(
    year == ds_sum$earliest |
    year == ds_sum$most_recent |
    # average might not be an integer, so rounding is typical
    year == round(ds_sum$average)
  ) %>%
  arrange(year)

### Question 8 ----------
# Fix the error for the song "Brass in Pocket" (should be 1979)
# Then recalculate decade, ds_sum, and re-run the filter
ds <- ds %>%
  mutate(
    year = ifelse(song == "Brass in Pocket", 1979, year)
  ) %>%
  mutate(decade = floor(year / 10) * 10)

# Recompute ds_sum
ds_sum <- ds %>%
  summarize(
    earliest    = min(year, na.rm = TRUE),
    most_recent = max(year, na.rm = TRUE),
    average     = mean(year, na.rm = TRUE)
  )
ds_sum

# Filter again for the updated earliest/most_recent/average
ds %>%
  filter(
    year == ds_sum$earliest |
    year == ds_sum$most_recent |
    year == round(ds_sum$average)
  ) %>%
  arrange(year)

### Question 9 ----------
# Group by decade, ignoring NA values for decade,
# then find average rank & number of songs
ds %>%
  filter(!is.na(decade)) %>%
  group_by(decade) %>%
  summarize(
    avg_rank = mean(rank, na.rm = TRUE),
    n_songs  = n()
  )

### Question 10 ----------
# Use dplyr::count to count songs by decade,
# then slice_max() to find the decade with the most songs
ds %>%
  count(decade) %>%
  slice_max(n, n = 1)

  
