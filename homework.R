# PSYC 259 Homework 2 - Data Transformation
# List names of students collaborating with: [Rasam Dorri]

### SETUP: RUN THIS BEFORE STARTING ----------

# Load required packages
library(tidyverse)

# Read in the data
ds <- read_csv("data_raw/rolling_stone_500.csv")


### Question 1 ---------- 
# Use glimpse to check the type of "Year". 
# Then, convert it to a numeric, saving it back to 'ds'
# Use typeof to check that your conversion succeeded

# Check the structure of ds to see current types
glimpse(ds)

# Convert 'Year' to numeric 
ds <- ds %>% mutate(Year = as.numeric(Year))

# Verify conversion by checking the type
typeof(ds$Year)  # Expected output: "double"


### Question 2 ---------- 
# Using a dplyr function, change ds so that all of the variable names are lowercase

ds <- ds %>% rename_with(tolower)


### Question 3 ---------- 
# Use mutate to create a new variable in ds that has the decade of the year as a number.
# For example, 1971 becomes 1970, 2001 becomes 2000.
# Hint: read the documentation for ?floor

ds <- ds %>% mutate(decade = floor(Year / 10) * 10)


### Question 4 ---------- 
# Sort the dataset by rank so that 1 is at the top

ds <- ds %>% arrange(rank)


### Question 5 ---------- 
# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

top10 <- ds %>% 
  filter(rank <= 10) %>% 
  select(artist, song)

print(top10)


### Question 6 ---------- 
# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

ds_sum <- ds %>% 
  summarize(
    earliest = min(Year, na.rm = TRUE),
    most_recent = max(Year, na.rm = TRUE),
    avg_year = round(mean(Year, na.rm = TRUE))
  )


 print(ds_sum)


### Question 7 ---------- 
# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

ds_filtered <- ds %>% 
  filter(Year %in% c(ds_sum$earliest, ds_sum$most_recent, ds_sum$avg_year)) %>% 
  arrange(Year)


 print(ds_filtered)


### Question 8 ---------- 
# There's an error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then recalculate the responses from Questions 6-7 
# to find the correct oldest, averag-ist, and most recent songs

# Correct the erroneous year for "Brass in Pocket" by The Pretenders
ds <- ds %>% 
  mutate(Year = ifelse(song == "Brass in Pocket" & artist == "The Pretenders", 1979, Year),
         decade = floor(Year / 10) * 10)

# Recalculate ds_sum with corrected data
ds_sum <- ds %>% 
  summarize(
    earliest = min(Year, na.rm = TRUE),
    most_recent = max(Year, na.rm = TRUE),
    avg_year = round(mean(Year, na.rm = TRUE))
  )

# Recalculate the filtered dataset for the specified years
ds_filtered <- ds %>% 
  filter(Year %in% c(ds_sum$earliest, ds_sum$most_recent, ds_sum$avg_year)) %>% 
  arrange(Year)

print(ds_sum) 
print(ds_filtered)


### Question 9 ---------
# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. 
# Filter out the NA values from decade before summarizing.
# Use the pipe %>% to string the commands together.

ds %>% 
  filter(!is.na(decade)) %>% 
  group_by(decade) %>% 
  summarize(
    avg_rank = mean(rank, na.rm = TRUE),
    num_songs = n()
  ) %>% 
  arrange(decade) -> decade_summary


 print(decade_summary)


### Question 10 --------
# Look up the dplyr "count" function.
# Use it to count up the number of songs by decade.
# Then use slice_max() to pull the row with the most songs.
# Use the pipe %>% to string the commands together.

ds %>% 
  count(decade) %>% 
  slice_max(n, n = 1) -> most_common_decade


print(most_common_decade)