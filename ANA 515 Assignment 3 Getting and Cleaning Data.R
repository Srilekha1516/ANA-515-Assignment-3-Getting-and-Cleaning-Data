getwd()

# Loading and calling tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Step 1: Reading the data from the csv file
Storm_Events <- read_csv("E:/Data Analytics - Spring 2023/ANA 515 - Fundamentals of Data Storage/StormEvents_details-ftp_v1.0_d2010_c20220425.csv.csv")

# Step 2: Limit the dataframe to the specified columns
Storm_Events <- subset(Storm_Events, select = c(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE))
head(Storm_Events,6)

# Step 3: Arrange the data by state name (STATE)
library(dplyr)
Storm_Events <- Storm_Events %>%
arrange(STATE)
head(Storm_Events,6)

# Step 4: Change state and county names to title case
library(stringr)
Storm_Events$STATE <- str_to_title(Storm_Events$STATE)
Storm_Events$CZ_NAME <- str_to_title(Storm_Events$CZ_NAME)
head(Storm_Events,6)

# Step 5: Limit to the events listed by county FIPS (CZ_TYPE of "C") and remove CZ_TYPE column
Storm_Events <- Storm_Events %>%
filter(CZ_TYPE == "C") %>%
select(-CZ_TYPE)
head(Storm_Events,6)

# Step 6: Pad the state and county FIPS with a "0" at the beginning and unite the two columns
library(stringr)
Storm_Events$STATE_FIPS <- str_pad(Storm_Events$STATE_FIPS, width = 2, pad = "0")
Storm_Events$CZ_FIPS <- str_pad(Storm_Events$CZ_FIPS, width = 3, pad = "0")
Storm_Events <- unite(Storm_Events, FIPS, STATE_FIPS, CZ_FIPS, sep = "-")

# Step 7: Change all column names to lower case
Storm_Events <- Storm_Events %>%
rename_all(tolower)

# Step 8: Load the state information dataframe using base R
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
head(us_state_info)

# Step 9: Create a dataframe with the number of events per state in the year of my birth
# Year of my birth
birth_year <- "1996"
birth_year_events <- Storm_Events %>%
filter(substr(begin_yearmonth, 1, 4) == birth_year)
# Calculate the frequencies
event_counts <- data.frame(table(birth_year_events$state))
# Create a new dataframe
Newset <- data.frame(state = names(state_frequencies), events_count = as.vector(state_frequencies))
head(Newset)
# Merge in the state information dataframe
merged <- merge(x = Newset, y = us_state_info, by.x = "state", by.y = "state")
head(merged)

# Step 10: Createing the plot
install.packages("ggplot2")
library(ggplot2)
plot <- ggplot(merged, aes(x = area, y = events_count)) +
  geom_point(aes(color=region)) +
  labs(x = "Land Area (square miles)", y = "Number of Storm Events") +
  ggtitle("Number of Storm Events in 2010 vs. Land Area") +
  theme_minimal()
print(plot)


# Step 11: Save the plot as an image file (e.g., PNG)
