library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)  

setwd("~/Documents/DATA/Data 332/files given/group project data")
getwd()

# Define state names
state_names <- c(
  "AK" = "Alaska", "AL" = "Alabama", "AR" = "Arkansas", "AZ" = "Arizona",
  "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DC" = "District of Columbia",
  "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii",
  "IA" = "Iowa", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana",
  "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "MA" = "Massachusetts",
  "MD" = "Maryland", "ME" = "Maine", "MI" = "Michigan", "MN" = "Minnesota",
  "MO" = "Missouri", "MS" = "Mississippi", "MT" = "Montana", "NC" = "North Carolina",
  "ND" = "North Dakota", "NE" = "Nebraska", "NH" = "New Hampshire", "NJ" = "New Jersey",
  "NM" = "New Mexico", "NV" = "Nevada", "NY" = "New York", "OH" = "Ohio",
  "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island",
  "SC" = "South Carolina", "SD" = "South Dakota", "TN" = "Tennessee",
  "TX" = "Texas", "UT" = "Utah", "VA" = "Virginia", "VT" = "Vermont",
  "WA" = "Washington", "WI" = "Wisconsin", "WV" = "West Virginia", "WY" = "Wyoming"
)

deathrate <- read_csv('uscs_map_death_all.csv')
airquality <- read_csv('AQI By State 1980-2022.csv')

# Function to replace spaces with periods in column names
replace_spaces <- function(name) {
  gsub(" ", ".", name)
}

# Update column names by replacing spaces with periods
colnames(deathrate) <- replace_spaces(colnames(deathrate))
colnames(airquality) <- replace_spaces(colnames(airquality))

# Assuming 'state_names' is correctly defined elsewhere
# Use the modified column names for operations like mutate
deathrate <- deathrate %>%
  mutate(State = state_names[State])

# Combine the data, ensuring the key for joining has correct column name if modified
combined_data <- left_join(airquality, deathrate, by = "State")
# Aggregate data
state_data <- combined_data %>%
  group_by(State) %>%
  summarise(
    Average_Median_AQI = mean(`Median.AQI`, na.rm = TRUE),
    Average_Death_Rate = mean(Rate, na.rm = TRUE)
  ) %>%
  ungroup()

# Extract top 5 states based on Median AQI
top5_aqi <- state_data %>%
  arrange(desc(Average_Median_AQI)) %>%
  slice_head(n = 5)

# Extract top 5 states based on Death Rate
top5_death_rates <- state_data %>%
  arrange(desc(Average_Death_Rate)) %>%
  slice_head(n = 5)

# Save aggregated data
saveRDS(state_data, "state_data.rds")
saveRDS(top5_aqi, "top5_aqi.rds")
saveRDS(top5_death_rates, "top5_death_rates.rds")


