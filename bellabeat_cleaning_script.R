library(dplyr)
library(lubridate)
library(readr)
library(skimr)
library(purrr)
library(janitor)
library(tidyr)

# List subfolders inside "data"
folders <- list.dirs("data", recursive = FALSE, full.names = TRUE)

# Function to load and clean a CSV from a folder
load_csv_file <- function(folder, filename) { 
  file_path <- file.path(folder, filename)
  if (file.exists(file_path)){
    df <- read_csv(file_path) %>%
      clean_names()  # ⬅️ lowercase + snake_case
    df$source_folder <- basename(folder)
    return(df)
  } else {
    message("File not found in: ", folder)
    return(NULL)
  }
}

# ✅ Create output folder BEFORE writing anything
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# --------------------------
# LOAD ALL DATASETS
# --------------------------

# --- DAILY ACTIVITY ---
target_file <- "dailyActivity_merged.csv"
daily_activity <- map_dfr(folders, ~load_csv_file(.x, target_file)) %>% distinct()

# --- SLEEP DATA ---
target_file <- "sleepDay_merged.csv"
sleep_data <- map_dfr(folders, ~load_csv_file(.x, target_file)) %>% distinct()

# --- WEIGHT LOG ---
target_file <- "weightLogInfo_merged.csv"
weight_data <- map_dfr(folders, ~load_csv_file(.x, target_file)) %>% distinct()

# --- HEART RATE ---
target_file <- "heartrate_seconds_merged.csv"
heartrate_data <- map_dfr(folders, ~load_csv_file(.x, target_file)) %>% distinct()

#--- INTENSITY DATA ---
target_file <- "dailyIntensities_merged.csv"
intensity_data <- map_dfr(folders, ~load_csv_file(.x, target_file)) %>%
  distinct()

# --------------------------
# CLEAN + TRANSFORM
# --------------------------

# Review structures
skim(daily_activity)
skim(sleep_data)
skim(weight_data)

names(daily_activity)
names(intensity_data) 
names(sleep_data)

# Clean Daily Activity
daily_activity <- daily_activity %>%
  mutate(
    activity_date = mdy(activity_date),
    day_of_week = weekdays(activity_date),
    activity_level = case_when(
      total_steps >= 10000 ~ "Highly Active",
      total_steps >= 5000 ~ "Moderately Active",
      TRUE ~ "Low Activity"
    )
  ) %>%
  filter(total_steps >= 0 & calories > 0)

# Clean Sleep Data
sleep_data <- sleep_data %>%
  mutate(
    sleep_day = mdy_hms(sleep_day),
    activity_date = as.Date(sleep_day),
    sleep_efficiency = round((total_minutes_asleep / total_time_in_bed) * 100, 1)
  ) %>%
  filter(!is.na(total_minutes_asleep))

# Clean Weight Data
weight_data <- weight_data %>%
  filter(!is.na(weight_kg))

# Clean Intensity Data
intensity_data <- intensity_data %>%
  rename(ActivityDate = activity_day) %>%
  mutate(ActivityDate = mdy(ActivityDate))

intensity_data <- intensity_data %>% 
  janitor::clean_names()
names(intensity_data) 

intensity_clean <- intensity_data %>%
  select(id, activity_date,
         lightly_active_minutes,
         fairly_active_minutes,
         very_active_minutes,
         sedentary_minutes,
         light_active_distance,
         moderately_active_distance,
         very_active_distance,
         sedentary_active_distance)

daily_activity_clean <- daily_activity %>%
  select(id, activity_date, total_steps, total_distance, calories, tracker_distance,
         logged_activities_distance, day_of_week, activity_level)

daily_activity <- left_join(daily_activity_clean, intensity_clean, by = c("id", "activity_date"))

activity_sleep_joined <- left_join(daily_activity, sleep_data, by = c("id", "activity_date"))

glimpse(activity_sleep_joined)


write_csv(daily_activity, "data/processed/cleaned_daily_activity.csv")
write_csv(sleep_data, "data/processed/cleaned_sleep_data.csv")
write_csv(weight_data, "data/processed/cleaned_weight_data.csv")
write_csv(activity_sleep_joined, "data/processed/cleaned_activity_sleep_joined.csv")

