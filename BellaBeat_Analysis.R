library(lubridate)
library(readr)
library(skimr)
library(purrr)
library(janitor)
library(tidyr)
library(dplyr)

#loaded Cleaned data 
daily_activity <- read_csv("data/processed/cleaned_daily_activity.csv")
sleep_data <- read_csv("data/processed/cleaned_sleep_data.csv")
weight_data <- read_csv("data/processed/cleaned_weight_data.csv")
activity_sleep_joined <- read_csv("data/processed/cleaned_activity_sleep_joined.csv")

#summary statistics

# Average daily steps and calories
daily_activity %>%
  summarize(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_sedentary_mins = mean(sedentary_minutes, na.rm = TRUE)
  )

daily_activity %>%
  count(activity_level) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

daily_activity %>%
  group_by(day_of_week) %>%
  summarize(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    days_tracked = n()
  ) %>%
  arrange(desc(avg_steps))

sleep_data %>%
  summarize(
    avg_minutes_asleep = mean(total_minutes_asleep, na.rm = TRUE),
    avg_time_in_bed = mean(total_time_in_bed, na.rm = TRUE),
    avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE)
  )

weight_data %>%
  count(is_manual_report) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

## Sleep Vs Steps Correlation 

cor(activity_sleep_joined$total_steps, activity_sleep_joined$total_minutes_asleep, use = "complete.obs")

library(ggplot2)
# Custom colors
activity_colors <- c(
  "Highly Active" = "#6A5ACD",     # Soft purple
  "Moderately Active" = "#4682B4", # Steel blue
  "Low Activity" = "#90EE90"       # Light green
)

intensity_colors <- c(
  "VeryActive" = "#6A5ACD",
  "FairlyActive" = "#FF69B4",
  "LightlyActive" = "#4682B4",
  "Sedentary" = "#90EE90"
)

# Theme for accessibility & soft visuals
theme_bellabeat <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(color = "#333333"),
    plot.background = element_rect(fill = "#F5F5F5", color = NA),
    panel.grid.major = element_line(color = "#DDDDDD"),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.title = element_text(face = "bold")
  )

dir.create("plots", showWarnings = FALSE)

ggplot(activity_sleep_joined, aes(x = total_steps, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "#4682B4") +
  geom_smooth(method = "lm", se = FALSE, color = "#6A5ACD") +
  labs(
    title = "Relationship Between Daily Steps and Sleep Duration",
    x = "Total Steps",
    y = "Total Minutes Asleep"
  ) +
  theme_bellabeat

## Does trend change by activity level?
steps_sleep_plot <- ggplot(activity_sleep_joined, aes(x = total_steps, y = total_minutes_asleep, color = activity_level)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = activity_colors) +
  labs(
    title = "Steps vs. Sleep by Activity Level",
    x = "Total Steps",
    y = "Total Minutes Asleep",
    color = "Activity Level"
  ) +
  theme_bellabeat

ggsave("plots/steps_vs_sleep_by_activity_level.png", plot = steps_sleep_plot, width = 8, height = 6, dpi = 300)

##---- Notes from scatter plots: Correlation result = -0.19; indicates very weak relationship


# Intensity Analysis

#Average Time Spent in Each Zone
activity_sleep_joined %>%
  summarize(
    avg_very_active = mean(very_active_minutes, na.rm = TRUE),
    avg_fairly_active = mean(fairly_active_minutes, na.rm = TRUE),
    avg_lightly_active = mean(lightly_active_minutes, na.rm = TRUE),
    avg_sedentary = mean(sedentary_minutes, na.rm = TRUE)
  )

#breakdown by level
activity_sleep_joined %>%
  group_by(activity_level) %>%
  summarize(
    avg_very_active = mean(very_active_minutes, na.rm = TRUE),
    avg_fairly_active = mean(fairly_active_minutes, na.rm = TRUE),
    avg_lightly_active = mean(lightly_active_minutes, na.rm = TRUE),
    avg_sedentary = mean(sedentary_minutes, na.rm = TRUE)
  )


#activity level distribution plot

activity_distribution <- daily_activity %>%
  count(activity_level) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

ggplot(activity_distribution, aes(x = activity_level, y = percent, fill = activity_level)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = activity_colors) +
  labs(
    title = "Distribution of Users by Activity Level",
    x = "Activity Level",
    y = "Percent of Users"
  ) +
  theme_bellabeat

# Save it
ggsave("plots/activity_level_distribution.png", width = 8, height = 6, dpi = 300)

#Visualize Stacked Chart
#--- shape to long format for stacking ---
intensity_long <- activity_sleep_joined %>%
  group_by(activity_level) %>%
  summarize(
    VeryActive = mean(very_active_minutes, na.rm = TRUE),
    FairlyActive = mean(fairly_active_minutes, na.rm = TRUE),
    LightlyActive = mean(lightly_active_minutes, na.rm = TRUE),
    Sedentary = mean(sedentary_minutes, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(VeryActive, FairlyActive, LightlyActive, Sedentary),
               names_to = "IntensityType", values_to = "AverageMinutes")

# Plot
intensity_bar_plot <- ggplot(intensity_long, aes(x = activity_level, y = AverageMinutes, fill = IntensityType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = intensity_colors) +
  labs(
    title = "Average Daily Intensity by Activity Level",
    x = "Activity Level", y = "Minutes per Day", fill = "Intensity Zone"
  ) +
  theme_bellabeat

# Save
ggsave("plots/intensity_by_activity_level_bar.png", plot = intensity_bar_plot, width = 8, height = 6, dpi = 300)


#Feature Engagement: Summary of weight logging engagement

weight_summary <- weight_data %>%
  count(is_manual_report) %>%
  mutate(percent = round(n / sum(n) * 100, 1))


weight_pie_plot <- ggplot(weight_summary, aes(x = "", y = percent, fill = is_manual_report)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Weight Logging Method",
    fill = "Manual Entry"
  ) +
  scale_fill_manual(values = c("#90EE90", "#6A5ACD")) +
  theme_void()

# Save it
ggsave("plots/weight_logging_pie_chart.png", plot = weight_pie_plot, width = 6, height = 6, dpi = 300)

#frequency
weight_log_hist <- weight_data %>%
  group_by(id) %>%
  summarize(entries = n()) %>%
  ggplot(aes(x = entries)) +
  geom_histogram(binwidth = 5, fill = "#6A5ACD", color = "white") +
  labs(title = "Distribution of Weight Log Frequency by User", x = "Entries", y = "User Count") +
  theme_bellabeat


ggsave("plots/exploratory/weight_log_histogram.png", plot = weight_log_hist, width = 8, height = 6, dpi = 300)

