# Installing packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

# Loading libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(conflicted)
library(janitor)
library(skimr)

# Dataset
202501-divvy-tripdata.csv
202502-divvy-tripdata.csv
202503-divvy-tripdata.csv
202504-divvy-tripdata.csv
202505-divvy-tripdata.csv
202506-divvy-tripdata.csv
202507-divvy-tripdata.csv
202508-divvy-tripdata.csv
202509-divvy-tripdata.csv
202510-divvy-tripdata.csv
202511-divvy-tripdata.csv
202512-divvy-tripdata.csv

# Load & Combine Data
files <- list.files(pattern = "*.csv")

cyclist_df <- files %>%
  map_df(~ read_csv(.x))

# Initial Exploration
glimpse(cyclist_df)
skim(cyclist_df)

# Data Cleaning

# Fix column names
cyclist_df <- clean_names(cyclist_df)

# Convert date-time
cyclist_df <- cyclist_df %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )

# Create ride length
cyclist_df <- cyclist_df %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )
# Removed negative and zero durations caused by system or docking errors.
cyclist_df <- cyclist_df %>%
  filter(ride_length > 0)

# Creating new, useful columns (features) from raw data to make analysis clearer and more meaningful.
cyclist_df <- cyclist_df %>%
  mutate(
    day_of_week = wday(started_at, label = TRUE),
    month = month(started_at, label = TRUE),
    hour = hour(started_at)
  )
 # Total rides by user type
cyclist_df %>%
  count(member_casual)

# Average ride length
cyclist_df %>%
  group_by(member_casual) %>%
  summarise(avg_ride = mean(ride_length))

# Weekly Usage Pattern
cyclist_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(total_rides = n())

# Number of rides by Members vs Casual riders across days of the week
library(scales)

cyclist_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(day_of_week, total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  labs(
    title = "Weekly Ride Pattern by User Type",
    x = "Day of Week",
    y = "Number of Rides"
  )

# Total Rides by User Type (Comparison)
cyclist_df %>%
  group_by(member_casual) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(member_casual, total_rides, fill = member_casual)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title = "Total Rides by User Type",
    x = "User Type",
    y = "Number of Rides"
  ) +
  theme_minimal()

#Total Rides (Weekday vs Weekend)
cyclist_df %>%
  group_by(member_casual, day_type) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(day_type, total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title = "Weekday vs Weekend Rides by User Type",
    x = "Day Type",
    y = "Number of Rides"
  ) +
  theme_minimal()

# Average Ride Length by User Type
ggplot(avg_ride_length, aes(member_casual, avg_ride_length, fill = member_casual)) +
  geom_col(width = 0.6) +
  labs(
    title = "Average Ride Length by User Type",
    x = "User Type",
    y = "Average Ride Duration (Minutes)"
  ) +
  theme_minimal()

# Peak Ride Hour by User Type
cyclist_df %>%
  group_by(member_casual, hour) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(factor(hour), total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  labs(
    title = "Hourly Ride Distribution by User Type",
    x = "Hour of Day",
    y = "Number of Rides"
  ) +
  theme_minimal()

# Monthly Rides by User Type
cyclist_df %>%
  group_by(member_casual, month) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(month, total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  labs(
    title = "Monthly Ride Distribution by User Type",
    x = "Month",
    y = "Number of Rides"
  ) +
  theme_minimal()




   




