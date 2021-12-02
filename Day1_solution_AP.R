library(tidyverse)
library(RcppRoll)

day1_data <- readr::read_csv("Day1_data.csv", col_names = FALSE)

day1_data <- day1_data %>%
  mutate(lag = lag(X1),
         change = X1 - lag,
         direction = ifelse(change > 0, "increased", "decreased"))

day1_data %>% group_by(direction) %>%
  summarise(count = n())

day1_data_roll <- day1_data %>%
  mutate(roll_sum = RcppRoll::roll_sum(X1, 3, align = "right", fill = NA),
         lag_roll_sum = lag(roll_sum),
         change_roll_sum = roll_sum - lag_roll_sum,
         direction_roll_sum = ifelse(change_roll_sum > 0, "increased", "decreased"))

day1_data_roll %>% group_by(direction_roll_sum) %>%
  summarise(count = n())
