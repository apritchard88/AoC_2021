library(tidyverse)
library(RcppRoll)

day1_data <- readr::read_csv("Day1_data.csv", col_names = FALSE)

# first question wants to find how many times there is an increase from previous value
# can just use lag function to do this
day1_data <- day1_data %>%
  mutate(lag = lag(X1),
         change = X1 - lag,
         direction = ifelse(change > 0, "increased", "decreased"))

# now group and summarise to find number of increased
day1_data %>% group_by(direction) %>%
  summarise(count = n())

# part 2 wants us to find how often the rolling 3 day sum increases
# can use the roll_sum function from RcppRoll package to calculate
# then same lag method as previous
day1_data_roll <- day1_data %>%
  mutate(roll_sum = RcppRoll::roll_sum(X1, 3, align = "right", fill = NA),
         lag_roll_sum = lag(roll_sum),
         change_roll_sum = roll_sum - lag_roll_sum,
         direction_roll_sum = ifelse(change_roll_sum > 0, "increased", "decreased"))

day1_data_roll %>% group_by(direction_roll_sum) %>%
  summarise(count = n())
