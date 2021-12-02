library(tidyverse)

data_day2 <- readr::read_delim("Day2_data.txt", delim=" ", col_names = FALSE)
names(data_day2) <- c("direction", "magnitude")

distance_data <- data_day2 %>%
  mutate(depth_change = case_when(direction == "down" )