library(tidyverse)

data_day2 <- readr::read_delim("Day2_data.txt", delim=" ", col_names = FALSE)
names(data_day2) <- c("direction", "magnitude")

# add columns specifying only the depth and forward position changes
distance_data <- data_day2 %>%
  mutate(depth_change = case_when(direction == "down" ~ magnitude,
                                  direction == "up" ~ -magnitude,
                                  direction == "forward" ~ 0),
         forward_change = ifelse(direction == "forward", magnitude, 0))

# summarise to find the overall depth and forward movement
distance_summary <- distance_data %>% 
  summarise(depth = sum(depth_change),
            forward = sum(forward_change))

# answer to part 1 is the 2 parts multiplied
distance_summary["depth"][[1]]*distance_summary["forward"][[1]]

# now we find out that aim is the current angle of the submarine based on up and down moves
# can just use cumsum on the depth change column to find the current aim
# then new depth data is the current aim multipled by each forward move
aim_data <- distance_data %>%
  mutate(aim = cumsum(depth_change),
         new_depth_change = aim*forward_change)

# summarise and get the answer as before
new_distance_summary <- aim_data %>%
  summarise(depth = sum(new_depth_change),
            forward = sum(forward_change))
new_distance_summary["depth"][[1]]*new_distance_summary["forward"][[1]]
